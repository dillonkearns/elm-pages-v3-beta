module Route.Login exposing (ActionData, Data, Model, Msg, route)

import Api.Scalar exposing (Uuid(..))
import BackendTask exposing (BackendTask)
import BackendTask.Env
import BackendTask.Http
import BackendTask.Port
import Data.Session
import Dict exposing (Dict)
import EmailAddress exposing (EmailAddress)
import ErrorPage exposing (ErrorPage)
import Form
import Form.Field as Field
import Form.FieldView
import Form.Validation as Validation exposing (Combined, Field)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Json.Encode as Encode
import List.Nonempty
import MySession
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Request.Hasura
import Route
import RouteBuilder exposing (StatelessRoute, StaticPayload)
import SendGrid
import Server.Request as Request
import Server.Response exposing (Response)
import Server.Session as Session exposing (Session)
import Shared
import String.Nonempty exposing (NonemptyString)
import Time
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.serverRender
        { head = head
        , data = data
        , action = action
        }
        |> RouteBuilder.buildNoState { view = view }


now : BackendTask Time.Posix
now =
    BackendTask.Port.get "now"
        Encode.null
        (Decode.int |> Decode.map Time.millisToPosix)


emailToMagicLink : EmailAddress -> String -> BackendTask String
emailToMagicLink email baseUrl =
    now
        |> BackendTask.andThen
            (\now_ ->
                BackendTask.Port.get "encrypt"
                    (Encode.object
                        [ ( "text", Encode.string (EmailAddress.toString email) )
                        , ( "expiresAt", (Time.posixToMillis now_ + (1000 * 60 * 30)) |> Encode.int )
                        ]
                        |> Encode.encode 0
                        |> Encode.string
                    )
                    (Decode.string
                        |> Decode.map
                            (\encryptedString ->
                                baseUrl ++ "/login?magic=" ++ encryptedString
                            )
                    )
            )


type alias EnvVariables =
    { sendGridKey : String
    , siteUrl : String
    }


form : Form.DoneForm String (BackendTask (Combined String EmailAddress)) data (List (Html (Pages.Msg.Msg Msg)))
form =
    Form.init
        (\fieldEmail ->
            { combine =
                Validation.succeed
                    (\email ->
                        BackendTask.map2 EnvVariables
                            (BackendTask.Env.expect "TODOS_SEND_GRID_KEY")
                            (BackendTask.Env.get "BASE_URL"
                                |> BackendTask.map (Maybe.withDefault "http://localhost:1234")
                            )
                            |> BackendTask.andThen (sendEmailBackendTask email)
                            |> BackendTask.map
                                (\emailSendResult ->
                                    case emailSendResult of
                                        Ok () ->
                                            Validation.succeed email

                                        Err error ->
                                            Validation.fail "Whoops, something went wrong sending an email to that address. Try again?" Validation.global
                                )
                    )
                    |> Validation.andMap
                        (fieldEmail
                            |> Validation.map (EmailAddress.fromString >> Result.fromMaybe "Invalid email address")
                            |> Validation.fromResult
                        )
            , view =
                \info ->
                    [ fieldEmail |> fieldView info "Email"
                    , globalErrors info
                    , Html.button []
                        [ if info.isTransitioning then
                            Html.text "Logging in..."

                          else
                            Html.text "Login"
                        ]
                    ]
            }
        )
        |> Form.field "email" (Field.text |> Field.email |> Field.required "Required")
        |> Form.hiddenKind ( "kind", "login" ) "Expected kind"


logoutForm : Form.DoneForm String () data (List (Html (Pages.Msg.Msg Msg)))
logoutForm =
    Form.init
        { combine =
            Validation.succeed ()
        , view =
            \info ->
                [ Html.button []
                    [ if info.isTransitioning then
                        Html.text "Logging out..."

                      else
                        Html.text "Log out"
                    ]
                ]
        }
        |> Form.hiddenKind ( "kind", "logout" ) "Expected kind"


fieldView :
    Form.Context String data
    -> String
    -> Field String parsed Form.FieldView.Input
    -> Html msg
fieldView formState label field =
    Html.div []
        [ Html.label []
            [ Html.text (label ++ " ")
            , field |> Form.FieldView.input []
            ]
        , errorsForField formState field
        ]


errorsForField : Form.Context String data -> Field String parsed kind -> Html msg
errorsForField formState field =
    (if formState.submitAttempted then
        formState.errors
            |> Form.errorsForField field
            |> List.map (\error -> Html.li [] [ Html.text error ])

     else
        []
    )
        |> Html.ul [ Attr.style "color" "red" ]


globalErrors : Form.Context String data -> Html msg
globalErrors formState =
    formState.errors
        |> Form.errorsForField Validation.global
        |> List.map (\error -> Html.li [] [ Html.text error ])
        |> Html.ul [ Attr.style "color" "red" ]


data : RouteParams -> Request.Parser (BackendTask (Response Data ErrorPage))
data routeParams =
    Request.queryParam "magic"
        |> MySession.withSession
            (\magicLinkHash session ->
                let
                    okSessionThing : Session
                    okSessionThing =
                        session
                            |> Result.withDefault Session.empty

                    maybeSessionId : Maybe String
                    maybeSessionId =
                        okSessionThing
                            |> Session.get "sessionId"
                in
                case magicLinkHash of
                    Just magicHash ->
                        parseMagicHashIfNotExpired magicHash
                            |> BackendTask.andThen
                                (\emailIfValid ->
                                    case maybeSessionId of
                                        Just sessionId ->
                                            Data.Session.get sessionId
                                                |> Request.Hasura.backendTask
                                                |> BackendTask.map
                                                    (\maybeUserSession ->
                                                        ( okSessionThing
                                                        , maybeUserSession
                                                            |> Maybe.map .emailAddress
                                                            |> Data
                                                            |> Server.Response.render
                                                        )
                                                    )

                                        Nothing ->
                                            case emailIfValid of
                                                Just confirmedEmail ->
                                                    Data.Session.findOrCreateUser confirmedEmail
                                                        |> Request.Hasura.mutationBackendTask
                                                        |> BackendTask.andThen
                                                            (\userId ->
                                                                now
                                                                    |> BackendTask.andThen
                                                                        (\now_ ->
                                                                            let
                                                                                expirationTime : Time.Posix
                                                                                expirationTime =
                                                                                    Time.millisToPosix (Time.posixToMillis now_ + (1000 * 60 * 30))
                                                                            in
                                                                            Data.Session.create expirationTime userId
                                                                                |> Request.Hasura.mutationBackendTask
                                                                        )
                                                            )
                                                        |> BackendTask.map
                                                            (\(Uuid sessionId) ->
                                                                ( okSessionThing
                                                                    |> Session.insert "sessionId" sessionId
                                                                , Route.Visibility__ { visibility = Nothing }
                                                                    |> Route.redirectTo
                                                                )
                                                            )

                                                Nothing ->
                                                    BackendTask.succeed
                                                        ( okSessionThing
                                                          -- TODO give flash message saying it was an invalid magic link
                                                        , Nothing
                                                            |> Data
                                                            |> Server.Response.render
                                                        )
                                )

                    Nothing ->
                        maybeSessionId
                            |> Maybe.map (Data.Session.get >> Request.Hasura.backendTask)
                            |> Maybe.withDefault (BackendTask.succeed Nothing)
                            |> BackendTask.map
                                (\maybeUserSession ->
                                    ( okSessionThing
                                    , maybeUserSession
                                        |> Maybe.map .emailAddress
                                        |> Data
                                        |> Server.Response.render
                                    )
                                )
            )


allForms : Form.ServerForms String (BackendTask (Combined String Action))
allForms =
    logoutForm
        |> Form.toServerForm
        |> Form.initCombinedServer (\_ -> Logout)
        |> Form.combineServer LogIn form


action : RouteParams -> Request.Parser (BackendTask (Response ActionData ErrorPage))
action routeParams =
    Request.map2 Tuple.pair
        (Request.oneOf
            [ Request.formDataWithServerValidation allForms
            ]
        )
        Request.requestTime
        |> MySession.withSession
            (\( resolveFormBackendTask, requestTime ) session ->
                resolveFormBackendTask
                    |> BackendTask.andThen
                        (\usernameResult ->
                            let
                                okSession =
                                    session
                                        |> Result.withDefault Session.empty
                            in
                            case usernameResult of
                                Err (Form.Response error) ->
                                    ( okSession
                                    , Server.Response.render
                                        { maybeError = Just error
                                        , sentLink = False
                                        }
                                    )
                                        |> BackendTask.succeed

                                Ok ( _, Logout ) ->
                                    ( Session.empty
                                    , Route.redirectTo Route.Login
                                    )
                                        |> BackendTask.succeed

                                Ok ( _, LogIn emailAddress ) ->
                                    ( okSession
                                    , { maybeError = Nothing
                                      , sentLink = True
                                      }
                                        |> Server.Response.render
                                    )
                                        |> BackendTask.succeed
                        )
            )


type Action
    = LogIn EmailAddress
    | Logout


head :
    StaticPayload Data ActionData RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Login | elm-pages Todo List"
        , image =
            { url = Pages.Url.external "https://images.unsplash.com/photo-1499750310107-5fef28a66643?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=600&q=80"
            , alt = "Desk with a Todo List"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Login to manage your todo list in full-stack Elm!"
        , locale = Nothing
        , title = "Login | elm-pages Todo List"
        }
        |> Seo.website


type alias Data =
    { username : Maybe String
    }


type alias ActionData =
    { maybeError :
        Maybe
            { fields : List ( String, String )
            , errors : Dict String (List String)
            }
    , sentLink : Bool
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data ActionData RouteParams
    -> View (Pages.Msg.Msg Msg)
view _ sharedModel app =
    { title = "Login"
    , body =
        [ if app.action |> Maybe.map .sentLink |> Maybe.withDefault False then
            Html.text "Check your inbox for your login link!"

          else
            Html.div []
                [ Html.p []
                    [ case app.data.username of
                        Just username ->
                            Html.div []
                                [ Html.text <| "Hello! You are already logged in as " ++ username
                                , logoutForm
                                    |> Form.toDynamicTransition "logout"
                                    |> Form.renderHtml []
                                        Nothing
                                        app
                                        ()
                                ]

                        Nothing ->
                            Html.text "You aren't logged in yet."
                    ]
                , form
                    |> Form.toDynamicTransition "login"
                    |> Form.renderHtml []
                        (app.action
                            |> Maybe.andThen .maybeError
                        )
                        app
                        ()
                ]
        ]
    }


sendFake : Bool
sendFake =
    False


sendEmailBackendTask : EmailAddress -> EnvVariables -> BackendTask (Result SendGrid.Error ())
sendEmailBackendTask recipient env =
    emailToMagicLink recipient env.siteUrl
        |> BackendTask.andThen
            (\magicLinkString ->
                let
                    message : NonemptyString
                    message =
                        String.Nonempty.NonemptyString 'W' ("elcome! Please confirm that this is your email address.\n" ++ magicLinkString)
                in
                if sendFake then
                    message
                        |> String.Nonempty.toString
                        |> log
                        |> BackendTask.map Ok

                else
                    let
                        senderEmail : Maybe EmailAddress
                        senderEmail =
                            EmailAddress.fromString "dillon@incrementalelm.com"
                    in
                    senderEmail
                        |> Maybe.map
                            (\justSender ->
                                SendGrid.textEmail
                                    { subject = String.Nonempty.NonemptyString 'T' "odo app login"
                                    , to = List.Nonempty.fromElement recipient
                                    , content = message
                                    , nameOfSender = "Todo App"
                                    , emailAddressOfSender = justSender
                                    }
                                    |> sendEmail env.sendGridKey
                            )
                        |> Maybe.withDefault (BackendTask.fail "Expected a valid sender email address.")
            )


sendEmail :
    String
    -> SendGrid.Email
    -> BackendTask (Result SendGrid.Error ())
sendEmail apiKey_ email_ =
    BackendTask.Http.requestWithOptions
        { method = "POST"
        , headers = [ ( "Authorization", "Bearer " ++ apiKey_ ) ]
        , url = SendGrid.sendGridApiUrl
        , body = SendGrid.encodeSendEmail email_ |> BackendTask.Http.jsonBody
        }
        BackendTask.Http.expectStringResponse
        |> BackendTask.map
            (\response ->
                case response of
                    BackendTask.Http.BadUrl_ url ->
                        SendGrid.BadUrl url |> Err

                    BackendTask.Http.Timeout_ ->
                        Err SendGrid.Timeout

                    BackendTask.Http.NetworkError_ ->
                        Err SendGrid.NetworkError

                    BackendTask.Http.BadStatus_ metadata body ->
                        SendGrid.decodeBadStatus metadata body |> Err

                    BackendTask.Http.GoodStatus_ _ _ ->
                        Ok ()
            )


parseMagicHash : String -> BackendTask ( String, Time.Posix )
parseMagicHash magicHash =
    BackendTask.Port.get "decrypt"
        (Encode.string magicHash)
        (Decode.string
            |> Decode.map
                (Decode.decodeString
                    (Decode.map2 Tuple.pair
                        (Decode.field "text" Decode.string)
                        (Decode.field "expiresAt" (Decode.int |> Decode.map Time.millisToPosix))
                    )
                    >> Result.mapError Decode.errorToString
                )
        )
        |> BackendTask.andThen BackendTask.fromResult


parseMagicHashIfNotExpired : String -> BackendTask (Maybe String)
parseMagicHashIfNotExpired magicHash =
    BackendTask.map2
        (\( email, expiresAt ) currentTime ->
            let
                isExpired : Bool
                isExpired =
                    Time.posixToMillis currentTime > Time.posixToMillis expiresAt
            in
            if isExpired then
                Nothing

            else
                Just email
        )
        (parseMagicHash magicHash)
        now


log : String -> BackendTask ()
log message =
    BackendTask.Port.get "log"
        (Encode.string message)
        (Decode.succeed ())
