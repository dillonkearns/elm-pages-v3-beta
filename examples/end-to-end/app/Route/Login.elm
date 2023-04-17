module Route.Login exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Form
import Form.Field as Field
import Form.FieldView
import Form.Handler
import Form.Validation as Validation exposing (Validation)
import Head
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import MySession
import Pages.Form
import PagesMsg exposing (PagesMsg)
import Route
import RouteBuilder exposing (App, StatefulRoute, StatelessRoute)
import Server.Request as Request
import Server.Response as Response exposing (Response)
import Server.Session as Session
import Shared
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias ActionData =
    { errors : Form.ServerResponse String
    }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.serverRender
        { head = head
        , data = data
        , action = action
        }
        |> RouteBuilder.buildNoState { view = view }


action : RouteParams -> Request.Parser (BackendTask FatalError (Response ActionData ErrorPage))
action routeParams =
    Request.formDataWithServerValidation (form |> Form.Handler.init identity)
        |> MySession.withSession
            (\nameResultData session ->
                nameResultData
                    |> BackendTask.map
                        (\nameResult ->
                            case nameResult of
                                Err errors ->
                                    ( session
                                        |> Result.withDefault Session.empty
                                    , Response.render
                                        { errors = errors
                                        }
                                    )

                                Ok ( _, name ) ->
                                    ( session
                                        |> Result.withDefault Session.empty
                                        |> Session.insert "name" name
                                        |> Session.withFlash "message" ("Welcome " ++ name ++ "!")
                                    , Route.redirectTo Route.Greet
                                    )
                        )
            )


type alias Data =
    { username : Maybe String
    , flashMessage : Maybe String
    }


form : Pages.Form.FormWithServerValidations String String input (List (Html msg))
form =
    Form.form
        (\username ->
            { combine =
                Validation.succeed identity
                    |> Validation.andMap username
                    |> Validation.map
                        (\clientValidated ->
                            BackendTask.succeed
                                (Validation.succeed clientValidated
                                    |> Validation.withErrorIf
                                        (clientValidated == "error")
                                        username
                                        "Invalid username"
                                )
                        )
            , view =
                \formState ->
                    let
                        errors : Validation.Field String parsed anyKind -> List String
                        errors field =
                            formState.errors
                                |> Form.errorsForField field

                        errorsView : Validation.Field String parsed anyKind -> Html msg
                        errorsView field =
                            case
                                ( formState.submitAttempted
                                , errors field
                                )
                            of
                                ( _, first :: rest ) ->
                                    Html.div []
                                        [ Html.ul
                                            [ Attr.style "border" "solid red"
                                            ]
                                            (List.map
                                                (\error ->
                                                    Html.li []
                                                        [ Html.text error
                                                        ]
                                                )
                                                (first :: rest)
                                            )
                                        ]

                                _ ->
                                    Html.div [] []

                        fieldView : String -> Validation.Field String parsed Form.FieldView.Input -> Html msg
                        fieldView label field =
                            Html.div []
                                [ Html.label []
                                    [ Html.text (label ++ " ")
                                    , field |> Form.FieldView.inputStyled []
                                    ]
                                , errorsView field
                                ]
                    in
                    [ fieldView "Username" username
                    , Html.button []
                        [ (if formState.submitting then
                            "Logging in..."

                           else
                            "Log in"
                          )
                            |> Html.text
                        ]
                    ]
            }
        )
        |> Form.field "name" (Field.text |> Field.required "Required")


data : RouteParams -> Request.Parser (BackendTask FatalError (Response Data ErrorPage))
data routeParams =
    Request.oneOf
        [ Request.succeed ()
            |> MySession.withSession
                (\() session ->
                    case session of
                        Ok okSession ->
                            let
                                flashMessage : Maybe String
                                flashMessage =
                                    okSession
                                        |> Session.get "message"
                            in
                            ( okSession
                            , Data
                                (okSession |> Session.get "name")
                                flashMessage
                                |> Response.render
                            )
                                |> BackendTask.succeed

                        _ ->
                            ( Session.empty
                            , { username = Nothing, flashMessage = Nothing }
                                |> Response.render
                            )
                                |> BackendTask.succeed
                )
        ]


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    []


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app shared =
    { title = "Login"
    , body =
        [ app.data.flashMessage
            |> Maybe.map (\message -> flashView (Ok message))
            |> Maybe.withDefault (Html.p [] [ Html.text "No flash" ])
        , Html.p []
            [ Html.text
                (case app.data.username of
                    Just username ->
                        "Hello " ++ username ++ "!"

                    Nothing ->
                        "You aren't logged in yet."
                )
            ]
        , form
            |> Pages.Form.renderStyledHtml
                []
                Pages.Form.Serial
                (Form.options "form"
                    |> Form.withServerResponse (app.action |> Maybe.map .errors)
                )
                app
        ]
    }


flashView : Result String String -> Html msg
flashView message =
    Html.p
        [ Attr.style "background-color" "rgb(163 251 163)"
        ]
        [ Html.text <|
            case message of
                Ok okMessage ->
                    okMessage

                Err error ->
                    "Something went wrong: " ++ error
        ]
