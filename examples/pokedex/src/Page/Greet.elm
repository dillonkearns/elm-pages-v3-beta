module Page.Greet exposing (Data, Model, Msg, page)

import Codec exposing (Codec)
import DataSource exposing (DataSource)
import DataSource.Http
import Dict exposing (Dict)
import Head
import Head.Seo as Seo
import Html
import Html.Attributes as Attr
import Json.Decode
import Json.Encode
import OptimizedDecoder
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Secrets as Secrets
import Pages.Url
import Server.Request as Request exposing (Request)
import Server.Response exposing (Response)
import Server.SetCookie as SetCookie
import Session exposing (Session)
import Shared
import Time
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.serverRender
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


withSession :
    { name : String
    , secrets : Secrets.Value (List String)
    , sameSite : String
    }
    -> Session.Decoder decoded
    -> (DataSource (Result String decoded) -> DataSource ( Session.SessionUpdate, Response data ))
    -> Request (DataSource (Response data))
withSession config decoder toRequest =
    Request.cookie config.name
        |> Request.map
            (\maybeSessionCookie ->
                let
                    decrypted : DataSource (Result String decoded)
                    decrypted =
                        case maybeSessionCookie of
                            Just sessionCookie ->
                                decrypt config.secrets decoder sessionCookie
                                    |> DataSource.map Ok

                            Nothing ->
                                Err "TODO"
                                    |> DataSource.succeed

                    decryptedFull : DataSource (Dict String OptimizedDecoder.Value)
                    decryptedFull =
                        maybeSessionCookie
                            |> Maybe.map
                                (\sessionCookie -> decrypt config.secrets (OptimizedDecoder.dict OptimizedDecoder.value) sessionCookie)
                            |> Maybe.withDefault (DataSource.succeed Dict.empty)
                in
                decryptedFull
                    |> DataSource.andThen
                        (\cookieDict ->
                            DataSource.andThen
                                (\( sessionUpdate, response ) ->
                                    let
                                        encodedCookie : Json.Encode.Value
                                        encodedCookie =
                                            Session.setValues sessionUpdate cookieDict
                                    in
                                    DataSource.map2
                                        (\encoded originalCookieValues ->
                                            response
                                                |> Server.Response.withSetCookieHeader
                                                    (SetCookie.setCookie config.name encoded
                                                        |> SetCookie.httpOnly
                                                        |> SetCookie.withPath "/"
                                                     -- TODO set expiration time
                                                     -- TODO do I need to encrypt the session expiration as part of it
                                                     -- TODO should I update the expiration time every time?
                                                     --|> SetCookie.withExpiration (Time.millisToPosix 100000000000)
                                                    )
                                        )
                                        (encrypt config.secrets encodedCookie)
                                        decryptedFull
                                )
                                (toRequest decrypted)
                        )
            )


encrypt : Secrets.Value (List String) -> Json.Encode.Value -> DataSource.DataSource String
encrypt secrets input =
    DataSource.Http.request
        (secrets
            |> Secrets.map
                (\secretList ->
                    { url = "port://encrypt"
                    , method = "GET"
                    , headers = []

                    -- TODO pass through secrets here
                    , body =
                        DataSource.Http.jsonBody
                            (Json.Encode.object
                                [ ( "values", input )
                                , ( "secret"
                                  , Json.Encode.string
                                        (secretList
                                            |> List.head
                                            -- TODO use different default - require non-empty list?
                                            |> Maybe.withDefault ""
                                        )
                                  )
                                ]
                            )
                    }
                )
        )
        OptimizedDecoder.string


decrypt : Secrets.Value (List String) -> OptimizedDecoder.Decoder a -> String -> DataSource a
decrypt secrets decoder input =
    DataSource.Http.request
        (secrets
            |> Secrets.map
                (\secretList ->
                    { url = "port://decrypt"
                    , method = "GET"
                    , headers = []
                    , body =
                        DataSource.Http.jsonBody
                            (Json.Encode.object
                                [ ( "input", Json.Encode.string input )
                                , ( "secrets", Json.Encode.list Json.Encode.string secretList )
                                ]
                            )
                    }
                )
        )
        decoder


keys =
    { userId = ( "userId", Codec.int )
    }


data : RouteParams -> Request.Request (DataSource (Server.Response.Response Data))
data routeParams =
    Request.oneOf
        [ --Request.map2 Data
          --    (Request.expectQueryParam "name")
          --    Request.requestTime
          --    |> Request.map
          --        (\requestData ->
          --            requestData
          --                |> Server.Response.render
          --                |> Server.Response.withHeader
          --                    "x-greeting"
          --                    ("hello there " ++ requestData.username ++ "!")
          --                |> DataSource.succeed
          --        )
          --, Request.map2 Data
          --    (Request.expectCookie "username")
          --    Request.requestTime
          --    |> Request.map
          --        (\requestData ->
          --            requestData
          --                |> Server.Response.render
          --                |> Server.Response.withHeader
          --                    "x-greeting"
          --                    ("hello " ++ requestData.username ++ "!")
          --                |> DataSource.succeed
          --        ),
          withSession
            { name = "mysession"
            , secrets =
                Secrets.succeed
                    [ "secret4", "secret3", "secret2" ]
            , sameSite = "lax" -- TODO custom type
            }
            (OptimizedDecoder.field "userId" OptimizedDecoder.int)
            (\decryptSession ->
                decryptSession
                    |> DataSource.andThen
                        (\userIdResult ->
                            case userIdResult of
                                Err error ->
                                    DataSource.succeed
                                        ( Session.oneUpdate "userId" (Json.Encode.int 456)
                                          --, Server.Response.temporaryRedirect "/login"
                                        , { username = "NO USER"
                                          , requestTime = Time.millisToPosix 0
                                          }
                                            |> Server.Response.render
                                        )

                                Ok userId ->
                                    DataSource.succeed
                                        ( --Session.oneUpdate "userId" (Json.Encode.int 456)
                                          Session.noUpdates
                                        , --Server.Response.temporaryRedirect "/login"
                                          { username = String.fromInt userId
                                          , requestTime = Time.millisToPosix 0
                                          }
                                            |> Server.Response.render
                                        )
                        )
            )
        ]


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


type alias Data =
    { username : String
    , requestTime : Time.Posix
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "Hello!"
    , body =
        [ Html.text <| "Hello " ++ static.data.username ++ "!"
        , Html.text <| "Requested page at " ++ String.fromInt (Time.posixToMillis static.data.requestTime)
        , Html.div []
            [ Html.form
                [ Attr.method "post"
                , Attr.action "/api/logout"
                ]
                [ Html.button
                    [ Attr.type_ "submit"
                    ]
                    [ Html.text "Logout" ]
                ]
            ]
        ]
    }
