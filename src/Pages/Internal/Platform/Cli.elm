module Pages.Internal.Platform.Cli exposing (Flags, Model, Msg(..), Program, cliApplication, init, requestDecoder, update, currentCompatibilityKey)

{-| Exposed for internal use only (used in generated code).

@docs Flags, Model, Msg, Program, cliApplication, init, requestDecoder, update, currentCompatibilityKey

-}

import ApiRoute
import BuildError exposing (BuildError)
import Bytes exposing (Bytes)
import Bytes.Encode
import Codec
import DataSource exposing (DataSource)
import Dict
import Head
import Html exposing (Html)
import HtmlPrinter
import Internal.ApiRoute exposing (ApiRoute(..))
import Json.Decode as Decode
import Json.Encode
import PageServerResponse exposing (PageServerResponse)
import Pages.Flags
import Pages.Http
import Pages.Internal.NotFoundReason as NotFoundReason exposing (NotFoundReason)
import Pages.Internal.Platform.CompatibilityKey
import Pages.Internal.Platform.Effect as Effect exposing (Effect)
import Pages.Internal.Platform.StaticResponses as StaticResponses exposing (StaticResponses)
import Pages.Internal.Platform.ToJsPayload as ToJsPayload
import Pages.Internal.ResponseSketch as ResponseSketch
import Pages.Internal.StaticHttpBody as StaticHttpBody
import Pages.Msg
import Pages.ProgramConfig exposing (ProgramConfig)
import Pages.SiteConfig exposing (SiteConfig)
import Pages.StaticHttp.Request
import Pages.StaticHttpRequest as StaticHttpRequest exposing (RequestResult(..))
import Path exposing (Path)
import RenderRequest exposing (RenderRequest)
import RequestsAndPending exposing (RequestsAndPending)
import Task
import TerminalText as Terminal
import Url exposing (Url)


{-| -}
type alias Flags =
    Decode.Value


{-| -}
currentCompatibilityKey : Int
currentCompatibilityKey =
    Pages.Internal.Platform.CompatibilityKey.currentCompatibilityKey


{-| -}
type alias Model route =
    { staticResponses : StaticResponses
    , errors : List BuildError
    , allRawResponses : RequestsAndPending
    , unprocessedPages : List ( Path, route )
    , maybeRequestJson : RenderRequest route
    , isDevServer : Bool
    }


{-| -}
type Msg
    = GotDataBatch
        (List
            { request : Pages.StaticHttp.Request.Request
            , response : RequestsAndPending.Response
            }
        )
    | GotBuildError BuildError


{-| -}
type alias Program route =
    Platform.Program Flags (Model route) Msg


{-| -}
cliApplication :
    ProgramConfig userMsg userModel (Maybe route) pageData actionData sharedData effect mappedMsg errorPage
    -> Program (Maybe route)
cliApplication config =
    let
        site : SiteConfig
        site =
            getSiteConfig config

        getSiteConfig : ProgramConfig userMsg userModel (Maybe route) pageData actionData sharedData effect mappedMsg errorPage -> SiteConfig
        getSiteConfig fullConfig =
            case fullConfig.site of
                Just mySite ->
                    mySite

                Nothing ->
                    getSiteConfig fullConfig
    in
    Platform.worker
        { init =
            \flags ->
                let
                    renderRequest : RenderRequest (Maybe route)
                    renderRequest =
                        Decode.decodeValue (RenderRequest.decoder config) flags
                            |> Result.withDefault RenderRequest.default
                in
                init site renderRequest config flags
                    |> Tuple.mapSecond (perform site renderRequest config)
        , update =
            \msg model ->
                update site config msg model
                    |> Tuple.mapSecond (perform site model.maybeRequestJson config)
        , subscriptions =
            \_ ->
                Sub.batch
                    [ config.fromJsPort
                        |> Sub.map
                            (\jsonValue ->
                                let
                                    decoder : Decode.Decoder Msg
                                    decoder =
                                        Decode.field "tag" Decode.string
                                            |> Decode.andThen
                                                (\tag ->
                                                    case tag of
                                                        "BuildError" ->
                                                            Decode.field "data"
                                                                (Decode.map2
                                                                    (\message title ->
                                                                        { title = title
                                                                        , message = message
                                                                        , fatal = True
                                                                        , path = "" -- TODO wire in current path here
                                                                        }
                                                                    )
                                                                    (Decode.field "message" Decode.string |> Decode.map Terminal.fromAnsiString)
                                                                    (Decode.field "title" Decode.string)
                                                                )
                                                                |> Decode.map GotBuildError

                                                        _ ->
                                                            Decode.fail "Unhandled msg"
                                                )
                                in
                                Decode.decodeValue decoder jsonValue
                                    |> Result.mapError
                                        (\error ->
                                            ("From location 1: "
                                                ++ (error
                                                        |> Decode.errorToString
                                                   )
                                            )
                                                |> BuildError.internal
                                                |> GotBuildError
                                        )
                                    |> mergeResult
                            )
                    , config.gotBatchSub
                        |> Sub.map
                            (\newBatch ->
                                Decode.decodeValue batchDecoder newBatch
                                    |> Result.map GotDataBatch
                                    |> Result.mapError
                                        (\error ->
                                            ("From location 2: "
                                                ++ (error
                                                        |> Decode.errorToString
                                                   )
                                            )
                                                |> BuildError.internal
                                                |> GotBuildError
                                        )
                                    |> mergeResult
                            )
                    ]
        }


batchDecoder : Decode.Decoder (List { request : Pages.StaticHttp.Request.Request, response : RequestsAndPending.Response })
batchDecoder =
    Decode.map2 (\request response -> { request = request, response = response })
        (Decode.field "request" requestDecoder)
        (Decode.field "response" RequestsAndPending.decoder)
        |> Decode.list


mergeResult : Result a a -> a
mergeResult r =
    case r of
        Ok rr ->
            rr

        Err rr ->
            rr


{-| -}
requestDecoder : Decode.Decoder Pages.StaticHttp.Request.Request
requestDecoder =
    Pages.StaticHttp.Request.codec
        |> Codec.decoder


flatten : SiteConfig -> RenderRequest route -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage -> List Effect -> Cmd Msg
flatten site renderRequest config list =
    Cmd.batch (flattenHelp [] site renderRequest config list)


flattenHelp : List (Cmd Msg) -> SiteConfig -> RenderRequest route -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage -> List Effect -> List (Cmd Msg)
flattenHelp soFar site renderRequest config list =
    case list of
        first :: rest ->
            flattenHelp
                (perform site renderRequest config first :: soFar)
                site
                renderRequest
                config
                rest

        [] ->
            soFar


perform :
    SiteConfig
    -> RenderRequest route
    -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage
    -> Effect
    -> Cmd Msg
perform site renderRequest config effect =
    let
        canonicalSiteUrl : String
        canonicalSiteUrl =
            site.canonicalUrl
    in
    case effect of
        Effect.NoEffect ->
            Cmd.none

        Effect.Batch list ->
            flatten site renderRequest config list

        Effect.FetchHttp unmasked ->
            if unmasked.url == "$$elm-pages$$headers" then
                case
                    renderRequest
                        |> RenderRequest.maybeRequestPayload
                        |> Maybe.map (\json -> RequestsAndPending.Response Nothing (RequestsAndPending.JsonBody json))
                        |> Result.fromMaybe (Pages.Http.BadUrl "$$elm-pages$$headers is only available on server-side request (not on build).")
                of
                    Ok okResponse ->
                        Task.succeed
                            [ { request = unmasked
                              , response = okResponse
                              }
                            ]
                            |> Task.perform GotDataBatch

                    Err error ->
                        { title = "Static HTTP Error"
                        , message =
                            [ Terminal.text "I got an error making an HTTP request to this URL: "

                            -- TODO include HTTP method, headers, and body
                            , Terminal.yellow unmasked.url
                            , Terminal.text <| Json.Encode.encode 2 <| StaticHttpBody.encode unmasked.body
                            , Terminal.text "\n\n"
                            , case error of
                                Pages.Http.BadStatus metadata body ->
                                    Terminal.text <|
                                        String.join "\n"
                                            [ "Bad status: " ++ String.fromInt metadata.statusCode
                                            , "Status message: " ++ metadata.statusText
                                            , "Body: " ++ body
                                            ]

                                Pages.Http.BadUrl _ ->
                                    -- TODO include HTTP method, headers, and body
                                    Terminal.text <| "Invalid url: " ++ unmasked.url

                                Pages.Http.Timeout ->
                                    Terminal.text "Timeout"

                                Pages.Http.NetworkError ->
                                    Terminal.text "Network error"
                            ]
                        , fatal = True
                        , path = "" -- TODO wire in current path here
                        }
                            |> Task.succeed
                            |> Task.perform GotBuildError

            else
                ToJsPayload.DoHttp unmasked unmasked.useCache
                    |> Codec.encoder (ToJsPayload.successCodecNew2 canonicalSiteUrl "")
                    |> config.toJsPort
                    |> Cmd.map never

        Effect.SendSinglePage info ->
            let
                currentPagePath : String
                currentPagePath =
                    case info of
                        ToJsPayload.PageProgress toJsSuccessPayloadNew ->
                            toJsSuccessPayloadNew.route

                        _ ->
                            ""
            in
            info
                |> Codec.encoder (ToJsPayload.successCodecNew2 canonicalSiteUrl currentPagePath)
                |> config.toJsPort
                |> Cmd.map never

        Effect.SendSinglePageNew rawBytes info ->
            let
                currentPagePath : String
                currentPagePath =
                    case info of
                        ToJsPayload.PageProgress toJsSuccessPayloadNew ->
                            toJsSuccessPayloadNew.route

                        _ ->
                            ""
            in
            { oldThing =
                info
                    |> Codec.encoder (ToJsPayload.successCodecNew2 canonicalSiteUrl currentPagePath)
            , binaryPageData = rawBytes
            }
                |> config.sendPageData
                |> Cmd.map never

        Effect.Continue ->
            Cmd.none


flagsDecoder :
    Decode.Decoder
        { staticHttpCache : RequestsAndPending
        , isDevServer : Bool
        , compatibilityKey : Int
        }
flagsDecoder =
    Decode.map3
        (\staticHttpCache isDevServer compatibilityKey ->
            { staticHttpCache = staticHttpCache
            , isDevServer = isDevServer
            , compatibilityKey = compatibilityKey
            }
        )
        --(Decode.field "staticHttpCache"
        --    (Decode.dict
        --        (Decode.string
        --            |> Decode.map Just
        --        )
        --    )
        --)
        -- TODO remove hardcoding and decode staticHttpCache here
        (Decode.succeed Dict.empty)
        (Decode.field "mode" Decode.string |> Decode.map (\mode -> mode == "dev-server"))
        (Decode.field "compatibilityKey" Decode.int)


{-| -}
init :
    SiteConfig
    -> RenderRequest route
    -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage
    -> Decode.Value
    -> ( Model route, Effect )
init site renderRequest config flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok { staticHttpCache, isDevServer, compatibilityKey } ->
            if compatibilityKey == currentCompatibilityKey then
                initLegacy site renderRequest { staticHttpCache = staticHttpCache, isDevServer = isDevServer } config

            else
                let
                    elmPackageAheadOfNpmPackage : Bool
                    elmPackageAheadOfNpmPackage =
                        currentCompatibilityKey > compatibilityKey

                    message : String
                    message =
                        "The NPM package and Elm package you have installed are incompatible. If you are updating versions, be sure to update both the elm-pages Elm and NPM package.\n\n"
                            ++ (if elmPackageAheadOfNpmPackage then
                                    "The elm-pages Elm package is ahead of the elm-pages NPM package. Try updating the elm-pages NPM package?"

                                else
                                    "The elm-pages NPM package is ahead of the elm-pages Elm package. Try updating the elm-pages Elm package?"
                               )
                in
                updateAndSendPortIfDone
                    site
                    config
                    { staticResponses = StaticResponses.empty
                    , errors =
                        [ { title = "Incompatible NPM and Elm package versions"
                          , message = [ Terminal.text <| message ]
                          , fatal = True
                          , path = ""
                          }
                        ]
                    , allRawResponses = Dict.empty
                    , unprocessedPages = []
                    , maybeRequestJson = renderRequest
                    , isDevServer = False
                    }

        Err error ->
            updateAndSendPortIfDone
                site
                config
                { staticResponses = StaticResponses.empty
                , errors =
                    [ { title = "Internal Error"
                      , message = [ Terminal.text <| "Failed to parse flags: " ++ Decode.errorToString error ]
                      , fatal = True
                      , path = ""
                      }
                    ]
                , allRawResponses = Dict.empty
                , unprocessedPages = []
                , maybeRequestJson = renderRequest
                , isDevServer = False
                }


type ActionRequest
    = ActionResponseRequest
    | ActionOnlyRequest


isActionDecoder : Decode.Decoder (Maybe ActionRequest)
isActionDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "method" Decode.string)
        (Decode.field "headers" (Decode.dict Decode.string))
        |> Decode.map
            (\( method, headers ) ->
                case method |> String.toUpper of
                    "GET" ->
                        Nothing

                    "OPTIONS" ->
                        Nothing

                    _ ->
                        let
                            actionOnly : Bool
                            actionOnly =
                                case headers |> Dict.get "elm-pages-action-only" of
                                    Just _ ->
                                        True

                                    Nothing ->
                                        False
                        in
                        Just
                            (if actionOnly then
                                ActionOnlyRequest

                             else
                                ActionResponseRequest
                            )
            )


initLegacy :
    SiteConfig
    -> RenderRequest route
    -> { staticHttpCache : RequestsAndPending, isDevServer : Bool }
    -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage
    -> ( Model route, Effect )
initLegacy site renderRequest { staticHttpCache, isDevServer } config =
    let
        staticResponses : StaticResponses
        staticResponses =
            case renderRequest of
                RenderRequest.SinglePage _ singleRequest _ ->
                    let
                        globalHeadTags : DataSource (List Head.Tag)
                        globalHeadTags =
                            (config.globalHeadTags |> Maybe.withDefault (\_ -> DataSource.succeed [])) HtmlPrinter.htmlToString
                    in
                    case singleRequest of
                        RenderRequest.Page serverRequestPayload ->
                            let
                                isAction : Maybe ActionRequest
                                isAction =
                                    renderRequest
                                        |> RenderRequest.maybeRequestPayload
                                        |> Maybe.andThen (Decode.decodeValue isActionDecoder >> Result.withDefault Nothing)
                            in
                            StaticResponses.renderSingleRoute
                                (case isAction of
                                    Just _ ->
                                        config.action serverRequestPayload.frontmatter
                                            |> DataSource.andThen
                                                (\something ->
                                                    case something of
                                                        PageServerResponse.ErrorPage _ _ ->
                                                            DataSource.succeed something
                                                                |> DataSource.map (\_ -> ())

                                                        PageServerResponse.RenderPage _ _ ->
                                                            DataSource.map3 (\_ _ _ -> ())
                                                                (config.data serverRequestPayload.frontmatter)
                                                                config.sharedData
                                                                globalHeadTags

                                                        PageServerResponse.ServerResponse _ ->
                                                            DataSource.succeed something
                                                                |> DataSource.map (\_ -> ())
                                                )

                                    Nothing ->
                                        DataSource.map3 (\_ _ _ -> ())
                                            (config.data serverRequestPayload.frontmatter)
                                            config.sharedData
                                            globalHeadTags
                                )
                                (if isDevServer then
                                    config.handleRoute serverRequestPayload.frontmatter

                                 else
                                    DataSource.succeed Nothing
                                )

                        RenderRequest.Api ( path, ApiRoute apiRequest ) ->
                            StaticResponses.renderApiRequest
                                (DataSource.map2 (\_ _ -> ())
                                    (apiRequest.matchesToResponse path)
                                    globalHeadTags
                                )

                        RenderRequest.NotFound _ ->
                            StaticResponses.renderApiRequest
                                (DataSource.map2 (\_ _ -> ())
                                    (DataSource.succeed [])
                                    globalHeadTags
                                )

        unprocessedPages : List ( Path, route )
        unprocessedPages =
            case renderRequest of
                RenderRequest.SinglePage _ serverRequestPayload _ ->
                    case serverRequestPayload of
                        RenderRequest.Page pageData ->
                            [ ( pageData.path, pageData.frontmatter ) ]

                        RenderRequest.Api _ ->
                            []

                        RenderRequest.NotFound _ ->
                            []

        initialModel : Model route
        initialModel =
            { staticResponses = staticResponses
            , errors = []
            , allRawResponses = staticHttpCache
            , unprocessedPages = unprocessedPages
            , maybeRequestJson = renderRequest
            , isDevServer = isDevServer
            }
    in
    StaticResponses.nextStep initialModel Nothing
        |> nextStepToEffect site
            config
            initialModel


updateAndSendPortIfDone :
    SiteConfig
    -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage
    -> Model route
    -> ( Model route, Effect )
updateAndSendPortIfDone site config model =
    StaticResponses.nextStep
        model
        Nothing
        |> nextStepToEffect site config model


{-| -}
update :
    SiteConfig
    -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage
    -> Msg
    -> Model route
    -> ( Model route, Effect )
update site config msg model =
    case msg |> Debug.log "msg" of
        GotDataBatch batch ->
            let
                _ =
                    Debug.log "GotDataBatch" batch

                updatedModel : Model route
                updatedModel =
                    model
                        |> StaticResponses.batchUpdate batch
            in
            StaticResponses.nextStep
                updatedModel
                Nothing
                |> Debug.log "update nextStep"
                |> nextStepToEffect site config updatedModel

        GotBuildError buildError ->
            let
                updatedModel : Model route
                updatedModel =
                    { model
                        | errors =
                            buildError :: model.errors
                    }
            in
            StaticResponses.nextStep
                updatedModel
                Nothing
                |> nextStepToEffect site config updatedModel


nextStepToEffect :
    SiteConfig
    -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage
    -> Model route
    -> ( StaticResponses, StaticResponses.NextStep route )
    -> ( Model route, Effect )
nextStepToEffect site config model ( updatedStaticResponsesModel, nextStep ) =
    case nextStep |> Debug.log "nextStep" of
        StaticResponses.Continue updatedAllRawResponses httpRequests maybeRoutes ->
            let
                updatedUnprocessedPages : List ( Path, route )
                updatedUnprocessedPages =
                    case maybeRoutes of
                        Just newRoutes ->
                            newRoutes
                                |> List.map
                                    (\route ->
                                        ( Path.join (config.routeToPath route)
                                        , route
                                        )
                                    )

                        Nothing ->
                            model.unprocessedPages

                updatedModel : Model route
                updatedModel =
                    { model
                        | allRawResponses = updatedAllRawResponses
                        , staticResponses = updatedStaticResponsesModel
                        , unprocessedPages = updatedUnprocessedPages
                    }
            in
            if List.isEmpty httpRequests then
                nextStepToEffect site
                    config
                    updatedModel
                    (StaticResponses.nextStep
                        updatedModel
                        Nothing
                    )

            else
                ( updatedModel
                , (httpRequests
                    |> List.map Effect.FetchHttp
                  )
                    |> Debug.log "newRequests"
                    |> Effect.Batch
                )

        StaticResponses.Finish toJsPayload ->
            case toJsPayload of
                StaticResponses.ApiResponse ->
                    let
                        apiResponse : Effect
                        apiResponse =
                            case model.maybeRequestJson of
                                RenderRequest.SinglePage _ requestPayload _ ->
                                    let
                                        sharedDataResult : Result BuildError sharedData
                                        sharedDataResult =
                                            StaticHttpRequest.resolve config.sharedData model.allRawResponses
                                                |> requestResultToResult
                                                |> Result.mapError (StaticHttpRequest.toBuildError "")
                                    in
                                    case requestPayload of
                                        RenderRequest.Api ( path, ApiRoute apiHandler ) ->
                                            let
                                                thing : DataSource (Maybe ApiRoute.Response)
                                                thing =
                                                    apiHandler.matchesToResponse path
                                            in
                                            StaticHttpRequest.resolve
                                                thing
                                                model.allRawResponses
                                                |> requestResultToResult
                                                |> Result.mapError (StaticHttpRequest.toBuildError "TODO - path from request")
                                                |> (\response ->
                                                        case response of
                                                            Ok (Just okResponse) ->
                                                                { body = okResponse
                                                                , staticHttpCache = Dict.empty -- TODO do I need to serialize the full cache here, or can I handle that from the JS side?

                                                                -- model.allRawResponses |> Dict.Extra.filterMap (\_ v -> v)
                                                                , statusCode = 200
                                                                }
                                                                    |> ToJsPayload.SendApiResponse
                                                                    |> Effect.SendSinglePage

                                                            Ok Nothing ->
                                                                render404Page config (Result.toMaybe sharedDataResult) model (Path.fromString path) NotFoundReason.NoMatchingRoute

                                                            Err error ->
                                                                [ error ]
                                                                    |> ToJsPayload.Errors
                                                                    |> Effect.SendSinglePage
                                                   )

                                        RenderRequest.Page payload ->
                                            let
                                                pageFoundResult : Result BuildError (Maybe NotFoundReason)
                                                pageFoundResult =
                                                    StaticHttpRequest.resolve
                                                        (if model.isDevServer then
                                                            -- TODO OPTIMIZATION this is redundant
                                                            config.handleRoute payload.frontmatter

                                                         else
                                                            DataSource.succeed Nothing
                                                        )
                                                        model.allRawResponses
                                                        |> requestResultToResult
                                                        |> Result.mapError (StaticHttpRequest.toBuildError (payload.path |> Path.toAbsolute))
                                            in
                                            case pageFoundResult of
                                                Ok Nothing ->
                                                    sendSinglePageProgress site model.allRawResponses config model payload

                                                Ok (Just notFoundReason) ->
                                                    render404Page config
                                                        --Nothing
                                                        (Result.toMaybe sharedDataResult)
                                                        model
                                                        payload.path
                                                        notFoundReason

                                                Err error ->
                                                    [ error ] |> ToJsPayload.Errors |> Effect.SendSinglePage

                                        RenderRequest.NotFound path ->
                                            render404Page config
                                                --Nothing
                                                (Result.toMaybe sharedDataResult)
                                                model
                                                path
                                                NotFoundReason.NoMatchingRoute
                    in
                    ( model
                    , apiResponse
                    )

                StaticResponses.Errors errors ->
                    ( model
                    , errors |> ToJsPayload.Errors |> Effect.SendSinglePage
                    )


sendSinglePageProgress :
    SiteConfig
    -> RequestsAndPending
    -> ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage
    -> Model route
    -> { path : Path, frontmatter : route }
    -> Effect
sendSinglePageProgress site contentJson config model info =
    let
        ( page, route ) =
            ( info.path, info.frontmatter )
    in
    case model.maybeRequestJson of
        RenderRequest.SinglePage includeHtml _ _ ->
            let
                isAction : Maybe ActionRequest
                isAction =
                    model.maybeRequestJson
                        |> RenderRequest.maybeRequestPayload
                        |> Maybe.andThen (Decode.decodeValue isActionDecoder >> Result.withDefault Nothing)

                pageFoundResult : Result BuildError (Maybe NotFoundReason)
                pageFoundResult =
                    -- TODO OPTIMIZATION this is redundant
                    StaticHttpRequest.resolve
                        (if model.isDevServer then
                            config.handleRoute route

                         else
                            DataSource.succeed Nothing
                        )
                        model.allRawResponses
                        |> requestResultToResult
                        |> Result.mapError (StaticHttpRequest.toBuildError currentUrl.path)

                renderedResult : Result BuildError (PageServerResponse { head : List Head.Tag, view : String, title : String } errorPage)
                renderedResult =
                    case includeHtml of
                        RenderRequest.OnlyJson ->
                            pageDataResult
                                |> Result.map
                                    (\okPageData ->
                                        case okPageData of
                                            PageServerResponse.RenderPage responseInfo _ ->
                                                PageServerResponse.RenderPage
                                                    { statusCode = responseInfo.statusCode
                                                    , headers = responseInfo.headers
                                                    }
                                                    { head = []
                                                    , view = "This page was not rendered because it is a JSON-only request."
                                                    , title = "This page was not rendered because it is a JSON-only request."
                                                    }

                                            PageServerResponse.ServerResponse serverResponse ->
                                                PageServerResponse.ServerResponse serverResponse

                                            PageServerResponse.ErrorPage error record ->
                                                PageServerResponse.ErrorPage error record
                                    )

                        RenderRequest.HtmlAndJson ->
                            Result.map2 Tuple.pair pageDataResult sharedDataResult
                                |> Result.map
                                    (\( pageData_, sharedData ) ->
                                        case pageData_ of
                                            PageServerResponse.RenderPage responseInfo pageData ->
                                                let
                                                    currentPage : { path : Path, route : route }
                                                    currentPage =
                                                        { path = page, route = urlToRoute config currentUrl }

                                                    maybeActionData : Maybe actionData
                                                    maybeActionData =
                                                        case isAction of
                                                            Just _ ->
                                                                case actionDataResult of
                                                                    Ok (PageServerResponse.RenderPage _ actionData) ->
                                                                        Just actionData

                                                                    _ ->
                                                                        Nothing

                                                            Nothing ->
                                                                Nothing

                                                    pageModel : userModel
                                                    pageModel =
                                                        config.init
                                                            Pages.Flags.PreRenderFlags
                                                            sharedData
                                                            pageData
                                                            maybeActionData
                                                            (Just
                                                                { path =
                                                                    { path = currentPage.path
                                                                    , query = Nothing
                                                                    , fragment = Nothing
                                                                    }
                                                                , metadata = currentPage.route
                                                                , pageUrl = Nothing
                                                                }
                                                            )
                                                            |> Tuple.first

                                                    viewValue : { title : String, body : List (Html (Pages.Msg.Msg userMsg)) }
                                                    viewValue =
                                                        (config.view Dict.empty Dict.empty Nothing currentPage Nothing sharedData pageData maybeActionData |> .view) pageModel
                                                in
                                                PageServerResponse.RenderPage responseInfo
                                                    { head = config.view Dict.empty Dict.empty Nothing currentPage Nothing sharedData pageData maybeActionData |> .head
                                                    , view = viewValue.body |> bodyToString
                                                    , title = viewValue.title
                                                    }

                                            PageServerResponse.ServerResponse serverResponse ->
                                                PageServerResponse.ServerResponse serverResponse

                                            PageServerResponse.ErrorPage error record ->
                                                let
                                                    currentPage : { path : Path, route : route }
                                                    currentPage =
                                                        { path = page, route = urlToRoute config currentUrl }

                                                    pageModel : userModel
                                                    pageModel =
                                                        config.init
                                                            Pages.Flags.PreRenderFlags
                                                            sharedData
                                                            pageData
                                                            Nothing
                                                            (Just
                                                                { path =
                                                                    { path = currentPage.path
                                                                    , query = Nothing
                                                                    , fragment = Nothing
                                                                    }
                                                                , metadata = currentPage.route
                                                                , pageUrl = Nothing
                                                                }
                                                            )
                                                            |> Tuple.first

                                                    pageData : pageData
                                                    pageData =
                                                        config.errorPageToData error

                                                    viewValue : { title : String, body : List (Html (Pages.Msg.Msg userMsg)) }
                                                    viewValue =
                                                        (config.view Dict.empty Dict.empty Nothing currentPage Nothing sharedData pageData Nothing |> .view) pageModel
                                                in
                                                PageServerResponse.RenderPage
                                                    { statusCode = config.errorStatusCode error
                                                    , headers = record.headers
                                                    }
                                                    { head = config.view Dict.empty Dict.empty Nothing currentPage Nothing sharedData pageData Nothing |> .head
                                                    , view = viewValue.body |> List.map (HtmlPrinter.htmlToString Nothing) |> String.join "\n"
                                                    , title = viewValue.title
                                                    }
                                    )

                currentUrl : Url
                currentUrl =
                    { protocol = Url.Https
                    , host = site.canonicalUrl
                    , port_ = Nothing
                    , path = page |> Path.toRelative
                    , query = Nothing
                    , fragment = Nothing
                    }

                pageDataResult : Result BuildError (PageServerResponse pageData errorPage)
                pageDataResult =
                    -- TODO OPTIMIZATION can these three be included in StaticResponses.Finish?
                    StaticHttpRequest.resolve
                        (case isAction of
                            Just _ ->
                                config.action (urlToRoute config currentUrl)
                                    |> DataSource.andThen
                                        (\something ->
                                            case something of
                                                PageServerResponse.ErrorPage a b ->
                                                    PageServerResponse.ErrorPage a b
                                                        |> DataSource.succeed

                                                PageServerResponse.RenderPage _ _ ->
                                                    -- TODO the headers/response codes are ignored from the action here
                                                    -- is that okay? Should you always do a redirect or another kind of
                                                    -- server response if you want to control the headers/response code for an action (like logout & redirect, for example)?
                                                    config.data (urlToRoute config currentUrl)

                                                PageServerResponse.ServerResponse a ->
                                                    PageServerResponse.ServerResponse a
                                                        |> DataSource.succeed
                                        )

                            Nothing ->
                                config.data (urlToRoute config currentUrl)
                        )
                        contentJson
                        |> requestResultToResult
                        |> Result.mapError (StaticHttpRequest.toBuildError currentUrl.path)

                actionDataResult : Result BuildError (PageServerResponse actionData errorPage)
                actionDataResult =
                    -- TODO OPTIMIZATION can these three be included in StaticResponses.Finish?
                    StaticHttpRequest.resolve
                        (config.action (urlToRoute config currentUrl))
                        contentJson
                        |> requestResultToResult
                        |> Result.mapError (StaticHttpRequest.toBuildError currentUrl.path)

                sharedDataResult : Result BuildError sharedData
                sharedDataResult =
                    StaticHttpRequest.resolve
                        config.sharedData
                        contentJson
                        |> requestResultToResult
                        |> Result.mapError (StaticHttpRequest.toBuildError currentUrl.path)

                globalHeadTags : DataSource (List Head.Tag)
                globalHeadTags =
                    (config.globalHeadTags |> Maybe.withDefault (\_ -> DataSource.succeed [])) HtmlPrinter.htmlToString

                siteDataResult : Result BuildError (List Head.Tag)
                siteDataResult =
                    StaticHttpRequest.resolve
                        globalHeadTags
                        model.allRawResponses
                        |> requestResultToResult
                        |> Result.mapError (StaticHttpRequest.toBuildError "Site.elm")
            in
            case Result.map3 (\a b c -> ( a, b, c )) pageFoundResult renderedResult siteDataResult of
                Ok ( maybeNotFoundReason, renderedOrApiResponse, siteData ) ->
                    case maybeNotFoundReason of
                        Nothing ->
                            let
                                ( actionHeaders, byteEncodedPageData ) =
                                    case pageDataResult of
                                        Ok pageServerResponse ->
                                            case pageServerResponse of
                                                PageServerResponse.RenderPage ignored1 pageData ->
                                                    -- TODO want to encode both shared and page data in dev server and HTML-embedded data
                                                    -- but not for writing out the content.dat files - would be good to optimize this redundant data out
                                                    --if model.isDevServer then
                                                    case isAction of
                                                        Just actionRequestKind ->
                                                            case actionDataResult of
                                                                Ok (PageServerResponse.RenderPage ignored2 actionData) ->
                                                                    case actionRequestKind of
                                                                        ActionResponseRequest ->
                                                                            ( ignored2.headers
                                                                            , sharedDataResult
                                                                                |> Result.map (\sharedData -> ResponseSketch.HotUpdate pageData sharedData (Just actionData))
                                                                                |> Result.withDefault (ResponseSketch.RenderPage pageData (Just actionData))
                                                                                |> config.encodeResponse
                                                                                |> Bytes.Encode.encode
                                                                            )

                                                                        ActionOnlyRequest ->
                                                                            ---- TODO need to encode action data when only that is requested (not ResponseSketch?)
                                                                            ( ignored2.headers
                                                                            , actionData
                                                                                |> config.encodeAction
                                                                                |> Bytes.Encode.encode
                                                                            )

                                                                _ ->
                                                                    ( ignored1.headers
                                                                    , Bytes.Encode.encode (Bytes.Encode.unsignedInt8 0)
                                                                    )

                                                        Nothing ->
                                                            ( ignored1.headers
                                                            , sharedDataResult
                                                                |> Result.map (\something -> ResponseSketch.HotUpdate pageData something Nothing)
                                                                |> Result.withDefault (ResponseSketch.RenderPage pageData Nothing)
                                                                |> config.encodeResponse
                                                                |> Bytes.Encode.encode
                                                            )

                                                --else
                                                --    pageData
                                                --        |> ResponseSketch.RenderPage
                                                --        |> config.encodeResponse
                                                --        |> Bytes.Encode.encode
                                                PageServerResponse.ServerResponse serverResponse ->
                                                    -- TODO handle error?
                                                    ( serverResponse.headers
                                                    , PageServerResponse.toRedirect serverResponse
                                                        |> Maybe.map
                                                            (\{ location } ->
                                                                location
                                                                    |> ResponseSketch.Redirect
                                                                    |> config.encodeResponse
                                                            )
                                                        -- TODO handle other cases besides redirects?
                                                        |> Maybe.withDefault (Bytes.Encode.unsignedInt8 0)
                                                        |> Bytes.Encode.encode
                                                    )

                                                PageServerResponse.ErrorPage error { headers } ->
                                                    -- TODO this case should never happen
                                                    ( headers
                                                    , sharedDataResult
                                                        |> Result.map
                                                            (\sharedData ->
                                                                ResponseSketch.HotUpdate (config.errorPageToData error)
                                                                    sharedData
                                                                    Nothing
                                                            )
                                                        |> Result.map config.encodeResponse
                                                        |> Result.map Bytes.Encode.encode
                                                        |> Result.withDefault (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 0))
                                                    )

                                        _ ->
                                            -- TODO handle error?
                                            ( []
                                            , Bytes.Encode.encode (Bytes.Encode.unsignedInt8 0)
                                            )
                            in
                            case renderedOrApiResponse of
                                PageServerResponse.RenderPage responseInfo rendered ->
                                    { route = page |> Path.toRelative
                                    , contentJson = Dict.empty
                                    , html = rendered.view
                                    , errors = []
                                    , head = rendered.head ++ siteData
                                    , title = rendered.title
                                    , staticHttpCache = Dict.empty
                                    , is404 = False
                                    , statusCode = responseInfo.statusCode
                                    , headers =
                                        -- TODO should `responseInfo.headers` be used? Is there a problem in the case where there is both an action and data response in one? Do we need to make sure it is performed as two separate HTTP requests to ensure that the cookies are set correctly in that case?
                                        actionHeaders
                                    }
                                        |> ToJsPayload.PageProgress
                                        |> Effect.SendSinglePageNew byteEncodedPageData

                                PageServerResponse.ServerResponse serverResponse ->
                                    PageServerResponse.toRedirect serverResponse
                                        |> Maybe.map
                                            (\_ ->
                                                { route = page |> Path.toRelative
                                                , contentJson = Dict.empty
                                                , html = "This is intentionally blank HTML"
                                                , errors = []
                                                , head = []
                                                , title = "This is an intentionally blank title"
                                                , staticHttpCache = Dict.empty
                                                , is404 = False
                                                , statusCode =
                                                    case includeHtml of
                                                        RenderRequest.OnlyJson ->
                                                            -- if this is a redirect for a `content.dat`, we don't want to send an *actual* redirect status code because the redirect needs to be handled in Elm (not by the Browser)
                                                            200

                                                        RenderRequest.HtmlAndJson ->
                                                            serverResponse.statusCode
                                                , headers = serverResponse.headers
                                                }
                                                    |> ToJsPayload.PageProgress
                                                    |> Effect.SendSinglePageNew byteEncodedPageData
                                            )
                                        |> Maybe.withDefault
                                            ({ body = serverResponse |> PageServerResponse.toJson
                                             , staticHttpCache = Dict.empty
                                             , statusCode = serverResponse.statusCode
                                             }
                                                |> ToJsPayload.SendApiResponse
                                                |> Effect.SendSinglePage
                                            )

                                PageServerResponse.ErrorPage error responseInfo ->
                                    -- TODO this case should never happen
                                    { route = page |> Path.toRelative
                                    , contentJson = Dict.empty
                                    , html = "UNEXPECTED!" --HtmlPrinter.htmlToString rendered.body
                                    , errors = []
                                    , head = [] -- rendered.head ++ siteData -- TODO this should call ErrorPage.head maybe?
                                    , title = "UNEXPECTED CASE" --rendered.title
                                    , staticHttpCache = Dict.empty
                                    , is404 = False
                                    , statusCode = config.errorStatusCode error
                                    , headers = responseInfo.headers
                                    }
                                        |> ToJsPayload.PageProgress
                                        |> Effect.SendSinglePageNew byteEncodedPageData

                        Just notFoundReason ->
                            render404Page config (Result.toMaybe sharedDataResult) model page notFoundReason

                Err error ->
                    [ error ]
                        |> ToJsPayload.Errors
                        |> Effect.SendSinglePage


render404Page :
    ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage
    -> Maybe sharedData
    -> Model route
    -> Path
    -> NotFoundReason
    -> Effect
render404Page config sharedData model path notFoundReason =
    case ( model.isDevServer, sharedData ) of
        ( False, Just justSharedData ) ->
            let
                byteEncodedPageData : Bytes
                byteEncodedPageData =
                    ResponseSketch.HotUpdate
                        (config.errorPageToData config.notFoundPage)
                        justSharedData
                        -- TODO remove shared action data
                        Nothing
                        |> config.encodeResponse
                        |> Bytes.Encode.encode

                pageModel : userModel
                pageModel =
                    config.init
                        Pages.Flags.PreRenderFlags
                        justSharedData
                        pageData
                        Nothing
                        Nothing
                        |> Tuple.first

                pageData : pageData
                pageData =
                    config.errorPageToData config.notFoundPage

                pathAndRoute : { path : Path, route : route }
                pathAndRoute =
                    { path = path, route = config.notFoundRoute }

                viewValue : { title : String, body : List (Html (Pages.Msg.Msg userMsg)) }
                viewValue =
                    (config.view Dict.empty
                        Dict.empty
                        Nothing
                        pathAndRoute
                        Nothing
                        justSharedData
                        pageData
                        Nothing
                        |> .view
                    )
                        pageModel
            in
            { route = Path.toAbsolute path
            , contentJson = Dict.empty
            , html = viewValue.body |> bodyToString
            , errors = []
            , head = config.view Dict.empty Dict.empty Nothing pathAndRoute Nothing justSharedData pageData Nothing |> .head
            , title = viewValue.title
            , staticHttpCache = Dict.empty
            , is404 = True
            , statusCode = 404
            , headers = []
            }
                |> ToJsPayload.PageProgress
                |> Effect.SendSinglePageNew byteEncodedPageData

        _ ->
            let
                byteEncodedPageData : Bytes
                byteEncodedPageData =
                    ResponseSketch.NotFound { reason = notFoundReason, path = path }
                        |> config.encodeResponse
                        |> Bytes.Encode.encode

                notFoundDocument : { title : String, body : List (Html msg) }
                notFoundDocument =
                    { path = path
                    , reason = notFoundReason
                    }
                        |> NotFoundReason.document config.pathPatterns
            in
            { route = Path.toAbsolute path
            , contentJson = Dict.empty
            , html = bodyToString notFoundDocument.body
            , errors = []
            , head = []
            , title = notFoundDocument.title
            , staticHttpCache = Dict.empty

            -- TODO can I handle caching from the JS-side only?
            --model.allRawResponses |> Dict.Extra.filterMap (\_ v -> v)
            , is404 = True
            , statusCode = 404
            , headers = []
            }
                |> ToJsPayload.PageProgress
                |> Effect.SendSinglePageNew byteEncodedPageData


bodyToString : List (Html msg) -> String
bodyToString body =
    body |> List.map (HtmlPrinter.htmlToString Nothing) |> String.join "\n"


urlToRoute : ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage -> Url -> route
urlToRoute config url =
    if url.path |> String.startsWith "/____elm-pages-internal____" then
        config.notFoundRoute

    else
        config.urlToRoute url


requestResultToResult : RequestResult value -> Result StaticHttpRequest.Error value
requestResultToResult thing =
    case thing of
        Done doneValue ->
            Ok doneValue

        FatalError error ->
            error
                |> Err

        PendingRequests ( pendingUrls, _ ) ->
            StaticHttpRequest.MissingHttpResponse "" pendingUrls
                |> Err



--StaticHttpRequest.UserCalledStaticHttpFail "TODO - use better error message. There are pending URLs but didn't expect here to be."
--    |> Err
