module Pages.StaticHttpRequest exposing (Error(..), MockResolver, RawRequest(..), RequestResult(..), Status(..), cacheRequestResolution, map, mockResolve, resolve, resolveUrls, toBuildError)

import BuildError exposing (BuildError)
import Dict
import List.Extra
import Pages.StaticHttp.Request
import RequestsAndPending exposing (RequestsAndPending)
import TerminalText as Terminal


type alias MockResolver =
    Pages.StaticHttp.Request.Request
    -> Maybe RequestsAndPending.Response



--type RawRequest value
--    = Request (List Pages.StaticHttp.Request.Request) (Maybe MockResolver -> RequestsAndPending -> RawRequest value)
--    | RequestError Error
--    | ApiRoute value


type RawRequest value
    = RawRequest
        (Maybe MockResolver
         -> RequestsAndPending
         ->
            --Result
            --    ( List Pages.StaticHttp.Request.Request, RawRequest value )
            --    value
            RequestResult value
        )


map : (a -> b) -> RawRequest a -> RawRequest b
map fn (RawRequest requestInfo) =
    RawRequest
        (\a b ->
            case requestInfo a b of
                Done value ->
                    value |> fn |> Done

                FatalError error ->
                    FatalError error

                PendingRequests ( left, right ) ->
                    PendingRequests
                        ( left
                        , map fn right
                        )
        )


type RequestResult value
    = PendingRequests ( List Pages.StaticHttp.Request.Request, RawRequest value )
    | FatalError Error
    | Done value


type Error
    = MissingHttpResponse String (List Pages.StaticHttp.Request.Request)
    | DecoderError String
    | UserCalledStaticHttpFail String


toBuildError : String -> Error -> BuildError
toBuildError path error =
    case error of
        MissingHttpResponse missingKey _ ->
            { title = "Missing Http Response"
            , message =
                [ Terminal.text missingKey
                ]
            , path = path
            , fatal = True
            }

        DecoderError decodeErrorMessage ->
            { title = "Static Http Decoding Error"
            , message =
                [ Terminal.text decodeErrorMessage
                ]
            , path = path
            , fatal = True
            }

        UserCalledStaticHttpFail decodeErrorMessage ->
            { title = "Called Static Http Fail"
            , message =
                [ Terminal.text <| "I ran into a call to `DataSource.fail` with message: " ++ decodeErrorMessage
                ]
            , path = path
            , fatal = True
            }



--resolve : RawRequest value -> RequestsAndPending -> Result Error value
--resolve request rawResponses =
--    case request of
--        RequestError error ->
--            Err error
--
--        Request _ lookupFn ->
--            case lookupFn Nothing rawResponses of
--                nextRequest ->
--                    resolve nextRequest rawResponses
--
--        ApiRoute value ->
--            Ok value


resolve :
    RawRequest value
    -> RequestsAndPending
    -> RequestResult value
resolve (RawRequest lookupFn) rawResponses =
    --case request of
    --    RequestError error ->
    --        Err error
    --
    --    Request _ lookupFn ->
    --case lookupFn Nothing rawResponses of
    --    Err ( things, nextRequest ) ->
    --        --resolve nextRequest rawResponses
    --        Err ( things, nextRequest )
    --
    --    Ok value ->
    --        Ok value
    lookupFn Nothing rawResponses



--lookupFn
--ApiRoute value ->
--    Ok value


mockResolve : RawRequest value -> MockResolver -> Result Error value
mockResolve request mockResolver =
    --case request of
    --    RequestError error ->
    --        Err error
    --
    --    Request _ lookupFn ->
    --        case lookupFn (Just mockResolver) Dict.empty of
    --            nextRequest ->
    --                mockResolve nextRequest mockResolver
    --
    --    ApiRoute value ->
    --        Ok value
    Debug.todo ""


resolveUrls : RawRequest value -> RequestsAndPending -> List Pages.StaticHttp.Request.Request
resolveUrls request rawResponses =
    resolveUrlsHelp rawResponses [] request


resolveUrlsHelp : RequestsAndPending -> List Pages.StaticHttp.Request.Request -> RawRequest value -> List Pages.StaticHttp.Request.Request
resolveUrlsHelp rawResponses soFar (RawRequest request) =
    let
        _ =
            Debug.log "resolveUrlsHelp" ()
    in
    case request Nothing rawResponses of
        FatalError error ->
            case error of
                -- TODO is this obsolete?
                MissingHttpResponse _ next ->
                    (soFar ++ next)
                        |> List.Extra.uniqueBy Pages.StaticHttp.Request.hash

                _ ->
                    soFar

        PendingRequests ( urlList, RawRequest lookupFn ) ->
            resolveUrlsHelp
                rawResponses
                (soFar ++ urlList)
                -- TODO does this cause an infinite loop?
                (RawRequest (\_ rawResponses2 -> lookupFn Nothing rawResponses2))

        Done _ ->
            soFar


cacheRequestResolution :
    RawRequest value
    -> RequestsAndPending
    -> Status value
cacheRequestResolution request rawResponses =
    cacheRequestResolutionHelp [] rawResponses request


type Status value
    = Incomplete (List Pages.StaticHttp.Request.Request)
    | HasPermanentError Error
    | Complete


cacheRequestResolutionHelp :
    List Pages.StaticHttp.Request.Request
    -> RequestsAndPending
    -> RawRequest value
    -> Status value
cacheRequestResolutionHelp foundUrls rawResponses (RawRequest request) =
    case request Nothing rawResponses of
        FatalError error ->
            case error of
                MissingHttpResponse _ req ->
                    -- TODO do I need to pass through continuation URLs here? -- Incomplete (urlList ++ foundUrls)
                    Incomplete (req ++ foundUrls)
                        |> Debug.log "@@@here"

                DecoderError _ ->
                    HasPermanentError error

                UserCalledStaticHttpFail _ ->
                    HasPermanentError error

        PendingRequests ( urlList, RawRequest lookupFn ) ->
            --cacheRequestResolutionHelp urlList
            --    rawResponses
            case lookupFn Nothing rawResponses of
                PendingRequests ( yetMoreUrls, _ ) ->
                    Incomplete ((yetMoreUrls ++ foundUrls ++ urlList) |> Debug.log "???1")

                _ ->
                    Incomplete ((foundUrls ++ urlList) |> Debug.log "???2")

        --(case lookupFn Nothing rawResponses of
        --    PendingRequests (newUrls)
        --    Incomplete urlList
        --    --_ ->
        --    --    Debug.todo ""
        --)
        Done _ ->
            Complete
