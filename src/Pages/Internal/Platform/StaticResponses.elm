module Pages.Internal.Platform.StaticResponses exposing (FinishKind(..), NextStep(..), StaticResponses, batchUpdate, empty, nextStep, renderApiRequest, renderSingleRoute)

import BuildError exposing (BuildError)
import DataSource exposing (DataSource)
import Dict exposing (Dict)
import Dict.Extra
import Pages.Internal.NotFoundReason exposing (NotFoundReason)
import Pages.StaticHttp.Request as HashRequest
import Pages.StaticHttpRequest as StaticHttpRequest exposing (RequestResult(..))
import RequestsAndPending exposing (RequestsAndPending)
import Set exposing (Set)


type StaticResponses
    = ApiRequest StaticHttpResult
    | StaticResponses StaticHttpResult
    | CheckIfHandled (DataSource (Maybe NotFoundReason)) StaticHttpResult StaticHttpResult


type StaticHttpResult
    = NotFetched (DataSource ()) (Dict String (Result () String))


empty : StaticResponses
empty =
    StaticResponses
        (NotFetched (DataSource.succeed ()) Dict.empty)


renderSingleRoute :
    DataSource a
    -> DataSource (Maybe NotFoundReason)
    -> StaticResponses
renderSingleRoute request cliData =
    CheckIfHandled cliData
        (NotFetched
            (cliData
                |> DataSource.map (\_ -> ())
            )
            Dict.empty
        )
        (NotFetched (DataSource.map (\_ -> ()) request) Dict.empty)


renderApiRequest :
    DataSource response
    -> StaticResponses
renderApiRequest request =
    ApiRequest
        (NotFetched
            (request |> DataSource.map (\_ -> ()))
            Dict.empty
        )


batchUpdate :
    List
        { request : HashRequest.Request
        , response : RequestsAndPending.Response
        }
    ->
        { model
            | staticResponses : StaticResponses
            , allRawResponses : RequestsAndPending
        }
    ->
        { model
            | staticResponses : StaticResponses
            , allRawResponses : RequestsAndPending
        }
batchUpdate newEntries model =
    { model
        | allRawResponses = insertAll newEntries model.allRawResponses
    }


insertAll :
    List
        { request : HashRequest.Request
        , response : RequestsAndPending.Response
        }
    -> RequestsAndPending
    -> RequestsAndPending
insertAll newEntries dict =
    case newEntries of
        [] ->
            dict

        info :: rest ->
            insertAll
                rest
                (Dict.update (HashRequest.hash info.request) (\_ -> Just (Just info.response)) dict)


type NextStep route
    = Continue RequestsAndPending (List HashRequest.Request) (Maybe (List route))
    | Finish (FinishKind route)


type FinishKind route
    = ApiResponse
    | Errors (List BuildError)


nextStep :
    { model
        | staticResponses : StaticResponses
        , errors : List BuildError
        , allRawResponses : RequestsAndPending
    }
    -> Maybe (List route)
    -> ( StaticResponses, NextStep route )
nextStep ({ allRawResponses, errors } as model) maybeRoutes =
    let
        _ =
            Debug.log "nextStep" allRawResponses

        staticRequestsStatus : StaticHttpRequest.Status ()
        staticRequestsStatus =
            allRawResponses
                |> StaticHttpRequest.cacheRequestResolution request
                |> Debug.log "???status"

        request : DataSource ()
        request =
            case staticResponses of
                NotFetched request_ _ ->
                    request_

        staticResponses : StaticHttpResult
        staticResponses =
            case model.staticResponses of
                StaticResponses s ->
                    s

                ApiRequest staticHttpResult ->
                    staticHttpResult

                CheckIfHandled _ staticHttpResult _ ->
                    staticHttpResult

        pendingRequests : Bool
        pendingRequests =
            case staticResponses of
                NotFetched _ rawResponses ->
                    let
                        hasPermanentError : Bool
                        hasPermanentError =
                            case staticRequestsStatus of
                                StaticHttpRequest.HasPermanentError _ ->
                                    True

                                _ ->
                                    False

                        hasPermanentHttpError : Bool
                        hasPermanentHttpError =
                            not (List.isEmpty errors)

                        ( allUrlsKnown, knownUrlsToFetch ) =
                            case staticRequestsStatus of
                                StaticHttpRequest.Incomplete newUrlsToFetch ->
                                    ( False, newUrlsToFetch |> Debug.log "known to fetch" )

                                _ ->
                                    ( True, [] )

                        fetchedAllKnownUrls : Bool
                        fetchedAllKnownUrls =
                            (rawResponses
                                |> Dict.keys
                                |> Debug.log "rawResponses |> Dict.keys"
                                |> Set.fromList
                                |> Set.union (allRawResponses |> Dict.keys |> Set.fromList)
                            )
                                |> Set.diff
                                    (knownUrlsToFetch
                                        |> Debug.log "knownUrlsToFetch"
                                        |> List.map HashRequest.hash
                                        |> Set.fromList
                                    )
                                |> Set.isEmpty
                    in
                    not (hasPermanentHttpError || hasPermanentError || (allUrlsKnown && fetchedAllKnownUrls)) |> Debug.log "BOOL"
    in
    if pendingRequests then
        let
            urlsToPerform : List HashRequest.Request
            urlsToPerform =
                -- TODO is this redundant with cacheRequestResolution?
                StaticHttpRequest.resolveUrls request allRawResponses

            newAllRawResponses : RequestsAndPending
            newAllRawResponses =
                Dict.union allRawResponses dictOfNewUrlsToPerform

            dictOfNewUrlsToPerform : RequestsAndPending
            dictOfNewUrlsToPerform =
                urlsToPerform
                    |> List.map (\url -> ( HashRequest.hash url, Nothing ))
                    |> Dict.fromList

            maskedToUnmasked : Dict String HashRequest.Request
            maskedToUnmasked =
                urlsToPerform
                    |> List.map
                        (\secureUrl ->
                            ( HashRequest.hash secureUrl, secureUrl )
                        )
                    |> Dict.fromList

            alreadyPerformed : Set String
            alreadyPerformed =
                allRawResponses
                    |> Dict.keys
                    |> Set.fromList

            newThing : List HashRequest.Request
            newThing =
                maskedToUnmasked
                    |> Dict.Extra.removeMany alreadyPerformed
                    |> Dict.values
        in
        ( model.staticResponses, Continue newAllRawResponses newThing maybeRoutes )

    else
        let
            allErrors : List BuildError
            allErrors =
                let
                    failedRequests : List BuildError
                    failedRequests =
                        let
                            maybePermanentError : Maybe StaticHttpRequest.Error
                            maybePermanentError =
                                case staticRequestsStatus of
                                    StaticHttpRequest.HasPermanentError theError ->
                                        Just theError

                                    _ ->
                                        Nothing

                            decoderErrors : List BuildError
                            decoderErrors =
                                maybePermanentError
                                    |> Maybe.map (StaticHttpRequest.toBuildError "TODO PATH")
                                    |> Maybe.map List.singleton
                                    |> Maybe.withDefault []
                        in
                        decoderErrors
                in
                errors ++ failedRequests
        in
        case model.staticResponses of
            StaticResponses _ ->
                ( model.staticResponses
                , if List.length allErrors > 0 then
                    allErrors
                        |> Errors
                        |> Finish

                  else
                    Finish ApiResponse
                )

            ApiRequest _ ->
                ( model.staticResponses
                , if List.length allErrors > 0 then
                    allErrors
                        |> Errors
                        |> Finish

                  else
                    ApiResponse
                        |> Finish
                )

            CheckIfHandled pageFoundDataSource (NotFetched _ _) andThenRequest ->
                let
                    --pageFoundResult : Result StaticHttpRequest.Error (Maybe NotFoundReason)
                    pageFoundResult : StaticHttpRequest.RequestResult (Maybe NotFoundReason)
                    pageFoundResult =
                        StaticHttpRequest.resolve
                            pageFoundDataSource
                            allRawResponses
                in
                case pageFoundResult of
                    Done Nothing ->
                        nextStep { model | staticResponses = StaticResponses andThenRequest } maybeRoutes

                    Done (Just _) ->
                        ( StaticResponses andThenRequest
                          -- TODO is this right? Or should it be a fatal error, or try continuing here?
                        , Finish ApiResponse
                        )

                    PendingRequests ( urls, _ ) ->
                        ( StaticResponses andThenRequest
                          -- TODO is this right? Or should it be a fatal error, or try continuing here?
                        , Continue
                            -- TODO do I have a dict already?
                            Dict.empty
                            urls
                            maybeRoutes
                          --Finish ApiResponse
                          --  -- TODO change data type here so you can avoid running `resolve` again from `Cli.elm` since it can be expensive
                          --Continue newAllRawResponses newThing maybeRoutes
                          --nextStep model
                          --Finish
                          --  (Errors <|
                          --      StaticHttpRequest.toBuildError
                          --          -- TODO give more fine-grained error reference
                          --          "get static routes"
                          --  )
                        )

                    FatalError error_ ->
                        let
                            failedRequests : List BuildError
                            failedRequests =
                                let
                                    maybePermanentError : Maybe StaticHttpRequest.Error
                                    maybePermanentError =
                                        case staticRequestsStatus of
                                            StaticHttpRequest.HasPermanentError theError ->
                                                Just theError

                                            _ ->
                                                Nothing

                                    decoderErrors : List BuildError
                                    decoderErrors =
                                        maybePermanentError
                                            |> Maybe.map (StaticHttpRequest.toBuildError "TODO PATH")
                                            |> Maybe.map List.singleton
                                            |> Maybe.withDefault []
                                in
                                decoderErrors
                        in
                        ( model.staticResponses
                        , Finish
                            (Errors <|
                                (StaticHttpRequest.toBuildError
                                    -- TODO give more fine-grained error reference
                                    "get static routes"
                                    error_
                                    :: failedRequests
                                    ++ errors
                                )
                            )
                        )
