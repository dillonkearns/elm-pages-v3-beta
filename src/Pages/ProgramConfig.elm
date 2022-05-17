module Pages.ProgramConfig exposing (ProgramConfig)

import ApiRoute
import Browser.Navigation
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import DataSource exposing (DataSource)
import FormDecoder
import Head
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Json.Encode
import PageServerResponse exposing (PageServerResponse)
import Pages.Fetcher
import Pages.Flags
import Pages.Internal.NotFoundReason exposing (NotFoundReason)
import Pages.Internal.Platform.ToJsPayload
import Pages.Internal.ResponseSketch exposing (ResponseSketch)
import Pages.Internal.RoutePattern exposing (RoutePattern)
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.SiteConfig exposing (SiteConfig)
import Path exposing (Path)
import Url exposing (Url)


type alias ProgramConfig userMsg userModel route pageData actionData sharedData effect mappedMsg errorPage =
    { init :
        Pages.Flags.Flags
        -> sharedData
        -> pageData
        -> Maybe actionData
        -> Maybe Browser.Navigation.Key
        ->
            Maybe
                { path :
                    { path : Path
                    , query : Maybe String
                    , fragment : Maybe String
                    }
                , metadata : route
                , pageUrl : Maybe PageUrl
                }
        -> ( userModel, effect )
    , update : sharedData -> pageData -> Maybe Browser.Navigation.Key -> userMsg -> userModel -> ( userModel, effect )
    , subscriptions : route -> Path -> userModel -> Sub userMsg
    , sharedData : DataSource sharedData
    , data : route -> DataSource (PageServerResponse pageData errorPage)
    , action : route -> DataSource (PageServerResponse actionData errorPage)
    , onActionData : actionData -> Maybe userMsg
    , view :
        { path : Path
        , route : route
        }
        -> Maybe PageUrl
        -> sharedData
        -> pageData
        -> Maybe actionData
        ->
            { view : userModel -> { title : String, body : Html (Pages.Msg.Msg userMsg) }
            , head : List Head.Tag
            }
    , handleRoute : route -> DataSource (Maybe NotFoundReason)
    , getStaticRoutes : DataSource (List route)
    , urlToRoute : Url -> route
    , routeToPath : route -> List String
    , site : Maybe SiteConfig
    , toJsPort : Json.Encode.Value -> Cmd Never
    , fromJsPort : Sub Decode.Value
    , gotBatchSub : Sub Decode.Value
    , hotReloadData : Sub Bytes
    , onPageChange :
        { protocol : Url.Protocol
        , host : String
        , port_ : Maybe Int
        , path : Path
        , query : Maybe String
        , fragment : Maybe String
        , metadata : route
        }
        -> userMsg
    , apiRoutes :
        (Html Never -> String)
        -> List (ApiRoute.ApiRoute ApiRoute.Response)
    , pathPatterns : List RoutePattern
    , basePath : List String
    , sendPageData : Pages.Internal.Platform.ToJsPayload.NewThingForPort -> Cmd Never
    , byteEncodePageData : pageData -> Bytes.Encode.Encoder
    , byteDecodePageData : route -> Bytes.Decode.Decoder pageData
    , encodeResponse : ResponseSketch pageData actionData sharedData -> Bytes.Encode.Encoder
    , encodeAction : actionData -> Bytes.Encode.Encoder
    , decodeResponse : Bytes.Decode.Decoder (ResponseSketch pageData actionData sharedData)
    , globalHeadTags : Maybe (DataSource (List Head.Tag))
    , cmdToEffect : Cmd userMsg -> effect
    , perform :
        { fetchRouteData :
            { body : Maybe { contentType : String, body : String }
            , path : Maybe String
            , toMsg : Result Http.Error Url -> userMsg
            }
            -> Cmd mappedMsg
        , submit :
            { values : FormDecoder.FormData
            , encType : Maybe String
            , method : Maybe String
            , path : Maybe String
            , toMsg : Result Http.Error Url -> userMsg
            }
            -> Cmd mappedMsg
        , fromPageMsg : userMsg -> mappedMsg
        , runFetcher : Pages.Fetcher.Fetcher userMsg -> Cmd mappedMsg
        , key : Browser.Navigation.Key
        }
        -> effect
        -> Cmd mappedMsg
    , errorStatusCode : errorPage -> Int
    , notFoundPage : errorPage
    , internalError : String -> errorPage
    , errorPageToData : errorPage -> pageData
    , notFoundRoute : route
    }
