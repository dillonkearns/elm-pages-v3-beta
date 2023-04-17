module Pages.Transition exposing
    ( Transition(..), LoadingState(..), map
    , FetcherState, FetcherSubmitStatus(..)
    )

{-|

@docs Transition, LoadingState, map


## Fetchers

@docs FetcherState, FetcherSubmitStatus

-}

import Form
import Path exposing (Path)
import Time


type alias FormData =
    { fields : List ( String, String )
    , method : Form.Method
    , action : String
    , id : Maybe String
    }


{-| -}
type Transition
    = Submitting FormData
    | LoadAfterSubmit FormData Path LoadingState
    | Loading Path LoadingState


{-| -}
type LoadingState
    = Redirecting
    | Load
    | ActionRedirect


{-| -}
type alias FetcherState actionData =
    { status : FetcherSubmitStatus actionData
    , payload : FormData
    , initiatedAt : Time.Posix
    }


{-| -}
type FetcherSubmitStatus actionData
    = FetcherSubmitting
    | FetcherReloading actionData
    | FetcherComplete actionData


{-| -}
map : (a -> b) -> FetcherState a -> FetcherState b
map mapFn fetcherState =
    { status = mapStatus mapFn fetcherState.status
    , payload = fetcherState.payload
    , initiatedAt = fetcherState.initiatedAt
    }


mapStatus : (a -> b) -> FetcherSubmitStatus a -> FetcherSubmitStatus b
mapStatus mapFn fetcherSubmitStatus =
    case fetcherSubmitStatus of
        FetcherSubmitting ->
            FetcherSubmitting

        FetcherReloading value ->
            FetcherReloading (mapFn value)

        FetcherComplete value ->
            FetcherComplete (mapFn value)
