module Pages.Form exposing (renderHtml, renderStyledHtml)

import Dict exposing (Dict)
import Form
import Form.Validation exposing (Validation)
import Html
import Html.Styled
import Pages.Internal.Msg
import Pages.Transition
import PagesMsg exposing (PagesMsg)


renderHtml :
    String
    -> List (Html.Attribute (PagesMsg userMsg))
    ->
        --{ submitting : Bool, serverResponse : Maybe (Form.ServerResponse error), state : Form.Model }
        { --path : Path
          --, url : Maybe PageUrl
          --, action : Maybe action
          app
            | pageFormState : Form.Model
            , transition : Maybe Pages.Transition.Transition
            , fetchers : Dict String (Pages.Transition.FetcherState (Maybe action))
        }
    -> input
    -> Form.Form error { combine : Validation error parsed named constraints, view : Form.Context error input -> List (Html.Html (PagesMsg userMsg)) } parsed input (PagesMsg userMsg)
    -> Html.Html (PagesMsg userMsg)
renderHtml formId attrs app input form_ =
    form_
        |> Form.renderHtml
            formId
            attrs
            { state = app.pageFormState
            , serverResponse = Nothing -- TODO
            , submitting =
                (case app.fetchers |> Dict.get formId of
                    Just { status } ->
                        case status of
                            Pages.Transition.FetcherComplete _ ->
                                False

                            Pages.Transition.FetcherSubmitting ->
                                True

                            Pages.Transition.FetcherReloading _ ->
                                True

                    Nothing ->
                        False
                )
                    || (case app.transition of
                            Just (Pages.Transition.Submitting formData) ->
                                formData.id == Just formId

                            Just (Pages.Transition.LoadAfterSubmit submitData _ _) ->
                                submitData.id == Just formId

                            Just (Pages.Transition.Loading _ _) ->
                                False

                            Nothing ->
                                False
                       )
            , toMsg = Pages.Internal.Msg.FormMsg
            , onSubmit =
                Just
                    (\{ fields, action, parsed } ->
                        case parsed of
                            Form.Valid _ ->
                                Pages.Internal.Msg.Submit
                                    { useFetcher = False -- TODO
                                    , action = action
                                    , fields = fields
                                    , msg = Nothing -- TODO
                                    , id = formId
                                    , valid = True
                                    }

                            Form.Invalid _ _ ->
                                Pages.Internal.Msg.Submit
                                    { useFetcher = False -- TODO
                                    , action = action
                                    , fields = fields
                                    , msg = Nothing -- TODO
                                    , id = formId
                                    , valid = False
                                    }
                    )
            }
            input


renderStyledHtml :
    String
    -> List (Html.Styled.Attribute (PagesMsg userMsg))
    ->
        --{ submitting : Bool, serverResponse : Maybe (Form.ServerResponse error), state : Form.Model }
        { --path : Path
          --, url : Maybe PageUrl
          --, action : Maybe action
          app
            | pageFormState : Form.Model
            , transition : Maybe Pages.Transition.Transition
            , fetchers : Dict String (Pages.Transition.FetcherState (Maybe action))
        }
    -> input
    -> Form.Form error { combine : Validation error parsed named constraints, view : Form.Context error input -> List (Html.Styled.Html (PagesMsg userMsg)) } parsed input (PagesMsg userMsg)
    -> Html.Styled.Html (PagesMsg userMsg)
renderStyledHtml formId attrs app input form_ =
    form_
        |> Form.renderStyledHtml
            formId
            attrs
            { state = app.pageFormState
            , serverResponse = Nothing -- TODO
            , submitting =
                (case app.fetchers |> Dict.get formId of
                    Just { status } ->
                        case status of
                            Pages.Transition.FetcherComplete _ ->
                                False

                            Pages.Transition.FetcherSubmitting ->
                                True

                            Pages.Transition.FetcherReloading _ ->
                                True

                    Nothing ->
                        False
                )
                    || (case app.transition of
                            Just (Pages.Transition.Submitting formData) ->
                                formData.id == Just formId

                            Just (Pages.Transition.LoadAfterSubmit submitData _ _) ->
                                submitData.id == Just formId

                            Just (Pages.Transition.Loading _ _) ->
                                False

                            Nothing ->
                                False
                       )
            , toMsg = Pages.Internal.Msg.FormMsg
            , onSubmit =
                Just
                    (\{ fields, action, parsed } ->
                        case parsed of
                            Form.Valid _ ->
                                Pages.Internal.Msg.Submit
                                    { useFetcher = False -- TODO
                                    , fields = fields
                                    , msg = Nothing -- TODO
                                    , id = formId
                                    , valid = True
                                    , action = action
                                    }

                            Form.Invalid _ _ ->
                                Pages.Internal.Msg.Submit
                                    { useFetcher = False -- TODO
                                    , fields = fields
                                    , msg = Nothing -- TODO
                                    , id = formId
                                    , valid = False
                                    , action = action
                                    }
                    )
            }
            input
