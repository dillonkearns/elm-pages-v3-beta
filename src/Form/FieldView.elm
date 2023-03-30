module Form.FieldView exposing
    ( Input, Options, input, radio, Hidden, select, valueButton
    , radioStyled, selectStyled, inputStyled, valueButtonStyled
    )

{-|

@docs Input, Options, input, radio, Hidden, select, valueButton


## Html.Styled Helpers

@docs radioStyled, selectStyled, inputStyled, valueButtonStyled

-}

import Form.Validation
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Styled
import Html.Styled.Attributes as StyledAttr
import Internal.Input
import Json.Encode as Encode
import Pages.Internal.Form exposing (Validation(..), ViewField)


{-| -}
type alias Input =
    Internal.Input.Input


{-| There are no render helpers for hidden fields because the `Form.renderHtml` helper functions automatically render hidden fields for you.
-}
type alias Hidden =
    Internal.Input.Hidden


{-| -}
type alias Options a =
    Internal.Input.Options a


{-| Gives you a submit button that will submit the form with a specific value for the given Field.
-}
valueButton :
    String
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Form.Validation.Field error parsed kind
    -> Html msg
valueButton exactValue attrs children (Validation viewField fieldName _) =
    let
        justViewField : ViewField kind
        justViewField =
            expectViewField viewField

        rawField : { name : String, value : Maybe String, kind : ( kind, List ( String, Encode.Value ) ) }
        rawField =
            { name = fieldName |> Maybe.withDefault ""
            , value = Just exactValue --justViewField.value
            , kind = justViewField.kind
            }
    in
    case rawField.kind of
        ( _, properties ) ->
            Html.button
                (attrs
                    ++ toHtmlProperties properties
                    ++ [ Attr.value (rawField.value |> Maybe.withDefault "")
                       , Attr.name rawField.name
                       ]
                )
                children


{-| Gives you a submit button that will submit the form with a specific value for the given Field.
-}
valueButtonStyled :
    String
    -> List (Html.Styled.Attribute msg)
    -> List (Html.Styled.Html msg)
    -> Form.Validation.Field error parsed kind
    -> Html.Styled.Html msg
valueButtonStyled exactValue attrs children (Validation viewField fieldName _) =
    let
        justViewField : ViewField kind
        justViewField =
            expectViewField viewField

        rawField : { name : String, value : Maybe String, kind : ( kind, List ( String, Encode.Value ) ) }
        rawField =
            { name = fieldName |> Maybe.withDefault ""
            , value = Just exactValue
            , kind = justViewField.kind
            }
    in
    case rawField.kind of
        ( _, properties ) ->
            Html.Styled.button
                (attrs
                    ++ (toHtmlProperties properties |> List.map StyledAttr.fromUnstyled)
                    ++ ([ Attr.value (rawField.value |> Maybe.withDefault "")
                        , Attr.name rawField.name
                        ]
                            |> List.map StyledAttr.fromUnstyled
                       )
                )
                children


{-| -}
input :
    List (Html.Attribute msg)
    -> Form.Validation.Field error parsed Input
    -> Html msg
input attrs (Validation viewField fieldName _) =
    let
        justViewField : ViewField Input
        justViewField =
            expectViewField viewField

        rawField : { name : String, value : Maybe String, kind : ( Input, List ( String, Encode.Value ) ) }
        rawField =
            { name = fieldName |> Maybe.withDefault ""
            , value = justViewField.value
            , kind = justViewField.kind
            }
    in
    case rawField.kind of
        ( Internal.Input.Input (Internal.Input.Textarea { rows, cols }), properties ) ->
            Html.textarea
                (attrs
                    ++ toHtmlProperties properties
                    ++ ([ rows |> Maybe.map Attr.rows
                        , cols |> Maybe.map Attr.cols
                        ]
                            |> List.filterMap identity
                       )
                    ++ [ Attr.name rawField.name
                       ]
                )
                [ -- textarea does not support the `value` attribute, but instead uses inner text for its form value
                  Html.text (rawField.value |> Maybe.withDefault "")
                ]

        ( Internal.Input.Input inputType, properties ) ->
            Html.input
                (attrs
                    ++ toHtmlProperties properties
                    ++ [ (case inputType of
                            Internal.Input.Checkbox ->
                                Attr.checked ((rawField.value |> Maybe.withDefault "") == "on")

                            _ ->
                                Attr.value (rawField.value |> Maybe.withDefault "")
                          -- TODO is this an okay default?
                         )
                       , Attr.name rawField.name
                       , inputType |> Internal.Input.inputTypeToString |> Attr.type_
                       ]
                )
                []


{-| -}
inputStyled :
    List (Html.Styled.Attribute msg)
    -> Form.Validation.Field error parsed Input
    -> Html.Styled.Html msg
inputStyled attrs (Validation viewField fieldName _) =
    let
        justViewField : ViewField Input
        justViewField =
            expectViewField viewField

        rawField : { name : String, value : Maybe String, kind : ( Input, List ( String, Encode.Value ) ) }
        rawField =
            { name = fieldName |> Maybe.withDefault ""
            , value = justViewField.value
            , kind = justViewField.kind
            }
    in
    case rawField.kind of
        ( Internal.Input.Input (Internal.Input.Textarea { rows, cols }), properties ) ->
            Html.Styled.textarea
                (attrs
                    ++ (toHtmlProperties properties |> List.map StyledAttr.fromUnstyled)
                    ++ ([ rows |> Maybe.map StyledAttr.rows
                        , cols |> Maybe.map StyledAttr.cols
                        ]
                            |> List.filterMap identity
                       )
                    ++ ([ Attr.name rawField.name
                        ]
                            |> List.map StyledAttr.fromUnstyled
                       )
                )
                [ -- textarea does not support the `value` attribute, but instead uses inner text for its form value
                  Html.Styled.text (rawField.value |> Maybe.withDefault "")
                ]

        ( Internal.Input.Input inputType, properties ) ->
            Html.Styled.input
                (attrs
                    ++ (toHtmlProperties properties |> List.map StyledAttr.fromUnstyled)
                    ++ ([ (case inputType of
                            Internal.Input.Checkbox ->
                                Attr.checked ((rawField.value |> Maybe.withDefault "") == "on")

                            _ ->
                                Attr.value (rawField.value |> Maybe.withDefault "")
                           -- TODO is this an okay default?
                          )
                        , Attr.name rawField.name
                        , inputType |> Internal.Input.inputTypeToString |> Attr.type_
                        ]
                            |> List.map StyledAttr.fromUnstyled
                       )
                )
                []


{-| -}
select :
    List (Html.Attribute msg)
    ->
        (parsed
         ->
            ( List (Html.Attribute msg)
            , String
            )
        )
    -> Form.Validation.Field error parsed2 (Options parsed)
    -> Html msg
select selectAttrs enumToOption (Validation viewField fieldName _) =
    let
        justViewField : ViewField (Options parsed)
        justViewField =
            viewField |> expectViewField

        rawField : { name : String, value : Maybe String, kind : ( Options parsed, List ( String, Encode.Value ) ) }
        rawField =
            { name = fieldName |> Maybe.withDefault ""
            , value = justViewField.value
            , kind = justViewField.kind
            }

        (Internal.Input.Options parseValue possibleValues) =
            rawField.kind |> Tuple.first
    in
    Html.select
        (selectAttrs
            ++ [ Attr.value (rawField.value |> Maybe.withDefault "")
               , Attr.name rawField.name
               ]
        )
        (possibleValues
            |> List.filterMap
                (\possibleValue ->
                    let
                        parsed : Maybe parsed
                        parsed =
                            possibleValue
                                |> parseValue
                    in
                    case parsed of
                        Just justParsed ->
                            let
                                ( optionAttrs, content ) =
                                    enumToOption justParsed
                            in
                            Html.option
                                (if rawField.value == Just possibleValue then
                                    Attr.selected True :: Attr.value possibleValue :: optionAttrs

                                 else
                                    Attr.value possibleValue :: optionAttrs
                                )
                                [ Html.text content ]
                                |> Just

                        Nothing ->
                            Nothing
                )
        )


{-| -}
selectStyled :
    List (Html.Styled.Attribute msg)
    ->
        (parsed
         ->
            ( List (Html.Styled.Attribute msg)
            , String
            )
        )
    -> Form.Validation.Field error parsed2 (Options parsed)
    -> Html.Styled.Html msg
selectStyled selectAttrs enumToOption (Validation viewField fieldName _) =
    let
        justViewField : ViewField (Options parsed)
        justViewField =
            viewField |> expectViewField

        rawField : { name : String, value : Maybe String, kind : ( Options parsed, List ( String, Encode.Value ) ) }
        rawField =
            { name = fieldName |> Maybe.withDefault ""
            , value = justViewField.value
            , kind = justViewField.kind
            }

        (Internal.Input.Options parseValue possibleValues) =
            rawField.kind |> Tuple.first
    in
    Html.Styled.select
        (selectAttrs
            ++ [ StyledAttr.value (rawField.value |> Maybe.withDefault "")
               , StyledAttr.name rawField.name
               ]
        )
        (possibleValues
            |> List.filterMap
                (\possibleValue ->
                    let
                        parsed : Maybe parsed
                        parsed =
                            possibleValue
                                |> parseValue
                    in
                    case parsed of
                        Just justParsed ->
                            let
                                ( optionAttrs, content ) =
                                    enumToOption justParsed
                            in
                            Html.Styled.option
                                (if rawField.value == Just possibleValue then
                                    StyledAttr.selected True :: StyledAttr.value possibleValue :: optionAttrs

                                 else
                                    StyledAttr.value possibleValue :: optionAttrs
                                )
                                [ Html.Styled.text content ]
                                |> Just

                        Nothing ->
                            Nothing
                )
        )


{-| -}
radio :
    List (Html.Attribute msg)
    ->
        (parsed
         -> (List (Html.Attribute msg) -> Html msg)
         -> Html msg
        )
    -> Form.Validation.Field error parsed2 (Options parsed)
    -> Html msg
radio selectAttrs enumToOption (Validation viewField fieldName _) =
    let
        justViewField : ViewField (Options parsed)
        justViewField =
            viewField |> expectViewField

        rawField : { name : String, value : Maybe String, kind : ( Options parsed, List ( String, Encode.Value ) ) }
        rawField =
            { name = fieldName |> Maybe.withDefault ""
            , value = justViewField.value
            , kind = justViewField.kind
            }

        (Internal.Input.Options parseValue possibleValues) =
            rawField.kind |> Tuple.first
    in
    Html.fieldset
        (selectAttrs
            ++ [ Attr.value (rawField.value |> Maybe.withDefault "")
               , Attr.name rawField.name
               ]
        )
        (possibleValues
            |> List.filterMap
                (\possibleValue ->
                    let
                        parsed : Maybe parsed
                        parsed =
                            possibleValue
                                |> parseValue
                    in
                    case parsed of
                        Just justParsed ->
                            let
                                renderedElement : Html msg
                                renderedElement =
                                    enumToOption justParsed
                                        (\userHtmlAttrs ->
                                            Html.input
                                                ([ Attr.type_ "radio"
                                                 , Attr.value possibleValue
                                                 , Attr.name rawField.name
                                                 , Attr.checked (rawField.value == Just possibleValue)
                                                 ]
                                                    ++ userHtmlAttrs
                                                )
                                                []
                                        )
                            in
                            Just renderedElement

                        Nothing ->
                            Nothing
                )
        )


expectViewField : Maybe (ViewField kind) -> ViewField kind
expectViewField viewField =
    case viewField of
        Just justViewField ->
            justViewField

        Nothing ->
            expectViewField viewField


{-| -}
radioStyled :
    List (Html.Styled.Attribute msg)
    ->
        (parsed
         -> (List (Html.Styled.Attribute msg) -> Html.Styled.Html msg)
         -> Html.Styled.Html msg
        )
    -> Form.Validation.Field error parsed2 (Options parsed)
    -> Html.Styled.Html msg
radioStyled selectAttrs enumToOption (Validation viewField fieldName _) =
    let
        justViewField : ViewField (Options parsed)
        justViewField =
            viewField |> expectViewField

        rawField : { name : String, value : Maybe String, kind : ( Options parsed, List ( String, Encode.Value ) ) }
        rawField =
            { name = fieldName |> Maybe.withDefault ""
            , value = justViewField.value
            , kind = justViewField.kind
            }

        (Internal.Input.Options parseValue possibleValues) =
            rawField.kind |> Tuple.first
    in
    Html.Styled.fieldset
        (selectAttrs
            ++ [ StyledAttr.value (rawField.value |> Maybe.withDefault "")
               , StyledAttr.name rawField.name
               ]
        )
        (possibleValues
            |> List.filterMap
                (\possibleValue ->
                    let
                        parsed : Maybe parsed
                        parsed =
                            possibleValue
                                |> parseValue
                    in
                    case parsed of
                        Just justParsed ->
                            let
                                renderedElement : Html.Styled.Html msg
                                renderedElement =
                                    enumToOption justParsed
                                        (\userHtmlAttrs ->
                                            Html.Styled.input
                                                (([ Attr.type_ "radio"
                                                  , Attr.value possibleValue
                                                  , Attr.name rawField.name
                                                  , Attr.checked (rawField.value == Just possibleValue)
                                                  ]
                                                    |> List.map StyledAttr.fromUnstyled
                                                 )
                                                    ++ userHtmlAttrs
                                                )
                                                []
                                        )
                            in
                            Just renderedElement

                        Nothing ->
                            Nothing
                )
        )


toHtmlProperties : List ( String, Encode.Value ) -> List (Html.Attribute msg)
toHtmlProperties properties =
    properties
        |> List.map
            (\( key, value ) ->
                Attr.property key value
            )
