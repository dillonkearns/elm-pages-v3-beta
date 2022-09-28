module HtmlPrinter exposing (htmlToString)

import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode
import Test.Html.Internal.ElmHtml.InternalTypes exposing (decodeElmHtml)
import Test.Html.Internal.ElmHtml.ToString exposing (defaultFormatOptions, nodeToStringWithOptions)
import VirtualDom


htmlToString : Html msg -> String
htmlToString viewHtml =
    case
        Decode.decodeValue
            (decodeElmHtml (\_ _ -> VirtualDom.Normal (Decode.succeed ())))
            (asJsonView viewHtml)
    of
        Ok str ->
            nodeToStringWithOptions defaultFormatOptions str

        Err err ->
            "Error pre-rendering HTML in HtmlPrinter.elm: " ++ Decode.errorToString err


asJsonView : Html msg -> Decode.Value
asJsonView x =
    Json.Encode.string "REPLACE_ME_WITH_JSON_STRINGIFY"
