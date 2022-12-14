module MarkdownCodec exposing (isPlaceholder, noteTitle, titleAndDescription, withFrontmatter, withoutFrontmatter)

import BackendTask exposing (BackendTask)
import BackendTask.File as StaticFile
import Exception exposing (Throwable)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import List.Extra
import Markdown.Block as Block exposing (Block)
import Markdown.Parser
import Markdown.Renderer
import MarkdownExtra


isPlaceholder : String -> BackendTask Throwable (Maybe ())
isPlaceholder filePath =
    filePath
        |> StaticFile.bodyWithoutFrontmatter
        |> BackendTask.throw
        |> BackendTask.andThen
            (\rawContent ->
                Markdown.Parser.parse rawContent
                    |> Result.mapError (\_ -> Exception.fromString "Markdown error")
                    |> Result.map
                        (\blocks ->
                            List.any
                                (\block ->
                                    case block of
                                        Block.Heading _ inlines ->
                                            False

                                        _ ->
                                            True
                                )
                                blocks
                                |> not
                        )
                    |> BackendTask.fromResult
            )
        |> BackendTask.map
            (\bool ->
                if bool then
                    Nothing

                else
                    Just ()
            )


noteTitle : String -> BackendTask Throwable String
noteTitle filePath =
    titleFromFrontmatter filePath
        |> BackendTask.andThen
            (\maybeTitle ->
                maybeTitle
                    |> Maybe.map BackendTask.succeed
                    |> Maybe.withDefault
                        (StaticFile.bodyWithoutFrontmatter filePath
                            |> BackendTask.throw
                            |> BackendTask.andThen
                                (\rawContent ->
                                    Markdown.Parser.parse rawContent
                                        |> Result.mapError (\_ -> "Markdown error")
                                        |> Result.map
                                            (\blocks ->
                                                List.Extra.findMap
                                                    (\block ->
                                                        case block of
                                                            Block.Heading Block.H1 inlines ->
                                                                Just (Block.extractInlineText inlines)

                                                            _ ->
                                                                Nothing
                                                    )
                                                    blocks
                                            )
                                        |> Result.andThen
                                            (Result.fromMaybe <|
                                                ("Expected to find an H1 heading for page " ++ filePath)
                                            )
                                        |> Result.mapError Exception.fromString
                                        |> BackendTask.fromResult
                                )
                        )
            )


titleAndDescription : String -> BackendTask Throwable { title : String, description : String }
titleAndDescription filePath =
    filePath
        |> StaticFile.onlyFrontmatter
            (Decode.map2 (\title description -> { title = title, description = description })
                (Json.Decode.Extra.optionalField "title" Decode.string)
                (Json.Decode.Extra.optionalField "description" Decode.string)
            )
        |> BackendTask.throw
        |> BackendTask.andThen
            (\metadata ->
                Maybe.map2 (\title description -> { title = title, description = description })
                    metadata.title
                    metadata.description
                    |> Maybe.map BackendTask.succeed
                    |> Maybe.withDefault
                        (StaticFile.bodyWithoutFrontmatter filePath
                            |> BackendTask.throw
                            |> BackendTask.andThen
                                (\rawContent ->
                                    Markdown.Parser.parse rawContent
                                        |> Result.mapError (\_ -> "Markdown error")
                                        |> Result.map
                                            (\blocks ->
                                                Maybe.map
                                                    (\title ->
                                                        { title = title
                                                        , description =
                                                            case metadata.description of
                                                                Just description ->
                                                                    description

                                                                Nothing ->
                                                                    findDescription blocks
                                                        }
                                                    )
                                                    (case metadata.title of
                                                        Just title ->
                                                            Just title

                                                        Nothing ->
                                                            findH1 blocks
                                                    )
                                            )
                                        |> Result.andThen (Result.fromMaybe <| "Expected to find an H1 heading for page " ++ filePath)
                                        |> Result.mapError Exception.fromString
                                        |> BackendTask.fromResult
                                )
                        )
            )


findH1 : List Block -> Maybe String
findH1 blocks =
    List.Extra.findMap
        (\block ->
            case block of
                Block.Heading Block.H1 inlines ->
                    Just (Block.extractInlineText inlines)

                _ ->
                    Nothing
        )
        blocks


findDescription : List Block -> String
findDescription blocks =
    blocks
        |> List.Extra.findMap
            (\block ->
                case block of
                    Block.Paragraph inlines ->
                        Just (MarkdownExtra.extractInlineText inlines)

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault ""


titleFromFrontmatter : String -> BackendTask Throwable (Maybe String)
titleFromFrontmatter filePath =
    StaticFile.onlyFrontmatter
        (Json.Decode.Extra.optionalField "title" Decode.string)
        filePath
        |> BackendTask.throw


withoutFrontmatter :
    Markdown.Renderer.Renderer view
    -> String
    -> BackendTask Throwable (List Block)
withoutFrontmatter renderer filePath =
    (filePath
        |> StaticFile.bodyWithoutFrontmatter
        |> BackendTask.throw
        |> BackendTask.andThen
            (\rawBody ->
                rawBody
                    |> Markdown.Parser.parse
                    |> Result.mapError (\_ -> Exception.fromString "Couldn't parse markdown.")
                    |> BackendTask.fromResult
            )
    )
        |> BackendTask.andThen
            (\blocks ->
                blocks
                    |> Markdown.Renderer.render renderer
                    -- we don't want to encode the HTML since it contains functions so it's not serializable
                    -- but we can at least make sure there are no errors turning it into HTML before encoding it
                    |> Result.map (\_ -> blocks)
                    |> Result.mapError (\error -> Exception.fromString error)
                    |> BackendTask.fromResult
            )


withFrontmatter :
    (frontmatter -> List Block -> value)
    -> Decoder frontmatter
    -> Markdown.Renderer.Renderer view
    -> String
    -> BackendTask Throwable value
withFrontmatter constructor frontmatterDecoder_ renderer filePath =
    BackendTask.map2 constructor
        (StaticFile.onlyFrontmatter
            frontmatterDecoder_
            filePath
            |> BackendTask.throw
        )
        (StaticFile.bodyWithoutFrontmatter
            filePath
            |> BackendTask.throw
            |> BackendTask.andThen
                (\rawBody ->
                    rawBody
                        |> Markdown.Parser.parse
                        |> Result.mapError (\_ -> Exception.fromString "Couldn't parse markdown.")
                        |> BackendTask.fromResult
                )
            |> BackendTask.andThen
                (\blocks ->
                    blocks
                        |> Markdown.Renderer.render renderer
                        -- we don't want to encode the HTML since it contains functions so it's not serializable
                        -- but we can at least make sure there are no errors turning it into HTML before encoding it
                        |> Result.map (\_ -> blocks)
                        |> Result.mapError (\error -> Exception.fromString error)
                        |> BackendTask.fromResult
                )
        )
