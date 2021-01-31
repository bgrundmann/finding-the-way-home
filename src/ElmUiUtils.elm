module ElmUiUtils exposing (onKey, wrapped)

import Element exposing (Element, spacing, text)
import Html.Events
import Json.Decode as Decode


wrapped : String -> Element msg
wrapped longText =
    let
        lines =
            String.lines longText

        formatLine line =
            -- For each line we want to preserve the indentation
            -- but allow wrapping afterwards to deal with overly
            -- long error messages nicely
            let
                withoutIndent =
                    String.trimLeft line

                indentLen =
                    String.length line
                        - String.length withoutIndent

                indent =
                    String.repeat indentLen " "

                restOfLine =
                    String.words withoutIndent
                        |> List.intersperse " "
            in
            Element.row [] [ text indent, restOfLine |> List.map text |> Element.paragraph [] ]
    in
    Element.column [ spacing 10 ]
        (List.map formatLine lines)


onKey : { enter : Maybe msg, escape : Maybe msg } -> Element.Attribute msg
onKey { enter, escape } =
    let
        succeedIfInteresting what =
            case what of
                Nothing ->
                    Decode.fail "not interested"

                Just w ->
                    Decode.succeed w
    in
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            succeedIfInteresting enter

                        else if key == "Escape" then
                            succeedIfInteresting escape

                        else
                            Decode.fail "Not the enter or escape key"
                    )
            )
        )
