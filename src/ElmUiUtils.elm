module ElmUiUtils exposing (boldMono, id, mono, onKey)

import Element exposing (Element, el, spacing, text)
import Element.Font as Font
import Html.Attributes
import Html.Events
import Json.Decode as Decode


id : String -> Element.Attribute msg
id idString =
    Element.htmlAttribute (Html.Attributes.id idString)


mono s =
    el [ Font.family [ Font.monospace ] ] (text s)


boldMono s =
    el [ Font.family [ Font.monospace ], Font.bold ] (text s)


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
