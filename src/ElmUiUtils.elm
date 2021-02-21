module ElmUiUtils exposing (boldMono, id, mono, onKey, tabEl)

import Element exposing (Element, centerX, centerY, el, paddingEach, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Palette


id : String -> Element.Attribute msg
id idString =
    Element.htmlAttribute (Html.Attributes.id idString)


mono : String -> Element msg
mono s =
    el [ Font.family [ Font.monospace ] ] (text s)


boldMono : String -> Element msg
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


tabEl : (page -> msg) -> page -> { page : page, label : String } -> Element msg
tabEl makeMsg activePage thisTab =
    let
        isSelected =
            thisTab.page == activePage

        padOffset =
            if isSelected then
                0

            else
                2

        borderWidths =
            if isSelected then
                { left = 2, top = 2, right = 2, bottom = 0 }

            else
                { bottom = 2, top = 0, left = 0, right = 0 }

        corners =
            if isSelected then
                { topLeft = 6, topRight = 6, bottomLeft = 0, bottomRight = 0 }

            else
                { topLeft = 0, topRight = 0, bottomLeft = 0, bottomRight = 0 }
    in
    el
        [ Border.widthEach borderWidths
        , Border.roundEach corners
        , Border.color Palette.greenBook

        --, onClick <| UserSelectedTab tab
        ]
        (el
            [ centerX
            , centerY
            , paddingEach { left = 30, right = 30, top = 10 + padOffset, bottom = 10 - padOffset }
            ]
            -- (Element.link [] { url = toUrl thisTab.page, label = text thisTab.label })
            (Input.button [] { onPress = Just (makeMsg thisTab.page), label = text thisTab.label })
        )
