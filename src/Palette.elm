module Palette exposing
    ( black
    , blueBook
    , dangerousButton
    , greenBook
    , grey
    , redBook
    , regularButton
    , transparentGrey
    , white
    )

import Element exposing (Attribute, mouseOver, padding, rgb255, scale)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


redBook : Element.Color
redBook =
    rgb255 128 0 0


blueBook : Element.Color
blueBook =
    rgb255 1 1 75


greenBook : Element.Color
greenBook =
    rgb255 2 99 47


white : Element.Color
white =
    rgb255 255 255 255


black : Element.Color
black =
    rgb255 0 0 0


grey : Element.Color
grey =
    rgb255 128 128 128


transparentGrey : Element.Color
transparentGrey =
    Element.rgba255 128 128 128 0.5


regularButton : List (Attribute msg)
regularButton =
    [ mouseOver [ Border.glow grey 2 ]
    , Background.color white
    , Border.color blueBook
    , Border.width 1
    , Font.color black
    , padding 5
    , Border.rounded 5
    ]


dangerousButton : List (Attribute msg)
dangerousButton =
    [ mouseOver [ Border.glow grey 2 ]
    , Background.color white
    , Font.color black
    , Font.bold
    , padding 5
    , Border.rounded 5
    , Border.width 1
    , Border.color redBook
    ]
