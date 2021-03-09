module Palette exposing
    ( black
    , blueBook
    , clubsBlack
    , dangerousButton
    , font
    , greenBook
    , greenButton
    , grey
    , largeTextSpacing
    , linkButton
    , normalFontSize
    , normalTextSpacing
    , redBook
    , regularButton
    , smallFontSize
    , spadesBlack
    , transparentGrey
    , white
    )

import Element
    exposing
        ( Attribute
        , mouseOver
        , padding
        , rgb255
        , rgba255
        , spacing
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


font : Element.Attribute msg
font =
    Font.family [ Font.typeface "Times" ]


smallFontSize : Element.Attr decorative msg
smallFontSize =
    Font.size 12


normalFontSize : Element.Attr decorative msg
normalFontSize =
    Font.size 18


normalTextSpacing : Element.Attribute msg
normalTextSpacing =
    spacing 10


largeTextSpacing : Element.Attribute msg
largeTextSpacing =
    spacing 20


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


spadesBlack : Element.Color
spadesBlack =
    black


clubsBlack : Element.Color
clubsBlack =
    rgb255 75 50 75


grey : Element.Color
grey =
    rgb255 128 128 128


transparentGrey : Element.Color
transparentGrey =
    Element.rgba255 128 128 128 0.5


linkButton : List (Attribute msg)
linkButton =
    [ Border.color <| rgba255 255 255 255 255
    , Border.widthEach
        { bottom = 1
        , left = 0
        , top = 0
        , right = 0
        }
    , mouseOver [ Border.color redBook ]
    ]


regularButton : List (Attribute msg)
regularButton =
    [ mouseOver [ Border.glow grey 2 ]
    , Background.color white
    , Border.color blueBook
    , Border.width 2
    , Font.color black
    , padding 5
    , Border.rounded 5
    ]


greenButton : List (Attribute msg)
greenButton =
    [ mouseOver [ Border.glow grey 2 ]
    , Background.color white
    , Border.color greenBook
    , Border.width 2
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
    , Border.width 2
    , Border.color redBook
    ]
