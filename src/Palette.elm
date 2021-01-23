module Palette exposing (dangerousButton, greenBook, redBook, regularButton)

import Element exposing (Attribute, padding, rgb255)
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


regularButton : List (Attribute msg)
regularButton =
    [ Background.color blueBook, Font.color white, padding 5, Border.rounded 3 ]


dangerousButton : List (Attribute msg)
dangerousButton =
    [ Background.color redBook, Font.color white, padding 5, Border.rounded 3 ]
