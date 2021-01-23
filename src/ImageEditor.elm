module ImageEditor exposing (State, getImage, init, view)

import Element exposing (Element, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Image exposing (Image, view)
import Palette exposing (dangerousButton, regularButton)


green =
    Element.rgb255 0 87 45


type State
    = State
        { image : Image
        }


init : Image -> State
init i =
    State { image = i }


getImage : State -> Image
getImage (State { image }) =
    image


viewPileNameAndButtons : String -> Element msg
viewPileNameAndButtons pileName =
    row [ width fill, spacing 5 ]
        [ el [ width fill ] (text pileName)
        , Input.button regularButton { onPress = Nothing, label = text "Edit" }
        , Input.button dangerousButton { onPress = Nothing, label = text "Delete" }
        ]


view : (State -> msg) -> State -> Element msg
view toMsg (State { image }) =
    Image.view viewPileNameAndButtons image
