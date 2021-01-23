module ImageEditor exposing (State, getImage, init, view)

import Card
import Element exposing (Element, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Image exposing (Image, PileName, view)
import Palette exposing (dangerousButton, regularButton)


green =
    Element.rgb255 0 87 45


type State
    = State
        { image : Image
        }


type Msg
    = Delete PileName
    | Add


update : Msg -> State -> State
update msg (State state) =
    case msg of
        Delete pileName ->
            State { image = Image.update pileName (\_ -> Nothing) state.image }

        Add ->
            -- Todo: deal with duplicate pile names
            State { image = Image.update "deck2" (\_ -> Just Card.poker_deck) state.image }


init : Image -> State
init i =
    State { image = i }


getImage : State -> Image
getImage (State { image }) =
    image


viewPileNameAndButtons : (State -> msg) -> State -> String -> Element msg
viewPileNameAndButtons toMsg state pileName =
    let
        event msg =
            update msg state
                |> toMsg
    in
    row [ width fill, spacing 5 ]
        [ el [ width fill, Font.bold ] (text pileName)
        , Input.button regularButton { onPress = Nothing, label = text "Edit" }
        , Input.button dangerousButton { onPress = Delete pileName |> event |> Just, label = text "Delete" }
        ]


view : (State -> msg) -> State -> Element msg
view toMsg ((State s) as state) =
    let
        event msg =
            update msg state
                |> toMsg
    in
    column [ width fill, height fill, spacing 10 ]
        [ Image.view (viewPileNameAndButtons toMsg state) s.image
        , Input.button regularButton { onPress = Add |> event |> Just, label = text "Add" }
        ]
