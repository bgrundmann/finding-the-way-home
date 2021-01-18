module Card exposing (..)

import Element exposing (Element, el, text)
import Element.Font as Font
import List


type alias Pile =
    List Card


poker_deck : Pile
poker_deck =
    List.concatMap (\s -> List.map (\v -> card v s) all_values) all_suits



-- A red backed face up regular card


card : Value -> Suit -> Card
card value suit =
    { face = Face ( value, suit ), back = Back Red, orientation = FaceUp }


type Suit
    = Clubs
    | Spades
    | Hearts
    | Diamonds


all_suits : List Suit
all_suits =
    [ Clubs, Hearts, Spades, Diamonds ]


type Value
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


all_values : List Value
all_values =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


type alias RegularFace =
    ( Value, Suit )


type BackColor
    = Red
    | Green
    | Blue


type CardDesign
    = Face RegularFace
    | Back BackColor


type alias Card =
    { face : CardDesign
    , back : CardDesign
    , orientation : Orientation
    }


type Orientation
    = FaceUp
    | FaceDown


turnOver c =
    { c
        | orientation =
            case c.orientation of
                FaceUp ->
                    FaceDown

                FaceDown ->
                    FaceUp
    }


view : Card -> Element msg
view { face, back, orientation } =
    let
        d =
            case orientation of
                FaceUp ->
                    face

                FaceDown ->
                    back
    in
    el [ Font.size 60 ] (viewCardDesign d)


viewCardDesign : CardDesign -> Element msg
viewCardDesign d =
    case d of
        Face rf ->
            viewRegularFace rf

        Back b ->
            viewBack b


viewBack : BackColor -> Element msg
viewBack b =
    let
        cardBack =
            0x0001F0A0

        ( code, color ) =
            case b of
                Red ->
                    ( cardBack, Element.rgb255 139 0 0 )

                Green ->
                    ( cardBack, Element.rgb255 72 157 45 )

                Blue ->
                    ( cardBack, Element.rgb255 39 139 13 )
    in
    el [ Font.color color ] (Element.text (Char.fromCode code |> String.fromChar))


viewRegularFace : RegularFace -> Element msg
viewRegularFace ( value, suit ) =
    let
        ( suitVal, color ) =
            case suit of
                Spades ->
                    ( 0x0001F0A0, Element.rgb255 0 0 0 )

                Hearts ->
                    ( 0x0001F0B0, Element.rgb255 139 0 0 )

                Diamonds ->
                    ( 0x0001F0C0, Element.rgb255 139 0 0 )

                Clubs ->
                    ( 0x0001F0D0, Element.rgb255 0 0 0 )

        faceVal =
            case value of
                Ace ->
                    1

                Two ->
                    2

                Three ->
                    3

                Four ->
                    4

                Five ->
                    5

                Six ->
                    6

                Seven ->
                    7

                Eight ->
                    8

                Nine ->
                    9

                Ten ->
                    10

                Jack ->
                    11

                Queen ->
                    12

                King ->
                    13

        c =
            Char.fromCode (suitVal + faceVal)
                |> String.fromChar
                |> text
    in
    el [ Font.color color ] c
