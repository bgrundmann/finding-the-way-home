module Card exposing
    ( Card
    , CardDesign(..)
    , Suit(..)
    , Value(..)
    , all_suits
    , all_values
    , card
    , cardParser
    , toString
    , turnover
    , view
    )

import Element exposing (Element, el, text)
import Element.Font as Font
import Parser exposing ((|.), (|=), Parser, map, oneOf, succeed, symbol)


type alias RegularFace =
    ( Value, Suit )


type BackColor
    = Red
    | Green
    | Blue


type CardDesign
    = Face RegularFace
    | Back BackColor


type Suit
    = Clubs
    | Spades
    | Hearts
    | Diamonds


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


{-| A playing card has two designs one on each side.
A visible side and a side that is hidden.
-}
type Card
    = Card { visible : CardDesign, hidden : CardDesign }


{-| A red backed regular card. Face is visible.
-}
card : Value -> Suit -> Card
card value suit =
    Card { visible = Face ( value, suit ), hidden = Back Red }


all_suits : List Suit
all_suits =
    [ Clubs, Hearts, Spades, Diamonds ]


all_values : List Value
all_values =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


turnover : Card -> Card
turnover (Card { visible, hidden }) =
    Card { visible = hidden, hidden = visible }


view : Card -> Element msg
view (Card { visible }) =
    el [ Font.size 60 ] (viewCardDesign visible)


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

                -- Unicode is weird 12 is the knight of ...
                Queen ->
                    13

                King ->
                    14

        c =
            Char.fromCode (suitVal + faceVal)
                |> String.fromChar
                |> text
    in
    el [ Font.color color ] c



-- Serialisation


toString : Card -> String
toString (Card { visible, hidden }) =
    case hidden of
        Back Red ->
            cardDesignToString visible

        _ ->
            cardDesignToString visible ++ "/" ++ cardDesignToString hidden


cardDesignToString : CardDesign -> String
cardDesignToString design =
    case design of
        Face ( value, suit ) ->
            valueToString value ++ suitToString suit

        Back Red ->
            "R"

        Back Blue ->
            "B"

        Back Green ->
            "G"


valueToString : Value -> String
valueToString v =
    case v of
        Ace ->
            "A"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"


suitToString : Suit -> String
suitToString s =
    case s of
        Clubs ->
            "C"

        Hearts ->
            "H"

        Spades ->
            "S"

        Diamonds ->
            "D"


suitParser : Parser Suit
suitParser =
    oneOf
        [ symbol "C" |> map (always Clubs)
        , symbol "H" |> map (always Hearts)
        , symbol "S" |> map (always Spades)
        , symbol "D" |> map (always Diamonds)
        ]


valueParser : Parser Value
valueParser =
    oneOf
        [ symbol "A" |> map (always Ace)
        , symbol "2" |> map (always Two)
        , symbol "3" |> map (always Three)
        , symbol "4" |> map (always Four)
        , symbol "5" |> map (always Five)
        , symbol "6" |> map (always Six)
        , symbol "7" |> map (always Seven)
        , symbol "8" |> map (always Eight)
        , symbol "9" |> map (always Nine)
        , symbol "10" |> map (always Ten)
        , symbol "J" |> map (always Jack)
        , symbol "Q" |> map (always Queen)
        , symbol "K" |> map (always King)
        ]


cardDesignParser : Parser CardDesign
cardDesignParser =
    oneOf
        [ succeed (\value suit -> Face ( value, suit ))
            |= valueParser
            |= suitParser
        , succeed Back
            |= oneOf
                [ symbol "R" |> map (always Red)
                , symbol "G" |> map (always Green)
                , symbol "B" |> map (always Blue)
                ]
        ]


cardParser : Parser Card
cardParser =
    succeed (\visible hidden -> Card { visible = visible, hidden = hidden })
        |= cardDesignParser
        |= oneOf
            [ succeed identity
                |. symbol "/"
                |= cardDesignParser
            , succeed (Back Red)
            ]
