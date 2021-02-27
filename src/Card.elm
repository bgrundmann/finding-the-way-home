module Card exposing
    ( BackColor(..)
    , Card
    , CardDesign(..)
    , Suit(..)
    , Value(..)
    , all_suits
    , all_values
    , blank
    , card
    , cardParser
    , fromString
    , toString
    , turnover
    , view
    , withHidden
    , withVisible
    )

import Element exposing (Element, el, text)
import Element.Font as Font
import Palette
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
    | Blank


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


{-| The most primitive card is one that is blank on both sides.
-}
blank : Card
blank =
    Card { visible = Blank, hidden = Blank }


{-| A red backed regular card. Face is hidden.
-}
card : Value -> Suit -> Card
card value suit =
    Card { hidden = Face ( value, suit ), visible = Back Red }


{-| Set the visible side of a card.
-}
withVisible : CardDesign -> Card -> Card
withVisible design (Card c) =
    Card { c | visible = design }


{-| Set the hidden side of a card.
-}
withHidden : CardDesign -> Card -> Card
withHidden design (Card c) =
    Card { c | hidden = design }


all_suits : List Suit
all_suits =
    [ Clubs, Hearts, Spades, Diamonds ]


all_values : List Value
all_values =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


turnover : Card -> Card
turnover (Card { visible, hidden }) =
    Card { visible = hidden, hidden = visible }


{-| View the visible side of the card
-}
view : Card -> Element msg
view (Card { visible, hidden }) =
    el [ Element.behindContent <| el [ Element.alpha 0.4 ] <| viewCardDesign hidden ] <|
        viewCardDesign visible


viewCardDesign : CardDesign -> Element msg
viewCardDesign d =
    case d of
        Face rf ->
            viewRegularFace rf

        Back b ->
            viewBack b

        Blank ->
            text "_"


viewBack : BackColor -> Element msg
viewBack b =
    let
        cardBack =
            0x0001F0A0

        ( code, color ) =
            case b of
                Red ->
                    ( cardBack, Palette.redBook )

                Green ->
                    ( cardBack, Palette.greenBook )

                Blue ->
                    ( cardBack, Palette.blueBook )
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
    case visible of
        Back Red ->
            cardDesignToString hidden

        _ ->
            cardDesignToString hidden ++ "/" ++ cardDesignToString visible


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

        Blank ->
            "_"


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
    succeed
        (\one two ->
            case two of
                Nothing ->
                    -- Shortcut syntax, one only means visible side is red back
                    -- Or with other words this is a facedown red backed card
                    Card { visible = Back Red, hidden = one }

                Just design ->
                    -- Otherwise you still write the hidden side first.
                    Card { visible = design, hidden = one }
        )
        |= cardDesignParser
        |= oneOf
            [ succeed identity
                |. symbol "/"
                |= cardDesignParser
                |> map Just
            , succeed () |> map (always Nothing)
            ]


fromString : String -> Maybe Card
fromString s =
    Result.toMaybe (Parser.run (cardParser |. Parser.end) s)
