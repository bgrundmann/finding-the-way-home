module Main exposing (..)

import Browser
import Card exposing (Card, Pile, Suit, Value, poker_deck)
import Cardician exposing (..)
import Dict exposing (Dict)
import Element exposing (Element, el, fill, height, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Image exposing (Image)
import List
import Move exposing (Move(..))
import Result


tamariz_from_phoenix =
    """cut 26 deck table
faro table deck deck
cut 26 deck table
faro table deck deck
cut 26 deck table
faro table deck deck
cut 26 deck table
faro table deck deck
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 1 deck hand
cut 26 deck hand
cut 52 hand deck
cut 18 deck hand
faro hand deck deck
"""


type alias PileName =
    String


turnOver : Pile -> Pile
turnOver pile =
    List.reverse (List.map Card.turnOver pile)


cardician : Move -> Cardician ()
cardician move =
    case move of
        Cut { n, pile, to } ->
            Cardician.cutOff n pile
                |> andThen (Cardician.put to)

        Turnover pile ->
            Cardician.take pile
                |> andThen
                    (\cards ->
                        Cardician.put pile (turnOver cards)
                    )

        Faro { pile1, pile2, result } ->
            Cardician.take pile1
                |> andThen
                    (\cards1 ->
                        Cardician.take pile2
                            |> andThen
                                (\cards2 ->
                                    Cardician.faro cards1 cards2
                                        |> andThen
                                            (\cards ->
                                                Cardician.put result cards
                                            )
                                )
                    )


apply : List Move -> Image -> Result String Image
apply moves image =
    let
        c =
            List.map cardician moves
                |> List.foldl Cardician.compose (Cardician.return ())

        ( or_error, newImage ) =
            perform c image
    in
    case or_error of
        Err msg ->
            Err msg

        Ok () ->
            Ok newImage


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias ErrorMessage =
    String


type PerformanceResult
    = InvalidMoves ErrorMessage
    | CannotPerform (List Move) ErrorMessage
    | Performed (List Move) Image


type alias Model =
    { initialImage : Image
    , movesText : String
    , performanceResult : PerformanceResult
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialImage =
            [ ( "deck", poker_deck ) ]

        movesText =
            ""

        performanceResult =
            Performed [] initialImage
    in
    ( { initialImage = initialImage, movesText = movesText, performanceResult = performanceResult }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetMoves String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMoves text ->
            let
                movesOrError =
                    Move.parseMoves text

                performanceResult =
                    case movesOrError of
                        Err whyInvalidMoves ->
                            InvalidMoves whyInvalidMoves

                        Ok moves ->
                            case apply moves model.initialImage of
                                Err whyCannotPerform ->
                                    CannotPerform moves whyCannotPerform

                                Ok i ->
                                    Performed moves i
            in
            ( { model | movesText = text, performanceResult = performanceResult }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


blue =
    Element.rgb255 238 238 238


view : Model -> Html Msg
view model =
    let
        buttons =
            Element.row [ spacing 10 ]
                [ Input.button
                    [ Element.padding 10
                    , Border.rounded 5
                    , Background.color blue
                    ]
                    { label = text "Tamariz from phoenix", onPress = Just (SetMoves tamariz_from_phoenix) }
                ]

        initialImageView =
            Image.view model.initialImage

        movesBorderColor =
            case model.performanceResult of
                Performed _ _ ->
                    Element.rgb 0 255 0

                InvalidMoves _ ->
                    Element.rgb 255 0 255

                CannotPerform _ _ ->
                    Element.rgb 255 0 0

        movesView =
            Input.multiline [ width fill, height fill, Border.color movesBorderColor ]
                { label = Input.labelAbove [] (Element.text "Moves")
                , onChange = SetMoves
                , text = model.movesText
                , placeholder = Nothing
                , spellcheck = False
                }

        finalImageView =
            let
                viewErrorMessage m =
                    el [ Font.bold, width fill, height fill ] (text m)
            in
            case model.performanceResult of
                Performed _ finalImage ->
                    Image.view finalImage

                InvalidMoves errorMsg ->
                    viewErrorMessage errorMsg

                CannotPerform _ errorMsg ->
                    viewErrorMessage errorMsg
    in
    Element.layout []
        (Element.column [ Element.padding 20, width fill, height fill, spacing 10 ]
            [ buttons
            , Element.row [ spacing 10, width fill, height fill ]
                [ initialImageView
                , movesView
                , finalImageView
                ]
            ]
        )
