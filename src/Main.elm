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


defaultInfoText =
    """cut <N> <from-pile> <to-pile>
turnover <pile>
repeat <N>
  <move>
  ...
end
def <move-name> <pile>|<N> ...
  <move>
end
"""


sample =
    """def deal
cut 1 deck table
end
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

        Repeat n moves ->
            cardicianFromMoves moves
                |> List.repeat n
                |> List.foldl Cardician.compose (Cardician.return ())

        Do { name, moves } ->
            cardicianFromMoves moves


cardicianFromMoves : List Move -> Cardician ()
cardicianFromMoves moves =
    List.map cardician moves
        |> List.foldl Cardician.compose (Cardician.return ())


apply : List Move -> Image -> Result String Image
apply moves image =
    let
        c =
            cardicianFromMoves moves

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
                performanceResult =
                    case Move.parseMoves text of
                        Err whyInvalidMoves ->
                            InvalidMoves whyInvalidMoves

                        Ok { moves, definitions } ->
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
                    { label = text "Tamariz from phoenix", onPress = Just (SetMoves sample) }
                ]

        initialImageView =
            Image.view model.initialImage

        viewMessage title m =
            Element.column [ width fill, height fill, spacing 10 ]
                [ el [ Font.bold, width fill ] (text title)
                , el [ width fill, height fill, Font.family [ Font.monospace ] ] (text m)
                ]

        ( movesBorderColor, infoText ) =
            case model.performanceResult of
                Performed _ _ ->
                    ( Element.rgb 0 255 0, viewMessage "Reference" defaultInfoText )

                InvalidMoves errorMsg ->
                    ( Element.rgb 255 0 255, viewMessage "Error" errorMsg )

                CannotPerform _ errorMsg ->
                    ( Element.rgb 255 0 0, viewMessage "Error" errorMsg )

        movesView =
            Element.column [ width fill, height fill, spacing 10 ]
                [ Input.multiline [ width fill, height fill, Border.color movesBorderColor ]
                    { label = Input.labelAbove [] (Element.text "Moves")
                    , onChange = SetMoves
                    , text = model.movesText
                    , placeholder = Nothing
                    , spellcheck = False
                    }
                , infoText
                ]

        finalImageView =
            case model.performanceResult of
                Performed _ finalImage ->
                    Image.view finalImage

                InvalidMoves errorMsg ->
                    initialImageView

                CannotPerform _ errorMsg ->
                    initialImageView
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
