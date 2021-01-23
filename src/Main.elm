module Main exposing (..)

import Browser
import Card exposing (Card, Pile, Suit, Value, poker_deck)
import Cardician exposing (..)
import Dict exposing (Dict)
import Element exposing (Element, el, fill, fillPortion, height, minimum, padding, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Image exposing (Image)
import ImageEditor
import List
import Move exposing (ExprValue(..), Move(..), MovesOrPrimitive(..))
import MoveParser
import Palette exposing (greenBook, redBook)
import Result


defaultInfoText =
    String.join "\n" (List.map Move.signature Move.primitives)
        ++ """

repeat N
  move1
  ...
end

def move-name pile|N ...
  move1
  ...
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


cardician : Move ExprValue -> Cardician ()
cardician move =
    case move of
        Repeat nExpr moves ->
            case nExpr of
                Int n ->
                    cardicianFromMoves moves
                        |> List.repeat n
                        |> List.foldl Cardician.compose (Cardician.return ())

                Pile _ ->
                    Cardician.fail "Internal error: type checker failed"

        Do { name, movesOrPrimitive, args } actuals ->
            case movesOrPrimitive of
                Moves moves ->
                    case Move.substituteArguments actuals moves of
                        Err msg ->
                            Cardician.fail ("Internal error: substitution failed " ++ msg)

                        Ok substitutedMoves ->
                            cardicianFromMoves substitutedMoves

                Primitive p ->
                    p actuals


cardicianFromMoves : List (Move ExprValue) -> Cardician ()
cardicianFromMoves moves =
    List.map cardician moves
        |> List.foldl Cardician.compose (Cardician.return ())


apply : List (Move ExprValue) -> Image -> Result String Image
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
    | CannotPerform (List (Move ExprValue)) ErrorMessage
    | Performed (List (Move ExprValue)) Image


type alias Model =
    { initialImage : ImageEditor.State
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
    ( { initialImage = ImageEditor.init initialImage, movesText = movesText, performanceResult = performanceResult }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetMoves String
    | ImageEditorChanged ImageEditor.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMoves text ->
            let
                performanceResult =
                    case MoveParser.parseMoves (Move.primitives |> List.map (\d -> ( d.name, d )) |> Dict.fromList) text of
                        Err whyInvalidMoves ->
                            InvalidMoves whyInvalidMoves

                        Ok { moves, definitions } ->
                            case apply moves (ImageEditor.getImage model.initialImage) of
                                Err whyCannotPerform ->
                                    CannotPerform moves whyCannotPerform

                                Ok i ->
                                    Performed moves i
            in
            ( { model | movesText = text, performanceResult = performanceResult }
            , Cmd.none
            )

        ImageEditorChanged state ->
            ( { model | initialImage = state }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        buttons =
            Element.none

        initialImageView =
            ImageEditor.view ImageEditorChanged model.initialImage

        viewMessage title m =
            Element.column [ width fill, height (fillPortion 1), spacing 10 ]
                [ el [ Font.bold, width fill ] (text title)
                , el [ width fill, height fill, Font.family [ Font.monospace ] ] (text m)
                ]

        ( movesBorderColor, infoText ) =
            case model.performanceResult of
                Performed _ _ ->
                    ( greenBook, viewMessage "Reference" defaultInfoText )

                InvalidMoves errorMsg ->
                    ( redBook, viewMessage "Error" errorMsg )

                CannotPerform _ errorMsg ->
                    ( redBook, viewMessage "Error" errorMsg )

        movesView =
            Element.column [ width fill, height fill, spacing 10 ]
                [ Input.multiline [ width fill, height (minimum 0 (fillPortion 2)), scrollbarY, Border.color movesBorderColor ]
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
                    el [ width fill, height fill ] (Image.view (\t -> el [ Font.bold ] (text t)) finalImage)

                InvalidMoves errorMsg ->
                    initialImageView

                CannotPerform _ errorMsg ->
                    initialImageView
    in
    Element.layout [ width fill, height fill ]
        (Element.column [ Element.padding 20, width fill, height fill, spacing 10 ]
            [ buttons
            , Element.row [ spacing 10, width fill, height fill ]
                [ initialImageView
                , movesView
                , finalImageView
                ]
            ]
        )
