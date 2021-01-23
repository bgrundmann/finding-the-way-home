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


apply : List (Move ExprValue) -> Image -> Result Cardician.Error Image
apply moves image =
    perform (Move.cardicianFromMoves moves) image


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
    = InvalidMoves ErrorMessage Image -- Image is the final image last time moves were ok
    | CannotPerform (List (Move ExprValue)) Cardician.Error
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


{-| What is the image we should currently display on the right hand side?
-}
finalImageToDisplay : Model -> Image
finalImageToDisplay model =
    case model.performanceResult of
        InvalidMoves _ i ->
            i

        CannotPerform _ { lastImage } ->
            lastImage

        Performed _ image ->
            image


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
                            InvalidMoves whyInvalidMoves (finalImageToDisplay model)

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

                InvalidMoves errorMsg _ ->
                    ( redBook, viewMessage "Error" errorMsg )

                CannotPerform _ { message } ->
                    ( redBook, viewMessage "Error" message )

        movesView =
            Element.column [ width fill, height fill, spacing 10 ]
                [ Input.multiline [ width fill, height (minimum 0 (fillPortion 2)), scrollbarY, Border.color movesBorderColor ]
                    { label = Input.labelAbove [] (el [ Font.bold ] (text "Definitions & Moves"))
                    , onChange = SetMoves
                    , text = model.movesText
                    , placeholder = Nothing
                    , spellcheck = False
                    }
                , infoText
                ]

        finalImageView =
            let
                finalImage =
                    finalImageToDisplay model
            in
            el [ width fill, height fill ] (Image.view (\t -> el [ Font.bold ] (text t)) finalImage)
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
