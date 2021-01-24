module Main exposing (main)

import Browser
import Card exposing (poker_deck)
import Cardician
import Dict exposing (Dict)
import Element exposing (el, fill, fillPortion, height, minimum, padding, row, scrollbarY, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Image exposing (Image)
import ImageEditor
import List
import Move exposing (ExprValue(..), Move(..), MoveDefinition, MovesOrPrimitive(..))
import MoveParser
import Palette exposing (greenBook, redBook)


defaultInfoText : String
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


apply : List (Move ExprValue) -> Image -> Result Cardician.Error Image
apply moves image =
    Cardician.perform (Move.cardicianFromMoves moves) image


main : Program () Model Msg
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



-- In backwards mode we display the initial image on the right and evaluate the moves backwards


type alias Model =
    { initialImage : ImageEditor.State
    , movesText : String
    , backwards : Bool
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
    ( { initialImage = ImageEditor.init initialImage
      , movesText = movesText
      , performanceResult = performanceResult
      , backwards = False
      }
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
    | ToggleForwardsBackwards


primitivesDict : Dict String MoveDefinition
primitivesDict =
    Move.primitives |> List.map (\d -> ( d.name, d )) |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMoves text ->
            let
                performanceResult =
                    case MoveParser.parseMoves primitivesDict text of
                        Err whyInvalidMoves ->
                            InvalidMoves whyInvalidMoves (finalImageToDisplay model)

                        Ok { moves } ->
                            let
                                maybeBackwardsMoves =
                                    if model.backwards then
                                        Move.backwardsMoves identity moves

                                    else
                                        moves
                            in
                            case apply maybeBackwardsMoves (ImageEditor.getImage model.initialImage) of
                                Err whyCannotPerform ->
                                    CannotPerform moves whyCannotPerform

                                Ok i ->
                                    Performed moves i
            in
            ( { model | movesText = text, performanceResult = performanceResult }
            , Cmd.none
            )

        ImageEditorChanged state ->
            -- TODO: reapply moves
            ( { model | initialImage = state }
            , Cmd.none
            )

        ToggleForwardsBackwards ->
            ( toggleForwardsBackwards model, Cmd.none )


toggleForwardsBackwards : Model -> Model
toggleForwardsBackwards model =
    let
        newInitialImage =
            finalImageToDisplay model
    in
    -- TODO: reapply moves
    { model | initialImage = ImageEditor.init newInitialImage, backwards = not model.backwards }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        buttons =
            let
                directionLabel =
                    if model.backwards then
                        "Go forwards"

                    else
                        "Go backwards"
            in
            row [ width fill ] [ Input.button Palette.regularButton { onPress = Just ToggleForwardsBackwards, label = text directionLabel } ]

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

        mainElements =
            if model.backwards then
                [ finalImageView, movesView, initialImageView ]

            else
                [ initialImageView, movesView, finalImageView ]
    in
    Element.layout [ width fill, height fill ]
        (Element.column [ Element.padding 20, width fill, height fill, spacing 10 ]
            [ buttons
            , Element.row [ spacing 10, width fill, height fill ]
                mainElements
            ]
        )
