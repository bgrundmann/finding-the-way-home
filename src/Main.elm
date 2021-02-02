module Main exposing (main)

import Browser
import Dict
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , fillPortion
        , height
        , minimum
        , mouseOver
        , padding
        , paragraph
        , row
        , scale
        , scrollbarY
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Eval
import EvalResult exposing (EvalResult)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html)
import Image exposing (Image)
import ImageEditor
import List
import Move exposing (ExprValue(..), Move(..), UserDefinedOrPrimitive(..))
import MoveParseError exposing (MoveParseError)
import MoveParser exposing (Definitions)
import Palette exposing (greenBook, redBook, white)
import Pile
import Primitives exposing (primitives)
import Task



-- MODEL
-- In backwards mode we display the initial image on the right and evaluate the moves backwards


type alias Model =
    { initialImage : ImageEditor.State
    , movesText : String
    , movesAndDefinitions :
        Result MoveParseError
            { moves : List Move
            , definitions : Definitions
            }
    , performanceFailure : Maybe EvalResult.EvalError
    , finalImage : Image -- The last successfully computed final Image
    , backwards : Bool
    }


type Msg
    = SetMoves String
    | ImageEditorChanged ImageEditor.Msg
    | ToggleForwardsBackwards
    | Save
    | SelectLoad
    | Load File
    | GotLoad String


defaultInfoText : String
defaultInfoText =
    String.join "\n" (List.map Move.signature (primitives |> Dict.values))
        ++ """

repeat N
  move1
  ...
end

def moveName pile|N ...
  [doc ...]
  move1
  ...
end

ignore move
"""


apply : List Move -> Image -> EvalResult
apply moves image =
    Eval.eval image moves


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialImage =
            [ ( "deck", Pile.poker_deck ) ]

        movesText =
            ""
    in
    ( { initialImage = ImageEditor.init initialImage
      , finalImage = initialImage
      , movesText = movesText
      , movesAndDefinitions = Ok { moves = [], definitions = Dict.empty }
      , performanceFailure = Nothing
      , backwards = False
      }
    , Cmd.none
    )


{-| Parse the moves text and update the model accordingly. This does NOT apply the moves.
-}
parseMoves : Model -> Model
parseMoves model =
    { model
        | movesAndDefinitions =
            MoveParser.parseMoves primitives model.movesText
                |> Result.map (\{ definitions, moves } -> { definitions = MoveParser.definitionsFromList definitions, moves = moves })
    }


applyMoves : Model -> Model
applyMoves model =
    case model.movesAndDefinitions of
        Err _ ->
            model

        Ok { moves } ->
            let
                maybeBackwardsMoves =
                    if model.backwards then
                        Move.backwardsMoves moves

                    else
                        moves

                result =
                    apply maybeBackwardsMoves (ImageEditor.getImage model.initialImage)
            in
            { model
                | finalImage = result.lastImage
                , performanceFailure = result.error
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMoves text ->
            ( { model | movesText = text }
                |> parseMoves
                |> applyMoves
            , Cmd.none
            )

        ImageEditorChanged imageEditorMsg ->
            ( { model | initialImage = ImageEditor.update imageEditorMsg model.initialImage }
                |> applyMoves
            , Cmd.none
            )

        ToggleForwardsBackwards ->
            ( toggleForwardsBackwards model, Cmd.none )

        Save ->
            ( model, save model )

        SelectLoad ->
            ( model, Select.file [ "text/text" ] Load )

        Load file ->
            ( model, Task.perform GotLoad (File.toString file) )

        GotLoad content ->
            ( { model | movesText = content }
                |> parseMoves
                |> applyMoves
            , Cmd.none
            )


save : Model -> Cmd Msg
save model =
    Download.string "moves.txt" "text/text" model.movesText


toggleForwardsBackwards : Model -> Model
toggleForwardsBackwards model =
    let
        newInitialImage =
            model.finalImage
    in
    -- reapplyMoves will take care of updating performanceFailure
    { model
        | initialImage = ImageEditor.init newInitialImage
        , backwards = not model.backwards
        , finalImage = newInitialImage
    }
        |> applyMoves



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


topBar : Element Msg
topBar =
    row [ spacing 10, padding 10, Background.color greenBook, Font.color white, width fill ]
        [ el [ Font.bold ] (text "🍺 Virtual Denis Behr")
        , Input.button [ mouseOver [ scale 1.1 ] ] { label = text "Save", onPress = Just Save }
        , Input.button [ mouseOver [ scale 1.1 ] ] { label = text "Load", onPress = Just SelectLoad }
        ]


view : Model -> Html Msg
view model =
    let
        directionButton =
            let
                directionLabel =
                    if model.backwards then
                        "☚"

                    else
                        "☛"
            in
            Input.button [ Font.size 35, Font.color Palette.blueBook, padding 10, mouseOver [ scale 1.1 ] ]
                { onPress = Just ToggleForwardsBackwards
                , label = text directionLabel
                }

        initialImageView =
            Element.Lazy.lazy2 ImageEditor.view ImageEditorChanged model.initialImage

        viewMessage title m =
            Element.column [ width fill, height (minimum 0 (fillPortion 1)), scrollbarY, spacing 10 ]
                [ el [ Font.bold, width fill ] (text title)
                , el [ width fill, height fill, Font.family [ Font.monospace ] ] (text m)
                ]

        viewErrorMessage title error =
            Element.column [ width fill, height (minimum 0 (fillPortion 1)), scrollbarY, spacing 10 ]
                [ el [ Font.bold, width fill ] (text title)
                , el [ width fill, height fill ] error
                ]

        ( movesBorderColor, infoText ) =
            case ( model.movesAndDefinitions, model.performanceFailure ) of
                ( Ok _, Nothing ) ->
                    ( greenBook, viewMessage "Reference" defaultInfoText )

                ( Err errorMsg, _ ) ->
                    ( redBook
                    , viewErrorMessage "That makes no sense"
                        (MoveParseError.view model.movesText errorMsg)
                    )

                ( Ok _, Just error ) ->
                    ( redBook
                    , viewErrorMessage "Failure during performance"
                        (EvalResult.viewError model.movesText error)
                    )

        movesView =
            Element.column [ width fill, height fill, spacing 10 ]
                [ Input.multiline [ width fill, height (minimum 0 (fillPortion 2)), scrollbarY, Border.color movesBorderColor ]
                    { label =
                        Input.labelAbove []
                            (row [ spacing 40 ]
                                [ el [ Font.bold ] (text "Definitions & Moves")
                                , directionButton
                                ]
                            )
                    , onChange = SetMoves
                    , text = model.movesText
                    , placeholder = Nothing
                    , spellcheck = False
                    }
                , infoText
                ]

        finalImageView =
            el [ width fill, height fill ] (Element.Lazy.lazy2 Image.view (\t -> el [ Font.bold ] (text t)) model.finalImage)

        mainElements =
            if model.backwards then
                [ finalImageView, movesView, initialImageView ]

            else
                [ initialImageView, movesView, finalImageView ]
    in
    Element.layout [ width fill, height fill ] <|
        column [ width fill, height fill ]
            [ topBar
            , Element.column [ Element.padding 20, width fill, height fill, spacing 10 ]
                [ Element.row [ spacing 20, width fill, height fill ]
                    mainElements
                ]
            ]
