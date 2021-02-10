module MoveEditor exposing
    ( Model
    , Msg
    , StoredState
    , encodeStoredState
    , getDefinitions
    , getStoredState
    , init
    , storedStateDecoder
    , update
    , view
    )

import Browser
import Browser.Dom as Dom
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
        , paddingXY
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
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Move exposing (ExprValue(..), Move(..), UserDefinedOrPrimitive(..))
import MoveParseError exposing (MoveParseError)
import MoveParser exposing (Definitions)
import Palette exposing (greenBook, redBook, white)
import Pile
import Ports
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
    | Focus (Result Dom.Error ())


getDefinitions : Model -> Definitions
getDefinitions model =
    case model.movesAndDefinitions of
        Err _ ->
            Dict.empty

        Ok { definitions } ->
            definitions


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
  [temp t1 t2...]
  [nested definition1]
  ...
  move1
  ...
end

ignore move
"""


apply : List Move -> Image -> EvalResult
apply moves image =
    Eval.eval image moves


freshStartInitialState : StoredState
freshStartInitialState =
    { movesText = ""
    , initialImage =
        [ ( "deck", Pile.poker_deck ) ]
    , backwards = False
    }


init : Maybe StoredState -> ( Model, Cmd Msg )
init maybePreviousState =
    let
        previousStateOrInitial =
            Maybe.withDefault freshStartInitialState maybePreviousState

        model =
            { initialImage = ImageEditor.init previousStateOrInitial.initialImage
            , finalImage = previousStateOrInitial.initialImage
            , movesText = previousStateOrInitial.movesText
            , movesAndDefinitions = Ok { moves = [], definitions = Dict.empty }
            , performanceFailure = Nothing
            , backwards = previousStateOrInitial.backwards
            }
    in
    ( model
        |> parseMoves
        |> applyMoves
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
            let
                ( newInitialImage, imageCmd ) =
                    ImageEditor.update Focus imageEditorMsg model.initialImage

                newModel =
                    { model | initialImage = newInitialImage }
                        |> applyMoves
            in
            ( newModel
            , imageCmd
            )

        ToggleForwardsBackwards ->
            let
                newModel =
                    toggleForwardsBackwards model
            in
            ( newModel, Cmd.none )

        Save ->
            ( model, Cmd.none )

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

        Focus (Ok ()) ->
            ( model, Cmd.none )

        Focus (Err _) ->
            ( model, Cmd.none )


save : Model -> Cmd Msg
save model =
    Download.string "moves.txt" "text/text" model.movesText


type alias StoredState =
    { movesText : String
    , initialImage : Image
    , backwards : Bool
    }


encodeStoredState : StoredState -> Encode.Value
encodeStoredState ss =
    Encode.object
        [ ( "movesText", Encode.string ss.movesText )
        , ( "initialImage", Image.encode ss.initialImage )
        , ( "backwards", Encode.bool ss.backwards )
        ]


storedStateDecoder : Decode.Decoder StoredState
storedStateDecoder =
    Decode.map3 StoredState
        (Decode.field "movesText" Decode.string)
        (Decode.field "initialImage" Image.decoder)
        (Decode.field "backwards" Decode.bool)


{-| Return the data that we want to store to be able to restore this session.
-}
getStoredState : Model -> StoredState
getStoredState model =
    { movesText = model.movesText
    , initialImage = model.initialImage |> ImageEditor.getImage
    , backwards = model.backwards
    }


toggleForwardsBackwards : Model -> Model
toggleForwardsBackwards model =
    let
        newInitialImage =
            model.finalImage
    in
    -- applyMoves will take care of updating performanceFailure
    { model
        | initialImage = ImageEditor.init newInitialImage
        , backwards = not model.backwards
        , finalImage = newInitialImage
    }
        |> applyMoves



-- VIEW


view : Model -> Element Msg
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
            el [ width fill, height (minimum 0 fill), scrollbarY, paddingXY 20 10 ]
                (Element.Lazy.lazy2 ImageEditor.view ImageEditorChanged model.initialImage)

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
                [ Input.multiline
                    [ width fill
                    , height (minimum 0 (fillPortion 2))
                    , scrollbarY
                    , Border.color movesBorderColor
                    , Input.focusedOnLoad
                    ]
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
            el [ width fill, height (minimum 0 fill), scrollbarY, paddingXY 20 10 ]
                (Element.Lazy.lazy2 Image.view (\t -> el [ Font.bold ] (text t)) model.finalImage)

        withWidthPortion n =
            el [ width (fillPortion n), height fill ]

        mainElements =
            if model.backwards then
                [ withWidthPortion 4 finalImageView
                , withWidthPortion 3 movesView
                , withWidthPortion 4 initialImageView
                ]

            else
                [ withWidthPortion 4 initialImageView
                , withWidthPortion 3 movesView
                , withWidthPortion 4 finalImageView
                ]
    in
    Element.row [ spacing 20, width fill, height (minimum 0 fill) ]
        mainElements
