module MoveEditor exposing
    ( Model
    , Msg
    , StoredState
    , encodeStoredState
    , getDefinitions
    , getLibrary
    , getStoredState
    , init
    , storedStateDecoder
    , update
    , view
    )

import Browser.Dom as Dom
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
import Image exposing (Image)
import ImageEditor
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Move
    exposing
        ( ExprValue(..)
        , Move(..)
        , MoveDefinition
        , MoveIdentifier
        , UserDefinedOrPrimitive(..)
        )
import MoveLibrary exposing (MoveLibrary)
import MoveParseError exposing (MoveParseError)
import MoveParser
import Palette exposing (greenBook, redBook, white)
import Pile
import Primitives
import Task
import ViewMove exposing (prettyPrint, prettyPrintDefinition)



-- MODEL
-- In backwards mode we display the initial image on the right and evaluate the moves backwards
-- INVARIANT: A move either exists in the editor or in the library not both.
-- This is obviously not necessarily true while the user is entering a definition
-- that is already in the library.
-- But it will fail to compile and force him to remove the definition from the library
-- and move it into the editor, if he wants to go ahead.


type alias Model =
    { initialImage : ImageEditor.State
    , text : String
    , movesAndDefinitions :
        Result MoveParseError
            { moves : List Move
            , definitions : List MoveDefinition
            }
    , performanceFailure : Maybe EvalResult.EvalError
    , finalImage : Image -- The last successfully computed final Image
    , backwards : Bool
    , library : MoveLibrary
    }


type Msg
    = SetMoves String
    | UpdateLibrary MoveIdentifier
    | ImageEditorChanged ImageEditor.Msg
    | ToggleForwardsBackwards
    | Save
    | SelectLoad
    | Load File
    | GotLoad String
    | Focus (Result Dom.Error ())


getLibrary : Model -> MoveLibrary
getLibrary model =
    model.library


getDefinitions : Model -> Maybe (List MoveDefinition)
getDefinitions model =
    case model.movesAndDefinitions of
        Err _ ->
            Nothing

        Ok { definitions } ->
            Just definitions


defaultInfoText : String
defaultInfoText =
    String.join "\n" (List.map Move.signature (Primitives.primitives |> MoveLibrary.toList))
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
    { text = ""
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
            , text = previousStateOrInitial.text
            , movesAndDefinitions = Ok { moves = [], definitions = [] }
            , performanceFailure = Nothing
            , backwards = previousStateOrInitial.backwards
            , library = Primitives.primitives
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
            MoveParser.parseMoves model.library model.text
                |> Result.map
                    (\{ definitions, moves } ->
                        { definitions = definitions
                        , moves = moves
                        }
                    )
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
            ( { model | text = text }
                |> parseMoves
                |> applyMoves
            , Cmd.none
            )

        UpdateLibrary moveId ->
            let
                newModel =
                    case model.movesAndDefinitions of
                        Err _ ->
                            model

                        Ok { definitions, moves } ->
                            case List.partition (\md -> Move.identifier md == moveId) definitions of
                                ( [ md ], others ) ->
                                    -- By construction of the definitions list there will be only
                                    -- one move for each identifier (otherwise its a compile error)
                                    let
                                        newLibrary =
                                            MoveLibrary.insert md model.library

                                        newDefinitionsText =
                                            List.map prettyPrintDefinition others
                                                |> String.join "\n"

                                        newMovesText =
                                            List.map prettyPrint moves
                                                |> String.join "\n"

                                        newText =
                                            case ( newDefinitionsText, newMovesText ) of
                                                ( "", _ ) ->
                                                    newMovesText

                                                ( _, "" ) ->
                                                    newDefinitionsText

                                                ( a, b ) ->
                                                    a ++ "\n" ++ b
                                    in
                                    { model | library = newLibrary, text = newText }

                                ( _, _ ) ->
                                    model
            in
            ( newModel, Cmd.none )

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
            ( { model | text = content }
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
    Download.string "moves.txt" "text/text" model.text


type alias StoredState =
    { text : String
    , initialImage : Image
    , backwards : Bool
    }


encodeStoredState : StoredState -> Encode.Value
encodeStoredState ss =
    Encode.object
        [ ( "text", Encode.string ss.text )
        , ( "initialImage", Image.encode ss.initialImage )
        , ( "backwards", Encode.bool ss.backwards )
        ]


storedStateDecoder : Decode.Decoder StoredState
storedStateDecoder =
    Decode.map3 StoredState
        (Decode.field "text" Decode.string)
        (Decode.field "initialImage" Image.decoder)
        (Decode.field "backwards" Decode.bool)


{-| Return the data that we want to store to be able to restore this session.
-}
getStoredState : Model -> StoredState
getStoredState model =
    { text = model.text
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
                        (MoveParseError.view model.text errorMsg)
                    )

                ( Ok _, Just error ) ->
                    ( redBook
                    , viewErrorMessage "Failure during performance"
                        (EvalResult.viewError model.text error)
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
                    , text = model.text
                    , placeholder = Nothing
                    , spellcheck = False
                    }
                , case model.movesAndDefinitions of
                    Err _ ->
                        Element.none

                    Ok { definitions } ->
                        case definitions of
                            [] ->
                                Element.none

                            ds ->
                                let
                                    updateButton md =
                                        Input.button
                                            [ padding 3
                                            , Border.rounded 3
                                            , Font.color Palette.white
                                            , Background.color Palette.greenBook
                                            , mouseOver [ Border.glow Palette.grey 1 ]
                                            ]
                                            { onPress = Just (UpdateLibrary (Move.identifier md))
                                            , label = text (Move.signature md)
                                            }
                                            |> el [ paddingXY 2 0 ]

                                    updateButtons =
                                        List.map updateButton ds
                                in
                                Element.paragraph [ width fill, Font.size 14, spacing 10 ]
                                    (text "Update in library: " :: updateButtons)
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
