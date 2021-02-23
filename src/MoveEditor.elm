module MoveEditor exposing
    ( DisplayMode(..)
    , Model
    , Msg
    , ReasonsNotToShow(..)
    , StoredState
    , editDefinition
    , encodeStoredState
    , getDefinitions
    , getDisplayMode
    , getLibrary
    , getStoredState
    , init
    , reasonsNotToShow
    , setDisplayMode
    , setLibrary
    , storedStateDecoder
    , update
    , updateMovesText
    , view
    )

import Browser.Dom as Dom
import Element
    exposing
        ( Element
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , minimum
        , mouseOver
        , padding
        , paddingXY
        , px
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
import Move
    exposing
        ( ExprValue(..)
        , Move(..)
        , MoveDefinition
        , MoveIdentifier
        , UserDefinedOrPrimitive(..)
        )
import MoveLibrary exposing (MoveLibrary)
import MoveLibraryJson
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


type EvalResultState
    = Complete EvalResult
    | Partial
        { partial : EvalResult

        -- We know that a partial Result had to end in an error state
        -- (EarlyExit)
        , partialError : EvalResult.EvalError
        , complete : EvalResult
        }


type alias Model =
    { initialImage : ImageEditor.State
    , text : String
    , movesAndDefinitions :
        Result MoveParseError
            { moves : List Move
            , definitions : List MoveDefinition
            }
    , evalResult : EvalResultState
    , backwards : Bool
    , library : MoveLibrary
    , displayMode : DisplayMode
    }


type Msg
    = SetMoves String
    | MoveDefinitionsIntoLibrary
    | ImageEditorChanged ImageEditor.Msg
    | ToggleForwardsBackwards
    | Save
    | SelectLoad
    | Load File
    | GotLoad String
    | Focus (Result Dom.Error ())
    | Play
    | SetStep Int
    | StepOver


type DisplayMode
    = Show
    | Edit


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
    String.join "\n" (List.map Move.signature (Primitives.primitives |> MoveLibrary.toListTopSort))
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


freshStartInitialState : StoredState
freshStartInitialState =
    { text = ""
    , initialImage =
        [ ( "deck", Pile.poker_deck ) ]
    , backwards = False
    , library = Primitives.primitives
    }


init : Maybe StoredState -> ( Model, Cmd Msg )
init maybePreviousState =
    let
        previousStateOrInitial =
            Maybe.withDefault freshStartInitialState maybePreviousState

        model =
            { initialImage = ImageEditor.init previousStateOrInitial.initialImage
            , evalResult =
                -- This just creates a dummy eval result which will be overriden in a moment
                -- by applyMoves
                Complete <| Eval.eval (always True) previousStateOrInitial.initialImage []
            , text = previousStateOrInitial.text
            , movesAndDefinitions = Ok { moves = [], definitions = [] }
            , backwards = previousStateOrInitial.backwards
            , library = previousStateOrInitial.library
            , displayMode = Edit
            }
    in
    ( model
        |> parseMoves
        |> applyMoves (always True)
    , Cmd.none
    )


{-| Parse the moves text and update the model accordingly. This does NOT apply the moves.
In fact it leaves evalResult untouched, so that in case of a parse error the last
successful evaluation stays visible. This is the more useful behaviour during an interactive
edit session.
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


continueUntilCurrentMax : Model -> (EvalResult -> Bool)
continueUntilCurrentMax model =
    case model.evalResult of
        Complete complete ->
            always True

        Partial { partial } ->
            \{ steps } -> steps <= partial.steps


applyMoves : (EvalResult -> Bool) -> Model -> Model
applyMoves continue model =
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

                initialImage =
                    ImageEditor.getImage model.initialImage

                result =
                    Eval.eval continue initialImage maybeBackwardsMoves

                evalResult =
                    case result.error of
                        Just ({ problem } as error) ->
                            case problem of
                                EvalResult.EarlyExit ->
                                    Partial
                                        { partial = result
                                        , partialError = error
                                        , complete =
                                            Eval.eval
                                                (always True)
                                                initialImage
                                                maybeBackwardsMoves
                                        }

                                _ ->
                                    Complete result

                        Nothing ->
                            Complete result

                -- apply
                -- maybeBackwardsMoves
                -- (ImageEditor.getImage model.initialImage)
                --
                -- TODO FIX
            in
            { model
                | evalResult = evalResult
            }


updateMovesText : (String -> String) -> Model -> Model
updateMovesText f model =
    { model | text = f model.text }
        |> parseMoves
        |> applyMoves (always True)


editDefinition : MoveIdentifier -> Model -> Model
editDefinition id model =
    let
        ( moveDefinitions, newLibrary ) =
            MoveLibrary.remove id model.library
    in
    { model | library = newLibrary }
        |> updateMovesText
            (\t ->
                ((moveDefinitions |> List.map ViewMove.prettyPrintDefinition)
                    |> String.join "\n\n"
                )
                    ++ "\n"
                    ++ t
            )


type ReasonsNotToShow
    = ParseError
    | BackwardsNotSupported


{-| Could the display Mode be changed to show?
-}
reasonsNotToShow : Model -> Maybe ReasonsNotToShow
reasonsNotToShow model =
    if model.backwards then
        Just BackwardsNotSupported

    else
        case model.movesAndDefinitions of
            Ok _ ->
                Nothing

            Err _ ->
                Just ParseError


setDisplayMode : DisplayMode -> Model -> Model
setDisplayMode displayMode model =
    case displayMode of
        Show ->
            if reasonsNotToShow model == Nothing then
                { model | displayMode = displayMode }

            else
                model

        Edit ->
            { model | displayMode = displayMode }


getDisplayMode : Model -> DisplayMode
getDisplayMode model =
    model.displayMode


setLibrary : MoveLibrary -> Model -> Model
setLibrary library model =
    { model | library = library }
        |> updateMovesText identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMoves text ->
            ( updateMovesText (always text) model
            , Cmd.none
            )

        MoveDefinitionsIntoLibrary ->
            let
                newModel =
                    case model.movesAndDefinitions of
                        Err _ ->
                            model

                        Ok { definitions, moves } ->
                            if List.isEmpty definitions then
                                model

                            else
                                let
                                    newLibrary =
                                        List.foldl MoveLibrary.insert model.library definitions

                                    newMovesText =
                                        List.map prettyPrint moves
                                            |> String.join "\n"
                                in
                                { model
                                    | library = newLibrary
                                    , text = newMovesText
                                    , movesAndDefinitions = Ok { definitions = [], moves = moves }
                                }
            in
            ( newModel, Cmd.none )

        ImageEditorChanged imageEditorMsg ->
            let
                ( newInitialImage, imageCmd ) =
                    ImageEditor.update Focus imageEditorMsg model.initialImage

                newModel =
                    { model | initialImage = newInitialImage }
                        |> applyMoves (continueUntilCurrentMax model)
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
                |> applyMoves (always True)
            , Cmd.none
            )

        Focus (Ok ()) ->
            ( model, Cmd.none )

        Focus (Err _) ->
            ( model, Cmd.none )

        SetStep newValue ->
            ( model |> applyMoves (\{ steps } -> steps <= newValue)
            , Cmd.none
            )

        StepOver ->
            case model.evalResult of
                Complete _ ->
                    ( model, Cmd.none )

                Partial { partial } ->
                    ( model
                        |> applyMoves
                            (\{ steps, trace } ->
                                steps <= partial.steps + 1 || EvalResult.traceDepth trace > EvalResult.traceDepth partial.trace
                            )
                    , Cmd.none
                    )

        Play ->
            ( model |> applyMoves (always True)
            , Cmd.none
            )


save : Model -> Cmd Msg
save model =
    Download.string "moves.txt" "text/text" model.text


type alias StoredState =
    { text : String
    , initialImage : Image
    , backwards : Bool
    , library : MoveLibrary
    }


encodeStoredState : StoredState -> Encode.Value
encodeStoredState ss =
    Encode.object
        [ ( "text", Encode.string ss.text )
        , ( "initialImage", Image.encode ss.initialImage )
        , ( "backwards", Encode.bool ss.backwards )
        , ( "library", MoveLibraryJson.encode ss.library )
        ]


storedStateDecoder : Decode.Decoder StoredState
storedStateDecoder =
    Decode.map4 StoredState
        (Decode.field "text" Decode.string)
        (Decode.field "initialImage" Image.decoder)
        (Decode.field "backwards" Decode.bool)
        (Decode.field "library" MoveLibraryJson.decoder)


{-| Return the data that we want to store to be able to restore this session.
-}
getStoredState : Model -> StoredState
getStoredState model =
    { text = model.text
    , initialImage = model.initialImage |> ImageEditor.getImage
    , backwards = model.backwards
    , library = model.library
    }


toggleForwardsBackwards : Model -> Model
toggleForwardsBackwards model =
    let
        newInitialImage =
            case model.evalResult of
                Complete r ->
                    r.lastImage

                Partial { complete } ->
                    complete.lastImage
    in
    -- applyMoves will take care of updating evalResult
    -- TODO: Adjust partial result how?
    { model
        | initialImage = ImageEditor.init newInitialImage
        , backwards = not model.backwards
    }
        |> applyMoves (always True)



-- VIEW


viewDirectionButton : Model -> Element Msg
viewDirectionButton model =
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


editView : Model -> Element Msg
editView model =
    let
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

        maybeEvalError =
            case model.evalResult of
                Complete r ->
                    r.error

                Partial { partial } ->
                    partial.error

        ( movesBorderColor, infoText ) =
            case ( model.movesAndDefinitions, maybeEvalError ) of
                ( Ok _, Nothing ) ->
                    ( greenBook, viewMessage "Reference" defaultInfoText )

                ( Err errorMsg, _ ) ->
                    ( redBook
                    , viewErrorMessage "That makes no sense"
                        (MoveParseError.view model.text errorMsg)
                    )

                ( Ok _, Just error ) ->
                    let
                        title =
                            case error.problem of
                                EvalResult.EarlyExit ->
                                    "Stopped during performance"

                                _ ->
                                    "Failure during performance"
                    in
                    ( redBook
                    , viewErrorMessage title
                        (EvalResult.viewError error)
                    )

        moveDefinitionsIntoLibraryButton =
            case model.movesAndDefinitions of
                Err _ ->
                    Element.none

                Ok { definitions } ->
                    case definitions of
                        [] ->
                            Element.none

                        ds ->
                            Input.button
                                [ padding 3
                                , Border.rounded 3
                                , Font.color Palette.white
                                , Font.size 14
                                , Background.color Palette.greenBook
                                , mouseOver [ Border.glow Palette.grey 1 ]
                                ]
                                { onPress = Just MoveDefinitionsIntoLibrary
                                , label = text "Move definitions into Library"
                                }
                                |> el [ paddingXY 2 0 ]
    in
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
                        , viewDirectionButton model
                        , moveDefinitionsIntoLibraryButton
                        ]
                    )
            , onChange = SetMoves
            , text = model.text
            , placeholder = Nothing
            , spellcheck = False
            }
        , infoText
        ]


viewStepsInputs : Model -> Element Msg
viewStepsInputs model =
    let
        -- TODO
        ( current, max ) =
            case model.evalResult of
                Complete r ->
                    ( r.steps, r.steps )

                Partial { partial, complete } ->
                    ( partial.steps, complete.steps )

        ( oneStepBackMsg, oneStepForwardMsg ) =
            ( SetStep (Basics.max 0 (current - 1))
            , SetStep (Basics.min max (current + 1))
            )

        allBackwardButton =
            Input.button Palette.regularButton { onPress = Just (SetStep 0), label = text "«" }

        oneStepBackButton =
            Input.button Palette.regularButton { onPress = Just oneStepBackMsg, label = text "‹" }

        oneStepForwardButton =
            Input.button Palette.regularButton { onPress = Just oneStepForwardMsg, label = text "›" }

        stepOverButton =
            Input.button Palette.regularButton { onPress = Just StepOver, label = text "⤼" }

        allForwardButton =
            Input.button Palette.regularButton { onPress = Just Play, label = text "»" }

        currentValue =
            text (String.fromInt current ++ "/" ++ String.fromInt max)
    in
    row [ paddingXY 10 0, width fill, spacing 5 ]
        [ allBackwardButton
        , oneStepBackButton
        , oneStepForwardButton
        , stepOverButton
        , allForwardButton
        , currentValue
        , Input.slider
            [ height (px 30)
            , Element.behindContent
                (el
                    [ width fill
                    , height (px 2)
                    , centerY
                    , Background.color Palette.greenBook
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { onChange = SetStep << round
            , label = Input.labelHidden "Steps"
            , min = 0
            , max = toFloat max
            , step = Just 1
            , thumb = Input.defaultThumb
            , value =
                toFloat current
            }
        ]



-- ViewMove.view ViewMove.defaultConfig (Move.Do def exprs)


view : Model -> Element Msg
view model =
    let
        initialImageView =
            el [ width fill, height (minimum 0 fill), scrollbarY, paddingXY 20 10 ]
                (Element.Lazy.lazy2 ImageEditor.view ImageEditorChanged model.initialImage)

        finalImageView =
            let
                finalImage =
                    case model.evalResult of
                        Complete r ->
                            r.lastImage

                        Partial { partial } ->
                            partial.lastImage
            in
            el [ width fill, height (minimum 0 fill), scrollbarY, paddingXY 20 10 ]
                (Element.Lazy.lazy2 Image.view (\t -> el [ Font.bold ] (text t)) finalImage)

        withWidthPortion n =
            el [ width (fillPortion n), height fill ]

        movesView =
            case ( model.displayMode, model.evalResult ) of
                ( Show, Partial { partial, partialError } ) ->
                    Element.column [ width fill, height (minimum 0 fill), spacing 20, paddingXY 0 10 ]
                        [ el [ height fill, width fill, scrollbarY ] <|
                            EvalResult.viewEvalTrace ViewMove.defaultConfig partial.trace
                        , EvalResult.viewError partialError
                        ]

                ( Show, Complete _ ) ->
                    case model.movesAndDefinitions of
                        Err _ ->
                            Element.none

                        Ok { definitions, moves } ->
                            el
                                [ width fill
                                , height (minimum 0 fill)
                                , scrollbarY
                                , paddingXY 20 10
                                ]
                            <|
                                ViewMove.viewDefinitionsAndMoves ViewMove.defaultConfig definitions moves

                ( Edit, _ ) ->
                    editView model

        ( leftImage, rightImage ) =
            if model.backwards then
                ( finalImageView, initialImageView )

            else
                ( initialImageView, finalImageView )

        mainElements =
            Element.row [ spacing 20, width fill, height (minimum 0 fill) ]
                [ withWidthPortion 4 leftImage
                , withWidthPortion 3 movesView
                , withWidthPortion 4 rightImage
                ]
    in
    case model.displayMode of
        Show ->
            Element.column [ spacing 10, width fill, height (minimum 0 fill) ]
                [ mainElements
                , viewStepsInputs model
                ]

        Edit ->
            mainElements
