module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Dict
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , minimum
        , mouseOver
        , padding
        , paddingEach
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
import MoveEditor
import MoveParseError exposing (MoveParseError)
import MoveParser exposing (Definitions)
import Palette exposing (greenBook, redBook, white)
import Pile
import Ports
import Primitives exposing (primitives)
import Task
import ViewMove



-- MODEL


type alias Model =
    { moveEditor : MoveEditor.Model
    , selectedMove : String -- That name is not guaranteed to actually exist.
    , activePage : ActivePage
    }


type ActivePage
    = MoveEditorPage
    | LibraryPage


type Msg
    = MoveEditorChanged MoveEditor.Msg
    | SetActivePage ActivePage
    | SelectDefinition String


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Encode.Value -> ( Model, Cmd Msg )
init previousStateJson =
    let
        maybePreviousStoredState =
            case Decode.decodeValue MoveEditor.storedStateDecoder previousStateJson of
                Ok previousState ->
                    Just previousState

                Err _ ->
                    Nothing

        ( moveEditor, cmd ) =
            MoveEditor.init maybePreviousStoredState
    in
    ( { moveEditor = moveEditor
      , activePage = MoveEditorPage
      , selectedMove = ""
      }
    , Cmd.map MoveEditorChanged cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveEditorChanged moveMsg ->
            let
                ( newMoveEditorModel, moveCmd ) =
                    MoveEditor.update moveMsg model.moveEditor

                newModel =
                    { model | moveEditor = newMoveEditorModel }

                saveCmd =
                    saveState newModel
            in
            ( newModel, Cmd.batch [ Cmd.map MoveEditorChanged moveCmd, saveCmd ] )

        SetActivePage page ->
            let
                newModel =
                    { model | activePage = page }
            in
            ( newModel, Cmd.none )

        SelectDefinition name ->
            ( { model | selectedMove = name }, Cmd.none )



-- Session storage


saveState : Model -> Cmd Msg
saveState model =
    let
        storedState =
            MoveEditor.getStoredState model.moveEditor
    in
    MoveEditor.encodeStoredState storedState
        |> Ports.storeState



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


tabEl : (tab -> msg) -> tab -> { tab : tab, label : String } -> Element msg
tabEl makeMsg selectedTab thisTab =
    let
        isSelected =
            thisTab.tab == selectedTab

        padOffset =
            if isSelected then
                0

            else
                2

        borderWidths =
            if isSelected then
                { left = 2, top = 2, right = 2, bottom = 0 }

            else
                { bottom = 2, top = 0, left = 0, right = 0 }

        corners =
            if isSelected then
                { topLeft = 6, topRight = 6, bottomLeft = 0, bottomRight = 0 }

            else
                { topLeft = 0, topRight = 0, bottomLeft = 0, bottomRight = 0 }
    in
    el
        [ Border.widthEach borderWidths
        , Border.roundEach corners
        , Border.color Palette.greenBook

        --, onClick <| UserSelectedTab tab
        ]
        (el
            [ centerX
            , centerY
            , paddingEach { left = 30, right = 30, top = 10 + padOffset, bottom = 10 - padOffset }
            ]
            (Input.button [] { onPress = Just (makeMsg thisTab.tab), label = text thisTab.label })
        )


topBar : ActivePage -> Element Msg
topBar activePage =
    let
        tab =
            tabEl SetActivePage activePage

        tabs =
            row [ centerX ]
                [ tab { tab = MoveEditorPage, label = "Performance" }
                , tab { tab = LibraryPage, label = "Library" }
                ]
    in
    row
        [ paddingEach { top = 10, bottom = 0, left = 10, right = 10 }

        -- , Background.color greenBook
        -- , Font.color white
        , width fill
        ]
        [ el
            [ Font.bold
            , width fill
            , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , paddingEach { left = 9, right = 0, top = 12, bottom = 8 }
            , Border.color Palette.greenBook
            ]
            (text "🍺 Virtual Denis Behr")
        , tabs
        , el
            [ width fill
            , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , paddingEach { left = 0, right = 0, top = 12, bottom = 8 }
            , Border.color Palette.greenBook
            ]
            (text "")

        -- , Input.button [ mouseOver [ scale 1.1 ] ] { label = text "Save", onPress = Just Save }
        -- , Input.button [ mouseOver [ scale 1.1 ] ] { label = text "Load", onPress = Just SelectLoad }
        ]


viewLibrary : String -> Definitions -> Element Msg
viewLibrary selectedMove definitions =
    let
        selectedDefinition =
            Dict.get selectedMove definitions
    in
    row [ spacing 10, width fill, height fill ]
        [ column
            [ width (fillPortion 1)
            , height fill
            , Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
            , Border.color Palette.greenBook
            , spacing 10
            , padding 10
            ]
            (Dict.values definitions
                |> List.map
                    (\md ->
                        let
                            listEl =
                                column [ width fill, spacing 5 ]
                                    [ el [ Font.family [ Font.monospace ] ] (text (Move.signature md))
                                    , el [ Font.size 12 ] (text md.doc)
                                    ]
                        in
                        Input.button [] { onPress = Just (SelectDefinition md.name), label = listEl }
                    )
            )
        , el
            [ width (fillPortion 4)
            , height fill
            ]
            (case selectedDefinition of
                Nothing ->
                    Element.none

                Just d ->
                    ViewMove.viewDefinition SelectDefinition d
            )
        ]


view : Model -> Html Msg
view model =
    let
        pageContent =
            case model.activePage of
                MoveEditorPage ->
                    Element.Lazy.lazy MoveEditor.view model.moveEditor
                        |> Element.map MoveEditorChanged

                LibraryPage ->
                    Element.Lazy.lazy2
                        viewLibrary
                        model.selectedMove
                        (MoveEditor.getDefinitions model.moveEditor)
    in
    Element.layout [ width fill, height fill ] <|
        column [ width fill, height fill ]
            [ topBar model.activePage
            , pageContent
            ]
