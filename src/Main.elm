module Main exposing (main)

import Browser
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
        , padding
        , paddingEach
        , row
        , scrollbarX
        , scrollbarY
        , spacing
        , text
        , width
        )
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Move exposing (ExprValue(..), Move(..), MoveIdentifier, UserDefinedOrPrimitive(..))
import MoveEditor
import MoveLibrary exposing (MoveLibrary)
import Palette exposing (greenBook)
import Ports
import Toasts
import ViewMove



-- MODEL


type alias Model =
    { moveEditor : MoveEditor.Model
    , selectedMove : MoveIdentifier
    , activePage : ActivePage
    , toasts : Toasts.Toasts
    }


type ActivePage
    = MoveEditorPage
    | LibraryPage


type Msg
    = MoveEditorChanged MoveEditor.Msg
    | ToastsChanged Toasts.Msg
    | SetActivePage ActivePage
    | SelectDefinition MoveIdentifier


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
        ( maybePreviousStoredState, firstToast ) =
            case Decode.decodeValue MoveEditor.storedStateDecoder previousStateJson of
                Ok previousState ->
                    ( Just previousState, "Welcome back.\nPrevious state loaded." )

                Err _ ->
                    ( Nothing, "Welcome!\nLooks like this is your first time here.  Or maybe you cleared the browser cache?" )

        ( moveEditor, moveEditorCmd ) =
            MoveEditor.init maybePreviousStoredState

        ( toasts, toastCmd ) =
            Toasts.add (Toasts.toast firstToast) Toasts.init
    in
    ( { moveEditor = moveEditor
      , activePage = MoveEditorPage
      , selectedMove = ( "", [] )
      , toasts = toasts
      }
    , Cmd.batch
        [ Cmd.map MoveEditorChanged moveEditorCmd
        , Cmd.map ToastsChanged toastCmd
        ]
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

        ToastsChanged toastsMsg ->
            let
                newToasts =
                    Toasts.update toastsMsg model.toasts

                newModel =
                    { model | toasts = newToasts }
            in
            ( newModel, Cmd.none )

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


viewLibrary : MoveIdentifier -> MoveLibrary -> Element Msg
viewLibrary selectedMove library =
    let
        selectedDefinition =
            MoveLibrary.get selectedMove library
    in
    row [ spacing 10, width (minimum 0 fill), height (minimum 0 fill) ]
        [ column
            [ width (fillPortion 1)
            , height (minimum 0 fill)
            , scrollbarY
            , scrollbarX
            , Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
            , Border.color Palette.greenBook
            , spacing 10
            , padding 10
            ]
            (MoveLibrary.toList library
                |> List.map
                    (\md ->
                        let
                            listEl =
                                column [ width fill, spacing 5 ]
                                    [ el [ Font.family [ Font.monospace ] ] (text (Move.signature md))
                                    , el [ Font.size 12 ] (text md.doc)
                                    ]
                        in
                        Input.button []
                            { onPress = Just (SelectDefinition (Move.identifier md))
                            , label = listEl
                            }
                    )
            )
        , el
            [ width (fillPortion 3)
            , height (minimum 0 fill)
            , scrollbarY
            , padding 10
            ]
            (case selectedDefinition of
                Nothing ->
                    Element.none

                Just d ->
                    ViewMove.viewDefinition (Just SelectDefinition) d
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
                        (MoveEditor.getLibrary model.moveEditor)
    in
    Element.layout [ width fill, height fill, Toasts.view model.toasts ] <|
        column [ width fill, height fill ]
            [ topBar model.activePage
            , pageContent
            ]
