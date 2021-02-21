module Main exposing (main)

import Browser
import Browser.Navigation as Nav
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
        , paragraph
        , px
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
import ElmUiUtils exposing (mono)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Move exposing (ExprValue(..), Move(..), MoveIdentifier, UserDefinedOrPrimitive(..))
import MoveEditor
import MoveLibrary exposing (MoveLibrary)
import MoveParser
import Palette exposing (greenBook)
import Ports
import Primitives
import Route
import Toasts
import Url exposing (Url)
import ViewMove



-- MODEL


type alias Model =
    { moveEditor : MoveEditor.Model
    , selectedMove : Maybe MoveIdentifier
    , activePage : ActivePage
    , toasts : Toasts.Toasts
    , userKnows : String
    , nav : Nav.Key
    }


type ActivePage
    = EditorPage
    | LibraryPage


type Msg
    = MoveEditorChanged MoveEditor.Msg
    | ToastsChanged Toasts.Msg
    | SetActivePage ( ActivePage, MoveEditor.DisplayMode )
    | EditDefinition MoveIdentifier
    | UserKnowsChanged String
    | GotInitialLibrary (Result Http.Error String)
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


loadInitialLibrary : Cmd Msg
loadInitialLibrary =
    Http.get { url = "/init.txt", expect = Http.expectString GotInitialLibrary }


init : Encode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init previousStateJson url key =
    let
        ( maybePreviousStoredState, firstToast, loadCmd ) =
            case Decode.decodeValue MoveEditor.storedStateDecoder previousStateJson of
                Ok previousState ->
                    ( Just previousState, "Welcome back.\nPrevious state loaded.", Cmd.none )

                Err errorMessage ->
                    {-
                       let
                           _ =
                               Debug.log "loading failed" errorMessage
                       in
                    -}
                    ( Nothing
                    , "Welcome!\nLooks like this is your first time here.  Or maybe you cleared the browser cache?"
                    , loadInitialLibrary
                    )

        ( moveEditor, moveEditorCmd ) =
            MoveEditor.init maybePreviousStoredState

        ( toasts, toastCmd ) =
            Toasts.add (Toasts.toast firstToast) Toasts.init
    in
    ( { moveEditor = moveEditor
      , activePage = EditorPage
      , selectedMove = Nothing
      , toasts = toasts
      , userKnows = ""
      , nav = key
      }
    , Cmd.batch
        [ loadCmd
        , Cmd.map MoveEditorChanged moveEditorCmd
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

        SetActivePage ( page, dm ) ->
            case ( page, dm ) of
                ( LibraryPage, _ ) ->
                    ( model, Nav.pushUrl model.nav (Route.routeToString (Route.Library model.selectedMove)) )

                ( EditorPage, MoveEditor.Show ) ->
                    if MoveEditor.couldShow model.moveEditor then
                        ( model, Nav.pushUrl model.nav (Route.routeToString Route.Show) )

                    else
                        let
                            ( toasts, toastCmd ) =
                                Toasts.add (Toasts.toast "You are not ready to show this!") model.toasts
                        in
                        ( { model | toasts = toasts }, Cmd.map ToastsChanged toastCmd )

                ( EditorPage, MoveEditor.Edit ) ->
                    ( model, Nav.pushUrl model.nav (Route.routeToString Route.Editor) )

        EditDefinition id ->
            ( { model
                | moveEditor =
                    MoveEditor.editDefinition id model.moveEditor
                        |> MoveEditor.setDisplayMode MoveEditor.Edit
                , activePage = EditorPage
              }
            , Cmd.none
            )

        UserKnowsChanged s ->
            ( { model | userKnows = s }, Cmd.none )

        GotInitialLibrary (Err error) ->
            let
                ( toasts, toastCmd ) =
                    Toasts.add (Toasts.toast "Failed to load initial library") model.toasts
            in
            ( { model | toasts = toasts }, Cmd.map ToastsChanged toastCmd )

        GotInitialLibrary (Ok what) ->
            case MoveParser.parseMoves Primitives.primitives what of
                Err _ ->
                    ( model, Cmd.none )

                Ok { definitions } ->
                    let
                        newLibrary =
                            MoveLibrary.fromList (MoveLibrary.toListTopSort Primitives.primitives ++ definitions)
                    in
                    ( { model | moveEditor = MoveEditor.setLibrary newLibrary model.moveEditor }, Cmd.none )

        UrlChanged url ->
            case Route.urlToRoute url of
                Nothing ->
                    ( model, Cmd.none )

                Just Route.Show ->
                    ( { model
                        | activePage = EditorPage
                        , moveEditor = MoveEditor.setDisplayMode MoveEditor.Show model.moveEditor
                      }
                    , Cmd.none
                    )

                Just Route.Editor ->
                    ( { model
                        | activePage = EditorPage
                        , moveEditor = MoveEditor.setDisplayMode MoveEditor.Edit model.moveEditor
                      }
                    , Cmd.none
                    )

                Just (Route.Library selectedMove) ->
                    ( { model | activePage = LibraryPage, selectedMove = selectedMove }, Cmd.none )

        UrlRequested (Browser.Internal url) ->
            ( model, Nav.pushUrl model.nav (Url.toString url) )

        UrlRequested (Browser.External href) ->
            ( model, Nav.load href )



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


getUrlOfPage : ( ActivePage, MoveEditor.DisplayMode ) -> String
getUrlOfPage ( activePage, displayMode ) =
    let
        route =
            case ( activePage, displayMode ) of
                ( EditorPage, MoveEditor.Show ) ->
                    Route.Show

                ( EditorPage, MoveEditor.Edit ) ->
                    Route.Editor

                ( LibraryPage, _ ) ->
                    Route.Library Nothing
    in
    Route.routeToString route


topBar : ActivePage -> MoveEditor.DisplayMode -> Element Msg
topBar activePage displayMode =
    let
        tab =
            ElmUiUtils.tabEl SetActivePage ( activePage, displayMode )

        tabs =
            row [ centerX ]
                [ tab { page = ( EditorPage, MoveEditor.Show ), label = "Show" }
                , tab { page = ( EditorPage, MoveEditor.Edit ), label = "Learn" }
                , tab { page = ( LibraryPage, displayMode ), label = "Library" }
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
            (text "🍺 Finding The Way Home")
        , tabs
        , el
            [ width fill
            , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , paddingEach { left = 0, right = 0, top = 12, bottom = 8 }
            , Border.color Palette.greenBook
            ]
            (text "")
        ]


viewLibrary : Maybe MoveIdentifier -> MoveLibrary -> Element Msg
viewLibrary selectedMove library =
    let
        selectedDefinition =
            selectedMove
                |> Maybe.andThen (\m -> MoveLibrary.get m library)
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
            (MoveLibrary.toListAlphabetic library
                |> List.map
                    (\md ->
                        let
                            listEl =
                                column [ width fill, spacing 5 ]
                                    [ mono (Move.signature md)
                                    , el [ Font.size 12 ] (text md.doc)
                                    ]
                        in
                        Element.link []
                            { url = Route.routeToString (Route.Library (Just (Move.identifier md)))
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
                    let
                        uses =
                            case
                                MoveLibrary.getUsedBy (Move.identifier d) library
                                    |> List.filterMap (\id -> MoveLibrary.get id library)
                            of
                                [] ->
                                    Element.none

                                l ->
                                    paragraph [ spacing 20 ]
                                        (text "This is used by "
                                            :: (List.map
                                                    (\md ->
                                                        Element.link Palette.linkButton
                                                            { url =
                                                                Route.Library (Just (Move.identifier md))
                                                                    |> Route.routeToString
                                                            , label = mono (Move.signature md)
                                                            }
                                                    )
                                                    l
                                                    |> List.intersperse (text ", ")
                                               )
                                        )

                        editButton =
                            case d.body of
                                Primitive _ ->
                                    Element.none

                                UserDefined _ ->
                                    Input.button Palette.regularButton
                                        { onPress = EditDefinition (Move.identifier d) |> Just
                                        , label = text "Edit"
                                        }

                        viewConfig =
                            ViewMove.defaultConfig
                                |> ViewMove.withMoveUrl (\id -> Route.routeToString (Route.Library (Just id)))
                    in
                    column [ spacing 30 ]
                        [ ViewMove.viewDefinition viewConfig d
                        , uses
                        , editButton
                        ]
            )
        ]


view : Model -> Browser.Document Msg
view model =
    let
        page =
            -- This is obviously a little silly, but this isn't really there for any
            -- form of security
            if String.reverse model.userKnows /= "larutan" then
                [ Input.text
                    [ width (px 200)
                    , centerX
                    , centerY
                    , Input.focusedOnLoad
                    ]
                    { label = Input.labelAbove [] (text "Dai Vernon said: Be ...")
                    , onChange = UserKnowsChanged
                    , placeholder = Nothing
                    , text = model.userKnows
                    }
                ]

            else
                let
                    content =
                        case model.activePage of
                            EditorPage ->
                                Element.Lazy.lazy MoveEditor.view model.moveEditor
                                    |> Element.map MoveEditorChanged

                            LibraryPage ->
                                Element.Lazy.lazy2
                                    viewLibrary
                                    model.selectedMove
                                    (MoveEditor.getLibrary model.moveEditor)
                in
                [ topBar model.activePage (MoveEditor.getDisplayMode model.moveEditor)
                , content
                ]

        body =
            Element.layout [ width fill, height fill, Toasts.view model.toasts ] <|
                column [ width fill, height fill ]
                    page
    in
    { title = "Finding The Way Home"
    , body = [ body ]
    }
