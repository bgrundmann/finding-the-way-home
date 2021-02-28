module ImageEditor exposing (Msg, State, getImage, init, update, view)

import Browser.Dom as Dom
import Card
import Dict exposing (Dict)
import Element exposing (Element, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ElmUiUtils exposing (onKey)
import Image exposing (Image, PileName, view)
import List.Extra
import MoveParser exposing (validatePileName)
import Palette exposing (dangerousButton, greenButton, regularButton)
import Pile
import Set exposing (Set)
import Task


type Editing
    = NotEditing
    | EditingPileName EditingPileNameState
    | EditingPile EditingPileState
    | ChoosingImageToAdd
    | Selected (Set ( PileName, Int ))


type alias State =
    { image : Image
    , editing : Editing
    , options : Dict String Image
    }


type alias EditingPileNameState =
    { oldName : String
    , newName : String
    , errorMessage : Maybe String
    }


type alias EditingPileState =
    { pileName : String
    , text : String
    }


type Msg
    = Delete PileName
    | AddPile Image
    | OpenAdd
    | SortPile PileName
    | ReversePile PileName
    | TurnoverPile PileName
    | StartEditPile PileName
    | EditPile EditingPileState
    | EditPileName { oldName : String, newName : String }
    | CancelEditing
    | Save
    | ToggleSelection PileName Int
    | TakeOut
    | Swap


defaultOptions : List ( String, Image )
defaultOptions =
    [ ( "red deck (face down)", [ ( "deck", Pile.poker_deck ) ] )
    , ( "blue deck (face down)"
      , [ ( "bluedeck", Pile.poker_deck |> List.map (Card.withVisible (Card.Back Card.Blue)) ) ]
      )
    , ( "green deck (face down)"
      , [ ( "greendeck", Pile.poker_deck |> List.map (Card.withVisible (Card.Back Card.Green)) ) ]
      )
    ]


init : Image -> State
init i =
    { image = i, editing = NotEditing, options = Dict.fromList defaultOptions }


isNameUsed : Image -> String -> Bool
isNameUsed image name =
    List.any (\n -> n == name) (Image.names image)


findUnusedName : Image -> String -> String
findUnusedName image prefix =
    let
        findLoop ndx =
            let
                candidate =
                    if ndx == 0 then
                        prefix

                    else
                        prefix ++ String.fromInt ndx
            in
            if isNameUsed image candidate then
                findLoop (ndx + 1)

            else
                candidate
    in
    findLoop 0


idOfPileNameEditor : String
idOfPileNameEditor =
    -- Similar to the id of the Pile we assume there is only one
    "imageEditorPileNameEditor"


takeOut : Set ( PileName, Int ) -> Image -> Image
takeOut what image =
    let
        helper ( pileName, num ) ( takenOutAcc, imageAcc ) =
            let
                ( maybePile, imageAcc1 ) =
                    Image.take pileName imageAcc

                pile =
                    Maybe.withDefault [] maybePile
            in
            case List.Extra.getAt num pile of
                Nothing ->
                    ( takenOutAcc, imageAcc )

                Just card ->
                    ( card :: takenOutAcc, Image.put pileName (List.Extra.removeAt num pile) imageAcc1 )

        ( takenOut, newImage ) =
            Set.toList what
                |> List.sortWith (\( _, n1 ) ( _, n2 ) -> compare n2 n1)
                |> List.foldl helper ( [], image )
    in
    Image.put (findUnusedName newImage "cards") takenOut newImage


update : (Result Dom.Error () -> a) -> Msg -> State -> ( State, Cmd a )
update toFocusMsg msg state =
    case msg of
        Delete pileName ->
            ( { state | image = Image.update pileName (\_ -> Nothing) state.image }, Cmd.none )

        EditPileName { oldName, newName } ->
            let
                wasAlreadyEditingThisPileName =
                    case state.editing of
                        EditingPileName pn ->
                            pn.oldName == oldName

                        _ ->
                            False

                errorMessagePileName =
                    validatePileName newName

                errorMessage =
                    case errorMessagePileName of
                        Just m ->
                            Just m

                        Nothing ->
                            if isNameUsed state.image newName && newName /= oldName then
                                Just "Name is used, pick another"

                            else
                                Nothing

                setFocusIfNecessary =
                    if wasAlreadyEditingThisPileName then
                        Cmd.none

                    else
                        Task.attempt toFocusMsg (Dom.focus idOfPileNameEditor)
            in
            ( { state
                | editing =
                    EditingPileName
                        { oldName = oldName
                        , newName = newName
                        , errorMessage = errorMessage
                        }
              }
            , setFocusIfNecessary
            )

        CancelEditing ->
            ( { state | editing = NotEditing }, Cmd.none )

        Save ->
            case state.editing of
                NotEditing ->
                    ( state, Cmd.none )

                Selected _ ->
                    ( state, Cmd.none )

                ChoosingImageToAdd ->
                    ( state, Cmd.none )

                EditingPile { pileName, text } ->
                    case Pile.fromString text of
                        Err _ ->
                            ( state, Cmd.none )

                        Ok cards ->
                            ( { state
                                | image = Image.update pileName (always (Just cards)) state.image
                                , editing = NotEditing
                              }
                            , Cmd.none
                            )

                EditingPileName { oldName, newName, errorMessage } ->
                    case errorMessage of
                        Just _ ->
                            ( state, Cmd.none )

                        Nothing ->
                            ( { state
                                | image = Image.renamePile oldName newName state.image
                                , editing = NotEditing
                              }
                            , Cmd.none
                            )

        ReversePile pileName ->
            let
                reverse maybePile =
                    Maybe.map (\pile -> List.reverse pile) maybePile
            in
            ( { state
                | image = Image.update pileName reverse state.image
                , editing = NotEditing
              }
            , Cmd.none
            )

        SortPile pileName ->
            let
                sort maybePile =
                    Maybe.map Pile.sort maybePile
            in
            ( { state
                | image = Image.update pileName sort state.image
                , editing = NotEditing
              }
            , Cmd.none
            )

        TurnoverPile pileName ->
            let
                turnover maybePile =
                    Maybe.map (\pile -> Pile.turnover pile) maybePile
            in
            ( { state
                | image = Image.update pileName turnover state.image
                , editing = NotEditing
              }
            , Cmd.none
            )

        StartEditPile pileName ->
            let
                text =
                    Image.get pileName state.image
                        |> Maybe.withDefault []
                        |> Pile.toString
            in
            ( { state | editing = EditingPile { pileName = pileName, text = text } }
            , Task.attempt toFocusMsg (Dom.focus idOfPileEditor)
            )

        EditPile newState ->
            ( { state | editing = EditingPile newState }
            , Cmd.none
            )

        OpenAdd ->
            ( { state | editing = ChoosingImageToAdd }
            , Cmd.none
            )

        AddPile imageToAdd ->
            let
                newImage =
                    List.foldl
                        (\( name, pile ) image ->
                            Image.update (findUnusedName image name)
                                (always (Just pile))
                                image
                        )
                        state.image
                        (Image.piles imageToAdd)
            in
            ( { state
                | image = newImage
                , editing = NotEditing
              }
            , Cmd.none
            )

        ToggleSelection pileName num ->
            let
                newEditing =
                    case state.editing of
                        EditingPileName _ ->
                            state.editing

                        EditingPile _ ->
                            state.editing

                        ChoosingImageToAdd ->
                            state.editing

                        NotEditing ->
                            Selected (Set.singleton ( pileName, num ))

                        Selected s ->
                            if Set.member ( pileName, num ) s then
                                let
                                    newSet =
                                        Set.remove ( pileName, num ) s
                                in
                                if Set.isEmpty newSet then
                                    NotEditing

                                else
                                    Selected newSet

                            else
                                Set.insert ( pileName, num ) s
                                    |> Selected
            in
            ( { state | editing = newEditing }, Cmd.none )

        Swap ->
            let
                newState =
                    case state.editing of
                        Selected s ->
                            case Set.toList s of
                                [ ( pileNameA, ndxA ), ( pileNameB, ndxB ) ] ->
                                    { state
                                        | image = Image.swap pileNameA ndxA pileNameB ndxB state.image
                                        , editing = NotEditing
                                    }

                                _ ->
                                    state

                        _ ->
                            state
            in
            ( newState, Cmd.none )

        TakeOut ->
            let
                newState =
                    case state.editing of
                        Selected s ->
                            { state
                                | image = takeOut s state.image
                                , editing = NotEditing
                            }

                        _ ->
                            state
            in
            ( newState, Cmd.none )


getImage : State -> Image
getImage { image } =
    image


ifEditingThisPileName : String -> State -> Maybe EditingPileNameState
ifEditingThisPileName pileName state =
    case state.editing of
        EditingPileName ({ oldName } as s) ->
            if pileName == oldName then
                Just s

            else
                Nothing

        EditingPile _ ->
            Nothing

        NotEditing ->
            Nothing

        ChoosingImageToAdd ->
            Nothing

        Selected _ ->
            Nothing


viewPileNameAndButtons : (Msg -> msg) -> State -> String -> Element msg
viewPileNameAndButtons toMsg state pileName =
    let
        pileNameLabelOrEditor =
            case ifEditingThisPileName pileName state of
                Nothing ->
                    Input.button []
                        { onPress =
                            EditPileName { oldName = pileName, newName = pileName }
                                |> toMsg
                                |> Just
                        , label = text pileName
                        }

                Just editingPileName ->
                    let
                        maybeWarnColor =
                            case editingPileName.errorMessage of
                                Nothing ->
                                    []

                                Just _ ->
                                    [ Border.color Palette.redBook ]
                    in
                    Input.text
                        (onKey
                            { enter = Save |> toMsg |> Just
                            , escape = CancelEditing |> toMsg |> Just
                            }
                            :: ElmUiUtils.id idOfPileNameEditor
                            :: maybeWarnColor
                        )
                        { label = Input.labelHidden "PileName"
                        , placeholder = Nothing
                        , text = editingPileName.newName
                        , onChange =
                            \s ->
                                EditPileName { oldName = pileName, newName = s }
                                    |> toMsg
                        }

        buttons =
            case ifEditingThisPile pileName state of
                Nothing ->
                    [ Input.button regularButton
                        { onPress = SortPile pileName |> toMsg |> Just
                        , label = text "Sort"
                        }
                    , Input.button regularButton
                        { onPress = ReversePile pileName |> toMsg |> Just
                        , label = text "Reverse"
                        }
                    , Input.button regularButton
                        { onPress = TurnoverPile pileName |> toMsg |> Just
                        , label = text "Turnover"
                        }
                    , Input.button regularButton
                        { onPress = StartEditPile pileName |> toMsg |> Just
                        , label = text "Edit"
                        }
                    , Input.button dangerousButton
                        { onPress = Delete pileName |> toMsg |> Just
                        , label = text "Delete"
                        }
                    ]

                Just _ ->
                    [ Input.button regularButton
                        { onPress = Save |> toMsg |> Just
                        , label = text "Save"
                        }
                    ]
    in
    row [ width fill, spacing 10 ]
        (el [ width fill, Font.bold ] pileNameLabelOrEditor :: buttons)


ifEditingThisPile : String -> State -> Maybe EditingPileState
ifEditingThisPile name state =
    case state.editing of
        NotEditing ->
            Nothing

        ChoosingImageToAdd ->
            Nothing

        EditingPileName _ ->
            Nothing

        EditingPile ({ pileName } as s) ->
            if pileName == name then
                Just s

            else
                Nothing

        Selected _ ->
            Nothing


viewImageToAddChooser : (Msg -> msg) -> Dict String Image -> Element msg
viewImageToAddChooser toMsg options =
    el [ width fill, height fill, Background.color Palette.transparentGrey ] <|
        column
            [ padding 10
            , Border.color Palette.blueBook
            , Border.width 2
            , Border.rounded 5
            , Background.color Palette.white
            , Font.color Palette.black
            , Element.alignBottom
            ]
            [ column [ Element.alignBottom, spacing 5 ]
                (options
                    |> Dict.toList
                    |> List.sortBy Tuple.first
                    |> List.map
                        (\( name, image ) ->
                            Input.button
                                [ width fill
                                , Element.mouseOver [ Font.color Palette.greenBook ]
                                ]
                                { onPress = Just (AddPile image |> toMsg), label = text name }
                        )
                )
            ]


idOfPileEditor : String
idOfPileEditor =
    -- Here we assume that there is a single imageEditor on the page
    -- and in addition we currently only allow at most one pile to
    -- be edited at any given moment in time.
    "imageEditorPileEditor"


view : (Msg -> msg) -> State -> Element msg
view toMsg state =
    let
        pilesView =
            Image.piles state.image
                |> List.sortBy Tuple.first
                |> List.map
                    (\( pileName, pile ) ->
                        column [ spacing 10, width fill ]
                            [ viewPileNameAndButtons toMsg state pileName
                            , case ifEditingThisPile pileName state of
                                Nothing ->
                                    let
                                        isSelected =
                                            case state.editing of
                                                Selected selections ->
                                                    \num _ ->
                                                        Set.member ( pileName, num ) selections

                                                _ ->
                                                    \_ _ -> False

                                        toggleSelection num card =
                                            ToggleSelection pileName num
                                                |> toMsg

                                        maybeToggleSelection =
                                            case state.editing of
                                                NotEditing ->
                                                    Just toggleSelection

                                                Selected _ ->
                                                    Just toggleSelection

                                                _ ->
                                                    Nothing
                                    in
                                    Pile.view isSelected maybeToggleSelection pile

                                Just { text } ->
                                    Input.multiline
                                        [ width fill
                                        , onKey
                                            { enter = Nothing
                                            , escape = CancelEditing |> toMsg |> Just
                                            }
                                        , ElmUiUtils.id idOfPileEditor
                                        ]
                                        { label = Input.labelHidden pileName
                                        , text = text
                                        , placeholder = Nothing
                                        , onChange =
                                            \s ->
                                                EditPile { pileName = pileName, text = s }
                                                    |> toMsg
                                        , spellcheck = False
                                        }
                            ]
                    )
                |> column [ width fill, spacing 10 ]

        imageToAddChooser =
            case state.editing of
                ChoosingImageToAdd ->
                    [ Element.inFront (viewImageToAddChooser toMsg state.options)
                    ]

                _ ->
                    []

        selectionButtons =
            let
                takeOutButton =
                    Input.button greenButton
                        { onPress = Just (TakeOut |> toMsg)
                        , label = text "Take out"
                        }
            in
            case state.editing of
                Selected sel ->
                    if Set.size sel == 2 then
                        [ takeOutButton
                        , Input.button greenButton
                            { onPress = Just (Swap |> toMsg)
                            , label = text "Swap"
                            }
                        ]

                    else
                        [ takeOutButton ]

                _ ->
                    []
    in
    column ([ width fill, spacing 10 ] ++ imageToAddChooser)
        [ pilesView
        , row [ spacing 10 ]
            (Input.button regularButton
                { onPress = OpenAdd |> toMsg |> Just
                , label = text "Add pile"
                }
                :: selectionButtons
            )
        ]
