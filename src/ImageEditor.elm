module ImageEditor exposing (Msg, State, getImage, init, update, view)

import Browser.Dom as Dom
import Card exposing (Card)
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
import Pile exposing (Pile)
import Set exposing (Set)
import Task


type Editing
    = NotEditing
    | EditingPileName EditingPileNameState
    | EditingPile EditingPileState
    | ChoosingImageToAdd
    | Selected (Set ( PileName, Int ))
    | ChoosingSortOrder WhatToSort


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


type WhatToSort
    = Selection (Set ( PileName, Int ))
    | Pile PileName


type Msg
    = Delete PileName
    | AddPile Image
    | OpenAdd
    | OpenSort WhatToSort
    | Sort WhatToSort Pile
    | ReversePile PileName
    | TurnoverPile PileName
    | StartEditPile PileName
    | EditPile EditingPileState
    | EditPileName { oldName : String, newName : String }
    | CancelEditing
    | Save
    | ToggleSelection PileName Int
    | TurnoverSelection
    | InvertSelection
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


sortingOptions : List ( Element msg, Pile )
sortingOptions =
    let
        asc s =
            ( row [] [ text "A-K", Card.viewSuit s ], Pile.all s )

        desc s =
            ( row [] [ text "K-A", Card.viewSuit s ], Pile.all s |> List.reverse )

        deck_of ( la, pa ) ( lb, pb ) ( lc, pc ) ( ld, pd ) =
            ( row [] [ la, text " ", lb, text " ", lc, text " ", ld ]
            , pa ++ pb ++ pc ++ pd
            )
    in
    [ deck_of (asc Card.Clubs) (asc Card.Diamonds) (desc Card.Hearts) (desc Card.Spades)
    , deck_of (asc Card.Clubs) (asc Card.Hearts) (asc Card.Spades) (asc Card.Diamonds)
    , deck_of (desc Card.Clubs) (desc Card.Hearts) (desc Card.Spades) (desc Card.Diamonds)
    , deck_of (desc Card.Clubs) (desc Card.Spades) (desc Card.Hearts) (desc Card.Diamonds)
    , deck_of (asc Card.Clubs) (asc Card.Spades) (desc Card.Hearts) (desc Card.Diamonds)
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


takeOut : Set ( PileName, Int ) -> Image -> ( Pile, Image )
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
    in
    Set.toList what
        |> List.sortWith
            (\( p1, n1 ) ( p2, n2 ) ->
                case compare p1 p2 of
                    EQ ->
                        compare n2 n1

                    other ->
                        other
            )
        |> List.foldl helper ( [], image )


putBack : Set ( PileName, Int ) -> List Card -> Image -> Image
putBack place what image =
    let
        helper ( ( pileName, num ), card ) imageAcc =
            Image.update pileName
                (\maybePile ->
                    let
                        ( a, b ) =
                            List.Extra.splitAt num (Maybe.withDefault [] maybePile)
                    in
                    Just (a ++ card :: b)
                )
                imageAcc
    in
    List.map2 Tuple.pair (Set.toList place) what
        |> List.foldl helper image


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

                ChoosingSortOrder _ ->
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

        OpenSort what ->
            ( { state | editing = ChoosingSortOrder what }, Cmd.none )

        Sort what desiredOrder ->
            case what of
                Pile pileName ->
                    let
                        sort maybePile =
                            Maybe.map (Pile.sort desiredOrder) maybePile
                    in
                    ( { state
                        | image = Image.update pileName sort state.image
                        , editing = NotEditing
                      }
                    , Cmd.none
                    )

                Selection s ->
                    let
                        ( cards, imageWithout ) =
                            takeOut s state.image

                        sortedCards =
                            Pile.sort desiredOrder cards
                    in
                    ( { state | image = putBack s sortedCards imageWithout, editing = NotEditing }, Cmd.none )

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

        InvertSelection ->
            let
                invertSelection currentSelection =
                    let
                        newSelection =
                            Image.piles state.image
                                |> List.concatMap
                                    (\( pileName, pile ) ->
                                        pile
                                            |> List.indexedMap (\ndx card -> ( ndx, card ))
                                            |> List.filterMap
                                                (\( ndx, card ) ->
                                                    if Set.member ( pileName, ndx ) currentSelection then
                                                        Nothing

                                                    else
                                                        Just ( pileName, ndx )
                                                )
                                    )
                                |> Set.fromList
                    in
                    ( { state | editing = Selected newSelection }, Cmd.none )
            in
            case state.editing of
                Selected s ->
                    invertSelection s

                NotEditing ->
                    invertSelection Set.empty

                _ ->
                    ( state, Cmd.none )

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

                        ChoosingSortOrder _ ->
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
                            let
                                ( cards, imageWithout ) =
                                    takeOut s state.image
                            in
                            { state
                                | image = Image.put (findUnusedName imageWithout "cards") cards imageWithout
                                , editing = NotEditing
                            }

                        _ ->
                            state
            in
            ( newState, Cmd.none )

        TurnoverSelection ->
            let
                newState =
                    case state.editing of
                        Selected s ->
                            let
                                newImage =
                                    Set.foldl
                                        (\( pileName, num ) imageAcc ->
                                            Image.update pileName
                                                (\maybePile ->
                                                    Maybe.withDefault [] maybePile
                                                        |> List.Extra.updateAt num Card.turnover
                                                        |> Just
                                                )
                                                imageAcc
                                        )
                                        state.image
                                        s
                            in
                            { state | image = newImage, editing = NotEditing }

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

        ChoosingSortOrder _ ->
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
                        { onPress = OpenSort (Pile pileName) |> toMsg |> Just
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

        ChoosingSortOrder _ ->
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


viewSortOrderChooser : (Msg -> msg) -> WhatToSort -> List ( Element msg, Pile ) -> Element msg
viewSortOrderChooser toMsg what options =
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
                    |> List.map
                        (\( label, pile ) ->
                            Input.button
                                [ width fill
                                , Element.mouseOver [ Font.color Palette.greenBook ]
                                ]
                                { onPress = Just (Sort what pile |> toMsg), label = label }
                        )
                )
            ]


idOfPileEditor : String
idOfPileEditor =
    -- Here we assume that there is a single imageEditor on the page
    -- and in addition we currently only allow at most one pile to
    -- be edited at any given moment in time.
    "imageEditorPileEditor"


maybe : (data -> cfg -> cfg) -> Maybe data -> cfg -> cfg
maybe f m cfg =
    case m of
        Nothing ->
            cfg

        Just data ->
            f data cfg


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
                                        toggleSelection num card =
                                            ToggleSelection pileName num
                                                |> toMsg

                                        config =
                                            Pile.defaultConfig
                                                |> maybe Pile.withIsSelected
                                                    (case state.editing of
                                                        Selected selections ->
                                                            Just (\num _ -> Set.member ( pileName, num ) selections)

                                                        _ ->
                                                            Nothing
                                                    )
                                                |> maybe Pile.withOnClick
                                                    (case state.editing of
                                                        NotEditing ->
                                                            Just toggleSelection

                                                        Selected _ ->
                                                            Just toggleSelection

                                                        _ ->
                                                            Nothing
                                                    )
                                    in
                                    Pile.view config pile

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

        sortOrderChooser =
            case state.editing of
                ChoosingSortOrder what ->
                    [ Element.inFront (viewSortOrderChooser toMsg what sortingOptions)
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

                turnoverSelectionButton =
                    Input.button greenButton
                        { onPress = Just (TurnoverSelection |> toMsg)
                        , label = text "Turnover"
                        }

                sortButton sel =
                    Input.button greenButton
                        { onPress = Just (OpenSort (Selection sel) |> toMsg)
                        , label = text "Sort"
                        }

                invertSelectionButton =
                    Input.button greenButton
                        { onPress = Just (InvertSelection |> toMsg)
                        , label = text "Invert"
                        }

                withSelectionButtons sel =
                    [ invertSelectionButton
                    , takeOutButton
                    , turnoverSelectionButton
                    , invertSelectionButton
                    , sortButton sel
                    ]
            in
            case state.editing of
                Selected sel ->
                    if Set.size sel == 2 then
                        withSelectionButtons sel
                            ++ [ Input.button greenButton
                                    { onPress = Just (Swap |> toMsg)
                                    , label = text "Swap"
                                    }
                               ]

                    else
                        withSelectionButtons sel

                _ ->
                    [ invertSelectionButton ]
    in
    column ([ width fill, spacing 10 ] ++ imageToAddChooser ++ sortOrderChooser)
        [ pilesView
        , el [ width fill, Element.centerX, height (Element.px 1), Background.color Palette.grey ] Element.none
        , row [ spacing 10 ]
            (Input.button regularButton
                { onPress = OpenAdd |> toMsg |> Just
                , label = text "Add pile"
                }
                :: selectionButtons
            )
        ]
