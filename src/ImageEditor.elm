module ImageEditor exposing (Msg, State, getImage, init, update, view)

import Card
import Dict exposing (Dict)
import Element exposing (Element, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ElmUiUtils exposing (onKey)
import Image exposing (Image, PileName, view)
import MoveParser exposing (validatePileName)
import Palette exposing (dangerousButton, regularButton)
import Pile


type Editing
    = NotEditing
    | EditingPileName EditingPileNameState
    | EditingPile EditingPileState
    | ChoosingImageToAdd


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
    | Add Image
    | OpenAdd
    | ReversePile PileName
    | TurnoverPile PileName
    | StartEditPile PileName
    | EditPile EditingPileState
    | EditPileName { oldName : String, newName : String }
    | CancelEditing
    | Save


defaultOptions =
    [ ( "red backed deck (face down)", [ ( "deck", Pile.poker_deck ) ] )
    , ( "blue backed deck (face down)"
      , [ ( "deck", Pile.poker_deck |> List.map (Card.withVisible (Card.Back Card.Blue)) ) ]
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


update : Msg -> State -> State
update msg state =
    case msg of
        Delete pileName ->
            { state | image = Image.update pileName (\_ -> Nothing) state.image }

        EditPileName { oldName, newName } ->
            let
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
            in
            { state
                | editing =
                    EditingPileName
                        { oldName = oldName
                        , newName = newName
                        , errorMessage = errorMessage
                        }
            }

        CancelEditing ->
            { state | editing = NotEditing }

        Save ->
            case state.editing of
                NotEditing ->
                    state

                ChoosingImageToAdd ->
                    state

                EditingPile { pileName, text } ->
                    case Pile.fromString text of
                        Err _ ->
                            state

                        Ok cards ->
                            { state
                                | image = Image.update pileName (always (Just cards)) state.image
                                , editing = NotEditing
                            }

                EditingPileName { oldName, newName, errorMessage } ->
                    case errorMessage of
                        Just _ ->
                            state

                        Nothing ->
                            { state | image = Image.renamePile oldName newName state.image, editing = NotEditing }

        ReversePile pileName ->
            let
                reverse maybePile =
                    Maybe.map (\pile -> List.reverse pile) maybePile
            in
            { state | image = Image.update pileName reverse state.image, editing = NotEditing }

        TurnoverPile pileName ->
            let
                turnover maybePile =
                    Maybe.map (\pile -> Pile.turnover pile) maybePile
            in
            { state | image = Image.update pileName turnover state.image, editing = NotEditing }

        StartEditPile pileName ->
            let
                text =
                    Image.get pileName state.image
                        |> Maybe.withDefault []
                        |> Pile.toString
            in
            { state | editing = EditingPile { pileName = pileName, text = text } }

        EditPile newState ->
            { state | editing = EditingPile newState }

        OpenAdd ->
            { state | editing = ChoosingImageToAdd }

        Add imageToAdd ->
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
            { state | image = newImage, editing = NotEditing }


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


viewImageToAddChooser : (Msg -> msg) -> Dict String Image -> Element msg
viewImageToAddChooser toMsg options =
    column
        [ spacing 5
        , padding 20
        , Border.color Palette.blueBook
        , Border.width 2
        , Border.rounded 5
        , Background.color Palette.white
        , Font.color Palette.black
        ]
        (options
            |> Dict.toList
            |> List.sortBy Tuple.first
            |> List.map
                (\( name, image ) ->
                    Input.button
                        [ width fill
                        , Element.mouseOver [ Font.color Palette.greenBook ]
                        ]
                        { onPress = Just (Add image |> toMsg), label = text name }
                )
        )


view : (Msg -> msg) -> State -> Element msg
view toMsg state =
    let
        pilesView =
            Image.piles state.image
                |> List.map
                    (\( pileName, pile ) ->
                        column [ spacing 10, width fill ]
                            [ viewPileNameAndButtons toMsg state pileName
                            , case ifEditingThisPile pileName state of
                                Nothing ->
                                    Pile.view pile

                                Just { text } ->
                                    Input.multiline
                                        [ width fill
                                        , onKey
                                            { enter = Nothing
                                            , escape = CancelEditing |> toMsg |> Just
                                            }
                                        ]
                                        { label = Input.labelHidden "pile"
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
                    [ Element.above (viewImageToAddChooser toMsg state.options)
                    ]

                _ ->
                    []
    in
    column [ width fill, height fill, spacing 10 ]
        [ pilesView
        , el imageToAddChooser <|
            Input.button regularButton
                { onPress = OpenAdd |> toMsg |> Just
                , label = text "Add"
                }
        ]
