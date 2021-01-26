module ImageEditor exposing (Msg, State, getImage, init, update, view)

import Card
import Element exposing (Element, column, el, fill, height, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ElmUiUtils exposing (onKey)
import Image exposing (Image, PileName, view)
import List.Extra
import MoveParser exposing (validatePileName)
import Palette exposing (dangerousButton, regularButton)
import Pile


type Editing
    = NotEditing
    | EditingPileName EditingPileNameState
    | EditingPile EditingPileState


type alias State =
    { image : Image
    , editing : Editing
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
    | Add
    | StartEditPile PileName
    | EditPile EditingPileState
    | EditPileName { oldName : String, newName : String }
    | Save


init : Image -> State
init i =
    { image = i, editing = NotEditing }


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
    case Debug.log "ImageEditor.update" msg of
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

        Save ->
            case state.editing of
                NotEditing ->
                    state

                EditingPile { pileName, text } ->
                    case Debug.log "Pile.fromString" <| Pile.fromString text of
                        Err e ->
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

        Add ->
            { state
                | image =
                    Image.update (findUnusedName state.image "deck")
                        (\_ -> Just Pile.poker_deck)
                        state.image
            }


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
                        ([ onKey
                            { enter = Save |> toMsg |> Just
                            , escape = Nothing
                            }
                         ]
                            ++ maybeWarnColor
                        )
                        { label = Input.labelHidden "PileName"
                        , placeholder = Nothing
                        , text = editingPileName.newName
                        , onChange =
                            \s ->
                                EditPileName { oldName = pileName, newName = s }
                                    |> toMsg
                        }

        editButton =
            case ifEditingThisPile pileName state of
                Nothing ->
                    Input.button regularButton { onPress = StartEditPile pileName |> toMsg |> Just, label = text "Edit" }

                Just _ ->
                    Input.button regularButton { onPress = Save |> toMsg |> Just, label = text "Save" }
    in
    row [ width fill, spacing 5 ]
        [ el [ width fill, Font.bold ] pileNameLabelOrEditor
        , editButton
        , Input.button dangerousButton { onPress = Delete pileName |> toMsg |> Just, label = text "Delete" }
        ]


ifEditingThisPile : String -> State -> Maybe EditingPileState
ifEditingThisPile name state =
    case state.editing of
        NotEditing ->
            Nothing

        EditingPileName _ ->
            Nothing

        EditingPile ({ pileName } as s) ->
            if pileName == name then
                Just s

            else
                Nothing


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
                                    Image.viewPile pile

                                Just { text } ->
                                    Input.multiline [ width fill ]
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
    in
    column [ width fill, height fill, spacing 10 ]
        [ pilesView
        , Input.button regularButton { onPress = Add |> toMsg |> Just, label = text "Add" }
        ]
