module ImageEditor exposing (State, getImage, init, view)

import Card
import Element exposing (Element, column, el, fill, height, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ElmUiUtils exposing (onKey)
import Image exposing (Image, PileName, view)
import MoveParser exposing (validatePileName)
import Palette exposing (dangerousButton, regularButton)


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
    | StartEditPile String
    | EditPile EditingPileState
    | EditPileName { oldName : String, newName : String }
    | SaveNewPileName


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

        SaveNewPileName ->
            case state.editing of
                NotEditing ->
                    state

                EditingPile _ ->
                    state

                EditingPileName { oldName, newName, errorMessage } ->
                    case errorMessage of
                        Just _ ->
                            state

                        Nothing ->
                            { state | image = Image.renamePile oldName newName state.image, editing = NotEditing }

        StartEditPile pileName ->
            let
                -- TODO: Introduce Pile.elm and move appropriate functions there
                text =
                    ""
            in
            { state | editing = EditingPile { pileName = pileName, text = text } }

        EditPile newState ->
            { state | editing = EditingPile newState }

        Add ->
            { state | image = Image.update (findUnusedName state.image "deck") (\_ -> Just Card.poker_deck) state.image }


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


viewPileNameAndButtons : (State -> msg) -> State -> String -> Element msg
viewPileNameAndButtons toMsg state pileName =
    let
        event msg =
            update msg state
                |> toMsg

        pileNameLabelOrEditor =
            case ifEditingThisPileName pileName state of
                Nothing ->
                    Input.button []
                        { onPress =
                            EditPileName { oldName = pileName, newName = pileName }
                                |> event
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
                            { enter = SaveNewPileName |> event |> Just
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
                                    |> event
                        }
    in
    row [ width fill, spacing 5 ]
        [ el [ width fill, Font.bold ] pileNameLabelOrEditor
        , Input.button regularButton { onPress = Nothing, label = text "Edit" }
        , Input.button dangerousButton { onPress = Delete pileName |> event |> Just, label = text "Delete" }
        ]


view : (State -> msg) -> State -> Element msg
view toMsg state =
    let
        event msg =
            update msg state
                |> toMsg

        pilesView =
            Image.piles state.image
                |> List.map
                    (\( pileName, pile ) ->
                        column [ spacing 10 ]
                            [ viewPileNameAndButtons toMsg state pileName
                            , Image.viewPile pile
                            ]
                    )
                |> column [ spacing 10 ]
    in
    column [ width fill, height fill, spacing 10 ]
        [ Image.view (viewPileNameAndButtons toMsg state) state.image
        , Input.button regularButton { onPress = Add |> event |> Just, label = text "Add" }
        ]
