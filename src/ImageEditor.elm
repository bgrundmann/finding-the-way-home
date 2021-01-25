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


type alias State =
    { image : Image
    , editingPileName : Maybe EditingPileNameState
    }


type alias EditingPileNameState =
    { oldName : String
    , newName : String
    , errorMessage : Maybe String
    }


type Msg
    = Delete PileName
    | Add
    | EditPileName { oldName : String, newName : String }
    | SaveNewPileName


init : Image -> State
init i =
    { image = i, editingPileName = Nothing }


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
                | editingPileName =
                    Just
                        { oldName = oldName
                        , newName = newName
                        , errorMessage = errorMessage
                        }
            }

        SaveNewPileName ->
            case state.editingPileName of
                Nothing ->
                    state

                Just { oldName, newName, errorMessage } ->
                    case errorMessage of
                        Just _ ->
                            state

                        Nothing ->
                            { state | image = Image.renamePile oldName newName state.image, editingPileName = Nothing }

        Add ->
            { state | image = Image.update (findUnusedName state.image "deck") (\_ -> Just Card.poker_deck) state.image }


getImage : State -> Image
getImage { image } =
    image


ifEditingThisPileName : String -> State -> Maybe EditingPileNameState
ifEditingThisPileName pileName state =
    case state.editingPileName of
        Just ({ oldName } as s) ->
            if pileName == oldName then
                Just s

            else
                Nothing

        Nothing ->
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
    in
    column [ width fill, height fill, spacing 10 ]
        [ Image.view (viewPileNameAndButtons toMsg state) state.image
        , Input.button regularButton { onPress = Add |> event |> Just, label = text "Add" }
        ]
