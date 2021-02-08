module MoveLibrary exposing (MoveLibrary, fromList, get)

import Dict exposing (Dict)
import List.Extra
import Move exposing (MoveDefinition, MoveIdentifier)


type alias MoveLibrary =
    Dict String (List MoveDefinition)


fromList : List MoveDefinition -> MoveLibrary
fromList dl =
    dl
        |> List.map (\md -> ( md.name, [ md ] ))
        |> Dict.fromList


get : MoveIdentifier -> MoveLibrary -> Maybe MoveDefinition
get ident library =
    let
        ( name, _ ) =
            ident
    in
    Dict.get name library
        |> Maybe.map
            (\candidates ->
                List.Extra.find (\md -> Move.identifier md == ident) candidates
            )
