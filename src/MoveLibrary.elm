module MoveLibrary exposing (MoveLibrary, add, fromList, getByName, toList)

import Dict exposing (Dict)
import List.Extra
import Move exposing (MoveDefinition, MoveIdentifier)


type alias MoveLibrary =
    Dict String MoveDefinition


toList : MoveLibrary -> List MoveDefinition
toList l =
    Dict.values l


fromList : List MoveDefinition -> MoveLibrary
fromList dl =
    dl
        |> List.map (\md -> ( md.name, md ))
        |> Dict.fromList


get : MoveIdentifier -> MoveLibrary -> Maybe MoveDefinition
get ident library =
    let
        ( name, _ ) =
            ident
    in
    Dict.get name library


getByName : String -> MoveLibrary -> Maybe MoveDefinition
getByName name library =
    Dict.get name library


{-| Add the given definition to the library. Or update an existing definition.
-}
add : MoveDefinition -> MoveLibrary -> MoveLibrary
add md ml =
    Dict.insert md.name md ml
