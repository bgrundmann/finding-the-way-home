module MoveLibrary exposing (MoveLibrary, fromList, get, getByName, insert, toList)

import Dict exposing (Dict)
import List.Extra
import Move exposing (ArgumentKind, MoveDefinition, MoveIdentifier)


{-| BEWARE: This representation allows for some invalid states on
the type level, most obviously multiple move definitions for
the same list of argument kinds.

    So take care when writing add or update to do the right thing.

-}
type alias MoveLibrary =
    Dict String (List ( List ArgumentKind, MoveDefinition ))


toList : MoveLibrary -> List MoveDefinition
toList l =
    Dict.values l
        |> List.concatMap (List.map Tuple.second)


empty : MoveLibrary
empty =
    Dict.empty


fromList : List MoveDefinition -> MoveLibrary
fromList dl =
    List.foldl insert empty dl


{-| Insert the given definition into the library. Replaces any existing definition
with the same Identifier.
-}
insert : MoveDefinition -> MoveLibrary -> MoveLibrary
insert md ml =
    let
        ( name, argKinds ) =
            Move.identifier md
    in
    Dict.update
        name
        (\maybeSameNameMds ->
            ( argKinds, md )
                :: List.filter (\( aks, _ ) -> argKinds /= aks) (Maybe.withDefault [] maybeSameNameMds)
                |> Just
        )
        ml


get : MoveIdentifier -> MoveLibrary -> Maybe MoveDefinition
get ident library =
    let
        ( name, argKinds ) =
            ident
    in
    Dict.get name library
        |> Maybe.andThen
            (\mdsWithSameName ->
                List.Extra.find (\( aks, _ ) -> aks == argKinds) mdsWithSameName
                    |> Maybe.map Tuple.second
            )



{- TODO: Fix the below -}


getByName : String -> MoveLibrary -> List MoveDefinition
getByName name library =
    Dict.get name library
        |> Maybe.withDefault []
        |> List.map Tuple.second
