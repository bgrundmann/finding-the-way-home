module MoveLibrary exposing
    ( MoveLibrary
    , fromList
    , get
    , getByName
    ,  insert
       --, remove

    , toList
    )

import Dict exposing (Dict)
import List.Extra
import Move exposing (ArgumentKind, MoveDefinition, MoveIdentifier)


type alias MoveIdentifierText =
    String


{-| BEWARE: This representation allows for some invalid states on
the type level, most obviously multiple move definitions for
the same list of argument kinds.

    So take care when writing add or update to do the right thing.

-}
type alias MoveLibrary =
    { definitions : Dict MoveIdentifierText MoveDefinition
    , sameName : Dict MoveIdentifierText (List MoveIdentifier)

    -- , usedBy : Dict String (List ( List ArgumentKind, List MoveIdentifier ))
    }


toList : MoveLibrary -> List MoveDefinition
toList l =
    Dict.values l.definitions


empty : MoveLibrary
empty =
    { definitions = Dict.empty
    , sameName = Dict.empty -- , usedBy = Dict.empty
    }


fromList : List MoveDefinition -> MoveLibrary
fromList dl =
    List.foldl insert empty dl


{-| Insert the given definition into the library. Replaces any existing definition
with the same Identifier.
-}
insert : MoveDefinition -> MoveLibrary -> MoveLibrary
insert md library =
    let
        name =
            md.name

        identifier =
            Move.identifier md
    in
    { definitions = Dict.insert (Move.identifierText identifier) md library.definitions
    , sameName =
        Dict.update
            name
            (\maybeSameNameMds ->
                identifier
                    :: List.filter (\id -> id /= identifier) (Maybe.withDefault [] maybeSameNameMds)
                    |> Just
            )
            library.sameName
    }



{-
   remove : MoveIdentifier -> MoveLibrary -> MoveLibrary
   remove ident library =
       let
           ( name, argKinds ) =
               ident
       in
       { definitions =
           Dict.update
               name
               (Maybe.map
                   (\mdsWithSameName ->
                       List.filter (\( aks, _ ) -> aks /= argKinds) mdsWithSameName
                   )
               )
               library.definitions
       }
-}


get : MoveIdentifier -> MoveLibrary -> Maybe MoveDefinition
get ident library =
    Dict.get (Move.identifierText ident) library.definitions


getByName : String -> MoveLibrary -> List MoveDefinition
getByName name library =
    Dict.get name library.sameName
        |> Maybe.withDefault []
        |> List.filterMap (\i -> get i library)
