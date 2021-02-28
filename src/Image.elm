module Image exposing
    ( Image
    , PileName
    , decoder
    , encode
    , get
    , names
    , piles
    , put
    , renamePile
    , swap
    , take
    , update
    , view
    )

import Card exposing (Card)
import Element
    exposing
        ( Element
        , column
        , fill
        , spacing
        , width
        )
import Element.Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import List
import List.Extra
import Pile exposing (Pile)



-- For a cardician at any given point the Image we present to the audience is just
-- piles of cards..
-- Each pile has a name.


type alias PileName =
    String


type alias Image =
    List ( PileName, Pile )


names : Image -> List String
names i =
    List.map (\( n, _ ) -> n) i


piles : Image -> List ( String, Pile )
piles image =
    image


{-| Return the pile with the given Name.
-}
get : PileName -> Image -> Maybe Pile
get pileName image =
    case List.filter (\( n, _ ) -> n == pileName) image of
        [] ->
            Nothing

        ( _, x ) :: _ ->
            Just x


{-| Swap two cards.
-}
swap : PileName -> Int -> PileName -> Int -> Image -> Image
swap pileNameA ndxA pileNameB ndxB image =
    let
        cardA =
            get pileNameA image
                |> Maybe.andThen (List.Extra.getAt ndxA)

        cardB =
            get pileNameB image
                |> Maybe.andThen (List.Extra.getAt ndxB)
    in
    case ( cardA, cardB ) of
        ( Just a, Just b ) ->
            image
                |> update pileNameA
                    (\p ->
                        List.Extra.setAt ndxA b (Maybe.withDefault [] p)
                            |> Just
                    )
                |> update pileNameB
                    (\p ->
                        List.Extra.setAt ndxB a (Maybe.withDefault [] p)
                            |> Just
                    )

        ( _, _ ) ->
            image


{-| Remove the given pile from the image.
-}
take : PileName -> Image -> ( Maybe Pile, Image )
take pileName image =
    case List.partition (\( n, _ ) -> n == pileName) image of
        ( [], newImage ) ->
            ( Nothing, newImage )

        ( ( _, x ) :: _, newImage ) ->
            ( Just x, newImage )


{-| Put pile on top of the cards in pilename, creating pilename if necessary.
-}
put : PileName -> Pile -> Image -> Image
put pileName pile image =
    update pileName
        (\alreadyThere ->
            Just (pile ++ Maybe.withDefault [] alreadyThere)
        )
        image


{-| Rename oldname to newname. Does nothing if no pile has oldname.
-}
renamePile : PileName -> PileName -> Image -> Image
renamePile oldName newName image =
    List.map
        (\( n, v ) ->
            if oldName == n then
                ( newName, v )

            else
                ( n, v )
        )
        image


update : PileName -> (Maybe Pile -> Maybe Pile) -> Image -> Image
update pileName f image =
    let
        loop res l =
            case l of
                [] ->
                    case f Nothing of
                        Nothing ->
                            List.reverse res

                        Just [] ->
                            List.reverse res

                        Just newPile ->
                            List.reverse res ++ [ ( pileName, newPile ) ]

                ( pN, v ) :: ls ->
                    if pN == pileName then
                        case f (Just v) of
                            Nothing ->
                                List.reverse res ++ ls

                            Just [] ->
                                List.reverse res ++ ls

                            Just newPile ->
                                List.reverse (( pN, newPile ) :: res) ++ ls

                    else
                        loop (( pN, v ) :: res) ls
    in
    loop [] image


view : (String -> Element msg) -> (Int -> Card -> Bool) -> Maybe (Int -> Card -> msg) -> Image -> Element msg
view viewPileName isSelected maybeOnClick world =
    Element.Keyed.column [ spacing 10, width fill ]
        (world
            |> List.sortBy Tuple.first
            |> List.map
                (\( name, pile ) ->
                    ( name
                    , column []
                        [ viewPileName name
                        , Pile.view isSelected maybeOnClick pile
                        ]
                    )
                )
        )


encode : Image -> Encode.Value
encode i =
    Encode.object
        (List.map (\( name, pile ) -> ( name, Encode.string (Pile.toString pile) )) i)


decoder : Decode.Decoder Image
decoder =
    Decode.keyValuePairs (Decode.string |> Decode.andThen pileDecoder)


pileDecoder : String -> Decode.Decoder Pile
pileDecoder s =
    case Pile.fromString s of
        Ok p ->
            Decode.succeed p

        Err msg ->
            Decode.fail msg
