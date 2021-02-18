module Route exposing (Route(..), routeToString, urlToRoute)

import Move exposing (MoveIdentifier)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((<?>), Parser, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = Library (Maybe MoveIdentifier)
    | Editor


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Editor top
        , map Library
            (s "library"
                <?> Query.map (Maybe.map Move.unsafeIdentifierFromText) (Query.string "selection")
            )
        ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    parse routeParser url


{-| Convert the route back to A string suitable for Url.path
-}
routeToString : Route -> String
routeToString r =
    case r of
        Editor ->
            Url.Builder.absolute [] []

        Library (Just s) ->
            Url.Builder.absolute [ "library" ] [ Url.Builder.string "selection" (Move.identifierText s) ]

        Library Nothing ->
            Url.Builder.absolute [ "library" ] []
