module MoveParser exposing (parseMoves, validatePileName)

import Char
import Dict exposing (Dict)
import List
import Move
    exposing
        ( Argument
        , ArgumentKind(..)
        , Expr(..)
        , ExprValue(..)
        , Location
        , Move(..)
        , MoveDefinition
        , UserDefinedOrPrimitive(..)
        )
import MoveLibrary exposing (MoveLibrary)
import MoveParseError exposing (Context, DeadEnd, Expectation(..), MoveParseError, Problem(..))
import Parser.Advanced
    exposing
        ( (|.)
        , (|=)
        , Step(..)
        , Token(..)
        , andThen
        , chompWhile
        , end
        , getChompedString
        , int
        , loop
        , map
        , oneOf
        , problem
        , run
        , succeed
        , token
        , variable
        )
import Set


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


{-| The ParseEnv guides the parsers into making decisions. Unlike context which is
used by to augment the error messages. That said if the Elm parser library provided
a way to read the context during parsing we could have used that.
-}
type alias ParseEnv =
    { path : List String -- Here its stored in reverse (unlike MoveDefinition)
    , toplevel : Bool
    , library : MoveLibrary -- Including local definitions
    , arguments :
        Dict String
            { level : Int
            , ndx : Int
            , kind : ArgumentKind
            , isTemporary : Bool
            }

    -- arguments to the current scope
    }


addDefinition : ParseEnv -> MoveDefinition -> ParseEnv
addDefinition env md =
    { env | library = MoveLibrary.add md env.library }


{-| Lookup arguments. In the case of PileNames this will also lookup temporary piles.
-}
lookupArgument : ParseEnv -> String -> Maybe { name : String, ndx : Int, up : Int, kind : ArgumentKind, isTemporary : Bool }
lookupArgument env name =
    Dict.get name env.arguments
        |> Maybe.map
            (\{ level, ndx, kind, isTemporary } ->
                { name = name, ndx = ndx, up = List.length env.path - level, kind = kind, isTemporary = isTemporary }
            )


argumentsInScopeOfKind : ParseEnv -> ArgumentKind -> List String
argumentsInScopeOfKind env kind =
    Dict.toList env.arguments
        |> List.filter (\( _, a ) -> a.kind == kind)
        |> List.map Tuple.first


enterDefinition : String -> ParseEnv -> List { name : String, kind : ArgumentKind } -> List String -> ParseEnv
enterDefinition moveName env arguments temporaries =
    let
        thisPath =
            moveName :: env.path

        thisLevel =
            List.length thisPath

        localArguments =
            List.indexedMap
                (\ndx { name, kind } ->
                    ( name, { kind = kind, ndx = ndx, level = thisLevel, isTemporary = False } )
                )
                arguments
                |> Dict.fromList

        localTemporaries =
            List.indexedMap
                (\ndx name ->
                    ( name, { kind = KindPile, ndx = ndx, level = thisLevel, isTemporary = True } )
                )
                temporaries
                |> Dict.fromList

        newArguments =
            Dict.union localTemporaries (Dict.union localArguments env.arguments)
    in
    { env | arguments = newArguments, path = thisPath, toplevel = False }


enterRepeat : ParseEnv -> ParseEnv
enterRepeat env =
    { env | toplevel = False }


toplevelEnv : MoveLibrary -> ParseEnv
toplevelEnv initialLibrary =
    { path = []
    , toplevel = True
    , library = initialLibrary
    , arguments = Dict.empty
    }


keywords : Set.Set String
keywords =
    Set.fromList [ "repeat", "end", "def", "ignore", "doc" ]


keyword : String -> Parser ()
keyword string =
    Parser.Advanced.keyword (Token string (Expected (EKeyword string)))


keywordEnd : Parser ()
keywordEnd =
    keyword "end"


keywordDef : Parser ()
keywordDef =
    keyword "def"


keywordRepeat : Parser ()
keywordRepeat =
    keyword "repeat"


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ' || c == '\t')


{-| At least one newline, maybe preceeded by whitespace, and possibly followed by more newlines / spaces.
-}
newline : Parser ()
newline =
    spaces
        |. token (Token "\n" (Expected EEndOfLine))
        |. spaces
        |. loop ()
            (\() ->
                oneOf
                    [ (token (Token "\n" (Expected EEndOfLine)) |. spaces) |> map (\_ -> Loop ())
                    , succeed () |> map (\_ -> Done ())
                    ]
            )


{-| The end of a statement is a newline character. Or at the toplevel the end of the file is also ok.
-}
endOfStatement : ParseEnv -> Parser ()
endOfStatement env =
    if env.toplevel then
        oneOf [ newline, end (Expected EEndOfInput) ]

    else
        newline


pileNameParser : Problem -> Parser String
pileNameParser problem =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '_', reserved = Set.empty, expecting = problem }


numberNameParser : Problem -> Parser String
numberNameParser problem =
    variable { start = Char.isUpper, inner = \c -> Char.isUpper c || c == '_', reserved = Set.empty, expecting = problem }


moveNameParser : Parser String
moveNameParser =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = keywords, expecting = Expected EMoveName }


exprParser : ParseEnv -> Maybe MoveDefinition -> Argument -> Parser Expr
exprParser env move expectedArgument =
    let
        maybeArgument whenNot name =
            case lookupArgument env name of
                Nothing ->
                    whenNot name

                Just arg ->
                    if arg.isTemporary then
                        succeed (ExprTemporaryPile { name = arg.name, ndx = arg.ndx, up = arg.up })

                    else
                        succeed (ExprArgument { name = arg.name, ndx = arg.ndx, up = arg.up, kind = arg.kind })

        argProblem =
            ExpectedForArgument
                { move = move
                , argName = expectedArgument.name
                , argKind = expectedArgument.kind
                , options = argumentsInScopeOfKind env expectedArgument.kind
                }
    in
    case expectedArgument.kind of
        KindInt ->
            oneOf
                [ int argProblem argProblem |> map (\i -> ExprValue (Int i))
                , numberNameParser argProblem
                    |> andThen
                        (maybeArgument
                            (\n ->
                                problem (NoSuchArgument { name = n, kind = KindInt })
                            )
                        )
                ]

        KindPile ->
            let
                whenNot pileName =
                    if List.isEmpty env.path then
                        succeed (ExprValue (Pile pileName))

                    else
                        problem (NoSuchArgument { kind = KindPile, name = pileName })
            in
            pileNameParser argProblem |> andThen (maybeArgument whenNot)


getLocation : Parser Location
getLocation =
    Parser.Advanced.getRow |> map (\row -> { row = row })


doMoveParser : ParseEnv -> Parser Move
doMoveParser env =
    getLocation
        |> andThen
            (\location ->
                moveNameParser
                    |> andThen (lookupDefinition env.library)
                    |> andThen (actualsParser env location)
            )


actualsParser : ParseEnv -> Location -> MoveDefinition -> Parser Move
actualsParser env location moveDefinition =
    let
        helper actuals expectedArgs =
            case expectedArgs of
                [] ->
                    succeed (Do location moveDefinition (List.reverse actuals))

                expectedArg :: restExpectedArgs ->
                    (exprParser
                        env
                        (Just moveDefinition)
                        expectedArg
                        |. spaces
                    )
                        |> andThen
                            (\expr ->
                                helper (expr :: actuals) restExpectedArgs
                            )
    in
    succeed identity
        |. spaces
        |= helper [] moveDefinition.args


repeatParser : ParseEnv -> Parser Move
repeatParser env =
    succeed (\location n moves -> Repeat location n moves)
        |= getLocation
        |. keywordRepeat
        |. spaces
        |= exprParser env Nothing { name = "N", kind = KindInt }
        |. spaces
        |. newline
        |= movesParser (enterRepeat env)
        |. keywordEnd


moveParser : ParseEnv -> Parser (Maybe Move)
moveParser env =
    succeed (\ignoref p -> ignoref p)
        |= oneOf
            [ (keyword "ignore" |. spaces) |> map (\() -> always Nothing)
            , succeed () |> map (\() -> Just)
            ]
        |= oneOf
            [ repeatParser env
            , doMoveParser env
            ]


argsParser : Parser (List Argument)
argsParser =
    let
        helper result =
            oneOf
                [ succeed (\arg -> Loop (arg :: result))
                    |= argParser
                    |. spaces
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


{-| A argument as it occurrs in the def line of a move definition.
-}
argParser : Parser Argument
argParser =
    oneOf
        [ pileNameParser (Expected EPileName) |> map (\n -> { name = n, kind = KindPile })
        , numberNameParser (Expected ENumberName) |> map (\n -> { name = n, kind = KindInt })
        ]


{-| We just parsed a move name and are looking up the corresponding move.
-}
lookupDefinition : MoveLibrary -> String -> Parser MoveDefinition
lookupDefinition library moveName =
    case MoveLibrary.getByName moveName library of
        Nothing ->
            problem (UnknownMove moveName)

        Just d ->
            succeed d


movesParser : ParseEnv -> Parser (List Move)
movesParser env =
    let
        helper result =
            oneOf
                [ succeed
                    (\maybeMove ->
                        case maybeMove of
                            Nothing ->
                                Loop result

                            Just move ->
                                Loop (move :: result)
                    )
                    |= moveParser env
                    |. endOfStatement env
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


{-| Return a list of definitions and the correspondingly amended MoveLibrary
from the ParseEnv.
-}
definitionsParser : ParseEnv -> Parser ( ParseEnv, List MoveDefinition )
definitionsParser env =
    let
        helper ( localDefs, e ) =
            oneOf
                [ succeed
                    (\def ->
                        Loop ( def :: localDefs, addDefinition e def )
                    )
                    |= definitionParser e
                , succeed () |> map (\() -> Done ( e, localDefs ))
                ]
    in
    loop ( [], env ) helper


defLineParser : ParseEnv -> Parser { name : String, args : List Argument }
defLineParser env =
    succeed (\name args -> { name = name, args = args })
        |. keywordDef
        |. spaces
        |= (moveNameParser
                |> andThen
                    (\n ->
                        if Dict.member n env.library then
                            problem (DuplicateDefinition n)

                        else
                            succeed n
                    )
           )
        |. spaces
        |= argsParser
        |. newline


docParser : Parser String
docParser =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed identity
                |. keyword "doc"
                |. spaces
                |= (chompWhile (\c -> c /= '\n') |> getChompedString)
                |. newline
            , succeed () |> map (always "")
            ]


defineTemporaryPilesParser : Parser (List String)
defineTemporaryPilesParser =
    let
        temporariesHelper res =
            oneOf
                [ succeed (\pile -> Loop (pile :: res))
                    |= pileNameParser (Expected EPileName)
                    |. spaces
                , succeed () |> map (\() -> Done (List.reverse res))
                ]
    in
    succeed identity
        |. spaces
        |= oneOf
            [ succeed identity
                |. keyword "temp"
                |. spaces
                |= loop [] temporariesHelper
                |. newline
            , succeed () |> map (always [])
            ]


definitionParser : ParseEnv -> Parser MoveDefinition
definitionParser env =
    (succeed
        (\{ name, args } doc temporaryPiles ->
            { name = name, args = args, doc = doc, temporaryPiles = temporaryPiles }
        )
        |= defLineParser env
        |= docParser
        |= defineTemporaryPilesParser
    )
        |> andThen
            (\{ name, args, doc, temporaryPiles } ->
                succeed
                    (\( definitions, moves ) ->
                        { name = name
                        , args = args
                        , path = List.reverse env.path
                        , body =
                            UserDefined
                                { moves = moves
                                , definitions = definitions
                                , temporaryPiles = temporaryPiles
                                }
                        , doc = doc
                        }
                    )
                    |= definitionsAndMoves (enterDefinition name env args temporaryPiles)
                    |. keywordEnd
                    |. endOfStatement env
            )


definitionsAndMoves : ParseEnv -> Parser ( List MoveDefinition, List Move )
definitionsAndMoves env =
    definitionsParser env
        |> andThen
            (\( newEnv, defs ) ->
                movesParser newEnv
                    |> map (\moves -> ( defs, moves ))
            )


parser : ParseEnv -> Parser { definitions : List MoveDefinition, moves : List Move }
parser env =
    succeed (\( defs, moves ) -> { definitions = defs, moves = moves })
        |= definitionsAndMoves env


{-| Parse a list of definitions and moves as written at the toplevel of a program.
-}
parseMoves :
    MoveLibrary
    -> String
    -> Result MoveParseError { moves : List Move, definitions : List MoveDefinition }
parseMoves library text =
    let
        env =
            toplevelEnv library
    in
    case run (parser env |. end (Expected EEndOfInput)) text of
        Ok m ->
            Ok m

        Err deadEnds ->
            Err deadEnds


{-| Nothing if s is a valid pilename, Just errorMessage otherwise
-}
validatePileName : String -> Maybe String
validatePileName s =
    case run (pileNameParser (Expected EPileName) |. end (Expected EEndOfInput)) s of
        Ok _ ->
            Nothing

        Err _ ->
            Just "A pilename should look like this: deck, deck2, table, ..."
