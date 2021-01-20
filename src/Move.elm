module Move exposing (Move(..), parseMoves)

import Char
import Dict exposing (Dict)
import Image exposing (PileName)
import List
import Parser.Advanced exposing ((|.), (|=), Step(..), Token(..), andThen, chompWhile, end, int, keyword, loop, map, oneOf, problem, run, succeed, token, variable)
import Set


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type alias Context =
    ()


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


type Problem
    = ExpectedMoveName
    | UnknownMove String
    | ExpectedInt
    | ExpectedPileName
    | ExpectedEndOfInput
    | ExpectedDef
    | ExpectedEnd
    | ExpectedRepeat
    | ExpectedEndOfLine
    | DuplicateDefinition String
    | Problem String


type alias MoveDefinition =
    { name : String
    , moves : List Move
    }


type Move
    = Cut { n : Int, pile : PileName, to : PileName }
    | Repeat Int (List Move)
    | Turnover PileName
    | Do MoveDefinition


type Argument
    = Int Int
    | Pile PileName


keywords =
    Set.fromList [ "repeat", "end", "def" ]


moveNameParser : Parser String
moveNameParser =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = keywords, expecting = ExpectedMoveName }


primitiveMoveParser : Parser ( String, List Argument )
primitiveMoveParser =
    succeed (\cmd args -> ( cmd, args ))
        |= moveNameParser
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= argsParser
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEndOfInput ]


repeatParser : Dict String MoveDefinition -> Parser Move
repeatParser definitions =
    succeed (\n moves -> Repeat n moves)
        |. keyword (Token "repeat" ExpectedRepeat)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= int ExpectedInt ExpectedInt
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. token (Token "\n" ExpectedEndOfLine)
        |= movesParser definitions
        |. keyword (Token "end" ExpectedEnd)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEndOfInput ]


moveParser : Dict String MoveDefinition -> Parser Move
moveParser definitions =
    succeed identity
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= oneOf
            [ repeatParser definitions
            , primitiveMoveParser |> andThen (recognizeBuiltinsAndLookupDefinitions definitions)
            ]


argsParser : Parser (List Argument)
argsParser =
    let
        helper result =
            oneOf
                [ succeed (\arg -> Loop (arg :: result))
                    |= argParser
                    |. chompWhile (\c -> c == ' ' || c == '\t')
                , succeed () |> map (\() -> Done (List.reverse result))
                ]

        argParser =
            oneOf
                [ int ExpectedInt ExpectedInt |> map Int
                , variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = Set.empty, expecting = ExpectedPileName }
                    |> map Pile
                ]
    in
    loop [] helper


recognizeBuiltinsAndLookupDefinitions : Dict String MoveDefinition -> ( String, List Argument ) -> Parser Move
recognizeBuiltinsAndLookupDefinitions definitions ( cmd, args ) =
    case ( cmd, args ) of
        ( "cut", [ Int n, Pile pile, Pile to ] ) ->
            Cut { n = n, pile = pile, to = to }
                |> succeed

        ( "cut", _ ) ->
            Problem "cut <number> <pile> <to-pile>"
                |> problem

        ( "turnover", [ Pile name ] ) ->
            Turnover name
                |> succeed

        ( "turnover", _ ) ->
            Problem "turnover <pile>"
                |> problem

        ( moveName, _ ) ->
            case Dict.get moveName definitions of
                Nothing ->
                    problem (UnknownMove moveName)

                Just d ->
                    succeed (Do d)


movesParser : Dict String MoveDefinition -> Parser (List Move)
movesParser definitions =
    let
        helper result =
            oneOf
                [ succeed (\cmd -> Loop (cmd :: result))
                    |= moveParser definitions
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


definitionsParser : Parser (Dict String MoveDefinition)
definitionsParser =
    let
        helper definitions =
            oneOf
                [ succeed (\def -> Loop (Dict.insert def.name def definitions))
                    |= definitionParser definitions
                , succeed () |> map (\() -> Done definitions)
                ]
    in
    loop Dict.empty helper


definitionParser : Dict String MoveDefinition -> Parser MoveDefinition
definitionParser definitions =
    succeed (\name moves -> { name = name, moves = moves })
        |. keyword (Token "def" ExpectedDef)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= (moveNameParser
                |> andThen
                    (\n ->
                        if Dict.member n definitions then
                            problem (DuplicateDefinition n)

                        else
                            succeed n
                    )
           )
        |. token (Token "\n" ExpectedEndOfLine)
        |= movesParser definitions
        |. keyword (Token "end" ExpectedEnd)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEndOfInput ]


definitionsAndMoves : Parser ( Dict String MoveDefinition, List Move )
definitionsAndMoves =
    definitionsParser
        |> andThen
            (\defs ->
                movesParser defs
                    |> map (\moves -> ( defs, moves ))
            )


parser : Parser { definitions : Dict String MoveDefinition, moves : List Move }
parser =
    definitionsAndMoves
        |> map (\( defs, mvs ) -> { definitions = defs, moves = mvs })


deadEndsToString : String -> List DeadEnd -> String
deadEndsToString text s =
    let
        problemToString problem =
            case problem of
                UnknownMove n ->
                    "Don't know how to do '" ++ n ++ "'"

                ExpectedMoveName ->
                    "Expected a move (e.g. 'deal')"

                Problem msg ->
                    msg

                ExpectedRepeat ->
                    "Expected 'repeat'"

                ExpectedDef ->
                    "Expected 'def'"

                DuplicateDefinition d ->
                    "You already know how to '" ++ d ++ "'"

                ExpectedEnd ->
                    "Expected 'end'"

                ExpectedEndOfInput ->
                    "End of file expected"

                ExpectedInt ->
                    "Expected an int (e.g. 52)"

                ExpectedPileName ->
                    "Expected the name of a pile (e.g. deck, table, ...)"

                ExpectedEndOfLine ->
                    "Expected to see the next line."

        deadEndToString { row, col, problem } =
            String.fromInt row ++ "x" ++ String.fromInt col ++ problemToString problem
    in
    String.join "\n" (List.map deadEndToString s)


parseMoves : String -> Result String { moves : List Move, definitions : Dict String MoveDefinition }
parseMoves text =
    case run (parser |. end ExpectedEnd) text of
        Ok m ->
            Ok m

        Err deadEnds ->
            Err (deadEndsToString text deadEnds)
