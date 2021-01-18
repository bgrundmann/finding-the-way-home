module Move exposing (Move (..), parseMoves)
import List
import Image exposing (PileName)
import Parser exposing (Parser, (|.), (|=), succeed, run, int, keyword, variable, oneOf, end, chompWhile)

type Move
    = Deal { from : PileName, to : PileName }
    | Cut { pile : PileName, n : Int, to : PileName }
    | Assemble { pile : PileName, onTopOf : PileName }
    | Named String (List Move)


parser : Parser Move
parser =
  succeed (\x -> x)
  |. chompWhile (\c -> c == ' ' || c == '\t')
  |= oneOf
    [ dealParser
    ]
  
dealParser =
  succeed Deal
    |. keyword "deal"
    |= succeed { from= "deck", to="table" }


parseMoves : String -> Result String (List Move)
parseMoves text =
  case run (parser |. end) text of
    Ok m -> Ok [m]
    Err e -> Err "Parse error"
