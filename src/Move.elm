module Move exposing (Move (..), parser)
import List
import World exposing (PileName)
import Parser exposing (Parser, (|.), (|=), succeed, run, int, keyword, variable, oneOf, end)

type Move
    = Deal { from : PileName, to : PileName }
    | Cut { pile : PileName, n : Int, to : PileName }
    | Assemble { pile : PileName, onTopOf : PileName }
    | Named String (List Move)


parser : Parser Move
parser =
  oneOf
    [ dealParser
    ]

dealParser =
  succeed Deal
    |. keyword "deal"
    |= succeed { from= "deck", to="table" }



