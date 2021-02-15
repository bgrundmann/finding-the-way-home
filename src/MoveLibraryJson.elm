module MoveLibraryJson exposing (decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Move exposing (UserDefinedOrPrimitive(..))
import MoveLibrary exposing (MoveLibrary)
import MoveParser
import Primitives
import ViewMove


{-| A separate module because we depend on MoveParser
-}
encode : MoveLibrary -> Encode.Value
encode l =
    MoveLibrary.toListTopSort l
        |> List.filter
            (\md ->
                case md.body of
                    Primitive _ ->
                        False

                    UserDefined _ ->
                        True
            )
        |> List.map ViewMove.prettyPrintDefinition
        |> String.join "\n"
        |> Encode.string


decoder : Decode.Decoder MoveLibrary
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case MoveParser.parseMoves Primitives.primitives s of
                    Err what ->
                        Decode.fail ("Parser failed: " ++ Debug.toString what)

                    Ok res ->
                        Decode.succeed
                            (MoveLibrary.fromList
                                (MoveLibrary.toListAlphabetic Primitives.primitives ++ res.definitions)
                            )
            )
