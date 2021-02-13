module ViewMove exposing (prettyPrint, prettyPrintDefinition, view, viewDefinition)

import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , paragraph
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Element.Input as Input
import ElmUiUtils exposing (boldMono, mono)
import Move
    exposing
        ( Expr(..)
        , ExprValue(..)
        , Move(..)
        , MoveDefinition
        , MoveIdentifier
        , UserDefinedOrPrimitive(..)
        )


textSpacing : Int
textSpacing =
    8


indented : Element msg -> Element msg
indented elem =
    row []
        [ el [ width (px 30) ] Element.none
        , elem
        ]


view : Maybe (MoveIdentifier -> msg) -> Move -> Element msg
view maybeOnClickMove move =
    case move of
        Do _ def exprs ->
            let
                nameMaybeLinked =
                    let
                        vn =
                            mono def.name
                    in
                    case ( def.path, maybeOnClickMove ) of
                        ( [], Just onClickMove ) ->
                            Input.button []
                                { onPress = Just (onClickMove (Move.identifier def))
                                , label = vn
                                }

                        _ ->
                            vn
            in
            row [ spacing 10 ] (nameMaybeLinked :: List.map viewExpr exprs)

        Repeat _ n moves ->
            column [ spacing textSpacing ]
                (row [ spacing 10 ] [ boldMono "repeat", viewExpr n ]
                    :: List.map (indented << view maybeOnClickMove) moves
                    ++ [ boldMono "end" ]
                )


viewExpr : Expr -> Element msg
viewExpr e =
    case e of
        ExprArgument a ->
            mono a.name

        ExprValue (Int i) ->
            mono (String.fromInt i)

        ExprValue (Pile p) ->
            mono p

        ExprTemporaryPile pn ->
            mono pn.name


viewDefinition : Maybe (MoveIdentifier -> msg) -> MoveDefinition -> Element msg
viewDefinition onClickMoveName md =
    let
        body =
            case md.body of
                UserDefined { moves, definitions, temporaryPiles } ->
                    column [ spacing textSpacing, width fill ]
                        ((case temporaryPiles of
                            [] ->
                                Element.none

                            _ ->
                                indented (row [ spacing 10 ] (boldMono "temp" :: List.map mono temporaryPiles))
                         )
                            :: List.map (indented << viewDefinition onClickMoveName) definitions
                            ++ List.map (indented << view onClickMoveName) moves
                        )

                Primitive _ ->
                    indented (text "This is a builtin")
    in
    column [ spacing textSpacing, width fill ]
        (row [ spacing 10, width fill ] (boldMono "def" :: mono md.name :: List.map (mono << .name) md.args)
            :: (case md.doc of
                    "" ->
                        Element.none

                    d ->
                        indented
                            (paragraph [ spacing textSpacing, width fill ]
                                [ boldMono "doc"
                                , mono " "
                                , el [ Font.italic, Font.family [ Font.monospace ] ] (text d)
                                ]
                            )
               )
            :: body
            :: [ boldMono "end" ]
        )


ppIndented : List String -> List String
ppIndented l =
    List.map (\s -> "  " ++ s) l


doPrettyPrint : Move -> List String
doPrettyPrint move =
    case move of
        Do _ def exprs ->
            [ def.name ++ " " ++ String.join " " (List.map doPrettyPrintExpr exprs) ]

        Repeat _ n moves ->
            ("repeat " ++ doPrettyPrintExpr n)
                :: ppIndented (List.concatMap doPrettyPrint moves)
                ++ [ "end" ]


doPrettyPrintExpr : Expr -> String
doPrettyPrintExpr e =
    case e of
        ExprArgument a ->
            a.name

        ExprValue (Int i) ->
            String.fromInt i

        ExprValue (Pile p) ->
            p

        ExprTemporaryPile pn ->
            pn.name


doPrettyPrintDefinition : MoveDefinition -> List String
doPrettyPrintDefinition md =
    let
        body =
            case md.body of
                UserDefined { moves, definitions, temporaryPiles } ->
                    (case temporaryPiles of
                        [] ->
                            []

                        _ ->
                            [ "  temp " ++ String.join " " temporaryPiles ]
                    )
                        ++ ppIndented (List.concatMap doPrettyPrintDefinition definitions)
                        ++ ppIndented (List.concatMap doPrettyPrint moves)

                Primitive _ ->
                    []
    in
    ("def " ++ md.name ++ " " ++ String.join " " (List.map .name md.args))
        :: (case md.doc of
                "" ->
                    []

                d ->
                    [ "  doc " ++ d ]
           )
        ++ body
        ++ [ "end" ]


prettyPrint : Move -> String
prettyPrint m =
    doPrettyPrint m
        |> String.join "\n"


prettyPrintDefinition : MoveDefinition -> String
prettyPrintDefinition md =
    doPrettyPrintDefinition md
        |> String.join "\n"
