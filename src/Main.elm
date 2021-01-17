module Main exposing (..)

import Browser
import Card exposing (Card, Pile, Suit, Value, poker_deck)
import Cardician exposing (Cardician)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import List
import Result
import World exposing (World)



-- MAIN


type alias PileName =
    String


type Move
    = Deal { from : PileName, to : PileName }
    | Cut { pile : PileName, n : Int, to : PileName }
    | Assemble { pile : PileName, onTopOf : PileName }
    | Named String (List Move)


get : PileName -> World -> Result String Pile
get pileName world =
    case
        List.filterMap
            (\( n, v ) ->
                if n == pileName then
                    Just v

                else
                    Nothing
            )
            world
    of
        [] ->
            Err ("No pile called " ++ pileName)

        [ x ] ->
            Ok x

        _ ->
            Err ("Multiple piles called " ++ pileName)


mapPile : (Pile -> Pile) -> PileName -> World -> World
mapPile f pileName world =
    let
        loop l res =
            case l of
                [] ->
                    List.reverse (( pileName, f [] ) :: res)

                ( pn, v ) :: ls ->
                    if pn == pileName then
                        List.reverse (( pn, f v ) :: res) ++ ls

                    else
                        loop ls (( pn, v ) :: res)
    in
    loop world []


apply : Move -> World -> Result String World
apply move world =
    case move of
        Deal { from, to } ->
            get from world
                |> Result.andThen
                    (\p ->
                        case p of
                            [] ->
                                Err "pile is empty"

                            c :: cs ->
                                Ok (mapPile (\dst -> c :: dst) to world)
                    )

        Cut _ ->
            Err "not yet supported"

        Assemble _ ->
            Err "not yet supported"

        Named _ _ ->
            Err "not yet supported"


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { world : World
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ ( "deck", poker_deck ) ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Draw
    | Turn_over
    | Deal_clicked


turnOver : Pile -> Pile
turnOver pile =
    List.reverse (List.map Card.turnOver pile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model
            , Cmd.none
            )

        Deal_clicked ->
            case apply (Deal { from = "deck", to = "pile" }) model.world of
                Ok w ->
                    ( { world = w }, Cmd.none )

                Err errMsg ->
                    Debug.log errMsg ( { world = model.world }, Cmd.none )

        Turn_over ->
            ( { world = List.map (\( k, v ) -> ( k, turnOver v )) model.world }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Draw ] [ text "Draw" ]
        , button [ onClick Turn_over ] [ text "turn_over" ]
        , button [ onClick Deal_clicked ] [ text "Deal" ]
        , World.view model.world
        ]
