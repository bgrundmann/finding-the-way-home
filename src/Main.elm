module Main exposing (..)

import Browser
import Card exposing (Card, Pile, Suit, Value, poker_deck)
import Cardician exposing (..)
import Dict exposing (Dict)
import Element
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput, onClick)
import List
import Result
import World exposing (World)
import Move exposing (Move (..))



-- MAIN


type alias PileName =
    String



cardician : Move -> Cardician ()
cardician move =
    case move of
        Deal { from, to } ->
            get from
                |> andThen
                    (\src ->
                        getOrEmpty to
                            |> andThen
                                (\dst ->
                                    case src of
                                        [] ->
                                            fail "Nothing left to deal"

                                        card :: rest ->
                                            put from rest
                                                |> andThen
                                                    (\() ->
                                                        put to (card :: dst)
                                                    )
                                )
                    )

        Cut _ ->
            fail "not supported"

        Assemble _ ->
            fail "not supported"

        Named _ _ ->
            fail "not supported"


apply : Move -> World -> Result String World
apply move world =
    let
        c =
            cardician move

        ( or_error, new_world ) =
            perform c world
    in
    case or_error of
        Err msg ->
            Err msg

        Ok () ->
            Ok new_world


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
    , movesText : String
    , moves : Result String (List Move)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = [( "deck", poker_deck )], movesText = "", moves = Ok [] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Draw
    | Turn_over
    | Deal_clicked
    | SetMoves String


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
                    ( { model | world = w }, Cmd.none )

                Err errMsg ->
                    Debug.log errMsg ( { model | world = model.world }, Cmd.none )

        Turn_over ->
            ( { model | world = List.map (\( k, v ) -> ( k, turnOver v )) model.world }
            , Cmd.none
            )

        SetMoves text ->
            ( { model | movesText = text, moves = Err "Not implemented" }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  Element.layout []
    (Element.column [ Element.width Element.fill, Element.height Element.fill, Element.centerX, Element.spacing 10 ]
        [ button [ onClick Draw ] [ text "Draw" ] |> Element.html
        , button [ onClick Turn_over ] [ text "turn_over" ] |> Element.html
        , button [ onClick Deal_clicked ] [ text "Deal" ] |> Element.html
        , World.view model.world 
        , textarea [ onInput SetMoves, value model.movesText ] [] |> Element.html
        ])
