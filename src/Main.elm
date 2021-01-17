module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import List
import Dict exposing (Dict)
import Card exposing (Card, Pile, poker_deck, Suit, Value)
import World exposing (World)

-- MAIN



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
    ( Model [("deck", poker_deck)]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Draw
    | Turn_over


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

        Turn_over ->
            ( { world = List.map (\(k, v) -> (k, turnOver v)) model.world }
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
        , viewWorld model.world
        ]


viewWorld : World -> Html msg
viewWorld world =
   div [] (
    List.map (\(name, pile) -> div [] [text name, div [] (List.map Card.view pile)]) world)
