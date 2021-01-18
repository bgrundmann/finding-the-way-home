module Main exposing (..)

import Browser
import Card exposing (Card, Pile, Suit, Value, poker_deck)
import Cardician exposing (..)
import Dict exposing (Dict)
import Element exposing (Element, text)
import Element.Border as Border
import Element.Input as Input
import Element.Background as Background
import Html exposing (Html)
import List
import Result
import Image exposing (Image)
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


apply : Move -> Image-> Result String Image
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
    { world : Image
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

blue =
    Element.rgb255 238 238 238


view : Model -> Html Msg
view model =
  let
      buttons = Element.row [ Element.spacing 10 ]
        [ Input.button [Element.padding 10, Border.rounded 5, Background.color blue] { label = text "Draw", onPress = Just Draw }
        , Input.button [Element.padding 10, Border.rounded 5, Background.color blue] { label = text "turn_over", onPress = Just Turn_over }
        , Input.button [Element.padding 10, Border.rounded 5, Background.color blue] { label = text "Deal", onPress = Just Deal_clicked }
        ]

      initialImageView = Image.view model.world

      movesView = Input.multiline [Element.width Element.fill, Element.height Element.fill] { label = Input.labelAbove [] (Element.text "Moves"), onChange = SetMoves, text = model.movesText, placeholder = Nothing, spellcheck = False }

      finalImageView = Image.view model.world
  in
  Element.layout []
    (Element.column [ Element.padding 20, Element.width Element.fill, Element.height Element.fill, Element.spacing 10 ]
        [ buttons
        , Element.row [ Element.spacing 10, Element.width Element.fill, Element.height Element.fill ] [ initialImageView, movesView, finalImageView ]
        ])
