module Main exposing (..)

import List exposing (repeat, length, map)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Board exposing (Board, Pattern, Outcome)
import GamePiece exposing (GamePiece, show)
import Configuration exposing (Configuration)

main : Program Never Model Msg
main =
  Html.program
    { init = (init, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- MODEL

type alias Model =
  { cfg : Configuration
  , game : Game
  }

type Game
  = Setup
  | InGame Board
  | End Condition Board

type Condition = Win | Loss

init : Model
init =
  { cfg = Configuration.default
  , game = Setup
  }


-- UPDATE

type Msg
  = ChangeConfig Configuration
  | Begin
  | AddPiece GamePiece

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  let
    newModel : Model
    newModel =
      case (message, model.game) of
        (ChangeConfig cfg, _) -> { model | cfg = cfg }
        (Begin, Setup) -> { model | game = InGame (Board.create model.cfg) }
        (Begin, _) -> model
        (AddPiece piece, InGame board) -> model -- TODO
        (AddPiece p, _) -> model -- Invalid message
  in
    (newModel, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  case model.game of
    Setup -> Html.button [onClick Begin] [text "Start!"]
    InGame board -> renderBoard model.cfg board
    End Win _ -> Html.h3 [] [text "You win!"]
    End Loss _ -> Html.h3 [] [text "Game over. :("]

renderBoard : Configuration -> Board -> Html Msg
renderBoard cfg b =
  let
    futureCount : Int
    futureCount = cfg.guesses - length b.turns

    futureRow : Html Msg
    futureRow =
      div
        [class "gb-row"]
        (renderPiece Nothing |> (repeat cfg.patternLen))

    futureRows : List (Html Msg)
    futureRows =
      repeat futureCount futureRow

    prevRows : List (Html Msg)
    prevRows =
      map renderTurn b.turns
  in
    div [class "gb-game"] futureRows


renderTurn : (Pattern, Outcome) -> Html Msg
renderTurn (pattern, outcome) =
  div
    [class "gb-row"]
    ((pattern |> map Just |> renderPieces) ++ [renderOutcome outcome])

renderPieces : List (Maybe GamePiece) -> List (Html Msg)
renderPieces l =
  map renderPiece l

renderOutcome : Outcome -> Html Msg
renderOutcome { perfect, almost } =
  div [] []


-- renderActivePiece : Maybe GamePiece -> Html Msg
-- renderActivePiece

renderPiece : Maybe GamePiece -> Html Msg
renderPiece p =
  let
    colorClass : String
    colorClass =
      case p of
        Nothing -> "gb-piece-empty"
        Just color -> "gb-piece-" ++ show color
  in
    div ["gb-piece " ++ colorClass |> class] []
