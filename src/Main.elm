module Main exposing (..)

import List exposing (repeat, length, map, indexedMap)
import Html exposing (Html, div, text, span)
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
  | AddPiece Int GamePiece

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  let
    newModel : Model
    newModel =
      case (message, model.game) of
        (ChangeConfig cfg, _) ->
          { model | cfg = cfg }
        (Begin, Setup) ->
          { model | game = InGame (Board.create model.cfg) }
        (Begin, _) ->
          model
        (AddPiece index piece, InGame board) ->
          { model | game = InGame (updateBoard index piece board) }
        (AddPiece i p, _) -> model -- Invalid message
  in
    (newModel, Cmd.none)

updateBoard : Int -> GamePiece -> Board -> Board
updateBoard pos piece b =
  let
    setAtPos : Int -> (Maybe GamePiece) -> List (Maybe GamePiece) -> List (Maybe GamePiece)
    setAtPos pos val list =
      indexedMap
        (\i orig -> if i == pos then val else orig)
        list

    updatedTurn : List (Maybe GamePiece)
    updatedTurn =
      setAtPos pos (Just piece) b.current
  in
    { b | current = updatedTurn }


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
    futureCount = cfg.guesses - length b.turns - 1 -- -1 for current turn

    futureRow : Html Msg
    futureRow =
      div
        [class "gb-row"]
        (renderInactivePiece Nothing |> (repeat cfg.patternLen))

    futureRows : List (Html Msg)
    futureRows =
      repeat futureCount futureRow

    prevRows : List (Html Msg)
    prevRows =
      map renderPreviousTurn b.turns

    currentTurnRow : Html Msg
    currentTurnRow =
      renderActiveTurn b.current

  in
    div
      [class "gb-game"]
      (futureRows ++ [currentTurnRow] ++ prevRows)

renderActiveTurn : List (Maybe GamePiece) -> Html Msg
renderActiveTurn guesses =
  div
    [class "gb-row gb-row-active"]
    ( indexedMap renderActivePiece guesses)

renderInactiveTurn : (List (Maybe GamePiece), (Maybe Outcome)) -> Html Msg
renderInactiveTurn (pattern, outcome) =
  div
    [class "gb-row"]
    (map renderInactivePiece pattern)

renderPreviousTurn : (Pattern, Outcome) -> Html Msg
renderPreviousTurn (p, o) =
  let
    liftMaybe : (Pattern, Outcome) -> (List (Maybe GamePiece), Maybe Outcome)
    liftMaybe (p, o) =
      (p |> map Just, Just o)
  in
    liftMaybe (p, o) |> renderInactiveTurn

renderInactivePiece : Maybe GamePiece -> Html Msg
renderInactivePiece p =
  renderPiece Nothing p

renderActivePiece : Int -> Maybe GamePiece -> Html Msg
renderActivePiece i p =
  renderPiece (Just i) p

renderPiece : Maybe Int -> Maybe GamePiece -> Html Msg
renderPiece index p =
  let
    pieceColor : String
    pieceColor =
      case p of
        Nothing -> "gb-piece-empty"
        Just color -> "gb-piece-" ++ show color

    colorClass : String
    colorClass =
      "gb-piece " ++ pieceColor

    clickAction : List (Html.Attribute Msg)
    clickAction =
      case index of
        Nothing -> []
        Just idx -> [onClick (AddPiece idx GamePiece.Red)]
  in
    div
      (class colorClass :: clickAction)
      []

renderOutcome : Maybe Outcome -> Html Msg
renderOutcome outc =
  case outc of
    Nothing ->
      div [class "gb-outcome gb-outcome-empty"] []
    Just o ->
      div [class "gb-outcome"]
        [ span [class ""] [o.perfect |> toString |> text]
        , text ", "
        , span [class ""] [o.almost |> toString |> text]
        ]
