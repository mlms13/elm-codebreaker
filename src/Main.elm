module Main exposing (..)

import List exposing (repeat, length, map, indexedMap)
import List.Extra exposing (getAt)
import Maybe.Extra exposing (join)
import Random exposing (Seed)
import Time exposing (Time)
import Task exposing (Task)
import Html exposing (Html, div, text, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Board exposing (Board, Pattern, Outcome)
import GamePiece exposing (GamePiece)
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
  | GetTimeAndBegin
  | Begin Seed
  | AddPiece Int GamePiece
  | CyclePiece Int
  | Check (List GamePiece)

update : Msg -> Model -> (Model, Cmd Msg)
update message ({cfg, game} as model) =
  case (message, game) of
    (ChangeConfig config, _) ->
      ({ model | cfg = config }, Cmd.none )

    (GetTimeAndBegin, Setup) ->
      let
        timeToSeed : Time -> Seed
        timeToSeed t =
          t |> round |> Random.initialSeed
      in
        ( model, Task.perform (\t -> Begin (timeToSeed t)) Time.now)

    (Begin seed, Setup) ->
     ( { model | game = InGame (Board.create cfg seed) }, Cmd.none)

    (AddPiece index piece, InGame board) ->
      (
        { model | game = InGame (setPieceAt index (Just piece) board) }
        , Cmd.none
      )

    (CyclePiece index, InGame board) ->
      (
        { model | game = InGame (cyclePieceAt cfg index board) }
        , Cmd.none
      )

    (Check pattern, InGame board) ->
      ({ model | game = (checkCurrent cfg pattern board) }, Cmd.none)

    -- Unreachable
    (GetTimeAndBegin, _) -> (model, Cmd.none)
    (Begin _, _) -> (model, Cmd.none)
    (AddPiece i p, _) -> (model, Cmd.none)
    (CyclePiece _, _) -> (model, Cmd.none)
    (Check _, _) -> (model, Cmd.none)

cyclePieceAt : Configuration -> Int -> Board -> Board
cyclePieceAt cfg pos b =
  let
    list : List GamePiece
    list =
      GamePiece.sliceForConfig cfg

    next : Maybe GamePiece
    next =
      (getAt pos b.current) |> join |> GamePiece.cycle list
  in
    setPieceAt pos next b

setPieceAt : Int -> Maybe GamePiece -> Board -> Board
setPieceAt pos piece b =
  let
    setAtPos : Int -> (Maybe GamePiece) -> List (Maybe GamePiece) -> List (Maybe GamePiece)
    setAtPos pos val list =
      indexedMap
        (\i orig -> if i == pos then val else orig)
        list

    updatedTurn : List (Maybe GamePiece)
    updatedTurn =
      setAtPos pos piece b.current
  in
    { b | current = updatedTurn }

checkCurrent : Configuration -> Pattern -> Board -> Game
checkCurrent cfg guess b =
  let
    outcome : Outcome
    outcome =
      Board.comparePattern b.answer guess
  in
    if outcome.perfect == cfg.patternLen then
      End Win b
    else if length b.turns == cfg.guesses then
      End Loss b
    else
      Board.advanceTurn (guess, outcome) b |> InGame

-- VIEW

view : Model -> Html Msg
view model =
  let
    css : Html Msg
    css =
      Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "index.css" ] []

    game : Html Msg
    game =
      case model.game of
        Setup -> Html.button [onClick GetTimeAndBegin] [text "Start!"]
        InGame board -> renderBoard model.cfg board
        End Win _ -> Html.h3 [] [text "You win!"]
        End Loss _ -> Html.h3 [] [text "Game over. :("]

    content : List (Html Msg)
    content =
      [ game ]
  in
    div [ class "gb-container"] content


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
      b.turns |> List.reverse |> map renderPreviousTurn

    currentTurnRow : Html Msg
    currentTurnRow =
      renderActiveTurn b.current

  in
    div
      [class "gb-game"]
      (futureRows ++ [currentTurnRow] ++ prevRows)

renderActiveTurn : List (Maybe GamePiece) -> Html Msg
renderActiveTurn guesses =
  let
    viewGuesses : List (Html Msg)
    viewGuesses =
      indexedMap renderActivePiece guesses

    complete : Maybe (List GamePiece)
    complete =
      Maybe.Extra.combine guesses

    checkButton : List (Html Msg)
    checkButton =
      case complete of
        Nothing -> []
        Just pattern -> [renderCheckButton pattern]
  in
    div
      [class "gb-row gb-row-active"]
      (List.concat [viewGuesses, checkButton])

renderInactiveTurn : (List (Maybe GamePiece), (Maybe Outcome)) -> Html Msg
renderInactiveTurn (pattern, outcome) =
  div
    [class "gb-row"]
    ((map renderInactivePiece pattern) ++ [renderOutcome outcome])

renderPreviousTurn : (Pattern, Outcome) -> Html Msg
renderPreviousTurn (p, o) =
  let
    liftMaybe : (Pattern, Outcome) -> (List (Maybe GamePiece), Maybe Outcome)
    liftMaybe (p, o) =
      (p |> map Just, Just o)
  in
    liftMaybe (p, o) |> renderInactiveTurn

renderCheckButton : Pattern -> Html Msg
renderCheckButton pattern =
  Html.button [class "check-button", onClick (Check pattern)] [text "Check"]

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
        Just color -> "gb-piece-" ++ GamePiece.show color

    colorClass : String
    colorClass =
      "gb-piece " ++ pieceColor

    clickAction : List (Html.Attribute Msg)
    clickAction =
      case index of
        Nothing -> []
        Just idx -> [onClick (CyclePiece idx)]
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
