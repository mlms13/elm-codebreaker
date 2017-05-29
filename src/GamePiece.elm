module GamePiece exposing (..)
import Tuple
import Configuration exposing (Configuration)

type GamePiece
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Purple
  | Pink
  | Brown

show : GamePiece -> String
show gp =
  case gp of
    Red -> "red"
    Orange -> "orange"
    Yellow -> "yellow"
    Green -> "green"
    Blue -> "blue"
    Purple -> "purple"
    Pink -> "pink"
    Brown -> "brown"

list : List GamePiece
list =
  let
    matchAll c =
      case c of
        Red -> ()
        Orange -> ()
        Yellow -> ()
        Green -> ()
        Blue -> ()
        Purple -> ()
        Pink -> ()
        Brown -> ()
        -- add new instances to the list below!
  in
    [Red, Orange, Yellow, Green, Blue, Purple, Pink, Brown]

sliceForConfig : Configuration -> List GamePiece
sliceForConfig cfg =
  List.take cfg.colorCount list

cycle : List GamePiece -> Maybe GamePiece -> Maybe GamePiece
cycle list piece =
  let
    accumulate : GamePiece -> List GamePiece -> Maybe GamePiece
    accumulate orig list =
      List.foldl (\curr acc -> check orig acc curr) (False, Nothing) list |> Tuple.second

    check : GamePiece -> (Bool, Maybe GamePiece) -> GamePiece -> (Bool, Maybe GamePiece)
    check orig (takeNext, val) curr =
      if takeNext then
        (False, Just curr)
      else
        (curr == orig, val)
  in
    case piece of
      Nothing ->
        List.head list
      Just p ->
        accumulate p list
