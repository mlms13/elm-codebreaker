module GamePiece exposing (..)
import List.Extra exposing (elemIndex, getAt)
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
  case piece of
    Nothing ->
      List.head list
    Just p ->
      Maybe.andThen (\idx -> getAt (idx + 1) list) (elemIndex p list)
