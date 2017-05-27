module GamePiece exposing (..)

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
