-- from evancz/start-app

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Array
import Dict
import String exposing (..)

import Sudoku


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model = 
  {
    board: String
  }


init : (Model, Cmd String)
init =
  (Model randomBoard, Cmd.none)


randomBoard : String
randomBoard = 
  "003020600900305001001806400008102900700000008006708200002609500800203009005010300"


-- UPDATE

type Action = Reset | Update

update : String -> { board : String } -> ( Model, Cmd String )
update action model =
  (Model randomBoard, Cmd.none)


-- VIEW

view : { board : String } -> Html String
view model =
  div []
      [ div [] [ text (toString Sudoku.squares) ]
      , div [] [ text (toString (List.length Sudoku.squares)) ]
      , div [] [ text " ------- "]
      , div [] [ text (toString Sudoku.unitlist) ]
      , div [] [ text (toString (List.length Sudoku.unitlist)) ]
      , div [] [ text " ------- "]
      -- , div [] [ text (toString Sudoku.units) ]
      -- , div [] [ text (toString Sudoku.peers) ]
      , div [] [ text " ------- "]
      , div [] [ text (toString (Sudoku.getPeers "C2")) ]
      , div [] [ text " ------- "]
      , sudokuGrid randomBoard
      -- , cell "1"
      ]


cell : Char -> String -> Html String
cell val square = 
  div []
    [ input 
      [ value (String.fromChar val)
      , id square
      , Html.Attributes.size 1
      , cellStyle
      ]
      []
    ]


cellStyle : Attribute String
cellStyle =
  style
    [ ("height", "20px")
    , ("width", "20px")
    , ("padding", "4px 4px")
    , ("font-size", "20px")
    , ("text-align", "center")
    , ("border", "1px black solid")
    , ("float", "left")
    ]


sudokuGrid : String -> Html String
sudokuGrid board =
  -- let cells = List.map toString (String.toList board)
  let 
    noZero char =   
      case char of
        '0' -> ' '
        _   -> char

    cells = List.map noZero (String.toList board)
  in
  div [
        style [ ("width", "270px")
              , ("height", "270px")
              , ("margin", "20px")
              , ("border", "1px black solid")
              ] 
      ]
      (List.map2 cell cells Sudoku.squares)

