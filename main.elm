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

view : Model -> Html String
view model =
  div []
      [ sudokuGrid model.board
      -- , div [] [ text " ------- "]
      -- , div [] [ text " ------- "]
      -- , div [] [ text (toString Sudoku.squares) ]
      -- , div [] [ text (toString (List.length Sudoku.squares)) ]
      -- , div [] [ text " ------- "]
      -- , div [] [ text (toString Sudoku.unitlist) ]
      -- , div [] [ text (toString (List.length Sudoku.unitlist)) ]
      -- , div [] [ text " ------- "]
      -- , div [] [ text (toString Sudoku.blocks) ]
      -- , div [] [ text (toString Sudoku.units) ]
      -- , div [] [ text (toString Sudoku.peers) ]
      -- , div [] [ text " ------- "]
      -- , div [] [ text (toString (Sudoku.getPeers "C2")) ]
      -- , div [] [ text " ------- "]
      -- , div [] [ text (toString (cellBlock "C2")) ]
      ]

cellBlock : String -> Int
cellBlock cell =
  let
      cb = List.map (\x -> List.member cell x) Sudoku.blocks
        |> List.indexedMap (,) 
        |> List.filter (\x -> snd x == True)
        |> List.head
  in
    case cb of
      Just (a,b) -> a + 1
      _          -> 0


cell : Char -> String -> Html String
cell val square = 
  div []
    [ input 
      [ value (String.fromChar val)
      , id square
      , Html.Attributes.size 1
      , cellStyle square
      ]
      []
    ]


cellStyle : String -> Attribute String
cellStyle cell =
  let
    grey_block = List.member (cellBlock cell) [1,3,5,7,9]
    cell_bg =
      case grey_block of
        True    -> "#ddd"
        False   -> "#fff"
  in
    style
      [ ("height", "20px")
      , ("width", "20px")
      , ("padding", "4px 4px")
      , ("font-size", "20px")
      , ("text-align", "center")
      , ("border", "1px black solid")
      , ("float", "left")
      , ("background-color", cell_bg)
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

