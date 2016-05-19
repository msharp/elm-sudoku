import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import String exposing (..)
import Dict

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
    board: String,
    grid: Sudoku.Grid
  }


init : (Model, Cmd String)
init =
  (Model randomBoard (Sudoku.parseGrid randomBoard), Cmd.none)


randomBoard : String
randomBoard = 
  "003020600900305001001806400008102900700000008006708200002609500800203009005010300"


-- UPDATE

type Action = Reset | Update

update : String -> Model -> ( Model, Cmd String )
update action model =
  (Model randomBoard (Sudoku.parseGrid randomBoard), Cmd.none)


-- VIEW

view : Model -> Html String
view model =
  div []
      [ sudokuGrid model.board
      -- , div [] [ text " ------- "]
      , div [] [ text (toString model.grid) ]
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
      -- , div [] [ text (toString (cellBlockNumber "C2")) ]
      ]

cellBlockNumber : String -> Int
cellBlockNumber cell =
  let
    cb = 
      List.map (\x -> List.member cell x) Sudoku.blocks
      |> List.indexedMap (,) 
      |> List.filter (\x -> snd x == True)
      |> List.head
  in
    case cb of
      Just (a,b) -> a + 1
      _          -> 0


cellBlock : (String, Maybe Char) -> Html String
cellBlock (square, val) = 
  let 
    noZero v =   
      case v of
        Just v  -> String.fromChar v
        Nothing -> ""
  in
    div []
      [ input 
        [ value (noZero val)
        , id square
        , Html.Attributes.size 1
        , Html.Attributes.maxlength 1
        , cellStyle square
        ]
        []
      ]


cellStyle : String -> Attribute String
cellStyle cell =
  let
    block_number = 
      cellBlockNumber cell
    cell_bg =
      case List.member block_number [1,3,5,7,9] of
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
  let 
    cells = 
      Sudoku.gridValues board
      |> Dict.toList -- list can be mapped
  in
  div [
        style [ ("width", "270px")
              , ("height", "270px")
              , ("margin", "20px")
              , ("border", "1px black solid")
              ] 
      ]
      (List.map cellBlock cells)

