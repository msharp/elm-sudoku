-- from evancz/start-app

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Array
import Dict
import String exposing (..)

import Sudoku

main =
  StartApp.start { model = model, view = view, update = update }


model = randomBoard
  
view address model =
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
      ]
  
type Action = Reset | Update
update action model =
  case action of
    Reset -> "00302060090030500100180640000810290070000000800670820000260950080020300900501030"
    Update -> "10302060090030500100180640000810290070000100800670820000260950080120300900501031"

    
cell: String -> Html
cell val = 
  div []
    [ input 
      [ value val
      , placeholder "_"
      , Html.Attributes.size 1
      , myStyle
      ]
      []
    ]
    

myStyle : Attribute
myStyle =
  style
    [ ("height", "20px")
    , ("padding", "1px 4px")
    , ("font-size", "20px")
    , ("text-align", "center")
    ]


randomBoard : String
randomBoard = 
  "00302060090030500100180640000810290070000000800670820000260950080020300900501030"

