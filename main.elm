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
  App.beginnerProgram 
    { model = init
    , view = view
    , update = update 
    }

type alias Model = 
  {
    board: String
  }


init : (Model, Cmd String)
init =
  (Model randomBoard, Cmd.none)
  
view: (Model, Cmd String) -> Html Action
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
      ]
  
type Action = Reset | Update

update : Action -> (Model, Cmd String) -> (Model, Cmd String)
update action model =
  case action of
    Reset -> 
      (Model "00302060090030500100180640000810290070000000800670820000260950080020300900501030", Cmd.none)
    Update -> 
      (Model "10302060090030500100180640000810290070000100800670820000260950080120300900501031", Cmd.none)

    
cell: String -> Html String
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
    

myStyle : Attribute String
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

