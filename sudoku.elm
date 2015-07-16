-- from evancz/start-app

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import String exposing (..)

main =
  StartApp.start { model = model, view = view, update = update }


model = "00302060090030500100180640000810290070000000800670820000260950080020300900501030"
  
view address model =
  div []
      [ div [] [ text (toString squares) ]
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


-- the board elements

cross_cut ref list =
  List.map (\c -> String.append ref (String.fromChar c)) (String.toList list) 
  
cross a b =
  List.map (\d -> cross_cut (String.fromChar d) a) (String.toList b) 
  |> List.concat

digits  = "123456789"
rows    = "ABCDEFGHI"
cols    = digits
squares = cross rows cols


