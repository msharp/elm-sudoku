-- from evancz/start-app

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Array
import String exposing (..)

main =
  StartApp.start { model = model, view = view, update = update }


model = "00302060090030500100180640000810290070000000800670820000260950080020300900501030"
  
view address model =
  div []
      [ div [] [ text (toString unitlist) ]
      , div [] [ text (toString (List.length unitlist)) ]
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

cross_concat : String -> String -> List String
cross_concat rows cols =
  cross rows cols |> List.concat

inner_cross : String -> String -> List String
inner_cross row cols =
  List.map (\c -> String.append row (String.fromChar c)) (String.toList cols) 
  
cross : String -> String -> List (List String)
cross rows cols =
  List.map (\c -> inner_cross (String.fromChar c) cols) (String.toList rows) 

cross_multi : String -> List String -> List (List String)
cross_multi rows cols =
  List.map (\c -> cross_concat rows c) cols

digits  = "123456789"

digit_blocks = 
  blocks digits

rows    = "ABCDEFGHI"

row_blocks = 
  blocks rows

blocks : String -> List String
blocks rc =
  List.append [(String.slice 0 3 rc)] [(String.slice 3 6 rc)]
  |> List.append [(String.slice 6 9 rc)]


get_row_block : Int ->  List(String) -> String
get_row_block ix row_blocks =
  case (Array.get ix (Array.fromList row_blocks)) of
    Just block  -> block
    Nothing     -> ""
 

cols    = digits
squares = 
  cross_concat rows cols 
unitlist = 
  List.append (cross rows cols) (cross cols rows) 
  |> List.append (cross_multi (get_row_block 0 row_blocks) digit_blocks)
  |> List.append (cross_multi (get_row_block 1 row_blocks) digit_blocks)
  |> List.append (cross_multi (get_row_block 2 row_blocks) digit_blocks)

