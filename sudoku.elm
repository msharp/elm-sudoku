-- from evancz/start-app

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Array
import Dict
import String exposing (..)

main =
  StartApp.start { model = model, view = view, update = update }


model = random_board
  
view address model =
  div []
      [ div [] [ text (toString squares) ]
      , div [] [ text (toString (List.length squares)) ]
      , div [] [ text " ------- "]
      , div [] [ text (toString unitlist) ]
      , div [] [ text (toString (List.length unitlist)) ]
      , div [] [ text " ------- "]
      -- , div [] [ text (toString units) ]
      -- , div [] [ text (toString peers) ]
      , div [] [ text " ------- "]
      , div [] [ text (toString (get_peers "C2")) ]
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


random_board : String
random_board = 
  "00302060090030500100180640000810290070000000800670820000260950080020300900501030"

-- the board elements

digits    = "123456789"
rows      = "ABCDEFGHI"
cols      = digits
squares   = cross rows cols 
unitlist  = (cross_digits rows) 
            |> List.append (cross_alphas digits) 
            |> List.append block_units
units     = List.foldr (uuu) Dict.empty squares
peers     = List.foldr (ppp) Dict.empty squares


-- junk to generate board elements
alphas = rows

-- generate squares

cross alphas digits =
  List.map (\a -> cross_digs a digits) (str_list alphas) 
  |> List.concat

cross_digs alpha digits = 
  List.map (\d -> String.append alpha d) (str_list digits) 

str_list str =
  List.map String.fromChar (String.toList str) 

-- generate unitlists

cross_alphas digits =
  String.foldr (cfa) [] digits

cfa f a = 
  (cross alphas (String.fromChar f)) :: a

cross_digits alphas =
  String.foldr (cfd) [] alphas

cfd f a = 
  (cross (String.fromChar f) digits) :: a

-- fix this
block_units : List (List String)
block_units =
  (cross "GHI" "789") :: (cross "GHI" "456") :: (cross "GHI" "123") :: 
  (cross "DEF" "789") :: (cross "DEF" "456") :: (cross "DEF" "123") :: 
  (cross "ABC" "789") :: (cross "ABC" "456") :: (cross "ABC" "123") :: []

-- get units

square_units : String -> List(List String)
square_units sq =
   List.filter (\l -> List.member sq l) unitlist

uuu f a = 
  Dict.insert f (square_units f) a

-- get unique set of peers
get_peers : String -> List String
get_peers sq =
  case Dict.get sq units of
    Just peers  -> List.concat peers |> List.filter (\s -> s /= sq) |> set 
    Nothing     -> []

-- a unique set from a list
set : List String -> List String
set squares =
  List.foldr (set_foldr) [] squares

set_foldr f a = 
  if (List.member f a) then a else (List.append [f] a)

ppp f a =
  Dict.insert f (get_peers f) a

