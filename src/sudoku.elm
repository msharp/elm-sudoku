module Sudoku (
                rows, cols, squares, unitlist, units, peers, get_units, get_peers
                ) where
{-| Sudoku board representation and solver

@docs rows, cols, squares, unitlist, units, peers, get_units, get_peers
-}

import Array
import Dict
import String exposing (..)

-- the board elements

digits = "123456789"
alphas = "ABCDEFGHI"

{-| The rows of the board
-}
rows : String
rows = alphas

{-| The columns of the board
-}
cols : String
cols = digits

{-| The squares which make up a sudoku borad
-}
squares : List String 
squares = cross rows cols 

{-| The complete set of units on a board
-}
unitlist : List (List String)
unitlist  = (cross_digits rows) 
            |> List.append (cross_alphas digits) 
            |> List.append block_units

{-| The units to which each square belongs 
-}
units : Dict.Dict String (List (List String))
units = 
  List.foldr (uuu) Dict.empty squares

{-| The peers of each square 
-}
peers : Dict.Dict String (List String)
peers = 
  List.foldr (ppp) Dict.empty squares


-- junk to generate board elements

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

{-| Get the list of units for a given square
-}
get_units : String -> List(List String)
get_units sq =
   List.filter (\l -> List.member sq l) unitlist

uuu f a = 
  Dict.insert f (get_units f) a

-- get unique set of peers

{-| Get the list of peers for a given square
-}
get_peers : String -> List String
get_peers sq =
  case Dict.get sq units of
    Just peers  -> List.concat peers |> List.filter (\s -> s /= sq) |> set 
    Nothing     -> []

ppp f a =
  Dict.insert f (get_peers f) a

-- a unique set from a list

set : List String -> List String
set squares =
  List.foldr (set_foldr) [] squares

set_foldr f a = 
  if (List.member f a) then a else (List.append [f] a)

