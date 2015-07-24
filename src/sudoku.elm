module Sudoku (
                rows, cols, squares, unitlist, units, peers, getUnits, getPeers,
                gridValues
                ) where
{-| Sudoku board representation and solver

@docs rows, cols, squares, unitlist, units, peers, getUnits, getPeers

@docs gridValues
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
  let uuu f a =
    Dict.insert f (getUnits f) a
  in
    List.foldr (uuu) Dict.empty squares

{-| The peers of each square 
-}
peers : Dict.Dict String (List String)
peers = 
  let ppp f a =
    Dict.insert f (getPeers f) a
  in
    List.foldr (ppp) Dict.empty squares

{-| Get the list of units for a given square
-}
getUnits : String -> List(List String)
getUnits sq =
   List.filter (\l -> List.member sq l) unitlist

{-| Get the list of peers for a given square
-}
getPeers : String -> List String
getPeers sq =
  case Dict.get sq units of
    Just peers  -> List.concat peers |> List.filter (\s -> s /= sq) |> set 
    Nothing     -> []


{-| Parse a raw grid string into a dict of squares: values
-}
gridValues : String -> Dict.Dict String (Maybe String)
gridValues grid = 
  let 
      sq_value s =
        if List.member s (str_list digits) then
          Just s
        else
          Nothing
  in    
    str_list grid 
    |> List.map sq_value 
    |> zip squares 
    |> Dict.fromList



-- junk to generate board elements

-- transform a string into a list of single-char strings
str_list : String -> List String
str_list str =
  List.map String.fromChar (String.toList str) 

-- a unique set from a list
set : List String -> List String
set squares =
  let s f a = 
    if (List.member f a) then a else (List.append [f] a)
  in
    List.foldr (s) [] squares

-- generate squares

cross : String -> String -> List String
cross alphas digits =
  let cd alpha digits = 
    List.map (\d -> String.append alpha d) (str_list digits) 
  in
    List.map (\a -> cd a digits) (str_list alphas) 
    |> List.concat

-- generate unitlists

cross_alphas digits =
  let cfa f a = 
    (cross alphas (String.fromChar f)) :: a
  in
     String.foldr (cfa) [] digits

cross_digits alphas =
  let cfd f a = 
    (cross (String.fromChar f) digits) :: a
  in
    String.foldr (cfd) [] alphas

-- FIXME not very functional :(
block_units : List (List String)
block_units =
  (cross "GHI" "789") :: (cross "GHI" "456") :: (cross "GHI" "123") :: 
  (cross "DEF" "789") :: (cross "DEF" "456") :: (cross "DEF" "123") :: 
  (cross "ABC" "789") :: (cross "ABC" "456") :: (cross "ABC" "123") :: []

{-| The zip function takes in two lists and returns a combined
list. It combines the elements of each list pairwise until one
of the lists runs out of elements.

    zip [1,2,3] ['a','b','c'] == [(1,'a'), (2,'b'), (3,'c')]
    
-}
zip : List a -> List b -> List (a,b)
zip xs ys =
    case (xs, ys) of
      ( x :: xs', y :: ys' ) ->
        (x,y) :: zip xs' ys'
      (_, _) ->
         []
