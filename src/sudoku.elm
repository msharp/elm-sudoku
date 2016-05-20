module Sudoku exposing (
                rows, cols, squares, unitlist, units, peers, getUnits, getPeers,
                parseGrid, resolveGrid, blocks, Grid
                ) 
{-| Sudoku board representation and solver

@docs rows, cols, squares, unitlist, units, peers, getUnits, getPeers

@docs parseGrid, resolveGrid, blocks, Grid
-}

import Set
import Dict
import String exposing (..)

{-| A grid is all the cells with their possible values
-}
type alias Grid = 
  Dict.Dict String (List Char) 

-- solving --

{-| Parse a raw grid string into a dict of squares: values
-}
parseGrid : String -> Grid
parseGrid grid_string = 
  let   
      digs =
        String.toList digits

      sq_value s =
        if List.member s digs then
          [s]
        else
          digs
  in    
    String.toList grid_string
    |> List.map sq_value 
    |> zip squares 
    |> Dict.fromList


{-| Find any cells with one option and ensure it is eliminated from peers
-}
resolveGrid : Grid -> Grid
resolveGrid grid =
  let 
    opts =
      singularOptions grid

    change_cells = 
      List.map (\(cell,_) -> cell) opts

    resolved_grid = 
      resolveGrid' opts grid

    single_options = 
      singularOptions resolved_grid
      |> List.map (\(cell,_) -> cell)

  in
    if change_cells == single_options then
      resolved_grid -- simple puzzles are now solved
    else
      resolveGrid resolved_grid

resolveGrid' : List (String, Char) -> Grid -> Grid
resolveGrid' opts grid =
  case opts of
    [] -> grid
    (opt::remaining_opts) ->
      let 
        (cell,val) = opt
      in
        eliminateFromPeers val (getPeers cell) grid
        |> resolveGrid' remaining_opts 


assign : String -> Char -> Grid -> Grid
assign cell digit grid = 
    Dict.update cell (\x -> assignValue digit x) grid
    |> eliminateFromPeers digit (getPeers cell) 

assignValue : Char -> Maybe (List Char) -> Maybe (List Char)
assignValue val vals =
  case vals of
    Nothing   -> Nothing
    Just vals -> Just [val]

eliminate : String -> Char -> Grid -> Grid
eliminate cell digit values =
  let 
    -- remove the digit from the cell possibilities
    eliminated = 
      eliminateFromCell cell digit values
  in 
    -- now what is left?
    case Dict.get cell eliminated of
      Nothing -> 
        -- this should be an error in the board
        eliminated 

      Just remains ->
        case remains of
          [digit] -> 
            -- only one value left, can eliminate this from the peers
            eliminateFromPeers digit (getPeers cell) eliminated
          _ ->
            eliminated 

eliminateFromPeers : Char -> List String -> Grid -> Grid
eliminateFromPeers digit cell_peers grid =
  case cell_peers of 
    [] ->
      grid
    (peer::more_peers) ->
      eliminateFromCell peer digit grid
      |> eliminateFromPeers digit more_peers 

eliminateFromCell : String -> Char -> Grid -> Grid
eliminateFromCell cell digit values =
      Dict.update cell (\x -> eliminateValue digit x) values

eliminateValue : Char -> Maybe (List Char) -> Maybe (List Char)
eliminateValue val vals =
  case vals of
    Nothing   -> Nothing
    Just vals -> Just (List.filter (\v -> v /= val) vals)

{-| get the cells from a grid where only 1 value is present

FIXME this is a bit messy, fumbling with maybes and chars
-}
singularOptions : Grid -> List (String, Char)
singularOptions grid =
  let
    singleOption (cell, options) =
      case List.length options of
        1 -> True
        _ -> False

    asChar (cell, options) =
      let 
        opt = 
          case List.head options of
            Nothing -> ' '
            Just v  -> v
      in
         (cell, opt)

  in
    Dict.toList grid
    |> List.filter singleOption
    |> List.map asChar



-- the board elements

digits = "123456789"
alphas = "ABCDEFGHI"

{-| The rows of the board
-}
rows : String
rows = 
  alphas

{-| The columns of the board
-}
cols : String
cols =
  digits

{-| The squares which make up a sudoku borad
-}
squares : List String 
squares = 
  cross rows cols 

{-| The complete set of units on a board
-}
unitlist : List (List String)
unitlist  = 
  cross_digits rows 
  |> List.append (cross_alphas digits) 
  |> List.append blocks

{-| The units to which each square belongs 
-}
units : Dict.Dict String (List (List String))
units = 
  let 
    uuu f a =
      Dict.insert f (getUnits f) a
  in
    List.foldr (uuu) Dict.empty squares

{-| The peers of each square 
-}
peers : Dict.Dict String (List String)
peers = 
  let 
    ppp f a =
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
    Nothing     -> []
    Just peers  -> 
      List.concat peers 
      |> List.filter (\s -> s /= sq) 
      |> Set.fromList
      |> Set.toList
      


-- tests --

-- solved ?

-- broken ?
contradictoryGrid grid = 
  Dict.values grid
  |> List.member Nothing

-------------------------------------------------------
-- cruft to generate board elements
-------------------------------------------------------

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

{-| The 9 blocks of the sudoku grid
-}
blocks : List (List String)
blocks =
  let 
    a = 
      blockGroups alphas
      |> List.map (\l -> List.repeat 3 l) 
      |> List.concat
    n = 
      blockGroups digits
      |> List.repeat 3 
      |> List.concat 
  in
     List.map2 cross a n

blockGroups : String -> List String
blockGroups labels =
  List.append [] [(String.slice 0 3 labels)]
  |> List.append [(String.slice 3 6 labels)]
  |> List.append [(String.slice 6 9 labels)]
  |> List.reverse


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

