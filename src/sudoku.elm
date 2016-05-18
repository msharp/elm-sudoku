module Sudoku exposing (
                rows, cols, squares, unitlist, units, peers, getUnits, getPeers,
                gridValues, parseGrid, blocks, values
                ) 
{-| Sudoku board representation and solver

@docs rows, cols, squares, unitlist, units, peers, getUnits, getPeers

@docs gridValues, parseGrid, blocks, values
-}

import Array
import Dict
import String exposing (..)

type alias Grid = 
  Dict.Dict String (List Char) 

type alias Board = 
  Dict.Dict String (Maybe String)

-- solving --

a_board = 
  "003020600900305001001806400008102900700000008006708200002609500800203009005010300"

assignFromBoard : String -> Grid -> Grid
assignFromBoard board values =
  let 
    getFixed (cell,val) =
      case val of
        Nothing -> False
        Just val -> True

    valToChar (cell, val) =
      case val of
        Nothing -> (cell, ' ')
        Just val ->
          let
            vals = String.toList val
          in
            case List.head vals of
              Nothing -> (cell, ' ')
              Just value -> (cell, value)

    assignables =
      Dict.toList (gridValues board)
      |> List.filter (\v -> getFixed v)
      |> List.map (valToChar)

  in 
    assignFromBoardValues assignables values

assignFromBoardValues : List (String, Char) -> Grid -> Grid
assignFromBoardValues assignables values =
  case assignables of
    [] -> values
    (next_assign::rest) ->
      let 
        (a,v) =
          next_assign
        assigned =
          assign a v values
      in
         assignFromBoardValues rest assigned
        

{-| The possible values for all squares
-}
values : Grid
values =
  List.map (\s -> (s, (String.toList digits))) squares
  |> Dict.fromList
  |> assignFromBoard a_board

{-
  |> assign "A3" '3'
  |> assign "A5" '2'
  |> assign "A7" '6'
  |> assign "B1" '9'
  |> assign "C3" '1'
  -}


assign : String -> Char -> Grid -> Grid
assign cell digit values = 
  let
    assigned =
      Dict.update cell (\x -> assignValue digit x) values
  in
    case Dict.get cell peers of
      Just cell_peers ->
        eliminateFromPeers digit cell_peers assigned
      Nothing -> 
        assigned

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
            case Dict.get cell peers of
              Just cell_peers ->
                eliminateFromPeers digit cell_peers eliminated
              
              Nothing -> -- it has no peers (should never occur)
                eliminated 
          _ ->
            eliminated 

eliminateFromPeers : Char -> List String -> Grid -> Grid
eliminateFromPeers digit cell_peers values =
    let _ = Debug.log "peers" (toString cell_peers)
    in
    case cell_peers of 
      [] ->
        values
      (peer::more_peers) ->
        eliminateFromCell peer digit values
        |> eliminateFromPeers digit more_peers 

eliminateFromCell : String -> Char -> Grid -> Grid
eliminateFromCell cell digit values =
      Dict.update cell (\x -> eliminateValue digit x) values

eliminateValue : Char -> Maybe (List Char) -> Maybe (List Char)
eliminateValue val vals =
  case vals of
    Nothing   -> Nothing
    Just vals -> Just (List.filter (\v -> v /= val) vals)

assignValue : Char -> Maybe (List Char) -> Maybe (List Char)
assignValue val vals =
  case vals of
    Nothing   -> Nothing
    Just vals -> Just [val]



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
            |> List.append blocks

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

{-| Parse the grid and set fixed values and possible values for all squares
-}
parseGrid : String -> Dict.Dict String String
parseGrid grid =
  let
    values = 
      zip squares (List.repeat (List.length squares) digits)
    known_or_any sq =
      case sq of
        (s, Just sq_val) -> (s, sq_val)
        (s, Nothing)     -> (s, digits)
    parse_grid =
      Dict.toList (gridValues grid)
      |> List.map known_or_any 
      |> Dict.fromList
    -- TODO propagate failure to assign a value
  in
    -- if there are any Nothings left assignment failed
    --if (List.member Nothing (Dict.values parse_grid)) then
    --   Nothing
    --else
    --  Just 
    parse_grid
    
       


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


