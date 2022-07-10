module Structures
( Cell (..)
, Board (..)
, count_in
, count_free
, count_obs
, is_neighbour
, valid_board
, final_board
, edit_board_cell
, find_by_val
, get_neighbours
, gen_shuffle
, blank_board
, dark_board
) where


import Data.Char (isDigit)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set
import Debug.Trace
import System.Random


data Cell = Cell { row :: Int
                , column :: Int
                , value :: Int
                }


instance Eq Cell where
    c1 == c2 = row c1 == row c2 && column c1 == column c2


instance Ord Cell where
    compare cell1 cell2
        | row cell1 /= row cell2 = compare (row cell1) (row cell2)
        | column cell1 /= column cell2 = compare (column cell1) (column cell2)
        | otherwise = EQ


instance Show Cell where
    show cell = "(" ++ show (row cell) ++ "," ++ show (column cell) ++ "," ++ show (value cell) ++ ")"


instance Read Cell where
    reads_prec _ input =
        let (opar : rest1)   = input
            (rows, rest2)    = span isDigit rest1
            (comma1:rest3)   = rest2
            (columns, rest4) = span isDigit rest3
            (comma2 : rest5) = rest4
            (values, rest6)  = span isDigit rest5
            (cpar : rest7)   = rest6
            row              = read rows :: Int
            column           = read columns :: Int   
            value            = read values :: Int
        in 
            [(Cell row column value, rest7) | 
                opar == '(' && comma1 == comma2 && comma2 == ',' && cpar == ')']


get_neighbours :: Cell -> Int -> Int -> Int -> [Int] -> [Cell]
get_neighbours (Cell r c v) rs cs s seeds = [
        Cell nr nc s |
        dr <- gen_shuffle seeds [-1, 0, 1],
        dc <- gen_shuffle seeds [-1, 0, 1],
        let (nr, nc) = (r + dr, c + dc),
        nr > 0,
        nr <= rs,
        nc > 0,
        nc <= cs,
        not $ dr == 0 && dc == 0
    ]


f_yates_step :: RandomGen g => (Map Int Int, g) -> (Int, Int) -> (Map Int Int, g)
f_yates_step (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen


f_yates :: RandomGen g => g -> [Int] -> ([Int], g)
f_yates gen [] = ([], gen)
f_yates gen l = 
  toElems $ foldl f_yates_step (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)


gen_shuffle :: [Int] -> [Int] -> [Int]
gen_shuffle seeds list = let (xs, _) = f_yates (mkStdGen $ head seeds) list in xs


is_neighbour :: Cell -> Cell -> Bool
is_neighbour (Cell f1 c1 v1) (Cell f2 c2 v2)
            | Cell f1 c1 v1 == Cell f2 c2 v2           = False
            | abs (f1 - f2) >= 2 || abs (c1 - c2) >= 2 = False
            | v1 == (-1) || v2 == (-1)                 = False
            | otherwise                                = True


get_cell_val :: Cell -> Int -> String
get_cell_val (Cell _ _ value) size
    | value <  0    = (++) "x" $ concat $ replicate (max (size - 1) 0) " "
    | value == 0    = (++) "." $ concat $ replicate (max (size - 1) 0) " "
    | otherwise     = (++) valueStr $ concat $ replicate (max (size - lenValueStr) 0) " " 
    where
        valueStr    = show value
        lenValueStr = length $ show value


cell_eq :: Cell -> Cell -> Bool
cell_eq x y = row x == row y && column x == column y && value x == value y


cell_eqs' :: [Cell] -> [Cell] -> Bool -> Bool
cell_eqs' _ _ False = False
cell_eqs' [] [] x = x && True
cell_eqs' _ [] _ = False
cell_eqs' [] _ _ = False
cell_eqs' (x:xs) (y:ys) b = cell_eqs' xs ys (cell_eq x y)


cell_eqs :: [Cell] -> [Cell] -> Bool
cell_eqs xs ys = cell_eqs' xs ys True


data Board = Board { rows :: Int
                     , columns :: Int
                     , matrix :: Set Cell
                     }


instance Eq Board where
    m1 == m2 = rows m1 == rows m2 && columns m1 == columns m2 && cell_eqs (Set.elems $ matrix m1) (Set.elems $ matrix m2)


show_board_row :: [Cell] -> Int -> String
show_board_row row_cells size = unwords [get_cell_val cell size | cell <- row_cells ]


instance Show Board where
    show m = "{" ++ intercalate "\n " [ show_board_row (sort (filter (\(Cell crow _ _) -> crow == row) (Set.elems $ matrix m))) maxSize |
                            row <- [1..(rows m)]] ++ "}\n" where
                            maxSize = maximum [length (get_cell_val cell 0) | cell <- Set.elems $ matrix m]


parse_board_cell :: String -> (Int, String)
parse_board_cell "" = (-2, "")
parse_board_cell (s:rest)
    | isDigit s = let (nums, rest1) = span isDigit (s:rest) in
                    (read nums :: Int, rest1)
    | s == '.'  = (0, rest)
    | s == 'x'  = (-1, rest)
    | otherwise = (-2, s:rest)


parse_board_cellrec :: Int -> Int -> String -> (Set Cell, String)
parse_board_cellrec row_num column_num input = 
    let (_, rinput)    = span (' '==) input
        (value, rest1) = parse_board_cell rinput
    in if value == -2 then (Set.empty, rest1) else
        let (pcells, frest) = parse_board_cellrec row_num (column_num + 1) rest1
            pcell           = Cell row_num column_num value
        in (Set.insert pcell pcells, frest)


parse_board_row row_num = parse_board_cellrec row_num 1


parse_board_rec :: Int -> String -> (Set Cell, String)
parse_board_rec row_num (s:rest)
    | (s == '{' && row_num == 1) || s == '\n' = let (prow, rest1)  = parse_board_row row_num rest
                                                   (prows, rest2) = parse_board_rec (row_num + 1) rest1
                                                in (Set.union prow prows, rest2)
    | s == '}'                               = (Set.empty, rest)


parseMatrix = parse_board_rec 1


blank_board :: Int -> Int -> Board
blank_board rows columns = Board rows columns (Set.fromList [Cell row column 0 | row <- [1..rows], column <- [1..columns]] :: Set Cell)


dark_board :: Int -> Int -> Board
dark_board rows columns = Board rows columns (Set.fromList [Cell row column (-1) | row <- [1..rows], column <- [1..columns]] :: Set Cell)


count_in :: Int -> Board -> Int
count_in val (Board _ _ cells) =
                    foldl (\acc cell -> if value cell == val then acc + 1 else acc) 0 cells


count_obs :: Board -> Int
count_obs = count_in (-1) 


count_free :: Board -> Int
count_free = count_in 0 


valid_board :: Board -> Bool
valid_board m = let all_cells       = length (matrix m) == rows m * columns m
                      rigth_vals  = all (\(Cell _ _ val) -> val >= -1 && val <= rows m * columns m - count_obs m) 
                                        (matrix m)                    
                  in rigth_vals && all_cells


final_board :: Board -> Int -> Int -> Bool
final_board m@(Board rows columns cells) step obs
        | step < notObs = False
        | or [ value cell == 0 | cell <- Set.elems cells ] = False
        | not (and [ count_in val m == 1 | val <- [1..notObs] ]) = False
        | not (and [ or [ value cell1 + 1 == value cell2 | cell2 <- Set.elems cells, is_neighbour cell1 cell2 ]
                                     | cell1 <- Set.elems cells,
                                      value cell1 /= fvalue,
                                      value cell1 /= (-1) ]) = False
        | otherwise = True
        where fvalue = maximum [value cell | cell <- Set.elems cells]
              notObs = rows * columns - obs


instance Read Board where
    reads_prec _ input =
        let (matrixCells, rest) = parseMatrix input
            row_num              = maximum [ row cell | cell <- Set.elems matrixCells ]
            column_num           = maximum [ column cell | cell <- Set.elems matrixCells ]
            tboard           = Board row_num column_num matrixCells
        in [(tboard, rest) | valid_board tboard]


edit_board_cell :: Board -> Cell -> Board
edit_board_cell (Board r c cells) ncell = Board r c (Set.insert ncell cells)


find_by_val :: Board -> Int -> Set Cell
find_by_val m val = Set.filter (\cell -> value cell == val) $ matrix m
