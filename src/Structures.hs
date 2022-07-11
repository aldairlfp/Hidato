module Structures
( Cell (..)
, Board (..)
, countInBoard
, countFree
, countObstacles
, isAdjacent
, isValidBoard
, isFinalBoard
, editBoardCell
, findCellByValue
, getAdjacents
, genShuffle
, blankBoard
, darkBoard
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
    readsPrec _ input =
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


getAdjacents :: Cell -> Int -> Int -> Int -> [Int] -> [Cell]
getAdjacents (Cell r c v) rs cs s seeds = [
        Cell nr nc s |
        dr <- genShuffle seeds [-1, 0, 1],
        dc <- genShuffle seeds [-1, 0, 1],
        let (nr, nc) = (r + dr, c + dc),
        nr > 0,
        nr <= rs,
        nc > 0,
        nc <= cs,
        not $ dr == 0 && dc == 0
    ]


fisherYatesStep :: RandomGen g => (Map Int Int, g) -> (Int, Int) -> (Map Int Int, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen


fisherYates :: RandomGen g => g -> [Int] -> ([Int], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)


genShuffle :: [Int] -> [Int] -> [Int]
genShuffle seeds list = let (xs, _) = fisherYates (mkStdGen $ head seeds) list in xs


isAdjacent :: Cell -> Cell -> Bool
isAdjacent (Cell f1 c1 v1) (Cell f2 c2 v2)
            | Cell f1 c1 v1 == Cell f2 c2 v2           = False
            | abs (f1 - f2) >= 2 || abs (c1 - c2) >= 2 = False
            | v1 == (-1) || v2 == (-1)                 = False
            | otherwise                                = True


getCellChar :: Cell -> Int -> String
getCellChar (Cell _ _ value) size
    | value <  0    = (++) "x" $ concat $ replicate (max (size - 1) 0) " "
    | value == 0    = (++) "." $ concat $ replicate (max (size - 1) 0) " "
    | otherwise     = (++) valueStr $ concat $ replicate (max (size - lenValueStr) 0) " " 
    where
        valueStr    = show value
        lenValueStr = length $ show value


cellEqual :: Cell -> Cell -> Bool
cellEqual x y = row x == row y && column x == column y && value x == value y


cellEquals' :: [Cell] -> [Cell] -> Bool -> Bool
callEquals' _ _ False = False
cellEquals' [] [] x = x && True
cellEquals' _ [] _ = False
cellEquals' [] _ _ = False
cellEquals' (x:xs) (y:ys) b = cellEquals' xs ys (cellEqual x y)


cellEquals :: [Cell] -> [Cell] -> Bool
cellEquals xs ys = cellEquals' xs ys True


data Board = Board { rows :: Int
                     , columns :: Int
                     , board :: Set Cell
                     }


instance Eq Board where
    m1 == m2 = rows m1 == rows m2 && columns m1 == columns m2 && cellEquals (Set.elems $ board m1) (Set.elems $ board m2)


showBoardRow :: [Cell] -> Int -> String
showBoardRow rowCells size = unwords [getCellChar cell size | cell <- rowCells ]


instance Show Board where
    show m = "{" ++ intercalate "\n " [ showBoardRow (sort (filter (\(Cell crow _ _) -> crow == row) (Set.elems $ board m))) maxSize |
                            row <- [1..(rows m)]] ++ "}\n" where
                            maxSize = maximum [length (getCellChar cell 0) | cell <- Set.elems $ board m]


parseBoardCell :: String -> (Int, String)
parseBoardCell "" = (-2, "")
parseBoardCell (s:rest)
    | isDigit s = let (nums,rest1) = span isDigit (s:rest) in
                    (read nums :: Int, rest1)
    | s == '.'  = (0, rest)
    | s == 'x'  = (-1, rest)
    | otherwise = (-2, s:rest)


parseBoardRowRecursive :: Int -> Int -> String -> (Set Cell, String)
parseBoardRowRecursive rowNum columnNum input = 
    let (_, rinput)    = span (' '==) input
        (value, rest1) = parseBoardCell rinput
    in if value == -2 then (Set.empty, rest1) else
        let (parsedCells, frest) = parseBoardRowRecursive rowNum (columnNum + 1) rest1
            parsedCell           = Cell rowNum columnNum value
        in (Set.insert parsedCell parsedCells, frest)


parseBoardRow rowNum = parseBoardRowRecursive rowNum 1


parseBoardRecursive :: Int -> String -> (Set Cell, String)
parseBoardRecursive rowNum (s:rest)
    | (s == '{' && rowNum == 1) || s == '\n' = let (parsedRow, rest1)  = parseBoardRow rowNum rest
                                                   (parsedRows, rest2) = parseBoardRecursive (rowNum + 1) rest1
                                                in (Set.union parsedRow parsedRows, rest2)
    | s == '}'                               = (Set.empty, rest)


parseBoard = parseBoardRecursive 1


blankBoard :: Int -> Int -> Board
blankBoard rows columns = Board rows columns (Set.fromList [Cell row column 0 | row <- [1..rows], column <- [1..columns]] :: Set Cell)


darkBoard :: Int -> Int -> Board
darkBoard rows columns = Board rows columns (Set.fromList [Cell row column (-1) | row <- [1..rows], column <- [1..columns]] :: Set Cell)


countInBoard :: Int -> Board -> Int
countInBoard val (Board _ _ cells) =
                    foldl (\acc cell -> if value cell == val then acc + 1 else acc) 0 cells


countObstacles :: Board -> Int
countObstacles = countInBoard (-1) 


countFree :: Board -> Int
countFree = countInBoard 0 


isValidBoard :: Board -> Bool
isValidBoard m = let allCells       = length (board m) == rows m * columns m
                     correctValues  = all (\(Cell _ _ val) -> val >= -1 && val <= rows m * columns m - countObstacles m) 
                                        (board m)                    
                  in correctValues && allCells


isFinalBoard :: Board -> Int -> Int -> Bool
isFinalBoard m@(Board rows columns cells) step obs
        | step < notObs = False
        | or [ value cell == 0 | cell <- Set.elems cells ] = False
        | not (and [ countInBoard val m == 1 | val <- [1..notObs] ]) = False
        | not (and [ or [ value cell1 + 1 == value cell2 | cell2 <- Set.elems cells, isAdjacent cell1 cell2 ]
                                     | cell1 <- Set.elems cells,
                                      value cell1 /= fvalue,
                                      value cell1 /= (-1) ]) = False
        | otherwise = True
        where fvalue = maximum [value cell | cell <- Set.elems cells]
              notObs = rows * columns - obs


instance Read Board where
    readsPrec _ input =
        let (boardCells, rest) = parseBoard input
            rowNum              = maximum [ row cell | cell <- Set.elems boardCells ]
            columnNum           = maximum [ column cell | cell <- Set.elems boardCells ]
            theBoard           = Board rowNum columnNum boardCells
        in [(theBoard, rest) | isValidBoard theBoard]


editBoardCell :: Board -> Cell -> Board
editBoardCell (Board r c cells) newCell = Board r c (Set.insert newCell cells)


findCellByValue :: Board -> Int -> Set Cell
findCellByValue m val = Set.filter (\cell -> value cell == val) $ board m
