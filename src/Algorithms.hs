module Algorithms
( Difficulty(..)
, generate
, solve
, solve_all
, generate_game
, generate_random
, validate_template
) where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set, lookup_min, lookup_max)
import qualified Data.Set as Set
import Debug.Trace
import Structures
import System.Random
import System.Timeout


gen_r_cell :: Int -> Int -> Int -> IO Cell
gen_r_cell rn cn val = do
      r <- random_r_IO (1, rn)
      c <- random_r_IO (1, cn)
      return $ Cell r c val


gen_r_cell_from_set :: Set Cell -> IO [Cell]
gen_r_cell_from_set set = if Set.null set
      then return []
      else do
            let size = Set.size set
            r <- random_r_IO (0, size - 1)
            let e = Set.elemAt r set
            let newSet = Set.deleteAt r set
            sets <- gen_r_cell_from_set newSet
            return $ e : sets


generate_random :: Int -> Int -> Float -> IO Matrix
generate_random rn cn ratio = do
      let obs_ratio = if ratio < 0 || ratio > 1 then 0.33 else ratio
      let cant_obs = floor $ fromIntegral rn * fromIntegral cn * obs_ratio
      let (Matrix _ _ cells) = darkMatrix rn cn
      randomCells <- gen_r_cell_from_set cells
      let matrix = blankMatrix rn cn
      let obs_matrix = foldl editMatrixCell matrix (take cant_obs randomCells)
      first_cell <- gen_r_cell rn cn 1
      let newMatrix = editMatrixCell obs_matrix first_cell
      if validate_template newMatrix then
            return newMatrix
      else
            generate_random rn cn ratio


data Difficulty = Easy | Normal | Hard deriving (Ord, Eq, Show, Read)


emptyRatio :: Difficulty -> Float
emptyRatio Easy = 50/100
emptyRatio Normal = 60/100
emptyRatio Hard = 70/100


generate_randomGame :: Int -> Int -> Float -> IO Matrix
generate_randomGame rn cn ratio = do
      maybeTemplate <- timeout 1000000 $ generate_random rn cn ratio
      if isNothing maybeTemplate then
            generate_randomGame rn cn ratio
      else do
            let template = maybe (blankMatrix rn cn) id maybeTemplate
            seed <- randomIO :: IO Int
            let gen = mkStdGen seed
            let seeds = randoms gen :: [Int]
            let solutions = solve_all template seeds
            if null solutions then
                  generate_randomGame rn cn ratio
            else
              return $ head solutions


removeCells :: Matrix -> [Cell] -> Int -> [Int] -> Matrix
removeCells sol@(Matrix rn cn cs) cells n seeds =
      if    null cells || n < 0 then
            sol
      else
            let   (headCell: tailCells) = cells
                  rowCell = row headCell
                  colCell = column headCell
                  empty = Cell rowCell colCell 0
                  matrix = editMatrixCell sol empty
                  solutions = solve_all matrix seeds
                  isUnix = length (take 2 solutions) < 2
            in    if isUnix then
                        removeCells matrix tailCells (n - 1) seeds
                  else
                        removeCells sol tailCells n seeds


generate_game :: Int -> Int -> Float -> Difficulty -> Int -> IO (Bool, Matrix)
generate_game rn cn ratio dif to = do
      maybeMatrix <- timeout to $ generate_randomGame rn cn ratio
      if isNothing maybeMatrix then do
            return (False, blankMatrix 1 1)
      else do
            let solution = maybe (blankMatrix rn cn) id maybeMatrix
            let maxElem = rn * cn - countObstacles solution
            let setForRemove = Set.filter (\x -> let v = value x in v > 1 && v < maxElem) (matrix solution) :: Set Cell
            randomCells <- gen_r_cell_from_set setForRemove
            let total = Set.size setForRemove
            let cant_empty = floor $ fromIntegral total * emptyRatio dif
            seed <- randomIO :: IO Int
            let gen = mkStdGen seed
            let seeds = randoms gen :: [Int]
            let game = removeCells solution randomCells cant_empty seeds
            return (True, game)


generate :: Matrix
generate = read "{. x x x x x x x x \n . 8 x x x x x x x \n . . 11 x x x x x x \n 29 . 10 . x x x x x \n 30 . . . . x x x x \n . 31  1 38  .  . x x x \n . 32 . . 39 41 . x x \n . . . 22 . . 42 . x \n . . . . . . . 44 45}" :: Matrix


stepMatrix :: Int -> Matrix -> Cell -> Map Int Cell -> [Int] -> [(Matrix, Cell)]
stepMatrix step m@(Matrix rs cs ma) prevCell map seeds = if Map.notMember step map
      then [
            newMatrix | cell <- getAdjacents prevCell rs cs step seeds,
            let actCell = Set.elemAt (Set.findIndex cell ma) ma,
            value actCell == 0,
            let newMatrix = (Matrix rs cs (Set.insert cell ma), cell)
      ]
      else  let actCell = map Map.! step
            in [newMatrix | isAdjacent actCell prevCell || value actCell == 1, let newMatrix = (m, actCell)]


buildMap :: Matrix -> Map Int Cell
buildMap ma@(Matrix r c m) = Set.foldl (\acc cell -> Map.insert (value cell) cell acc) Map.empty m


validate_template :: Matrix -> Bool
validate_template (Matrix rn cn cells)
      | sum [ 1 | degree <- degrees, degree == 0 ] > 0 = False 
      | sum [ 1 | degree <- degrees, degree == 1 ] > 2 = False
      | otherwise                                      = True
      where degrees = [ length adjacents | cell <- Set.toList cells, value cell >= 0, 
                        let adjacents = [ adjR | adj <- getAdjacents cell rn cn 0 [1..],
                                          let Just adjR = Set.lookupGE adj cells, adjR /= cell, value adjR == 0]]


solveRecursiveDFS :: Matrix -> Int -> Cell -> Int -> Map Int Cell -> [Int] -> [Matrix]
solveRecursiveDFS actualMatrix step prevCell obs map seeds
      | step == obs + 1 = [actualMatrix]
      | otherwise = let toAdd = stepMatrix step actualMatrix prevCell map seeds
                    in concat [ solveRecursiveDFS matrix (step + 1) prevCell obs map (tail seeds) | (matrix, prevCell) <- toAdd ]


solve_all :: Matrix -> [Int] -> [Matrix]
solve_all m = solveRecursiveDFS m 1 (Cell 0 0 0) (rows m * columns m  - countObstacles m) (buildMap m)


solve :: Matrix -> [Int] -> Matrix
solve m seeds = let solves = solve_all m seeds
          in case solves of [] -> error "Solve not found"
                            (x: xs) -> x
