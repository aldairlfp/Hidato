module Algorithms
( Difficulty(..)
, generate
, solve
, solve_all
, gen_game
, gen_random
, validate_template
) where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set
import Debug.Trace
import Structures
import System.Random
import System.Timeout


gen_rcell :: Int -> Int -> Int -> IO Cell
gen_rcell rn cn val = do
      r <- randomRIO (1, rn)
      c <- randomRIO (1, cn)
      return $ Cell r c val


gen_rcell_set :: Set Cell -> IO [Cell]
gen_rcell_set set = if Set.null set
      then return []
      else do
            let size = Set.size set
            r <- randomRIO (0, size - 1)
            let e = Set.elemAt r set
            let newSet = Set.deleteAt r set
            sets <- gen_rcell_set newSet
            return $ e : sets


gen_random :: Int -> Int -> Float -> IO Board
gen_random rn cn ratio = do
      let obs_ratio = if ratio < 0 || ratio > 1 then 0.33 else ratio
      let cant_obs = floor $ fromIntegral rn * fromIntegral cn * obs_ratio
      let (Board _ _ cells) = darkMatrix rn cn
      randomCells <- gen_rcell_set cells
      let matrix = blankMatrix rn cn
      let obs_matrix = foldl editMatrixCell matrix (take cant_obs randomCells)
      first_cell <- gen_rcell rn cn 1
      let new_board = editMatrixCell obs_matrix first_cell
      if validate_template new_board then
            return new_board
      else
            gen_random rn cn ratio


data Difficulty = Easy | Normal | Hard deriving (Ord, Eq, Show, Read)


disable_ratio :: Difficulty -> Float
disable_ratio Easy = 50/100
disable_ratio Normal = 60/100
disable_ratio Hard = 70/100


gen_rgame :: Int -> Int -> Float -> IO Board
gen_rgame rn cn ratio = do
      try_template <- timeout 1000000 $ gen_random rn cn ratio
      if isNothing try_template then
            gen_rgame rn cn ratio
      else do
            let template = maybe (blankMatrix rn cn) id try_template
            seed <- randomIO :: IO Int
            let gen = mk_std_gen seed
            let seeds = randoms gen :: [Int]
            let solutions = solve_all template seeds
            if null solutions then
                  gen_rgame rn cn ratio
            else
              return $ head solutions


remove_cells :: Board -> [Cell] -> Int -> [Int] -> Board
remove_cells sol@(Board rn cn cs) cells n seeds =
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
                        remove_cells matrix tailCells (n - 1) seeds
                  else
                        remove_cells sol tailCells n seeds


gen_game :: Int -> Int -> Float -> Difficulty -> Int -> IO (Bool, Board)
gen_game rn cn ratio dif to = do
      try_board <- timeout to $ gen_rgame rn cn ratio
      if isNothing try_board then do
            return (False, blankMatrix 1 1)
      else do
            let solution = maybe (blankMatrix rn cn) id try_board
            let max = rn * cn - countObstacles solution
            let setForRemove = Set.filter (\x -> let v = value x in v > 1 && v < max) (matrix solution) :: Set Cell
            randomCells <- gen_rcell_set setForRemove
            let total = Set.size setForRemove
            let cant_empty = floor $ fromIntegral total * disable_ratio dif
            seed <- randomIO :: IO Int
            let gen = mk_std_gen seed
            let seeds = randoms gen :: [Int]
            let game = remove_cells solution randomCells cant_empty seeds
            return (True, game)


generate :: Board
generate = read "{. x x x x x x x x \n . 8 x x x x x x x \n . . 11 x x x x x x \n 29 . 10 . x x x x x \n 30 . . . . x x x x \n . 31  1 38  .  . x x x \n . 32 . . 39 41 . x x \n . . . 22 . . 42 . x \n . . . . . . . 44 45}" :: Board


step_board :: Int -> Board -> Cell -> Map Int Cell -> [Int] -> [(Board, Cell)]
step_board step m@(Board rs cs ma) prev_cell map seeds = if Map.notMember step map
      then [
            new_board | cell <- get_neighbours prev_cell rs cs step seeds,
            let actCell = Set.elemAt (Set.findIndex cell ma) ma,
            value actCell == 0,
            let new_board = (Board rs cs (Set.insert cell ma), cell)
      ]
      else  let actCell = map Map.! step
            in [new_board | is_neighbour actCell prev_cell || value actCell == 1, let new_board = (m, actCell)]


build_map :: Board -> Map Int Cell
build_map ma@(Board r c m) = Set.foldl (\acc cell -> Map.insert (value cell) cell acc) Map.empty m


validate_template :: Board -> Bool
validate_template (Board rn cn cells)
      | sum [ 1 | degree <- degrees, degree == 0 ] > 0 = False 
      | sum [ 1 | degree <- degrees, degree == 1 ] > 2 = False
      | otherwise                                      = True
      where degrees = [ length adjacents | cell <- Set.toList cells, value cell >= 0, 
                        let adjacents = [ adjR | adj <- get_neighbours cell rn cn 0 [1..],
                                          let Just adjR = Set.lookupGE adj cells, adjR /= cell, value adjR == 0]]


solve_DFS :: Board -> Int -> Cell -> Int -> Map Int Cell -> [Int] -> [Board]
solve_DFS actualMatrix step prev_cell obs map seeds
      | step == obs + 1 = [actualMatrix]
      | otherwise = let toAdd = step_board step actualMatrix prev_cell map seeds
                    in concat [ solve_DFS matrix (step + 1) prev_cell obs map (tail seeds) | (matrix, prev_cell) <- toAdd ]


solve_all :: Board -> [Int] -> [Board]
solve_all m = solve_DFS m 1 (Cell 0 0 0) (rows m * columns m  - countObstacles m) (build_map m)


solve :: Board -> [Int] -> Board
solve m seeds = let solves = solve_all m seeds
          in case solves of [] -> error "Solve not found"
                            (x: xs) -> x
