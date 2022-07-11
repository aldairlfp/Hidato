module Logic
( Difficulty(..)
, generate
, solve
, solveAll
, generateGame
, generateRandom
, validateTemplate
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


genRCell :: Int -> Int -> Int -> IO Cell
genRCell rn cn val = do
      r <- randomRIO (1, rn)
      c <- randomRIO (1, cn)
      return $ Cell r c val


genRCellFromSet :: Set Cell -> IO [Cell]
genRCellFromSet set = if Set.null set
      then return []
      else do
            let size = Set.size set
            r <- randomRIO (0, size - 1)
            let e = Set.elemAt r set
            let newSet = Set.deleteAt r set
            sets <- genRCellFromSet newSet
            return $ e : sets


generateRandom :: Int -> Int -> Float -> IO Board
generateRandom rn cn ratio = do
      let obs_ratio = if ratio < 0 || ratio > 1 then 0.33 else ratio
      let cant_obs = floor $ fromIntegral rn * fromIntegral cn * obs_ratio
      let (Board _ _ cells) = darkBoard rn cn
      randomCells <- genRCellFromSet cells
      let board = blankBoard rn cn
      let obs_board = foldl editBoardCell board (take cant_obs randomCells)
      first_cell <- genRCell rn cn 1
      let newBoard = editBoardCell obs_board first_cell
      if validateTemplate newBoard then
            return newBoard
      else
            generateRandom rn cn ratio


data Difficulty = Easy | Normal | Hard deriving (Ord, Eq, Show, Read)


emptyRatio :: Difficulty -> Float
emptyRatio Easy = 50/100
emptyRatio Normal = 60/100
emptyRatio Hard = 70/100


generateRandomGame :: Int -> Int -> Float -> IO Board
generateRandomGame rn cn ratio = do
      maybeTemplate <- timeout 1000000 $ generateRandom rn cn ratio
      if isNothing maybeTemplate then
            generateRandomGame rn cn ratio
      else do
            let template = maybe (blankBoard rn cn) id maybeTemplate
            seed <- randomIO :: IO Int
            let gen = mkStdGen seed
            let seeds = randoms gen :: [Int]
            let solutions = solveAll template seeds
            if null solutions then
                  generateRandomGame rn cn ratio
            else
              return $ head solutions


removeCells :: Board -> [Cell] -> Int -> [Int] -> Board
removeCells sol@(Board rn cn cs) cells n seeds =
      if    null cells || n < 0 then
            sol
      else
            let   (headCell: tailCells) = cells
                  rowCell = row headCell
                  colCell = column headCell
                  empty = Cell rowCell colCell 0
                  board = editBoardCell sol empty
                  solutions = solveAll board seeds
                  isUnix = length (take 2 solutions) < 2
            in    if isUnix then
                        removeCells board tailCells (n - 1) seeds
                  else
                        removeCells sol tailCells n seeds


generateGame :: Int -> Int -> Float -> Difficulty -> Int -> IO (Bool, Board)
generateGame rn cn ratio dif to = do
      maybeBoard <- timeout to $ generateRandomGame rn cn ratio
      if isNothing maybeBoard then do
            return (False, blankBoard 1 1)
      else do
            let solution = maybe (blankBoard rn cn) id maybeBoard
            let maxElem = rn * cn - countObstacles solution
            let setForRemove = Set.filter (\x -> let v = value x in v > 1 && v < maxElem) (board solution) :: Set Cell
            randomCells <- genRCellFromSet setForRemove
            let total = Set.size setForRemove
            let cant_empty = floor $ fromIntegral total * emptyRatio dif
            seed <- randomIO :: IO Int
            let gen = mkStdGen seed
            let seeds = randoms gen :: [Int]
            let game = removeCells solution randomCells cant_empty seeds
            return (True, game)


generate :: Board
generate = read "{. x x x x x x x x \n . 8 x x x x x x x \n . . 11 x x x x x x \n 29 . 10 . x x x x x \n 30 . . . . x x x x \n . 31  1 38  .  . x x x \n . 32 . . 39 41 . x x \n . . . 22 . . 42 . x \n . . . . . . . 44 45}" :: Board


stepBoard :: Int -> Board -> Cell -> Map Int Cell -> [Int] -> [(Board, Cell)]
stepBoard step m@(Board rs cs ma) prevCell map seeds = if Map.notMember step map
      then [
            newBoard | cell <- getAdjacents prevCell rs cs step seeds,
            let actCell = Set.elemAt (Set.findIndex cell ma) ma,
            value actCell == 0,
            let newBoard = (Board rs cs (Set.insert cell ma), cell)
      ]
      else  let actCell = map Map.! step
            in [newBoard | isAdjacent actCell prevCell || value actCell == 1, let newBoard = (m, actCell)]


buildMap :: Board -> Map Int Cell
buildMap ma@(Board r c m) = Set.foldl (\acc cell -> Map.insert (value cell) cell acc) Map.empty m


validateTemplate :: Board -> Bool
validateTemplate (Board rn cn cells)
      | sum [ 1 | degree <- degrees, degree == 0 ] > 0 = False 
      | sum [ 1 | degree <- degrees, degree == 1 ] > 2 = False
      | otherwise                                      = True
      where degrees = [ length adjacents | cell <- Set.toList cells, value cell >= 0, 
                        let adjacents = [ adjR | adj <- getAdjacents cell rn cn 0 [1..],
                                          let Just adjR = Set.lookupGE adj cells, adjR /= cell, value adjR == 0]]


solveRecursiveDFS :: Board -> Int -> Cell -> Int -> Map Int Cell -> [Int] -> [Board]
solveRecursiveDFS actualBoard step prevCell obs map seeds
      | step == obs + 1 = [actualBoard]
      | otherwise = let toAdd = stepBoard step actualBoard prevCell map seeds
                    in concat [ solveRecursiveDFS board (step + 1) prevCell obs map (tail seeds) | (board, prevCell) <- toAdd ]


solveAll :: Board -> [Int] -> [Board]
solveAll m = solveRecursiveDFS m 1 (Cell 0 0 0) (rows m * columns m  - countObstacles m) (buildMap m)


solve :: Board -> [Int] -> Board
solve m seeds = let solves = solveAll m seeds
          in case solves of [] -> error "Solve not found"
                            (x: xs) -> x
