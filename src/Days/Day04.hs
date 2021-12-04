module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative
import Control.Monad

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    draw <- many' $ decimal <* ("," <|> endOfLine *> "")
    skipSpace
    boardsLists <- many' $ replicateM 5 (replicateM 5 (decimal <* skipSpace))
    pure $ Input draw $ map (\xs -> Board xs (transpose xs)) boardsLists

------------ TYPES ------------
data Board = Board {
    rows :: [[Int]],
    columns :: [[Int]]
} deriving Show

data Input = Input {
    draw :: [Int],
    boards :: [Board]
} deriving Show

type OutputA = Int

type OutputB = Int

------------ PART A ------------
applyNumber :: Int -> Board -> Board
applyNumber called Board{..} = Board {
    rows = map (filter (/= called)) rows,
    columns = map (filter (/= called)) columns
}

hasWon :: Board -> Bool
hasWon Board{..} = any null rows || any null columns

score :: Board -> Int -> Int
score Board{..} called = called * sum (map sum rows)

doPartA :: [Board] -> [Int] -> Int
doPartA boards (called:futureCalls) = case filter hasWon updatedBoards of
    [] -> doPartA updatedBoards futureCalls
    x:_ -> score x called
    where
        updatedBoards = map (applyNumber called) boards

partA :: Input -> OutputA
partA Input{..} = doPartA boards draw

------------ PART B ------------
doPartB :: [Board] -> [Int] -> Int
doPartB boards (called:futureCalls) = case filter (not . hasWon) updatedBoards of
    [] -> score (head updatedBoards) called
    xs -> doPartB xs futureCalls
    where
        updatedBoards = map (applyNumber called) boards

partB :: Input -> OutputB
partB Input{..} = doPartB boards draw
