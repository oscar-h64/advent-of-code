module Days.Day01 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' (decimal <* skipSpace)

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (x:xs) = snd $ foldl (\(last, acc) next -> (next, acc + fromEnum (next > last))) (x, 0) xs 

------------ PART B ------------
partBF :: Input -> Int -> OutputB
partBF [_,_] _ = 0
partBF xs last = let next = sum (take 3 xs)
                 in fromEnum (next > last) + partBF (tail xs) next

partB :: Input -> OutputB
partB xs = partBF (tail xs) (sum $ take 3 xs)
