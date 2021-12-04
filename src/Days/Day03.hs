module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative

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
inputParser = many' (many' ("1" *> pure True <|> "0" *> pure False) <* endOfLine)

------------ TYPES ------------
type Input = [[Bool]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
mostCommon :: Int -> Int -> [Bool] -> Bool
mostCommon numTrue numTotal []     = 2 * numTrue >= numTotal
mostCommon numTrue numTotal (x:xs) = mostCommon (numTrue + fromEnum x) (numTotal + 1) xs

convert :: (Bool -> Bool) -> [Bool] -> Int
convert modifier bits = sum $ zipWith (*) (map (2^) [0..]) $ map (fromEnum . modifier) bits

partA :: Input -> OutputA
partA input = let bits = reverse $ map (mostCommon 0 0) $ transpose input
              in convert id bits * convert not bits

------------ PART B ------------
findMatch :: (Bool -> Bool) -> [[Bool]] -> [Bool] -> OutputB
findMatch modifier inputs prefix = case filteredList of
    []   -> error "No numbers matched"
    [x]  -> convert id (reverse x ++ prefix)
    xs   -> findMatch modifier (map tail xs) (lookingFor:prefix)
    where
        lookingFor = modifier $ mostCommon 0 0 $ map head inputs
        filteredList = filter filterFunc inputs
        filterFunc [] = False
        filterFunc (x:xs) = x == lookingFor

partB :: Input -> OutputB
partB input = findMatch id input [] * findMatch not input []
