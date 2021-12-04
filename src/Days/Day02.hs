module Days.Day02 (runDay) where

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
inputParser = many' (insParse Forward "forward" <|> insParse Down "down" <|> insParse Up "up")
    where insParse f s = fmap f $ s *> skipSpace *> decimal <* skipSpace

------------ TYPES ------------
data Instruction = Forward Int | Down Int | Up Int
    deriving Show

type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
f :: Instruction -> (Int, Int) -> (Int, Int)
f (Forward x') (x, d) = (x + x', d)
f (Down d')    (x, d) = (x, d + d')
f (Up d')      (x, d) = (x, d - d')

partA :: Input -> OutputA
partA = uncurry (*) . foldr f (0,0) 

------------ PART B ------------
g :: Instruction -> (Int, Int, Int) -> (Int, Int, Int)
g (Forward x') (x, d, a) = (x + x', d + (x' * a), a)
g (Down a')    (x, d, a) = (x, d, a + a')
g (Up a')      (x, d, a) = (x, d, a - a')

partB :: Input -> OutputB
partB = uncurry (*) . (\(x,y,z) -> (x,y)) . foldl (flip g) (0,0,0)
