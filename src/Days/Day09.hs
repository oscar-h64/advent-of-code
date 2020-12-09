--------------------------------------------------------------------------------

module Days.Day09 (runDay) where

--------------------------------------------------------------------------------

import           Control.Arrow

import           Data.List            as L
import           Data.Map             ( Map )
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Set             ( Set )
import qualified Data.Set             as S
import           Data.Vector          ( Vector )
import qualified Data.Vector          as V
import           Data.Void            ( Void )
import qualified Util.Util            as U

import           Data.Attoparsec.Text

import qualified Program.RunDay       as R ( runDay )

--------------------------------------------------------------------------------

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

--------------------------------------------------------------------------------
--                                   PARSER                                   --
--------------------------------------------------------------------------------

inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

type Input = [Int]

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

isSumOfPair :: Int -> [Int] -> Bool
isSumOfPair x xs = not $ null [() | (y:ys) <- tails xs, z <- ys, y + z == x]

getFirstInvalid :: [Int] -> [Int] -> Int
getFirstInvalid xs (y:ys) = if isSumOfPair y xs
                            then getFirstInvalid (tail xs ++ [y]) ys
                            else y

partA :: Input -> OutputA
partA = uncurry getFirstInvalid . splitAt 25


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

partB :: Input -> OutputB
partB = uncurry (+)
      . (minimum &&& maximum)
      . head
      . filter ((==167829540) . sum)
      . concatMap inits
      . tails
      . L.takeWhile (/= 167829540)


--------------------------------------------------------------------------------
