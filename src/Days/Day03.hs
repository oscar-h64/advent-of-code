--------------------------------------------------------------------------------

module Days.Day03 (runDay) where

--------------------------------------------------------------------------------

import           Data.List
import           Data.Map             ( Map )
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Set             ( Set )
import qualified Data.Set             as S
import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Vector          ( Vector )
import qualified Data.Vector          as V
import           Data.Void            ( Void )
import           Util.Parsers
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
inputParser = coordinateParser f 0
    where f '#' = pure ()
          f _   = Nothing


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

type Input = Map (Int, Int) ()

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

findTrees :: (Int, Int, Int) -> Int -> Int -> (Int, Int) -> Input -> Int
findTrees (x,y,count) xWidth yLim (xAdd, yAdd) treeMap
    | y <= yLim = case M.lookup (x,y) treeMap of
                      Just () -> findTrees ((x+xAdd) `mod` xWidth, y+yAdd, count + 1)
                                           xWidth yLim (xAdd, yAdd) treeMap
                      Nothing -> findTrees ((x+xAdd) `mod` xWidth, y+yAdd, count)
                                           xWidth yLim (xAdd, yAdd)treeMap
    | otherwise = count

partA :: Input -> OutputA
partA = findTrees (0,0,0) 31 323 (3, 1)


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

partB :: Input -> OutputB
partB inp = product $ map (flip (findTrees (0,0,0) 31 323) inp)
                          [(1,1), (3,1), (5,1), (7,1), (1,2)]


--------------------------------------------------------------------------------
