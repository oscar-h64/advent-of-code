--------------------------------------------------------------------------------

module Days.Day12 (runDay) where

--------------------------------------------------------------------------------

import           Data.List
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
inputParser = parseOne `sepBy` endOfLine
    where parseOne = do
            ins <- letter
            num <- decimal
            pure $ case ins of
                'L' -> Rotate (360-num)
                'R' -> Rotate num
                'F' -> Forward num
                'N' -> Move North num
                'E' -> Move East num
                'S' -> Move South num
                'W' -> Move West num


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

data Direction = North | East | South | West deriving (Show, Enum)

data Instruction = Move Direction Int | Rotate Int | Forward Int deriving Show

type Input = [Instruction]

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

nextDirA :: Direction -> Direction
nextDirA North = East
nextDirA East = South
nextDirA South = West
nextDirA West = North

applyInsA :: Instruction -> (Int, Int, Direction) -> (Int, Int, Direction)
applyInsA (Move North i) (x,y,d) = (x + i, y, d)
applyInsA (Move East i) (x,y,d) = (x, y + i, d)
applyInsA (Move South i) (x,y,d) = (x - i, y, d)
applyInsA (Move West i) (x,y,d) = (x, y - i, d)
applyInsA (Rotate i) (x,y,d) = (x, y, foldr (const nextDirA) d [1..i `div` 90])
applyInsA (Forward i) (x,y,d) = applyInsA (Move d i) (x,y,d)

manhattanDist :: (Int, Int, a) -> Int
manhattanDist (x,y,_) = abs x + abs y

partA :: Input -> OutputA
partA = manhattanDist . foldl' (flip applyInsA) (0,0,East)


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

-- First 2 offset from ship, second 2 position of waypoint

nextDirB :: (Int, Int, Int, Int, Direction) -> (Int, Int, Int, Int, Direction)
nextDirB (x',y',x,y,d) = (-y', x', x-x'-y', y-y'+x', d)

applyInsB :: Instruction
          -> (Int, Int, Int, Int, Direction)
          -> (Int, Int, Int, Int, Direction)
applyInsB (Move North i) (x',y',x,y,d) = (x'+i, y', x + i, y, d)
applyInsB (Move East i) (x',y',x,y,d) = (x', y'+i, x, y + i, d)
applyInsB (Move South i) (x',y',x,y,d) = (x'-i, y', x - i, y, d)
applyInsB (Move West i) (x',y',x,y,d) = (x', y'-i, x, y - i, d)
applyInsB (Rotate i) state = foldr (const nextDirB) state [1..i `div` 90]
applyInsB (Forward i) (x',y',x,y,d) = (x', y', x+x'*i, y+y'*i, d)

manhattanDistB :: (Int, Int, Int, Int, a) -> Int
manhattanDistB (x',y',x,y,_) = abs (x-x') + abs (y-y')

partB :: Input -> OutputB
partB = manhattanDistB . foldl' (flip applyInsB) (1,10,1,10,East)


--------------------------------------------------------------------------------
