--------------------------------------------------------------------------------

module Days.Day12 (runDay) where

--------------------------------------------------------------------------------

import           Control.Monad.State
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
--                                   SHARED                                   --
--------------------------------------------------------------------------------

class Day12State v where
    nextDir :: State v ()

    addOffset :: (Int, Int) -> State v ()

    goForward :: Int -> State v ()

    manhattanDist :: State v Int

getManDist :: Day12State v => v -> [Instruction] -> Int
getManDist init inp = evalState (mapM_ applyIns inp >> manhattanDist) init

applyIns :: Day12State v => Instruction -> State v ()
applyIns (Move North i) = addOffset ( i,  0)
applyIns (Move East  i) = addOffset ( 0,  i)
applyIns (Move South i) = addOffset (-i,  0)
applyIns (Move West  i) = addOffset ( 0, -i)
applyIns (Rotate i)     = replicateM_ (i `div` 90) nextDir
applyIns (Forward i)    = goForward i


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

instance Day12State (Int, Int, Direction) where
    nextDir = modify $ \(x,y,d) -> case d of
        North -> (x, y, East )
        East  -> (x, y, South)
        South -> (x, y, West )
        West  -> (x, y, North)

    addOffset (xi,yi) = modify $ \(x, y, d) -> (x + xi, y + yi, d)

    goForward i = get >>= \(_,_,d) -> applyIns (Move d i)

    manhattanDist = get >>= \(x,y,_) -> pure $ abs x + abs y

startStateA :: (Int, Int, Direction)
startStateA = (0,0,East)

partA :: Input -> OutputA
partA = getManDist startStateA


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

-- First 2 offset from ship, second 2 position of waypoint
instance Day12State (Int, Int, Int, Int, Direction) where
    nextDir = modify $ \(x',y',x,y,d) -> (-y', x', x-x'-y', y-y'+x', d)

    addOffset (xi,yi) = modify $ \(x',y',x,y,d) -> (x'+xi, y'+yi, x + xi, y+yi, d)

    goForward i = modify $ \(x',y',x,y,d) -> (x', y', x+x'*i, y+y'*i, d)

    manhattanDist = get >>= \(x',y',x,y,_) -> pure $ abs (x-x') + abs (y-y')

startStateB :: (Int, Int, Int, Int, Direction)
startStateB = (1,10,1,10,East)

partB :: Input -> OutputB
partB = getManDist startStateB


--------------------------------------------------------------------------------
