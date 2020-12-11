--------------------------------------------------------------------------------

module Days.Day11 (runDay) where

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

import           Debug.Trace          ( traceShowId )
import qualified Program.RunDay       as R ( runDay )
import           Util.Parsers         ( coordinateParser )

--------------------------------------------------------------------------------

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

--------------------------------------------------------------------------------
--                                   PARSER                                   --
--------------------------------------------------------------------------------

inputParser :: Parser Input
inputParser = coordinateParser (\x -> if x == 'L' then Just Empty else Nothing) 0


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

data SeatState = Occupied | Empty deriving (Eq, Show)

data Mode = PartA | PartB deriving Eq

type Input = Map (Int, Int) SeatState

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   SHARED                                   --
--------------------------------------------------------------------------------

processSeat :: Input -> Int -> Mode -> (Int, Int) -> SeatState -> Input -> Input
processSeat oldMap maxOccupied mode (x,y) currState = case currState of
    Empty -> if Just Occupied `notElem` getAdjacent
             then M.insert (x,y) Occupied
             else M.insert (x,y) Empty
    Occupied -> if length (filter (== Just Occupied) getAdjacent) >= maxOccupied
                then M.insert (x,y) Empty
                else M.insert (x,y) Occupied

    where getAdjacentA = map (oldMap M.!?) [ (x+1, y  ), (x-1, y  ), (x,  y+1)
                                           , (x,   y-1), (x+1, y+1), (x-1,y-1)
                                           , (x-1, y+1), (x+1, y-1)
                                           ]
          iterateTillSeat (x,y) f
            | x >= 0 && y >= 0 && x <= 100 && y <= 100 =
                case oldMap M.!? f (x,y) of
                    Nothing -> iterateTillSeat (f (x,y)) f
                    Just x  -> Just x
            | otherwise        = Nothing
          getAdjacentB = map (iterateTillSeat (x,y)) [ \(x',y') -> (x'+1, y'  )
                                                     , \(x',y') -> (x'-1, y'  )
                                                     , \(x',y') -> (x'  , y'+1)
                                                     , \(x',y') -> (x'  , y'-1)
                                                     , \(x',y') -> (x'+1, y'+1)
                                                     , \(x',y') -> (x'-1, y'-1)
                                                     , \(x',y') -> (x'-1, y'+1)
                                                     , \(x',y') -> (x'+1, y'-1)
                                                     ]
          getAdjacent = if mode == PartA then getAdjacentA else getAdjacentB

constructNewMap :: Int -> Mode -> Input -> Input
constructNewMap i mode input = M.foldrWithKey (processSeat input i mode)
                                              M.empty
                                              input

iterTillStable :: Int -> Mode -> Input -> Input
iterTillStable i mode lastIter =
    let newIter = constructNewMap i mode lastIter
    in if lastIter == newIter then newIter else iterTillStable i mode newIter


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

partA :: Input -> OutputA
partA = M.size . M.filter (== Occupied) . iterTillStable 4 PartA


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

partB :: Input -> OutputB
partB = M.size . M.filter (== Occupied) . iterTillStable 5 PartB


--------------------------------------------------------------------------------
