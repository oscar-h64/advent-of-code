--------------------------------------------------------------------------------

module Days.Day05 (runDay) where

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
inputParser = parseSeat `sepBy` endOfLine
    where
        parseSeat = do
            let foldFunc (l, r) (min, max) next =
                    let newBound = min + ((max - min) `div` 2) in
                    if | next == l -> (min, newBound)
                       | next == r -> (newBound + 1, max)

            row' <- count 7 letter
            let row = snd $ foldl (foldFunc ('F', 'B')) (0, 127) row'

            col' <- count 3 letter
            let col = snd $ foldl (foldFunc ('L', 'R')) (0, 7) col'

            pure $ 8 * row + col


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

type Input = [Int]

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

partA :: Input -> OutputA
partA = maximum


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

partB :: Input -> OutputB
partB xs = head
         $ filter (\x -> x+1 `elem` xs && x-1 `elem` xs)
         $ [1..1000] \\ xs


--------------------------------------------------------------------------------
