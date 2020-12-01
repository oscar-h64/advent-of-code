--------------------------------------------------------------------------------

module Days.Day01 (runDay) where

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
inputParser =
    many' $ do
        x <- decimal
        space
        pure x


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
partA xs = head [x*y | (x:ys) <- tails xs, y <- ys, x+y == 2020]


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

partB :: Input -> OutputB
partB xs = head [x*y*z | (x:ys) <- tails xs
                       , (y:zs) <- tails ys
                       , z <- zs
                       , x+y+z == 2020
                       ]


--------------------------------------------------------------------------------
