module Days.Day21 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.List
import           Data.Map             ( Map )
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Set             ( Set )
import qualified Data.Set             as S
import           Data.Vector          ( Vector )
import qualified Data.Vector          as V
import qualified Util.Util            as U

import           Data.Attoparsec.Text
import           Data.Void
import qualified Program.RunDay       as R ( runDay )
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
