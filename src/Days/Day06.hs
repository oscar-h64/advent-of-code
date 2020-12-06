--------------------------------------------------------------------------------

module Days.Day06 (runDay) where

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
inputParser = fmap unzip $ many' $ do
    groups <- fmap (map S.fromList) $ many1' $ many1' letter <* endOfLine
    skipSpace
    pure (S.unions groups, foldr1 S.intersection groups)


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

type Input = ([Set Char], [Set Char])

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

partA :: Input -> OutputA
partA = sum . map S.size . fst


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

partB :: Input -> OutputB
partB = sum . map S.size . snd


--------------------------------------------------------------------------------
