--------------------------------------------------------------------------------

module Days.Day08 (runDay) where

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

import           Control.Applicative  ( Alternative ((<|>)) )
import           Data.Functor         ( ($>) )
import qualified Program.RunDay       as R ( runDay )

--------------------------------------------------------------------------------

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

--------------------------------------------------------------------------------
--                                   PARSER                                   --
--------------------------------------------------------------------------------

inputParser :: Parser Input
inputParser = parseInstruction `sepBy` endOfLine
    where parseInstruction = do
            inst <- string "nop" $> NoOp
                <|> string "acc" $> Accum
                <|> string "jmp" $> Jump
            space
            mult <- char '+' $> 1 <|> char '-' $> -1
            inst . (* mult) <$> decimal


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------
data Instruction = NoOp Int | Accum Int | Jump Int deriving Show

type Input = [Instruction]

type OutputA = Int

type OutputB = Void


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

recurse :: (Int, Int, Set Int) -> [Instruction] -> Int
recurse (_,    acc, _   ) []  = acc
recurse (inst, acc, prev) ins = if inst `S.member` prev then acc else
    case ins !! inst of
        NoOp _  -> recurse (inst + 1, acc, inst `S.insert` prev) ins
        Accum i -> recurse (inst + 1, acc + i, inst `S.insert` prev) ins
        Jump i  -> recurse (inst + i, acc, inst `S.insert` prev) ins

partA :: Input -> OutputA
partA = recurse (0, 0, S.empty)


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"


--------------------------------------------------------------------------------
