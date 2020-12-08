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

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

recurseA :: (Int, Int, Set Int) -> [Instruction] -> Int
recurseA (inst, acc, prev) ins
    | inst > length ins = acc
    | otherwise         = if inst `S.member` prev then acc else
        case ins !! inst of
            NoOp _  -> recurseA (inst + 1, acc, inst `S.insert` prev) ins
            Accum i -> recurseA (inst + 1, acc + i, inst `S.insert` prev) ins
            Jump i  -> recurseA (inst + i, acc, inst `S.insert` prev) ins

partA :: Input -> OutputA
partA = recurseA (0, 0, S.empty)


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

recurseB :: (Int, Int, Set Int) -> [Instruction] -> Maybe Int
recurseB (inst, acc, prev) ins
    | inst >= length ins = Just acc
    | inst < 0          = Nothing
    | otherwise         = if inst `S.member` prev then Nothing else
        case ins !! inst of
            NoOp _  -> recurseB (inst + 1, acc, inst `S.insert` prev) ins
            Accum i -> recurseB (inst + 1, acc + i, inst `S.insert` prev) ins
            Jump i  -> recurseB (inst + i, acc, inst `S.insert` prev) ins

partB :: Input -> OutputB
partB xs = head $ mapMaybe (recurseB (0,0,S.empty)) allPoss
    where allPoss = fst $ foldl f ([],[[]]) xs
          f (accDone, accToDo) = \case
              NoOp i -> (map (++[NoOp i]) accDone ++ map (++[Jump i]) accToDo, map (++[NoOp i]) accToDo)
              Jump i -> (map (++[Jump i]) accDone ++ map (++[NoOp i]) accToDo, map (++[Jump i]) accToDo)
              Accum i -> (map (++[Accum i]) accDone, map (++[Accum i]) accToDo)



--------------------------------------------------------------------------------
