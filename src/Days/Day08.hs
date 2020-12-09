--------------------------------------------------------------------------------

module Days.Day08 (runDay) where

--------------------------------------------------------------------------------

import           Control.Applicative  ( Alternative ((<|>)) )
import           Control.Monad.State

import           Data.Either          ( fromLeft, rights )
import           Data.Functor         ( ($>) )
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
inputParser = fmap V.fromList $ parseInstruction `sepBy` endOfLine
    where parseInstruction = do
            inst <- string "nop" $> NoOp
                <|> string "acc" $> Accum
                <|> string "jmp" $> Jump
            space
            inst <$> signed decimal


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

data Instruction = NoOp Int | Accum Int | Jump Int deriving Show

type Input = Vector Instruction

type OutputA = Int

type OutputB = Int

type ComputerM = State (Int, Int, Set Int, Vector Instruction)

--------------------------------------------------------------------------------
--                                   SHARED                                   --
--------------------------------------------------------------------------------

recurse :: ComputerM (Either Int Int)
recurse = get >>= \(inst, acc, prev, ins) ->
    if | inst >= V.length ins -> pure $ Right acc
       | inst < 0 -> pure $ Left acc
       | otherwise -> if inst `S.member` prev then pure $ Left acc else
            case ins V.! inst of
                NoOp _  -> addLoc >> updateIns 1 >> recurse
                Accum i -> addLoc >> updateIns 1 >> updateAcc i >> recurse
                Jump i  -> addLoc >> updateIns i >> recurse


updateIns :: Int -> ComputerM ()
updateIns i = modify (\(w,x,y,z) -> (w+i, x, y, z))

updateAcc :: Int -> ComputerM ()
updateAcc i = modify (\(w,x,y,z) -> (w, x+i, y, z))

addLoc :: ComputerM ()
addLoc = modify (\(w,x,y,z) -> (w, x, w `S.insert` y, z))


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

partA :: Input -> OutputA
partA xs = fromLeft 0 $ evalState recurse (0, 0, S.empty, xs)


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

partB :: Input -> OutputB
partB xs = head $ rights
                $ map (\x -> evalState recurse (0, 0, S.empty, x)) allPoss
    where allPoss = fst $ foldl f ([],[V.empty]) xs
          addIns ins = map (`V.snoc` ins)
          f (accDone, accToDo) = \case
              NoOp i -> ( addIns (NoOp i) accDone ++ addIns (Jump i) accToDo
                        , addIns (NoOp i) accToDo
                        )
              Jump i -> ( addIns (Jump i) accDone ++ addIns (NoOp i) accToDo
                        , addIns (Jump i) accToDo
                        )
              Accum i -> (addIns (Accum i) accDone, addIns (Accum i) accToDo)


--------------------------------------------------------------------------------
