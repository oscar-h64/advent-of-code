--------------------------------------------------------------------------------

module Days.Day10 (runDay) where

--------------------------------------------------------------------------------

import           Data.Bifunctor       ( first )
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

--------------------------------------------------------------------------------

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

--------------------------------------------------------------------------------
--                                   PARSER                                   --
--------------------------------------------------------------------------------

inputParser :: Parser Input
inputParser = fmap ((0:) . sort) $ decimal `sepBy` endOfLine


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

type Input = [Int]

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

f :: (Int, Int, Int) -> Int -> [Int] -> (Int, Int, Int)
f xs         _    []          = xs
f (x1,x2,x3) last (next:rest) = case next - last of
    1 -> f (x1+1, x2, x3) next rest
    2 -> f (x1, x2+1, x3) next rest
    3 -> f (x1, x2, x3+1) next rest

partA :: Input -> OutputA
partA = (\(x1,_,x3) -> (1+x3)*x1) . uncurry (f (0,0,0)) . first head . splitAt 1


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

-- CHAINS ARE CONSTRUCTED IN REVERSE - LARGEST AT FRONT

-- O(HEAD-DEATH-OF-UNIVERSE):

-- findWith :: Int -> Int -> [Int] -> Maybe (Int, [Int])
-- findWith _    _   []     = Nothing
-- findWith last gap (x:xs) =
--     if | x - last == gap -> Just (x,xs)
--        | x - last > gap -> Nothing
--        | otherwise -> findWith last gap xs

-- applyFindWith :: [Int] -> (Int, [Int]) -> ([Int], [Int])
-- applyFindWith chain (x, xs) = (x:chain, xs)

-- g :: ([Int], [Int]) -> [([Int], [Int])]
-- g (chain, []) = pure (chain, [])
-- g (chain, remaining) = let last = head chain
--                        in map (applyFindWith chain) $ catMaybes [findWith last 1 remaining, findWith last 2 remaining, findWith last 3 remaining]

-- h :: [([Int], [Int])] -> [([Int], [Int])]
-- h chains = if all (null . snd) chains then chains else h $ concatMap g chains

-- partB :: Input -> OutputB
-- partB input = length $ filter ((== maximum input) . head) $ map fst $ h [([0], input)]

g :: Map Int Int -> Int -> Map Int Int
g possMap toDo = M.adjust (const $ sum $ map h [1,2,3]) toDo possMap
    where h i = traceShowId (fromMaybe 0 $ possMap M.!? (toDo - i))

partB :: Input -> OutputB
partB input = let possMap = M.fromList $ zip input (1:repeat 0)
              in traceShowId (foldl g possMap $ tail input) M.! last input 

--------------------------------------------------------------------------------
