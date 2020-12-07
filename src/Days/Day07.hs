--------------------------------------------------------------------------------

module Days.Day07 (runDay) where

--------------------------------------------------------------------------------

import           Data.List
import           Data.Map             ( Map )
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Set             ( Set )
import qualified Data.Set             as S
import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Vector          ( Vector )
import qualified Data.Vector          as V
import           Data.Void            ( Void )
import qualified Util.Util            as U

import           Data.Attoparsec.Text

import           Control.Applicative
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
            let readColour = fmap T.pack $ do
                    x <- many1' letter
                    space
                    y <- many1' letter
                    pure $ mconcat [x, " ", y]
            colour <- readColour
            space
            string "bags contain"
            space
            let
            inside <- (string "no other bags." >> pure []) <|> many1' (do
                number <- decimal
                space
                colour' <- readColour
                space
                string "bag, " <|> string "bags, "
                               <|> string "bags."
                               <|> string "bag."
                pure (number, colour'))
            pure $ BagRule colour $ S.fromList inside


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------
data BagRule = BagRule {
    brColour :: Text,
    brInside :: Set (Int, Text)
} deriving (Eq, Show)

type Input = [BagRule]

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

canHoldColour :: Text -> BagRule -> Bool
canHoldColour x = any ((x==) . snd) . S.elems . brInside

recursiveContains :: Text -> [BagRule] -> [BagRule]
recursiveContains _ []    = []
recursiveContains x rules = let direct = filter (canHoldColour x) rules
                                remaining = rules \\ direct
                            in direct ++ concatMap (flip recursiveContains remaining . brColour) direct

partA :: Input -> OutputA
partA = S.size . S.fromList . map brColour . recursiveContains "shiny gold"


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

recursiveCount :: Text -> [BagRule] -> Int
recursiveCount x xs = let toCheck = brInside $ head $ filter ((x ==) . brColour) xs
                      in sum $ map (\(i,y) -> i + i * recursiveCount y xs) $ S.toList toCheck

partB :: Input -> OutputB
partB = recursiveCount "shiny gold"


--------------------------------------------------------------------------------
