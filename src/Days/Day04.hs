--------------------------------------------------------------------------------

module Days.Day04 (runDay) where

--------------------------------------------------------------------------------

import           Data.Char            ( isDigit, isSpace )
import           Data.List
import           Data.Map             ( Map )
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Set             ( Set )
import qualified Data.Set             as S
import           Data.Text              ( Text )
import qualified Data.Text              as T
import           Data.Vector          ( Vector )
import qualified Data.Vector          as V
import           Data.Void            ( Void )
import qualified Util.Util            as U

import           Data.Attoparsec.Text as P

import           Text.Read            ( readMaybe )

import qualified Program.RunDay       as R ( runDay )

--------------------------------------------------------------------------------

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

--------------------------------------------------------------------------------
--                                   PARSER                                   --
--------------------------------------------------------------------------------

inputParser :: Parser Input
inputParser = fmap catMaybes $ flip sepBy endOfLine $ do
    xs <- many' $ do 
        x <- P.take 3
        char ':'
        value <- T.pack <$> many' (satisfy $ not . isSpace)
        space
        pure (x, value)

    pure $ Passport <$> (lookup "byr" xs >>= readMaybe . T.unpack)
                    <*> (lookup "iyr" xs >>= readMaybe . T.unpack)
                    <*> (lookup "eyr" xs >>= readMaybe . T.unpack)
                    <*> lookup "hgt" xs
                    <*> lookup "hcl" xs
                    <*> lookup "ecl" xs
                    <*> lookup "pid" xs
                    <*> pure (lookup "cid" xs)


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------
data Passport = Passport {
    ppByr :: Int,
    ppIyr :: Int,
    ppEyr :: Int,
    ppHgt :: Text,
    ppHcl :: Text,
    ppEcl :: Text,
    ppPid :: Text,
    ppCid :: Maybe Text
} deriving Show

type Input = [Passport]

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

partA :: Input -> OutputA
partA = length


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------
allEcl :: [Text]
allEcl = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidHeight :: Text -> Bool
isValidHeight x =
    let (num, unit) = T.splitAt (T.length x - 2) x
    in case readMaybe (T.unpack num) of
        Nothing -> False
        Just i -> case unit of
            "in" -> i >= 59 && i <= 76
            "cm" -> i >= 150 && i <= 193
            _ -> False 

filterFunc :: Passport -> Bool
filterFunc Passport{..} =
    ppByr >= 1920 && ppByr <= 2002 &&
    ppIyr >= 2010 && ppIyr <= 2020 &&
    ppEyr >= 2020 && ppEyr <= 2030 &&
    isValidHeight ppHgt &&
    T.head ppHcl == '#' && T.all (`elem` ['0'..'9']++['a'..'f']) (T.tail ppHcl) &&
    ppEcl `elem` allEcl &&
    T.length ppPid == 9 && T.all isDigit ppPid



partB :: Input -> OutputB
partB = length . filter filterFunc


--------------------------------------------------------------------------------
