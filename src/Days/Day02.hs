--------------------------------------------------------------------------------

module Days.Day02 (runDay) where

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

import qualified Program.RunDay       as R ( runDay )
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.IO (unsafePerformIO)

--------------------------------------------------------------------------------

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

--------------------------------------------------------------------------------
--                                   PARSER                                   --
--------------------------------------------------------------------------------

inputParser :: Parser Input
inputParser = many' $ do
        min <- decimal
        string "-"
        max <- decimal
        space
        policyLetter <- letter
        string ": "
        pw <- takeTill isEndOfLine
        skipMany endOfLine
        pure $ Password min max policyLetter pw


--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------
data Password = Password {
    pwPolicyMin :: Int,
    pwPolicyMax :: Int,
    pwPolicyLetter :: Char,
    pwPassword :: Text
} deriving Show

type Input = [Password]

type OutputA = Int

type OutputB = Int


--------------------------------------------------------------------------------
--                                   PART A                                   --
--------------------------------------------------------------------------------

partA :: Input -> OutputA
partA = foldr isValidPassword 0
    where isValidPassword Password{..} n = 
            let c = T.count (T.singleton pwPolicyLetter) pwPassword
            in if c >= pwPolicyMin && c <= pwPolicyMax then n + 1 else n


--------------------------------------------------------------------------------
--                                   PART B                                   --
--------------------------------------------------------------------------------

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _     _    = False

-- Please god forgive me for the use of index
partB :: Input -> OutputB
partB = foldr isValidPassword 0
    where isValidPassword Password{..} n = 
            if xor (T.index pwPassword (pwPolicyMin - 1) == pwPolicyLetter) 
                   (T.index pwPassword (pwPolicyMax - 1) == pwPolicyLetter)
            then n + 1 else n


--------------------------------------------------------------------------------
