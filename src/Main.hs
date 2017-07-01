module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Vector as Vec
import Options.Applicative
import System.Random (newStdGen, randoms, next)
import System.Exit (exitFailure)

import Wordlists


data Arguments = Arguments
  { argsShort :: Bool
  , argsSpecial :: Bool
  , argsCount :: Int
  , argsDelimiter :: String
  , argsAcrostic :: String
  }

args :: Parser Arguments
args = Arguments
      <$> switch (mconcat
          [ long "short"
          , help "Use the EFF list of short words"
          ])
      <*> switch (mconcat
          [ long "special"
          , help "Use the EFF list of special words"
          ])
      <*> option auto (mconcat
          [ long "count"
          , help "The number of passwords"
          , showDefault
          , value 1
          , metavar "INT"
          ])
      <*> strOption (mconcat
          [ long "delimiter"
          , help "The delimiter to use"
          , showDefault
          , value " "
          ])
      <*> strOption (mconcat
          [ long "acrostic"
          , help "The first letters of the words form this word."
          , showDefault
          , value ""
          ])

opts :: ParserInfo Arguments
opts = info (args <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Print a wordlist-based password"
  , header "gen-passwd - a password generator"
  ]


-- | Basically `unsafeIndex (concat vec)`, only faster
concatIndex :: Vec.Vector (Vec.Vector a) -> Int -> a
concatIndex vec n = if n > firstElemLen
    then concatIndex (Vec.tail vec) (n - firstElemLen)
    else Vec.unsafeIndex (Vec.head vec) n
  where
    firstElemLen = Vec.length (Vec.head vec)


normalPassword :: Int -> Wordlist -> BS.ByteString -> IO ()
normalPassword len wordlist delim = do
  gen <- newStdGen
  let ws = map (concatIndex wordlist . flip mod len) $ take 6 $ randoms gen
  BS.putStrLn $ BS.intercalate delim ws

acrosticPassword :: Int -> Wordlist -> BS.ByteString -> String -> IO ()
acrosticPassword len wordlist delim acrostic = do
  gen <- newStdGen
  ws <- go gen acrostic
  BS.putStrLn $ BS.intercalate delim ws
  where
    go _ [] = pure []
    go gen (x:xs) = do
      let (rand, nextgen) = next gen
      let letter = (subtract (ord 'a')) . ord . toLower $ x
      when (letter > 25 || letter < 0) $ do
        putStrLn $ "The character '" ++ x:[] ++ "' is not part of the wordlist."
        exitFailure
      let list = Vec.unsafeIndex wordlist letter
      let word = Vec.unsafeIndex list (rand `mod` Vec.length list)
      rest <- go nextgen xs
      pure (word : rest)


main :: IO ()
main = do
  args <- execParser opts
  (len, wordlist) <- case (argsShort args, argsSpecial args) of
        (False, False) -> pure (7776, longWords)
        (True , False) -> pure (1296, shortWords)
        (False, True ) -> pure (1296, specialWords)
        (True , True ) -> do
          putStrLn "Sorry, but you can't use the short and special list at the same time."
          exitFailure
  replicateM_ (argsCount args) $ case argsAcrostic args of
    ""       -> normalPassword len wordlist (BS.pack $ argsDelimiter args)
    acrostic -> acrosticPassword len wordlist (BS.pack $ argsDelimiter args) acrostic
