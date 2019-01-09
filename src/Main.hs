{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.Text as T
import Options.Applicative
import qualified Data.Set as S
import Control.Monad (filterM)
import System.Environment (lookupEnv)
import System.Directory (doesPathExist)

-- CLI Stuff
data ProgSpec =
  Spec { _verbose :: Bool
       , _path :: T.Text }

-- Typical program would run as `clean-path [--input][-i] [PATH]`
-- It always outputs to the command line
cliParser :: T.Text -> Parser ProgSpec
cliParser t = Spec
              <$> switch (short 'v' <> long "verbose" <> help "Verbosity")
              <*> strOption
              (short 'i'
               <> long "input"
               <> metavar "PATH"
               <> value t
               <> help "Text for path to clean")

cliInfo :: InfoMod ProgSpec
cliInfo = briefDesc <> progDesc "Cleans up messy PATH system variables. Use wisely"

-- Running the program
runClean :: IO T.Text
runClean = do path <- lookupEnv "PATH"
              case T.pack <$> path of
                Nothing -> return "PATH environment variable not found."
                Just p -> do let cli = info (cliParser p) cliInfo
                             Spec{..} <- execParser cli
                             if _verbose then putStrLn ("Path to clean: " <> T.unpack _path)
                               else return ()
                             clean _path

main :: IO ()
main = runClean >>= putStrLn . T.unpack
  
-- Actual cleaning algorithm
clean :: T.Text -> IO T.Text
clean t = let entries = T.words . T.map (\c -> if c == ':' then ' ' else c) $ t
          in (cleanPhantom $ cleanText entries) >>= return . T.intercalate ":"

-- Remove textual redundancies
cleanText :: [T.Text] -> [T.Text]
cleanText = S.toList . S.fromList 

-- Removes non-existent entries
cleanPhantom :: [T.Text] -> IO [T.Text]
cleanPhantom = filterM (doesPathExist . T.unpack)
