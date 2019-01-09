{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.Text as T
import Options.Applicative
import qualified Data.Set as S
import System.Environment (lookupEnv)

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
                             return $ cleanSimple _path

main :: IO ()
main = runClean >>= putStrLn . T.unpack
  
-- Actual cleaning algorithm
-- Removes redundant text-matching path entries
cleanSimple :: T.Text -> T.Text
cleanSimple t = let entries = T.words . T.map (\c -> if c == ':' then ' ' else c) $ t
                in T.intercalate ":" $ S.toList $ S.fromList entries 
