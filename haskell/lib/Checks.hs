{-# LANGUAGE OverloadedStrings #-}

module Checks where

import           Data.List              (isSuffixOf)
import qualified Data.Text              as T
import           Data.Text.ANSI
import qualified Data.Text.Lazy         as TL
import           Output
import           Program
import           System.Directory.Extra
import           System.Environment     (getEnv)
import           System.FilePath
import           Text.Replace

data Result = Exist | MustExist | NoExist
  deriving Show

expandPath :: String -> IO String
expandPath path = do
  home <- getEnv "HOME"
  let replacements = [ Replace "$HOME" (T.pack home) ]
  let result = replaceWithList replacements (TL.pack path)
  return (TL.unpack result)


checkFile :: T.Text -> Bool -> File -> IO Result
checkFile programName verbose file  = do
  path <- expandPath (path file)
  existsFile <- doesFileExist path
  existsDir <- doesDirectoryExist path
  case (existsFile || existsDir) of
    False -> case verbose of
      False -> return NoExist
      True -> do
        logFile programName file False
        return NoExist
    True -> do
      logFile programName file True
      case (supportLevel file) of
        Unsupported -> return MustExist
        _           -> return Exist

checkProgram :: Bool -> Program -> IO [Result]
checkProgram verbose program = sequence (map (checkFile (name program) verbose) (files program))

checkProgramFile :: String -> IO [Result]
checkProgramFile filename = do
  x <- readProgram filename
  case x of
    Just program -> do
      results <- checkProgram False program
      return results
    Nothing -> do
      putStrLn (T.unpack (red (T.pack "Error.")))
      return []

checkDir :: FilePath -> IO ()
checkDir dirname = do
  files <- getDirectoryContents dirname
  jsonFiles <- return ( map (\x -> dirname </> x) (filter (isSuffixOf ".json") files ))
  results <- sequence (map checkProgramFile jsonFiles)
  return ()
