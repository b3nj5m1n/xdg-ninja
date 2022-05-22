{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- I do not know haskell, this code is probably shit

module EditProgram where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Parser
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy     as B
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Text.ANSI
import           Data.UUID
import           Data.UUID.V4
import           GHC.Float                (double2Float)
import           GHC.Generics
import           Program
import           Prompts
import           System.Console.Haskeline
import           System.Environment       (getEnv)
import           System.Exit
import           System.IO
import           System.Process
import           Text.Printf              (printf)

getSupportLevel :: SupportLevel -> IO SupportLevel
getSupportLevel supportLevel = do
  changed <- promptBool (blue "Has the support level changed? (y/n) ") (red "Please provide a valid answer.") "y"
  if not changed
  then return supportLevel
  else do
      movable <- promptBool (blue "Can the file be moved? (y/n) ") (red "Please provide a valid answer.") "y"
      if movable
      then do
        envVars <- promptBool (blue "Do you have to export environment variables? (y/n) ") (red "Please provide a valid answer.") "y"
        if envVars
        then return EnvVars
        else do
          alias <- promptBool (blue "Do you have to set an alias? (y/n) ") (red "Please provide a valid answer.") "y"
          if alias
          then return Alias
          else return Supported
      else return Unsupported


getHelp :: String -> IO String
getHelp currentHelp = getInputMarkdown currentHelp

editFile :: File -> IO File
editFile f = do
  printf "%s\n" (T.unpack (italic (cyan (T.pack (path f)))))
  edit <- promptBool (green "Edit this file? (y/n) ") (red "Please provide a valid answer.") ""
  if not edit
  then return f
  else do
    supportLevel <- getSupportLevel (supportLevel f)
    help <- getHelp (help f)
    return File {path = path f, supportLevel = supportLevel, help = help}

editProgram :: String -> IO ()
editProgram filename = do
  program <- readProgram filename
  case program of
    Nothing -> printf "%s %s\n" (T.unpack (bold (red "Error parsing file"))) (T.unpack (italic (red (T.pack filename))))
    Just p -> do
      printf "%s %s\n" (T.unpack (cyan ("Editing"))) (T.unpack (italic (cyan (name p))))
      files <- return (files p)
      newFiles <- sequence (map editFile files)
      do_save <- promptBool (green "Save? (y/n) ") (red "Please provide a valid answer.") ""
      if do_save
      then save (T.pack filename) Program {name = (name p), files = newFiles}
      else return ()
