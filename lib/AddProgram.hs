{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- I do not know haskell, this code is probably shit

module AddProgram where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
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

getTemplate :: SupportLevel -> String
getTemplate Unsupported = "Currently unsupported.\n\n_Relevant issue:_ https://github.com/user/repo/issues/nr\n"
getTemplate EnvVars = "Export the following environment variables:\n\n```bash\n\n```"
getTemplate Alias = "Alias PROGRAM to use a custom configuration location:\n\n```bash\nalias PROGRAM=PROGRAM --config \"$XDG_CONFIG_HOME\"/PROGRAM/config\n```\n"
getTemplate Supported = "Supported since _VERSION_.\n\nYou can move the file to _XDG_CONFIG_HOME/PROGRAM/CONFIG.\n"

getHelp :: SupportLevel -> IO String
getHelp supportLevel = getInputMarkdown (getTemplate supportLevel)

getSupportLevel :: IO SupportLevel
getSupportLevel = do
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

getFile :: IO File
getFile = do
  path <- getProp (blue "Path to file: ") "$HOME/."
  supportLevel <- getSupportLevel
  help <- getHelp supportLevel
  return File {path = path, supportLevel = supportLevel, help = help}

getFiles :: [File] -> IO [File]
getFiles files =
  if Data.List.Extra.null files
  then do
    newFile <- getFile
    getFiles (newFile : files)
  else do
    new <- promptBool (green "Add another file? (y/n) ") (red "Please provide a valid answer.") ""
    if new
    then do
      newFile <- getFile
      getFiles (newFile : files)
    else return files

getProgram :: IO Program
getProgram = do
  name <- printf "%s\n" (T.unpack (bold (cyan "XDG-ninja Configuration Wizard")))
          >> printf "%s\n" (T.unpack (faint (italic (cyan "First, tell me what program you're creating a configuration for."))))
          >> getProp (yellow "Program name: ") ""
  files <- printf "%s\n" (T.unpack (faint (italic (cyan "Alright, now let's configure which files belong to this program."))))
           >> printf "%s\n" (T.unpack (faint (italic (cyan "I'm going to ask you for the path to the file, please use $HOME instead of ~."))))
           >> printf "%s\n" (T.unpack (faint (italic (cyan "I'll then ask you wether or not this file can be moved to a different directory."))))
           >> printf "%s\n" (T.unpack (faint (italic (cyan "Finally, your editor is going to open a markdown document. Enter instructions on moving the file in question, then save and close."))))
           >> getFiles []
  return Program {name = T.pack name, files = files}

saveProgram :: IO ()
saveProgram = do
  program <- getProgram
  do_save <- promptBool (green "Save? (y/n) ") (red "Please provide a valid answer.") ""
  if do_save
  then save program
  else return ()
