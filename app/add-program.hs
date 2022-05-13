{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- I do not know haskell, this code is probably shit

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import Data.List.Extra
import Data.Maybe
import qualified Data.Text as T
import Data.Text.ANSI
import Data.UUID
import Data.UUID.V4
import GHC.Float (double2Float)
import GHC.Generics
import System.Console.Haskeline
import System.Exit
import System.IO
import System.Process
import Text.Printf (printf)

data File = File
  { path :: String,
    movable :: Bool,
    help :: String
  }
  deriving (Generic, Show)

instance ToJSON File where
  toEncoding = genericToEncoding defaultOptions

data Program = Program
  { name :: T.Text,
    files :: [File]
  }
  deriving (Generic, Show)

instance ToJSON Program where
  toEncoding = genericToEncoding defaultOptions

save :: Program -> IO ()
save program = do
  let path = ( "./programs/" ++ (T.unpack (name program) ) ++ ".json")
  B.writeFile path (encodePretty program)

getHelp :: IO String
getHelp =
  toString <$> Data.UUID.V4.nextRandom >>= \id ->
    appendFile ( "/tmp/xdg-ninja." ++ id ++ ".md" ) "Export the following environment variables:\n\n```bash\n\n```" >>
    createProcess (shell ("nvim /tmp/xdg-ninja." ++ id ++ ".md")) >>= \r ->
      case r of
        (_, _, _, p) ->
          waitForProcess p
            >>= ( \f -> case f of
                    ExitSuccess -> readFile ("/tmp/xdg-ninja." ++ id ++ ".md")
                    (ExitFailure a) -> return ""
                )

getProp :: T.Text -> T.Text -> IO String
getProp prompt placeholder = do
  let string_prompt = T.unpack prompt
  let string_placholder = T.unpack placeholder
  let fuck = runInputT defaultSettings (getInputLineWithInitial string_prompt (string_placholder, ""))
  fuck
    >>= ( \x -> case x of
            (Just s) -> return s
            Nothing -> return ""
        )

data Answer = Yes | No | Unknown

stringToBool :: String -> Answer
stringToBool s = case lower s of
  "yes" -> Yes
  "y" -> Yes
  "1" -> Yes
  "no" -> No
  "n" -> No
  "0" -> No
  _ -> Unknown

promptBool :: T.Text -> T.Text -> T.Text -> IO Bool
promptBool prompt prompt_unrecognised placeholder =
  getProp prompt placeholder >>= \x -> case stringToBool x of
    Yes -> return True
    No -> return False
    Unknown -> printf "%s\n" prompt_unrecognised >> promptBool prompt prompt_unrecognised placeholder

getFile :: IO File
getFile =
  getProp (blue "Path to file: ") "$HOME/."
    >>= \path ->
      promptBool (blue "Can the file be moved? (y/n) ") (red "Please provide a valid answer.") "y"
        >>= \movable ->
          getHelp
            >>= \help -> return File {path = path, movable = movable, help = help}

getFiles :: [File] -> IO [File]
getFiles files =
  if Data.List.Extra.null files
    then getFile >>= \newFile -> getFiles (newFile : files)
    else
      promptBool (green "Add another file? (y/n) ") (red "Please provide a valid answer.") "" >>= \new ->
        if new
          then getFile >>= \newFile -> getFiles (newFile : files)
          else return files

getProgram :: IO Program
getProgram =
  printf "%s\n" (T.unpack (bold (cyan "XDG-ninja Configuration Wizard")))
    >> printf "%s\n" (T.unpack (faint (italic (cyan "First, tell me what program you're creating a configuration for."))))
    >> getProp (yellow "Program name: ") ""
    >>= \name ->
      printf "%s\n" (T.unpack (faint (italic (cyan "Alright, now let's configure which files belong to this program.")))) >>
      printf "%s\n" (T.unpack (faint (italic (cyan "I'm going to ask you for the path to the file, please use $HOME instead of ~.")))) >>
      printf "%s\n" (T.unpack (faint (italic (cyan "I'll then ask you wether or not this file can be moved to a different directory.")))) >>
      printf "%s\n" (T.unpack (faint (italic (cyan "Finally, your editor is going to open a markdown document. Enter instructions on moving the file in question, then save and close.")))) >>
      getFiles [] >>= \files ->
        return Program {name = T.pack name, files = files}

main :: IO ()
main =
  getProgram
    >>= save
