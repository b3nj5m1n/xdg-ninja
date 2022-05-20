module Prompts where

import           Data.List.Extra
import qualified Data.Text                as T
import           Data.UUID
import           Data.UUID.V4
import           Program
import           System.Console.Haskeline
import           System.Environment       (getEnv)
import           System.Exit
import           System.Process
import           Text.Printf              (printf)

getInputMarkdown :: String -> IO String
getInputMarkdown placeholder = do
  id <- toString <$> Data.UUID.V4.nextRandom
  editor <- appendFile ("/tmp/xdg-ninja." ++ id ++ ".md") placeholder >> (getEnv "EDITOR")
  (_, _, _, p) <- createProcess (shell (editor ++ " /tmp/xdg-ninja." ++ id ++ ".md"))
  f <- waitForProcess p
  case f of
    ExitSuccess   -> readFile ("/tmp/xdg-ninja." ++ id ++ ".md")
    ExitFailure a -> return ""

getProp :: T.Text -> T.Text -> IO String
getProp prompt placeholder = do
  let string_prompt = T.unpack prompt
  let string_placholder = T.unpack placeholder
  x <- runInputT defaultSettings (getInputLineWithInitial string_prompt (string_placholder, ""))
  case x of
    Just s  -> return s
    Nothing -> return ""

data Answer = Yes | No | Unknown

stringToBool :: String -> Answer
stringToBool s = case lower s of
  "yes" -> Yes
  "y"   -> Yes
  "1"   -> Yes
  "no"  -> No
  "n"   -> No
  "0"   -> No
  _     -> Unknown

promptBool :: T.Text -> T.Text -> T.Text -> IO Bool
promptBool prompt prompt_unrecognised placeholder = do
  x <- getProp prompt placeholder
  case stringToBool x of
    Yes -> return True
    No -> return False
    Unknown -> printf "%s\n" prompt_unrecognised >> promptBool prompt prompt_unrecognised placeholder

