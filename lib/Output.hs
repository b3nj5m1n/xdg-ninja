module Output where

import qualified Data.Text                as T
import Data.Char (isSpace)
import Data.Text.ANSI
import           Data.UUID
import           Data.UUID.V4
import           System.Exit
import           System.Process
import Program

getFilename :: IO String
getFilename = do
  id <- toString <$> Data.UUID.V4.nextRandom
  return ("/tmp/xdg-ninja." ++ id ++ ".txt")

renderMarkdown :: String -> IO String
renderMarkdown content = do
  filenameInput <- getFilename
  filenameOutput <- getFilename
  editor <- appendFile (filenameInput) content
  (_, _, _, p) <- createProcess (shell ("glow -s dark " ++ filenameInput ++ " > " ++ filenameOutput))
  f <- waitForProcess p
  case f of
    ExitSuccess   -> readFile filenameOutput
    ExitFailure a -> return ""

line :: (T.Text -> T.Text) -> String -> String -> String
line color name filename = T.unpack ((T.pack "[") <> bold (color (T.pack name)) <> (T.pack "]: ") <> bold (italic (T.pack filename)))

data Mode = ERR | WARN | INFO | SUCS | HELP

log :: Mode -> String -> String -> String -> IO ()
log mode name filename help = case mode of
    ERR -> do
      putStrLn (line red name filename)
      Output.log HELP name filename help
    WARN -> do
      putStrLn (line yellow name filename)
      Output.log HELP name filename help
    INFO -> do
      putStrLn (line cyan name filename)
      Output.log HELP name filename help
    SUCS -> putStrLn (line green name filename)
    HELP -> do
      md <- case (all isSpace help) of
        True -> renderMarkdown "_No help available._"
        False -> renderMarkdown help
      putStr md

logFile :: T.Text -> File -> Bool -> IO ()
logFile programName file onFilesystem = case onFilesystem of
  False -> Output.log SUCS (T.unpack programName) (path file) (help file)
  True -> case (supportLevel file) of
    Unsupported -> Output.log ERR (T.unpack programName) (path file) (help file)
    _ -> Output.log WARN (T.unpack programName) (path file) (help file)
