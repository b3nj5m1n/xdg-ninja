module PreviewProgram where

import qualified Data.Text      as T
import           Data.Text.ANSI
import           Output
import           Program

previewFile :: T.Text -> File -> IO ()
previewFile name file = logFile name file True

previewProgram :: Program -> IO ()
previewProgram program = do
  _ <- sequence (map (previewFile (name program)) (files program))
  return ()

previewProgramFile :: String -> IO ()
previewProgramFile filename = do
  x <- readProgram filename
  case x of
    Just program -> previewProgram program
    Nothing      -> putStrLn (T.unpack (red (T.pack "Error.")))
  return ()
