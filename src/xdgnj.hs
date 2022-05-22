module Main where


import qualified AddProgram          as PA
import           Data.Semigroup      ((<>))
import qualified EditProgram         as PE
import qualified PreviewProgram         as PP
import           Options.Applicative

data Args = AddProgram
    | EditProgram String
    | PreviewProgram String
    | LintProgram String
    | Lint
    deriving Show

editProgram :: Parser Args
editProgram = EditProgram <$> argument str (metavar "PROGRAM")

previewProgram :: Parser Args
previewProgram = PreviewProgram <$> argument str (metavar "PROGRAM")

lintProgram :: Parser Args
lintProgram = LintProgram <$> argument str (metavar "PROGRAM")

argsParser :: Parser Args
argsParser = subparser
        (command "add" (info (pure AddProgram) (progDesc "Add program"))
      <> command "edit" (info editProgram (progDesc "Edit program config"))
      <> command "prev" (info previewProgram (progDesc "Preview program config"))
      <> command "lintp" (info lintProgram (progDesc "Lint program config"))
      <> command "lint" (info (pure Lint) (progDesc "Lint all program configs")))

args :: ParserInfo Args
args = info (argsParser <**> helper)
    ( fullDesc
    <> progDesc "xdg-ninja utilities")

main :: IO ()
main = do
    args <- execParser args
    case args of
        AddProgram           -> PA.saveProgram
        EditProgram filename -> PE.editProgram filename
        PreviewProgram filename -> PP.previewProgramFile filename
        _                    -> print args
