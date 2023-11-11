import           Assembler
import           Checker
import           Data.Bifunctor
import           Machine
import           Parser
import           System.Environment
import           Tokenizer

compileAndRun src = do
  tokens <- first ("Tokenize error: " ++) $ tokenize src
  e <- first ("Parse error: " ++) $ parseProgram tokens
  cExpr <- first ("Type error: " ++) $ fst <$> check e
  pure $ run $ makeProgramFromChecked cExpr

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No input file"
    x:_ -> do
      src <- readFile x
      putStrLn $ either id showMachineResult $ compileAndRun src
