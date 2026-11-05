-- Offline adapter used by the bytecode replay. Imports the actual policy
-- re-exported by Preparation and used by Gateway; no JavaScript policy model.
import Plether.AA.PreparationGas (executionGasWithHeadroom)
import System.Environment (getArgs)

main :: IO ()
main = do
  [estimate] <- getArgs
  either (fail . show) print $ executionGasWithHeadroom (read estimate)
