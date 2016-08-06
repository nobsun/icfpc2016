
import System.Environment
import Vertex
import Solution


main :: IO ()
main = do
  [n, x, y, fn] <- getArgs
  mrSol (read n) (read x, read y) Nothing fn
