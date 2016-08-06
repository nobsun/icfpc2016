
import Text.Read (readMaybe)
import System.Environment

import Rotate
import Solution


usage :: IO ()
usage =
  putStrLn $ unlines
  [ "usage: move-rot-sol PROBLEM_NUM MOVE_X MOVE_Y [CounterClock|Clock] [TriLex|TriFlip] PYTHAGORAS_TRIPLE DEST_FILENAME"
  , "PYTHAGORAS_TRIPLE syntax is (x,y,z). z is " ]

main :: IO ()
main = do
  as  <-  getArgs
  let clError = usage *> fail "Command line error"
  (n, x, y, rot, tri, py, fn) <- case as of
    [n, x, y, rot, tri, py, fn]  ->
      maybe clError return $
      (,,,,,,)
      <$> readMaybe n
      <*> readMaybe x
      <*> readMaybe y
      <*> readMaybe rot
      <*> readMaybe tri
      <*> (Pythagoras <$> readMaybe py)
      <*> pure fn
    _                            ->
      clError
  moveRotSol n (x, y) rot tri py fn
