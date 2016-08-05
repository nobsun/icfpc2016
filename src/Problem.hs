module Problem where

import System.FilePath
import Text.ParserCombinators.ReadP
import Polygon
import Segment
import Vertex

data Problem = Problem
  { nPolygon :: Int
  , polygons :: [(Int, Polygon)]
  , nSegment :: Int
  , segments :: [Segment]
  }

instance Show Problem where
  show p = unlines
         $  show (nPolygon p) : map (show . snd) (polygons p)
         ++ show (nSegment p) : map showSegment (segments p)

instance Read Problem where
  readsPrec _ = readP_to_S parseProblem 

parseProblem :: ReadP Problem
parseProblem = do
  { np <- parseInt
  ; ps <- count np parsePolygon
  ; ns <- parseInt
  ; ss <- count ns parseSegment
  ; char '\n'
  ; return (Problem np (numbering 0 ps) ns ss)
  }

sample :: String
sample = unlines
  ["1"
  ,"4"
  ,"0,0"
  ,"1,0"
  ,"1,1"
  ,"0,1"
  ,"4"
  ,"0,0 1,0"
  ,"0,0 0,1"
  ,"1,0 1,1"
  ,"0,1 1,1"
  ]

valid :: Show a => a -> IO Bool
valid n = do
  q <- readFile ("problems/"++show n++".dat")
  p <- return $ head $ fst <$> readP_to_S parseProblem q
  return (q == show p)

-- |
-- >>> validAll
-- True
validAll :: IO Bool
validAll = return . all (==True) =<< mapM valid [1..101]

loadProblem :: Int -> IO Problem
loadProblem n = do
  q <- readFile $ "problems" </> show n <.> "dat"
  let p = head $ fst <$> readP_to_S parseProblem q
  return p
