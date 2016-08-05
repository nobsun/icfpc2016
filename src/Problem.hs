module Problem where

import Text.ParserCombinators.ReadP
import Vertex

data Problem = Problem
  { nPolygon :: Int
  , nVertex  :: Int
  , vertice  :: [(Int, Vertex)]
  , nSegment :: Int
  , segments :: [Segment]
  }

type Segment = (Vertex, Vertex)

instance Show Problem where
  show p = unlines
         $ show (nPolygon p) : show (nVertex  p)
         : ( map (show . snd) (vertice p)
             ++ show (nSegment p) : map showSegment (segments p))

showSegment :: Segment -> String
showSegment (p,q) = unwords [show p, show q]

instance Read Problem where
  readsPrec _ = readP_to_S parseProblem 

parseProblem :: ReadP Problem
parseProblem = do
  { np <- parseInt
  ; nv <- parseInt
  ; ps <- count nv parseVertex
  ; ns <- parseInt
  ; ss <- count ns parseSegment
  ; return (Problem np nv (numbering 0 ps) ns ss)
  }

parseInt :: ReadP Int
parseInt = skipSpaces >> readS_to_P (readsPrec 0)

parseSegment :: ReadP Segment
parseSegment = do
  { p <- parseVertex
  ; q <- parseVertex
  ; return (p,q)
  }

numbering :: Int -> [a] -> [(Int,a)]
numbering n = zip [n..]

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

validAll :: IO [Bool]
validAll = mapM valid [1..101]
