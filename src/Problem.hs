module Problem where

import System.FilePath
import Text.ParserCombinators.ReadP
import Polygon
import Vertex

data Problem = Problem
  { nPolygon :: Int
  , polygons :: [(Int, Polygon)]
  , nSegment :: Int
  , segments :: [Segment]
  }

type Segment = (Vertex, Vertex)

instance Show Problem where
  show p = unlines
         $  show (nPolygon p) : map (show . snd) (polygons p)
         ++ show (nSegment p) : map showSegment (segments p)

showSegment :: Segment -> String
showSegment (p,q) = unwords [show p, show q]

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

parseSegment :: ReadP Segment
parseSegment = do
  { p <- parseVertex
  ; q <- parseVertex
  ; return (p,q)
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

validAll :: IO Bool
validAll = return . all (==True) =<< mapM valid [1..101]

loadProblem :: Int -> IO Problem
loadProblem n = do
  q <- readFile $ "problems" </> show n <.> "dat"
  let p = head $ fst <$> readP_to_S parseProblem q
  return p
  

data WiseDir = CW | CCW
  deriving (Show, Eq)

-- |
-- >>> p25 <- loadProblem 25
-- >>> map (clockWiseDir.snd) $ polygons p25
-- [CCW,CW]
clockWiseDir :: Polygon -> WiseDir
clockWiseDir p = if foldr (\x xs -> f x + xs) 0 ps >= 0
                 then CCW
                 else CW
  where
    f :: (Vertex, Vertex) -> Rational
    f (v1, v2) = xcoord v1 * ycoord v2 - xcoord v2 * ycoord v1
    ps :: [(Vertex, Vertex)]
    ps = take (pnVertex p + 1) $ zip xs (tail xs)
      where
        (xs, vs) = (vs ++ xs, map snd $ pvertice p)
