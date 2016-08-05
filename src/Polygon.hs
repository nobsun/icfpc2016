module Polygon where

import Data.List
import Text.ParserCombinators.ReadP
import Vertex

data Polygon = Polygon
  { pnVertex  :: Int
  , pvertice  :: [(Int, Vertex)]
  }

instance Show Polygon where
  show p = intercalate "\n" $ show (pnVertex p) : map (show . snd) (pvertice p)

parsePolygon :: ReadP Polygon
parsePolygon = do
  { np <- parseInt
  ; ps <- count np parseVertex
  ; return (Polygon np (numbering 0 ps))
  }

parseInt :: ReadP Int
parseInt = skipSpaces >> readS_to_P (readsPrec 0)

numbering :: Int -> [a] -> [(Int,a)]
numbering n = zip [n..]
  

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
