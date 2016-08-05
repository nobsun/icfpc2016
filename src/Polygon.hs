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
