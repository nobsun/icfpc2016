module Segment where

import Text.ParserCombinators.ReadP
import Vertex


type Segment = (Vertex, Vertex)

showSegment :: Segment -> String
showSegment (p,q) = unwords [show p, show q]

parseSegment :: ReadP Segment
parseSegment = do
  { p <- parseVertex
  ; q <- parseVertex
  ; return (p,q)
  }

segLength2 :: Segment -> Rational
segLength2 (p,q) = (xcoord p - xcoord q)^(2 :: Int) + (ycoord p - ycoord q)^(2 :: Int)
