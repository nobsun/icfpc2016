module Segment where

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
segLength2 (p,q) = (xcoord p - xcoord q)^2 + (ycoord p - ycoord q)^2

