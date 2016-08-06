module Vertex where


import Data.Bool

import Data.Ratio
import Text.ParserCombinators.ReadP

data Vertex = Vertex { xcoord, ycoord :: Rational } deriving (Eq, Ord)

instance Show Vertex where
  show p = showRational (xcoord p) ++ "," ++ showRational (ycoord p)

showRational :: Rational -> String
showRational r = show (numerator r) ++ bool "" ('/':show d) (1 /= d)
  where d = denominator r

instance Read Vertex where
  readsPrec _ = readP_to_S parseVertex

parseVertex :: ReadP Vertex
parseVertex = do
  { x <- parseRational
  ; char ','
  ; y <- parseRational
  ; return (Vertex x y)
  }

parseRational :: ReadP Rational
parseRational = skipSpaces >> do
  { nd <- sepBy1 parseInteger (char '/')
  ; return $ case nd of
               [n]   -> n % 1
               [n,d] -> n % d
  }

parseInteger :: ReadP Integer
parseInteger = skipSpaces >> readS_to_P (Prelude.readsPrec 0)

distance :: Floating f => (Vertex, Vertex) -> f
distance = sqrt . fromRational . sqDistance

distance' :: Integral a => (Vertex, Vertex) -> Ratio a
distance' (v1, v2) = let x = sqDistance (v1, v2)
                     in (floor (sqrt (fromInteger (numerator x) :: Double))) % (floor (sqrt (fromInteger (denominator x) :: Double)))

sqDistance :: (Vertex, Vertex) -> Rational
sqDistance (v1, v2) = (xcoord v1 - xcoord v2)^(2 :: Int) + (ycoord v1 - ycoord v2)^(2 :: Int)


type Vec = (Rational,Rational)

moveVert :: Vec -> Vertex -> Vertex
moveVert (dx,dy) (Vertex x y) = Vertex (x+dx) (y+dy)
