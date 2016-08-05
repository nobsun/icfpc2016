module Vertex where

import Data.Char
import Data.Bool
import Data.Array
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

