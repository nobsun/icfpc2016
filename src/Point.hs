module Point where

import Data.Char
import Data.Bool
import Data.Array
import Data.Ratio
import Text.ParserCombinators.ReadP

data Point = Point { xcoord, ycoord :: Rational }

instance Show Point where
  show p = show (xcoord p, ycoord p)

instance Read Point where
  readsPrec _ = readP_to_S parsePoint

parsePoint :: ReadP Point
parsePoint = do { x <- parseRational
                ; char ','
                ; y <- parseRational
                ; return (Point x y)
                }

parseRational :: ReadP Rational
parseRational = do { nd <- sepBy1 parseInteger (char '/')
                   ; return $ case nd of
                       [n]   -> n % 1
                       [n,d] -> n % d
                   }

parseInteger :: ReadP Integer
parseInteger = readS_to_P (Prelude.readsPrec 0)

