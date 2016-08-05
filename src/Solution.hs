module Solution where

import Text.ParserCombinators.ReadP
import Vertex

data Solution = Solution
  { nVertex   :: Int
  , vertices  :: [(Int,Vertex)]
  , nFacet    :: Int
  , facets    :: [(Int,[Int])]
  , moves     :: [(Int,Int)]
  }
