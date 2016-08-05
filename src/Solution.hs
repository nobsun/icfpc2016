module Solution where

import Data.Ord (comparing)
import Data.List (group,sort)
import Data.Function (on)
import Text.ParserCombinators.ReadP
import Vertex
import Segment
import Polygon
import Problem

data Solution = Solution
  { snVertex  :: Int
  , svertice  :: [(Int,Vertex)]
  , nFacet   :: Int
  , facets   :: [Facet]
  , moves    :: [Vertex]
  }

type Facet = (Int, [Int])

instance Show Solution where
  show s = unlines
         $ show (snVertex s)
         : (map (show . snd) (svertice s)
           ++ (show (nFacet s) : (map showFacet (facets s)
                                  ++ (map show (moves s)))))

showFacet :: Facet -> String
showFacet (n,vs) = unwords (map show (n:vs))

instance Read Solution where
  readsPrec _ = readP_to_S parseSolution

parseSolution :: ReadP Solution
parseSolution = do
  { nv <- parseInt
  ; vs <- count nv parseVertex
  ; nf <- parseInt
  ; fs <- count nf parseFacet
  ; ms <- count nv parseVertex
  ; return (Solution nv (numbering 0 vs) nf fs ms)
  }

parseFacet :: ReadP Facet
parseFacet = do
  { nv <- parseInt
  ; vs <- count nv parseInt
  ; return (nv,vs)
  }

sampleSolution :: String
sampleSolution = unlines
  ["7"
  ,"0,0"
  ,"1,0"
  ,"1,1"
  ,"0,1"
  ,"0,1/2"
  ,"1/2,1/2"
  ,"1/2,1"
  ,"4"
  ,"4 0 1 5 4"
  ,"4 1 2 6 5"
  ,"3 4 5 3"
  ,"3 5 6 3"
  ,"0,0"
  ,"1,0"
  ,"0,0"
  ,"0,0"
  ,"0,1/2"
  ,"1/2,1/2"
  ,"0,1/2"
  ]

moveSol :: Vec -> Solution -> Solution
moveSol vec sol = sol { moves =  [ moveVert vec v | v <- moves sol ]  }
