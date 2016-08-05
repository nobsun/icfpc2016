module Solution where

import Data.Ord (comparing)
import Data.List (group,sort)
import Data.Function (on)
import Text.ParserCombinators.ReadP
import Vertex
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

check :: Solution -> [Bool]
check s = map ($ s) conds

type Condition = Solution -> Bool

ncond :: Int
ncond = 3

conds :: [Condition]
conds = map cond [1..ncond]

cond :: Int -> Condition
cond 1 s = all inRange (map snd (svertice s))
cond 2 s = uniqOccurrence (map snd (svertice s))
cond 3 s = all (simple (svertice s)) (facets s)

inRange :: Vertex -> Bool
inRange v = 0 <= x && x <= 1 && 0 <= y && y <= 1
  where
    x = xcoord v
    y = ycoord v

uniqOccurrence :: [Vertex] -> Bool
uniqOccurrence vs = length vs == length (group (sort vs))

simple :: [(Int,Vertex)] -> Facet -> Bool
simple tbl (n,ids) = all (not . segIntersection) $ comb 2 es
  where
    es = take n (zip vs (tail vs))
    vs = cycle $ map lookingup ids
    lookingup i = maybe undefined id (lookup i tbl)

segIntersection :: [Segment] -> Bool
segIntersection [(a,b),(c,d)] = tc * td < 0 && ta * tb < 0
  where
    ax = xcoord a
    ay = ycoord a
    bx = xcoord b
    by = ycoord b
    cx = xcoord c
    cy = ycoord c
    dx = xcoord d
    dy = ycoord d
    ta = (cx - dx) * (ay - cy) + (cy - dy) * (cx - ax)
    tb = (cx - dx) * (by - cy) + (cy - dy) * (cx - bx)
    tc = (ax - bx) * (cy - ay) + (ay - by) * (ax - cx)
    td = (ax - bx) * (dy - ay) + (ay - by) * (ax - dx)

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb n (x:xs) = map (x:) (comb (n-1) xs) ++ comb n xs

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
