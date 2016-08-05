module Valid where

import Data.List

import Data.Char
import Vertex
import Segment
import Polygon
import Solution

check :: Solution -> [Bool]
check s = map ($ s) conds

type Condition = Solution -> Bool

ncond :: Int
ncond = 9

conds :: [Condition]
conds = map cond [1..ncond]

cond :: Int -> Condition
cond 1 s = all inRange (map snd (svertice s))
cond 2 s = uniqOccurrence (map snd (svertice s))
cond 3 s = and $ map (all ((0<) . segLength2) . allsegs (svertice s)) (facets s)
cond 4 s = True
cond 5 s = all (simple (svertice s)) (facets s)
cond 6 s = True
cond 7 s = True
cond 8 s = True
cond 9 s = 5000 >= length (filter (not . isSpace) (show s))


inRange :: Vertex -> Bool
inRange v = 0 <= x && x <= 1 && 0 <= y && y <= 1
  where
    x = xcoord v
    y = ycoord v

uniqOccurrence :: [Vertex] -> Bool
uniqOccurrence vs = length vs == length (group (sort vs))

simple :: [(Int,Vertex)] -> Facet -> Bool
simple tbl f = all (not . segIntersection) $ comb 2 $ allsegs tbl f

allsegs :: [(Int,Vertex)] -> Facet -> [Segment]
allsegs tbl (n,ids) = take n (zip vs (tail vs))
  where
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
