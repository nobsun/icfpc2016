module Problem where

import Control.Arrow
import Control.Monad
import Data.List (find, delete, sortBy)
import Data.Function (on)
import Data.Maybe
import GHC.Real

import System.FilePath
import Text.ParserCombinators.ReadP
import Polygon
import Segment
import Vertex


data Problem = Problem
  { nPolygon :: Int
  , polygons :: [(Int, Polygon)]
  , nSegment :: Int
  , segments :: [Segment]
  }

instance Show Problem where
  show p = unlines
         $  show (nPolygon p) : map (show . snd) (polygons p)
         ++ show (nSegment p) : map showSegment (segments p)

instance Read Problem where
  readsPrec _ = readP_to_S parseProblem

parseProblem :: ReadP Problem
parseProblem = do
  { np <- parseInt
  ; ps <- count np parsePolygon
  ; ns <- parseInt
  ; ss <- count ns parseSegment
  ; void $ char '\n'
  ; return (Problem np (numbering 0 ps) ns ss)
  }

sample :: String
sample = unlines
  ["1"
  ,"4"
  ,"0,0"
  ,"1,0"
  ,"1,1"
  ,"0,1"
  ,"4"
  ,"0,0 1,0"
  ,"0,0 0,1"
  ,"1,0 1,1"
  ,"0,1 1,1"
  ]

valid :: Show a => a -> IO Bool
valid n = do
  q <- readFile ("problems/"++show n++".dat")
  p <- return $ head $ fst <$> readP_to_S parseProblem q
  return (q == show p)

-- |
-- >>> validAll
-- True
validAll :: IO Bool
validAll = return . all (==True) =<< mapM valid [1..101 :: Int]

loadProblem :: Int -> IO Problem
loadProblem n = do
  q <- readFile $ "problems" </> show n <.> "dat"
  maybe (fail "loadProblem: parse error") return
    $ listToMaybe [ x | (x, "") <- readP_to_S (parseProblem <* skipSpaces) q ]

moveVertex :: Vec -> Vertex -> Vertex
moveVertex (dx, dy) (Vertex x y) = (Vertex (x + dx) (y + dy))

moveSegment :: Vec -> Segment -> Segment
moveSegment v (dom, codom) = (moveVertex v dom, moveVertex v codom)

moveProb :: Vec -> Problem -> Problem
moveProb vec (Problem npoly polys nseg segs)
  = Problem npoly polys' nseg segs'
    where
      segs' = map (moveSeg vec) segs
      polys' = map (second (movePoly vec)) polys

moveSeg :: Vec -> Segment -> Segment
moveSeg vec (v1,v2) = (moveVert vec v1, moveVert vec v2)

movePoly :: Vec -> Polygon -> Polygon
movePoly vec (Polygon nv vs)
  = Polygon nv (map (second (moveVertex vec)) vs)

combinations :: Int -> [a] -> [[a]]
combinations n' xs' = comb n' (length xs') xs' where
  comb _ _ [] = [[]]
  comb 0 _ _  = [[]]
  comb r n a@(x:xs)
    | n == r = [a]
    | otherwise = map (x:) (comb (r-1) (n-1) xs) ++ comb r (n-1) xs

-- | for facets only
combinations' :: [a] -> [[a]]
combinations' xs = concatMap (\n -> combinations n xs) [3..(length xs)]

toFacet :: [Segment] -> Maybe [Segment]
toFacet xs =
  maybe Nothing (\(sorted, rest) ->
                   if null rest && cyclic sorted && convex sorted && not (intersected' sorted)
                   then Just (simplify sorted)
                   else Nothing
                   )
  $ chainSort' xs
  where
    simplify :: [Segment] -> [Segment]
    simplify ss = let (ss', h, l) = (merge ss, head ss', last ss')
                  in if crossProduct ((seg2vec h), (seg2vec l)) == 0
                     then tail ss' ++ [joint l h]
                     else ss'

merge :: [Segment] -> [Segment]
merge [] = []
merge (x:xs) = sub [] x xs
  where
    sub rs t [] = reverse (t:rs)
    sub rs t (s:ss) = if crossProduct ((seg2vec t), (seg2vec s)) == 0
                      then sub rs (joint t s) ss
                      else sub (t:rs) s ss

joint :: Segment -> Segment -> Segment
joint (x1, _) (_, y2) = (x1, y2)

-- |
-- all facets using splitSegments
facets' :: Problem -> [[Segment]]
facets' =
  filter (not.null).map (maybe [] id.toFacet).combinations'.splitSegments.segments

facets :: Problem -> [[Segment]]
facets = filter (not.null).map (maybe [] id.toFacet).combinations'.segments

chainSort' :: [Segment] -> Maybe ([Segment], [Segment])
chainSort' xs = chainSort [head xs] (tail xs)

chainSort :: [Segment] -> [Segment] -> Maybe ([Segment], [Segment])
chainSort xs [] = Just (xs, [])
chainSort xs ys =
  let x2 = snd $ last xs
      mnext = find (\(y1, y2) -> x2 == y1 || x2 == y2) ys
  in maybe
     Nothing
     (\(y1, y2) ->
        if x2 == y1
        then chainSort (xs++[(y1,y2)]) $ delete (y1,y2) ys
        else chainSort (xs ++ [(y2,y1)]) $ delete (y1,y2) ys)
     mnext

cyclic :: [Segment] -> Bool
cyclic = uncurry (==) . (fst . head &&& snd . last)

convex :: [Segment] -> Bool
convex xs =
  let (z:zs) = filter (/=0) $ take (length xs) $ map (signum.crossProduct) $ zip vs' (tail vs')
  in all (==z) zs
  where
    vs' = vs ++ vs'
    vs = map seg2vec xs

seg2vec :: Segment -> (Rational, Rational)
seg2vec (Vertex x1 y1, Vertex x2 y2) = (x2 - x1, y2 -y1)

crossProduct :: Num a => ((a, a), (a, a)) -> a
crossProduct ((ax, ay), (bx, by)) = ax * by - ay * bx

innerProduct :: Num a => ((a, a), (a, a)) -> a
innerProduct ((ax, ay), (bx, by)) = ax * bx + ay * by

volume' :: [Segment] -> Rational
volume' = volume . map fst

volume :: [Vertex] -> Rational
volume [] = 0
volume (x:xs) = let v = foldr (+) 0 (map vol trigons)
                in abs (numerator v % (2 * denominator v))
  where
    vol :: (Vertex, Vertex, Vertex) -> Rational
    vol (v1, v2, v3) = crossProduct (seg2vec (v2, v1), seg2vec (v3, v2))
    xs' :: [Vertex]
    xs' = xs ++ xs'
    trigons :: [(Vertex, Vertex, Vertex)]
    trigons = take (length xs - 1) $ map (\(y,z) -> (x, y, z)) $ zip xs' (tail xs')

intersected' :: [Segment] -> Bool
intersected' [] = False
intersected' (x:xs) = intersected' xs ||  any (intersected.(x,)) xs

intersected :: (Segment, Segment) -> Bool
intersected ((Vertex ax ay, Vertex bx by), (Vertex cx cy, Vertex dx dy)) =
  tc * td < 0 && ta * tb < 0
  where
    (cx_dx, ay_cy, cy_dy, cx_ax) = (cx - dx, ay - cy, cy - dy, cx - ax)
    (by_cy, cx_bx, ax_bx, cy_ay) = (by - cy, cx - bx, ax - bx, cy - ay)
    (ay_by, ax_cx, dy_ay, ax_dx) = (ay - by, ax - cx, dy - ay, ax - dx)
    ta = cx_dx * ay_cy + cy_dy * cx_ax
    tb = cx_dx * by_cy + cy_dy * cx_bx
    tc = ax_bx * cy_ay + ay_by * ax_cx
    td = ax_bx * dy_ay + ay_by * ax_dx

online :: Vertex -> Segment -> Bool
online v@(Vertex x y) (v1@(Vertex x1 y1), v2@(Vertex x2 y2)) =
  notChained && notIntersected && inRange
  where
    (va, vb) = ((x1 - x, y1 - y), (x2 - x, y2 - y))
    notChained = v /= v1 && v /= v2
    notIntersected = crossProduct (va, vb) == 0
    inRange = innerProduct (va, vb) < 0

split :: Segment -> [Vertex] -> [Segment]
split s@(v1, v2) vs = (zip <*> tail) orderedOnlineVertexes
  where
    onlineVertexes = filter (`online` s) vs
    orderedOnlineVertexes = sortBy (compare `on` (sqDistance.(Vertex 0 0,))) (v1:v2:onlineVertexes)

splitSegments :: [Segment] -> [Segment]
splitSegments xs = concatMap (\x -> x `split` vs) xs
  where
    vs = foldr (\(v1,v2) r -> v1:v2:r) [] xs
