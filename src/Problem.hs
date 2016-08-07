module Problem where

import Control.Arrow
import Control.Monad
import Data.List (find, delete)
import Data.Maybe
import GHC.Real

import System.Directory
import System.FilePath
import Text.ParserCombinators.ReadP
import Polygon
import Segment
import Vertex
import File (problemFile, solutionFile)

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

genSimpleAnswer' :: Int -> IO ()
genSimpleAnswer' n = do
    p <- loadProblem n
    let vs = concatMap (map snd.pvertice.snd) $ polygons p
        (dx, dy) = (minimum $ map xcoord vs, minimum $ map ycoord vs)
        vs' = map (mv (dx, dy)) [(0,0), (1,0), (1,1), (0,1)]
        ans = ["4", "0,0", "1,0", "1,1", "0,1", "1", "4 0 1 2 3"] ++ map showT vs'
    writeFile ("answers/"++show n++".dat") $ unlines ans
  where
    mv (dx, dy) (x, y) = (x+dx, y+dy)
    showR r = show (numerator r) ++ "/" ++ show (denominator r)
    showT (x, y) = showR x ++ "," ++ showR y

genSimpleAnswer :: Int -> IO ()
genSimpleAnswer n = do
  solFound  <-  doesFileExist $ solutionFile n
  b         <-  doesFileExist $ problemFile n
  if solFound
    then putStrLn $ "solution file already exists: " ++ solutionFile n
    else if b
         then genSimpleAnswer' n
         else fail $ "problem file not found: " ++ problemFile n

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
                   if null rest && cyclic sorted
                   then Just sorted
                   else Nothing
                   )
  $ sort' xs


facets :: Problem -> [[Segment]]
facets = filter (not.null) . map (maybe [] id .toFacet) . combinations' . segments

sort' :: [Segment] -> Maybe ([Segment], [Segment])
sort' xs = sort [head xs] (tail xs)

sort :: [Segment] -> [Segment] -> Maybe ([Segment], [Segment])
sort xs [] = Just (xs, [])
sort xs ys =
  let x2 = snd $ last xs
      mnext = find (\(y1, y2) -> x2 == y1 || x2 == y2) ys
  in maybe
     Nothing
     (\(y1, y2) ->
        if x2 == y1
        then sort (xs++[(y1,y2)]) $ delete (y1,y2) ys
        else sort (xs ++ [(y2,y1)]) $ delete (y1,y2) ys)
     mnext


combine :: Maybe [Segment] -> Maybe [Segment] -> Maybe [Segment]
combine Nothing   ys        = ys
combine xs        Nothing   = xs
combine (Just xs) (Just ys) =
  let (x1, x2) = (fst . head &&& snd . last) xs
      (y1, y2) = (fst . head &&& snd . last) ys
  in
    if      x1 == y1 then Just $ reverse xs ++ ys
    else if x2 == y1 then Just $ xs         ++ ys
    else if x1 == y2 then Just $ reverse xs ++ reverse ys
    else if x2 == y2 then Just $ xs         ++ reverse ys
    else Nothing

cyclic :: [Segment] -> Bool
cyclic = uncurry (==) . (fst . head &&& snd . last)
