module Problem where

import Control.Arrow
import Data.Maybe
import GHC.Real

import System.Directory
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
  ; char '\n'
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
validAll = return . all (==True) =<< mapM valid [1..101]

loadProblem :: Int -> IO Problem
loadProblem n = do
  q <- readFile $ "problems" </> show n <.> "dat"
  maybe (fail "loadProblem: parse error") return
    $ listToMaybe [ x | (x, "") <- readP_to_S (parseProblem <* skipSpaces) q ]

genSimpleAnswer :: Int -> IO ()
genSimpleAnswer n = do
  b <- doesFileExist $ "problems" </> show n <.> "dat"
  if b
    then do
    p <- loadProblem n
    let vs = concatMap (map snd.pvertice.snd) $ polygons p
        (dx, dy) = (minimum $ map xcoord vs, minimum $ map ycoord vs)
        vs' = map (mv (dx, dy)) [(0,0), (1,0), (1,1), (0,1)]
        ans = ["4", "0,0", "1,0", "1,1", "0,1", "1", "4 0 1 2 3"] ++ map showT vs'
    writeFile ("answers/"++show n++".dat") $ unlines ans
    else return ()
  where
    mv (dx, dy) (x, y) = (x+dx, y+dy)
    showR r = show (numerator r) ++ "/" ++ show (denominator r)
    showT (x, y) = showR x ++ "," ++ showR y


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
