module Solution where

-- import Data.Ord (comparing)
-- import Data.List (group,sort)
-- import Data.Function (on)
import Data.Ratio
import Data.Maybe
import Text.ParserCombinators.ReadP
import System.FilePath
import System.Directory

import Vertex
-- import Segment
import Polygon
import Problem hiding (facets)
import Rotate
import File (problemFile, solutionFile)


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


loadSolution :: Int -> IO Solution
loadSolution n = do
  q <- readFile $ "answers" </> show n <.> "dat"
  maybe (fail "loadSolution: parse error") return
    $ listToMaybe [ x | (x, "") <- readP_to_S (parseSolution <* skipSpaces) q ]

moveSol :: Vec -> Solution -> Solution
moveSol vec sol = sol { moves =  [ moveVert vec v | v <- moves sol ]  }

rotSol :: Rotate -> PyTri -> Pythagoras -> Solution -> Solution
rotSol rot tri py sol = sol { moves = [ rotVert rot tri py v | v <- moves sol ] }

moveRotSol :: Int -> Vec -> Rotate -> PyTri -> Pythagoras -> FilePath -> IO ()
moveRotSol n vec rot tri py fn = do
  sol <- moveSol vec . rotSol rot tri py <$> loadSolution n
  writeFile fn $ show sol


simpleSolution :: Problem -> String
simpleSolution p =
    unlines ans
  where
    vs = concatMap (map snd.pvertice.snd) $ polygons p
    (dx, dy) = (minimum $ map xcoord vs, minimum $ map ycoord vs)
    mv (x, y) = (x+dx, y+dy)
    vs' = map mv [(0,0), (1,0), (1,1), (0,1)]
    ans = ["4", "0,0", "1,0", "1,1", "0,1", "1", "4 0 1 2 3"] ++ map showT vs'

    showR r = show (numerator r) ++ "/" ++ show (denominator r)
    showT (x, y) = showR x ++ "," ++ showR y

genSimpleSolution :: Int -> IO ()
genSimpleSolution n = do
  solFound  <-  doesFileExist $ solutionFile n
  b         <-  doesFileExist $ problemFile n
  if solFound
    then putStrLn $ "solution file already exists: " ++ solutionFile n
    else if b
         then writeFile ("answers/"++show n++".dat") . simpleSolution =<< loadProblem n
         else fail $ "problem file not found: " ++ problemFile n

{-# DEPRECATED genSimpleAnswer "Use genSimpleSolution instead of this." #-}
genSimpleAnswer :: Int -> IO ()
genSimpleAnswer = genSimpleSolution
