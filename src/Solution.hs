module Solution where

import Data.Ord (comparing)
import Data.List (group,sort)
import Data.Function (on)
import Data.Ratio
import Data.Maybe
import Text.ParserCombinators.ReadP
import System.FilePath
import Vertex
import Segment
import Polygon
import Problem
import Rotate

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

-- _xx :: IO ()
-- _xx = mrSol 42 (22198364333 % 76542960639, (-130973206238) % 1148144409585) (Just RotLeft) "xx.dat"

-- _yy :: IO ()
-- _yy = mrSol 42 (0,0) (Just RotLeft) "yy.dat"

-- _zz :: IO ()
-- _zz = mrSol 42 (22198364333 % 76542960639, (-130973206238) % 1148144409585) Nothing "zz.dat"
