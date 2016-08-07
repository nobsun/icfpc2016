{-# LANGUAGE TemplateHaskell #-}
module Move where

import Control.Arrow ((&&&))

import Vertex
import Segment
import Solution
import Polygon

-- import qualified Rotate as Py

moveSol :: Vec -> Solution -> Solution
moveSol vec sol = sol
  { moves = [ moveVert vec v | v <- moves sol ] }

rotSol :: Vec -> Solution -> Solution
rotSol vec sol = sol
  { moves = [ rotVert vec v | v <- moves sol ] }

rotVert :: Vec -> Vertex -> Vertex
rotVert vec vertex = vec2vert $ rotvec (vec2rmat vec) $ vert2vec vertex

symVert :: Segment -> Vertex -> Vertex
symVert (u,v) p = vec2vert (symmove seg' (vert2vec p))
  where
    seg' = [vert2vec u, vert2vec v]

-- utilities

type Count = Int

srcFacetPolygons :: Solution -> [Polygon] -- [(Count,[(Int,Vertex)])]
srcFacetPolygons sol = [ toPolygon (c,map (retfacet tbl) vis) | (c,vis) <- facets sol ] 
  where
    tbl = svertice sol

dstFacetPolygons :: Solution -> [Polygon] -- [(Count,[(Int,Vertex)])]
dstFacetPolygons sol = [ toPolygon (c,map (retfacet tbl) vis) | (c,vis) <- facets sol ]
  where
    tbl = zip [0..] (moves sol)
    
retfacet :: [(Int,Vertex)] -> Int -> (Int,Vertex)
retfacet tbl i = (i,maybe (error "vertex search error") id (lookup i tbl))

-- converters

toPolygon :: (Count,[(Int,Vertex)]) -> Polygon
toPolygon = uncurry Polygon

vec2vert :: Vec -> Vertex
vec2vert = uncurry Vertex
vert2vec :: Vertex -> Vec
vert2vec = xcoord &&& ycoord

-- low level operators

symmove :: [Vec] -> Vec -> Vec
symmove seg v@(p,q) = v'
  where
    (a,b,c) = seg2coeff seg
    t  = - (a*p+b*q+c)/(a*a+b*b)
    h  = (p+t*a,q+t*b)
    vh = vsub h v
    v' = vadd vh $ vadd vh v
    
smulv :: Rational -> Vec -> Vec
smulv a (x,y) = (a*x,a*y)

vadd :: Vec -> Vec -> Vec
(x1,y1) `vadd` (x2,y2) = (x1+x2,y1+y2)

vneg :: Vec -> Vec
vneg (x,y) = (-x,-y)

vsub :: Vec -> Vec -> Vec
vsub u v = vadd u (vneg v)

type Coeff = (Rational,Rational,Rational)

seg2coeff :: [Vec] -> Coeff
seg2coeff seg = case seg of
  [a,b] -> case intercepts seg of
    [_,y0] -> case vsub b a of
      (abx,aby) -> if 0 == abx then (1,0,-fst a)
                   else if 0 == aby then (0,1,-snd a)
                        else (- aby/abx,1,-snd y0)
    _      -> error "imposible interceps"
  _     -> error "ill-formated segment"
  
intercepts :: [Vec] -> [Vec]
intercepts [u,v] = [xv,yv]
  where
    uv = v `vsub` u
    t  = - snd u/snd uv
    s  = - fst u/fst uv
    xv = u `vadd` (t `smulv` uv)
    yv = u `vadd` (s `smulv` uv)
intercepts _ = error "invalid vector"

vec2rmat :: Vec -> [Vec]
vec2rmat v@(a,b) = [(cos',-sin'),(sin',cos')]
  where
    r = toRational $ (sqrt :: Double -> Double)
      $ fromRational $ iprod v v
    cos' = a/r
    sin' = b/r

rotvec :: [Vec] -> Vec -> Vec
rotvec [v1,v2] v = (iprod v1 v, iprod v2 v)
rotvec _ _       = error "ill-formed matrix"

iprod :: Vec -> Vec -> Rational
iprod (a,b) (c,d) = a*c+b*d

oprod :: Vec -> Vec -> Rational
oprod (a,b) (c,d) = a*d-b*c

vmag :: Vec -> Rational
vmag v = toRational $ (sqrt :: Double -> Double) $ fromRational $ iprod v v

vnorm :: Vec -> Vec
vnorm v = smulv (recip (vmag v)) v

hflip :: Rational -> Vec -> Vec
hflip x0 (x,y) = (x0-x,y)

vflip :: Rational -> Vec -> Vec
vflip y0 (x,y) = (x,y0-y)

