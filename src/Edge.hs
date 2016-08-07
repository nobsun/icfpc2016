{-# LANGUAGE TupleSections #-}
module Edge where

import Data.Bool
import Data.List
import Data.Tuple

import Data.Graph.Inductive

import Problem hiding (sort)
import Solution (Facet)
import Vertex
import Segment
import Move (vert2vec,vsub,iprod,oprod,vnorm)

type Eid = Int
type Vid = Int
type Edge' = (Vid,Vid)

type VertexTable = [(Vid,Vertex)]
type RvVertexTable = [(Vertex,Vid)]
type Edge'Table = [(Eid,Edge')]

segs2edges :: VertexTable -> [Segment] -> Edge'Table
segs2edges vtbl = zip [0..] . map (seg2edge vtbl) 

seg2edge :: VertexTable -> Segment -> Edge'
seg2edge tbl (v1,v2) = (retVid rtbl v1, retVid rtbl v2)
  where
    rtbl = revVxTable tbl

revVxTable :: VertexTable -> RvVertexTable
revVxTable = map swap

retVid :: RvVertexTable -> Vertex -> Vid
retVid rtbl v = maybe (error "unknown vertex") id (lookup v rtbl)

segs2vtbl :: [Segment] -> VertexTable
segs2vtbl = zip [0..] . map head . group . sort . concatMap seg2vs

seg2vs :: Segment -> [Vertex]
seg2vs (v,v') = [v,v']

--

segs2graph :: [Segment] -> Gr Vertex Eid
segs2graph segs = makeGraph vtbl (segs2edges vtbl segs)
  where
    vtbl = segs2vtbl segs

makeGraph :: VertexTable -> Edge'Table -> Gr Vertex Eid
makeGraph vtbl etbl = undir $ mkGraph vtbl (map conv etbl)
  where
    conv (i,e) = toLEdge e i

--

sampleGraph :: Gr Vertex Eid
sampleGraph = segs2graph sampleSegs

sampleSegs :: [Segment]
sampleSegs = segments sampleProb

sampleProb :: Problem
sampleProb = read sampleProbS

sampleProbS :: String
sampleProbS = unlines
  ["1"
  ,"4"
  ,"0,0"
  ,"1,0"
  ,"1/2,1/2"
  ,"0,1/2"
  ,"5"
  ,"0,0 1,0"
  ,"1,0 1/2,1/2"
  ,"1/2,1/2 0,1/2"
  ,"0,1/2 0,0"
  ,"0,0 1/2,1/2"
  ]

allFacets :: Problem -> [Facet]
allFacets = exAllFacets . segs2graph . segments 

-- extract facets from graph

exAllFacets :: Gr Vertex Eid -> [Facet]
exAllFacets g
  = _exallfacets [] nvs g 0
    where
      nvs = length (nodes g)
      
_exallfacets :: [Facet] -> Int -> Gr Vertex Eid -> Node -> [Facet]
_exallfacets fs nvs g c
  | nvs <= c  = fs
  | otherwise = case exFacets g 0 of
      (fs',g') -> _exallfacets (fs'++fs) nvs g' (c+1)

exFacets :: Gr Vertex Eid -> Node -> ([Facet],Gr Vertex Eid)
exFacets g n = _exfacets [] g n 

_exfacets :: [Facet] -> Gr Vertex Eid -> Node -> ([Facet],Gr Vertex Eid)
_exfacets fs g c = case minAngle g c of
  Nothing -> (fs,g)
  Just ((p,_),_,_)
          -> case _exfacet [] g c p c of
               Nothing     -> (fs,g)
               Just (f,g') -> _exfacets (f:fs) g' c

_exfacet :: [Node] -> Gr Vertex Eid -> Node -> Node -> Node -> Maybe (Facet,Gr Vertex Eid)
_exfacet vs g e p c
  | null outs = Nothing
  | null angs = Nothing
  | e == n    = Just ((1+length vs,c:vs),delEdge (c,n) g)
  | otherwise = _exfacet (c:vs) (delEdge (c,n) g) e c n
  where
    outs = out g c
    angs = filter ccwp
             [(node2lnode g p,node2lnode g c,node2lnode g nx)
             |(_,nx,_) <- outs, nx /= c]
    (_,_,(n,_)) = minimumBy compareAngle angs

minAngle :: Gr Vertex Eid -> Node -> Maybe Angle
minAngle g c = case context g c of
  (prevs,_,cv,nexts)
    -> bool (Just $ minimumBy compareAngle cands) Nothing (null cands)
       where
         cands = filter ccwp
               $ [(node2lnode g pn,(c,cv),node2lnode g nn) | (pe,pn) <- prevs, (ne,nn) <- nexts, pe /= ne ]

ccwp :: Angle -> Bool
ccwp ((_,pv),(_,cv),(_,nv))
  = oprod pc cn > 0
    where
      pvec = vert2vec pv
      cvec = vert2vec cv
      nvec = vert2vec nv
      pc = vsub cvec pvec
      cn = vsub nvec cvec

node2lnode :: Gr a b -> Node -> LNode a
node2lnode g n = maybe (error "unlabeled node") (n,) (lab g n)

type Angle = (LNode Vertex, LNode Vertex, LNode Vertex)
                                           
compareAngle :: Angle -> Angle -> Ordering
compareAngle ((_,pv),(_,cv),(_,nv)) ((_,pv'),(_,cv'),(_,nv'))
  = compare (iprod pcuvec cnuvec) (iprod pcuvec' cnuvec')
    where
      [pvec,cvec,nvec,pvec',cvec',nvec']
        = map vert2vec [pv,cv,nv,pv',cv',nv']
      [pcuvec,cnuvec,pcuvec',cnuvec']
        = map vnorm
        $ zipWith vsub [cvec,nvec,cvec',nvec'] [pvec,cvec,pvec',cvec']

