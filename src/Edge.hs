module Edge where

import Data.Graph.Inductive

import Data.List
import Data.Tuple

import Vertex
import Segment

type Eid = Int
type Vid = Int
type Fid = Int
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
makeGraph vtbl etbl = undir $ insEdges (map conv etbl) $ insNodes vtbl empty
  where
    conv (i,e) = toLEdge e i
