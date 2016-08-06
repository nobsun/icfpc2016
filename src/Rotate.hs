module Rotate where

import Data.Ratio
import Vertex (Vertex (..))


newtype Pythagoras =
  Pythagoras (Integer, Integer, Integer)
  deriving (Eq, Ord)

instance Show Pythagoras where
  show (Pythagoras t) = show t

values :: [Pythagoras]
values =
  [ Pythagoras t
  | t <- [ (0,1,1)
         , (3,4,5), (5,12,13), (8,15,17), (20,21,29)
         , (12,35,37), (9,40,41), (28,45,53), (11,60,61)
         , (48,55,73), (39,80,89), (65,72,97), (20,99,101)
         , (60,91,109), (15,112,113), (88,105,137), (51,140,149)
         , (85,132,157), (52,165,173), (19,180,181), (95,168,193)
         , (28,195,197), (60,221,229), (105,208,233), (120,209,241)
         , (32,255,257), (69,260,269), (115,252,277), (160,231,281)
         , (68,285,293), (25,312,313), (75,308,317), (175,288,337)
         , (180,299,349), (225,272,353), (252,275,373), (189,340,389)
         , (228,325,397), (40,399,401), (120,391,409), (29,420,421)
         , (145,408,433), (280,351,449), (168,425,457), (261,380,461)
         , (220,459,509), (279,440,521), (341,420,541), (165,532,557)
         , (231,520,569), (48,575,577), (368,465,593), (240,551,601)
         , (35,612,613), (105,608,617), (200,609,641), (315,572,653)
         , (300,589,661), (385,552,673), (52,675,677), (260,651,701)
         , (259,660,709), (108,725,733), (468,595,757), (39,760,761)
         , (481,600,769), (195,748,773), (555,572,797), (280,759,809)
         , (429,700,821), (540,629,829), (205,828,853), (232,825,857)
         , (348,805,877), (369,800,881), (129,920,929), (215,912,937)
         , (580,741,941), (615,728,953), (248,945,977), (372,925,997)
         , (559,840,1009), (45,1012,1013), (660,779,1021), (192,1015,1033)
         , (320,999,1049), (620,861,1061), (731,780,1069), (132,1085,1093)
         , (585,928,1097), (141,1100,1109), (235,1092,1117), (329,1080,1129)
         , (528,1025,1153), (340,1131,1181), (832,855,1193), (49,1200,1201)
         , (245,1188,1213), (705,992,1217), (140,1221,1229), (612,1075,1237)
         ]
  ]


pyCos' :: Pythagoras -> Rational
pyCos' (Pythagoras (a, _, c)) = a % c

pySin' :: Pythagoras -> Rational
pySin' (Pythagoras (_, b, c)) = b % c

data PyTri
  = TriLex
  | TriFlip
  deriving (Eq, Ord, Show)

pyCos :: PyTri -> Pythagoras -> Rational
pyCos TriLex   = pyCos'
pyCos TriFlip  = pySin'

pySin :: PyTri -> Pythagoras -> Rational
pySin TriLex   = pySin'
pySin TriFlip  = pyCos'

data Rotate
  = CounterClock
  | Clock
  deriving (Eq, Ord, Show)

rotVert :: Rotate
        -> PyTri
        -> Pythagoras
        -> Vertex
        -> Vertex
rotVert rot tri py v = case rot of
    CounterClock  ->  Vertex (cos' * x - sin' * y) (sin' * x + cos' * y)
    Clock         ->  Vertex (cos' * x + sin' * y) (- sin' * x + cos' * y)
  where
    x = xcoord v
    y = ycoord v
    cos' = pyCos tri py
    sin' = pySin tri py
