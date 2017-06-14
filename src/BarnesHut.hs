{-Barnes Hut Algorithm implmenetation-}

module BarnesHut (QBox(..)
                 , QTree(..)
                 , fillTree
                 , createRoot
                 , BarnesHut.simulate
                 ) where
import Phys

-- Square bounding box
data QBox = QB Vec2D Double deriving (Show, Eq) -- mass center, position, size

-- Quad tree structure
data QTree = QN QBox Vec2D Double QTree QTree QTree QTree -- NODE Box, Mass (sub-nodes)
           | QL QBox Body -- Leaf (pos*mass, mass)
           | QE QBox-- Empty Leaf
        deriving (Show, Eq)

-- check if a QBox contains a point
contains :: QBox -> Vec2D -> Bool
contains (QB _ 0) _ = False
contains (QB (Vec2D l t) s) (Vec2D x y) =
  let w = x - l
      h = y - t
  in (w > 0) && (w <= s) && (h > 0) && (h <= s)

-- add body to a Quad Tree
addBody :: QTree -> Phys.Body -> QTree
addBody (QE box@(QB lt s)) body@(Phys.Body{pos=p, mass=m, mom=_}) | box `contains` p =
  QL (QB lt s) body
addBody node@(QE box)  (Phys.Body{pos=p, mass=_, mom=_}) | not (box `contains` p) = node
addBody node@(QL box _) (Phys.Body{pos=p, mass=_, mom=_}) | not (box `contains` p) = node
addBody (QL box@(QB lt s) body') body@(Phys.Body{pos=p, mass=m, mom=_}) | box `contains` p =
  let ns = s/2
      tln = addBody (addBody (QE (QB lt ns)) body') body
      bln = addBody (addBody (QE (QB (lt + (Vec2D 0 ns)) ns)) body') body
      trn = addBody (addBody (QE (QB (lt + (Vec2D ns 0)) ns)) body') body
      brn = addBody (addBody (QE (QB (lt + (Vec2D ns ns)) ns)) body') body
      totalmass = m + (mass body')
      avg_pos = ((m .* p) + ((mass body') .* (pos body'))) /. totalmass
  in QN (QB lt s) avg_pos totalmass tln bln trn brn
addBody node@(QN box _ _ _ _ _ _) Phys.Body{pos=p, mass=_, mom=_} | not (box `contains` p) = node
addBody (QN box@(QB lt s) mc mass tl bl tr br) body@(Phys.Body{pos=p, mass=m, mom=_}) | (box `contains` p) =
  let total_mass = mass + m
      nmc = mc + (((m .* p) - (mass .* mc)) /. total_mass)
      ntl = addBody tl body
      nbl = addBody bl body
      ntr = addBody tr body
      nbr = addBody br body
  in QN (QB lt s) nmc total_mass ntl nbl ntr nbr


-- Fill a tree with a list of bodies
fillTree :: QTree -> [Phys.Body] -> QTree
fillTree x [] = x
fillTree x (b:rest) = fillTree (addBody x b) rest


-- Compute the force of the quad tree over a body
computeForce :: Double -> QTree -> Phys.Body -> Vec2D
computeForce _ (QE _) b = zeroVec
computeForce _ (QL _ b1) b2 | b1 == b2 = zeroVec
computeForce g (QL _ b1) b2 | b1 /= b2 = force g b2 b1
computeForce g (QN box pmass size tl bl tr br) b@Body{pos=p, mass=_, mom=_} | (box `contains` p) || ((size/(dist pmass p))>=0.5) =
  (computeForce g tl b) + (computeForce g bl b) + (computeForce g tr b) + (computeForce g br b)
computeForce g (QN box@(QB _ size) pmass mass _ _ _ _) Body{pos=p, mass=m, mom=_} =
  let dist = pmass - p
      denom = ((magnetude dist)^3)
  in ((g * mass * m)/denom) .* dist

-- private simulation step
bhSimulate :: Double -> Double -> QTree -> [Body] -> [Body]
bhSimulate _ _ _ [] = []
bhSimulate g dT root (body@Body{pos=p, mass=m, mom=s}:rest) =
  let force = (computeForce g root body)
      acc = force /. m
      speed = s + (dT .* acc)
      pos = p + (dT .* speed)
  in Body{pos=pos, mass=m, mom=speed}:(bhSimulate g dT root rest)

-- create Empty universe rooot node
createRoot :: [Body] -> QTree
createRoot [] = QE (QB zeroVec 0)
createRoot bodies =
  let size = Phys.usize (Phys.b2vs bodies)
      bbox = QB (Vec2D ((-size)-1) ((-size) -1)) (2*size + 2)
  in QE bbox

-- simulate the universe using the BH Algorithm
simulate :: Double -> Double -> [Body] -> [Body]
simulate _ _ [] = []
simulate g dT bodies =
  let root = createRoot bodies
      tree = fillTree root bodies
  in bhSimulate g dT tree bodies
