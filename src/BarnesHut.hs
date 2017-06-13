{-Barnes Hut Algo-}

module BarnesHut (QBox(..)
                 , QTree(..)
                 , fillTree
                 , usize
                 , vecbodies
                 , createRoot
                 , b2vs
                 , BarnesHut.simulate
                 ) where
import Phys

data QBox = QB Vec2D Double deriving (Show, Eq) -- mass center, position, size

-- quad tree
data QTree = QN QBox Vec2D Double QTree QTree QTree QTree -- NODE Box, Mass (sub-nodes)
           | QL QBox Body -- Leaf (pos*mass, mass)
           | QE QBox-- Empty Leaf
        deriving (Show, Eq)


contains :: QBox -> Vec2D -> Bool
contains (QB _ 0) _ = False
contains (QB (Vec2D l t) s) (Vec2D x y) =
  let w = x - l
      h = y - t
  in (w > 0) && (w <= s) && (h > 0) && (h <= s)

b2vs :: [Body] -> [Vec2D]
b2vs [] = []
b2vs (Body{pos=p, mass=m, mom=_}:rest) = p:(b2vs rest)

mdist :: [Vec2D] -> Vec2D -> Double
mdist [] _ = 0
mdist bs b = maximum (map (Phys.dist b) bs)

usize :: [Vec2D] -> Double
usize [] = 0
usize bs = maximum (map (dist zeroVec) bs)


vecbodies :: [Body] -> [Vec2D]
vecbodies [] = []
vecbodies ((Body{pos=x, mass=_, mom=_}):rest) = x : (vecbodies rest)

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

fillTree :: QTree -> [Phys.Body] -> QTree
fillTree x [] = x
fillTree x (b:rest) = fillTree (addBody x b) rest


computeForce :: QTree -> Phys.Body -> Vec2D
computeForce (QE _) b = zeroVec
computeForce (QL _ b1) b2 | b1 == b2 = zeroVec
computeForce (QL _ b1) b2 | b1 /= b2 = force b2 b1
computeForce (QN box _ _ tl bl tr br) b@Body{pos=p, mass=_, mom=_} | (box `contains` p) =
  (computeForce tl b) + (computeForce bl b) + (computeForce tr b) + (computeForce br b)
computeForce (QN box@(QB _ size) pmass mass _ _ _ _) Body{pos=p, mass=m, mom=_} | (not (box `contains` p)) && ((size/(dist pmass p))<0.5) =
  let dist = pmass - p
      denom = ((magnetude dist)^3)
  in ((mass * m)/denom) .* dist
computeForce (QN box@(QB _ size) pmass _ tl bl tr br) b@Body{pos=p, mass=m, mom=_} | (not (box `contains` p)) && ((size/(dist pmass p))>=0.5) =
  (computeForce tl b) + (computeForce bl b) + (computeForce tr b) + (computeForce br b)

bhSimulate :: Double -> Double -> QTree -> [Body] -> [Body]
bhSimulate _ _ _ [] = []
bhSimulate g dT root (body@Body{pos=p, mass=m, mom=s}:rest) =
  let force = g .* (computeForce root body)
      acc = force /. m
      speed = s + (dT .* acc)
      pos = p + (dT .* speed)
  in Body{pos=pos, mass=m, mom=speed}:(bhSimulate g dT root rest)


createRoot :: [Body] -> QTree
createRoot [] = QE (QB zeroVec 0)
createRoot bodies =
  let size = usize (b2vs bodies)
      bbox = QB (Vec2D ((-size)-1) ((-size) -1)) (2*size + 2)
  in QE bbox

simulate :: Double -> Double -> [Body] -> [Body]
simulate _ _ [] = []
simulate g dT bodies =
  let root = createRoot bodies
      tree = fillTree root bodies
  in bhSimulate g dT tree bodies

-- test :: IO()
-- test = do
--   let bs = [Body{pos=(Vec2D 0.2 0.5), mass=1, mom=zeroVec},Body{pos=(Vec2D 0.3 0.2), mass=1, mom=zeroVec}]
--   let tree = fillTree (QE (QB zeroVec 2)) bs
--   print(tree)
