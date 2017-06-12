{-Barnes Hut Algo-}

module BarnesHut () where
import Phys

data QBox = QB Vec2D Vec2D Double deriving (Show, Eq) -- mass center, position, size

-- quad tree
data QTree = QN QBox Double QTree QTree QTree QTree -- NODE Box, Mass (sub-nodes)
           | QL QBox Body -- Leaf (pos*mass, mass)
           | QE QBox-- Empty Leaf
        deriving (Show, Eq)


contains :: QBox -> Vec2D -> Bool
contains (QB _ _ 0) _ = False
contains (QB _ (Vec2D l t) s) (Vec2D x y) =
  let w = x - l
      h = y - t
  in (w > 0) && (w < s) && (h > 0) && (h < s)

b2vs :: [Body] -> [Vec2D]
b2vs [] = []
b2vs (Body{pos=p, mass=m, mom=_}:rest) = p:(b2vs rest)

mdist :: [Vec2D] -> Vec2D -> Double
mdist [] _ = 0
mdist bs b = maximum (map (Phys.dist b) bs)

usize :: [Vec2D] -> Double
usize [] = 0
usize bs = maximum (map (mdist bs) bs)


addBody :: QTree -> Phys.Body -> QTree
addBody (QE box@(QB _ lt s)) body@(Phys.Body{pos=p, mass=m, mom=_}) | box `contains` p =
  QL (QB (m.*p) lt s) body
addBody node@(QE box)  (Phys.Body{pos=p, mass=_, mom=_}) | not (box `contains` p) = node
addBody node@(QL box _) (Phys.Body{pos=p, mass=_, mom=_}) | not (box `contains` p) = node
addBody (QL box@(QB _ lt s) body') body@(Phys.Body{pos=p, mass=m, mom=_}) | box `contains` p =
  let ns = s/2
      tln = addBody (addBody (QE (QB zeroVec lt ns)) body') body
      bln = addBody (addBody (QE (QB zeroVec (lt + (Vec2D 0 ns)) ns)) body') body
      trn = addBody (addBody (QE (QB zeroVec (lt + (Vec2D ns 0)) ns)) body') body
      brn = addBody (addBody (QE (QB zeroVec (lt + (Vec2D ns ns)) ns)) body') body
      totalmass = m + (mass body')
      avg_pos = ((m .* p) + ((mass body') .* (pos body'))) /. totalmass
  in QN (QB avg_pos lt s) totalmass tln bln trn brn
addBody node@(QN box _ _ _ _ _) Phys.Body{pos=p, mass=_, mom=_} | not (box `contains` p) = node
addBody (QN box@(QB mc lt s) mass tl bl tr br) body@(Phys.Body{pos=p, mass=m, mom=_}) | (box `contains` p) =
  let total_mass = mass + m
      nmc = mc + (((m .* p) - (mass .* mc)) /. total_mass)
      ntl = addBody tl body
      nbl = addBody bl body
      ntr = addBody tr body
      nbr = addBody br body
  in QN (QB nmc lt s) total_mass ntl nbl ntr nbr

-- -- createTree :: [Phys.Node] -> BHT -> BHT
-- createTree [] node = node
-- createTree (b@Body{pos=p, mass=m, mom=_}:rest) (Node pos mass size []) =
  -- createTree rest (Leaf pos (mass+m) (magnetude (p-pos)) [b])
-- createTree (b@Body{pos=p, mass=m, mom=_}:rest) (Leaf pos mass size (a:[]))) =
  -- let dist_bl = magnetude (p - pos)
      -- new_size = if dist_bl > size then size else dist_bl
  -- in createTree rest (Leaf pos (mass+m) new_size [a, b])
-- createTree (b@Body{pos=p, mass=m, mom_}:rest) (Leaf pos mass size (a:b:[])) =
  -- let dist_bl = magnetude (p - pos)
  -- in createTree rest (Node pos (mass+m) new_size [leaf_a, leaf_b])
-- createTree ()
