{-Barnes Hut Algo-}

module BarnesHut () where
import Phys
-- quad tree
data QTree = QN Vec2D Double Double QTree QTree QTree QTree -- NODE
           | QL Vec2D Double
           | QE

-- empty tree
eQTree = QN (Vec2D 0 0) 0 0 QE QE QE QE


b2vs :: [Body] -> [Vec2D]
b2vs [] = []
b2vs (Body{pos=p, mass=m, mom=_}:rest) = p:(b2vs rest)

mdist :: [Vec2D] -> Vec2D -> Double
mdist [] _ = 0
mdist bs b = maximum (map (Phys.dist b) bs)

usize :: [Vec2D] -> Double
usize [] = 0
usize bs = maximum (map (mdist bs) bs)

-- addBody :: Body -> QTree -> QTree
-- addBody Phys.Body{pos=p ,mass=m ,mom=_} QE = QL (p *. m) m
-- addBody Phys.Body{pos=p ,mass=m ,mom=_} (QL qmp qm) =
--   let tm = m + qm
--       qp = qmp /. qm
--       size = Phys.magnetude (p-qp)
--   in QN nmp tm size QE QE (QL
-- data BHT = Node Vec2D Double Double [Node]
--          | Leaf Vec2D Double Double [Phys.Body]


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
