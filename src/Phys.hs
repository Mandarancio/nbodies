module Phys
  (Vec2D(..)
  , Body(..)
  , (./)
  , (.*)
  , zeroVec
  , simulate
  , magnetude
  , acc
  ) where


data Vec2D = Vec2D Double Double

zeroVec = Vec2D 0 0

instance Show Vec2D where
  show (Vec2D x y) = "v("++(show x)++", "++(show y)++")"

instance Eq Vec2D where
  (==) (Vec2D a b) (Vec2D c d) = (a == c) && (b == d)

instance Num Vec2D where
  (+) (Vec2D a b) (Vec2D c d) = Vec2D (a + c) (b + d)
  (-) (Vec2D a b) (Vec2D c d) = Vec2D (a - c) (b - d)
  (*) (Vec2D a b) (Vec2D c d) = Vec2D (a * c) (b * d)
  abs (Vec2D a b) = Vec2D (abs a) (abs b)
  signum (Vec2D a b) = Vec2D (a/sqrt(a*a + b*b)) (b/sqrt(a*a + b*b))
  fromInteger a = Vec2D (fromInteger a ::Double) 0

(.*) :: Double -> Vec2D -> Vec2D
(.*) x (Vec2D a b) = Vec2D (x*a) (x*b)

(./) :: Vec2D -> Double -> Vec2D
(./) (Vec2D a b) x = Vec2D (a/x) (b/x)

magnetude :: Vec2D -> Double
magnetude (Vec2D a b) = sqrt(a*a + b*b)

data Body = Body {pos::Vec2D,mass::Double,mom::Vec2D}

instance Show Body where
  show (Body{pos=p, mass=m,mom=s}) = "b(p: "++(show p)++", m: "++(show m)++", s: "++(show s)++")"

acc :: Double ->  Body -> Body -> Vec2D
acc g (Body{pos=p1,mass=m1,mom=s1}) (Body{pos=p2,mass=_,mom=_}) | p1 == p2 =  zeroVec
acc g (Body{pos=p1,mass=m1,mom=_}) (Body{pos=p2,mass=m2,mom=_}) =
  let dist = (p1-p2)
  in (g*m2/(magnetude dist)^3) .* dist

computeStep :: Double -> Double -> [Body] -> Body -> Body
computeStep _ _ [] b = b
computeStep g dT bodies b@(Body{pos = p, mass = m, mom = s}) =
  let newSpeed =  dT .* (sum (map (acc g b) bodies)) + s
      newPos = (dT .* newSpeed) + p
  in Body{pos=newPos, mass=m, mom = newSpeed}

simulate :: Double -> Double -> [Body] -> [Body]
simulate g dT bodies = map (computeStep g dT bodies) bodies
