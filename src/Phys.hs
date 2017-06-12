{-Simple Physics module.-}

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

-- 2D Vector
data Vec2D = Vec2D Double Double deriving (Show, Eq)

-- null Vector
zeroVec = Vec2D 0 0

-- basic numerical operations between Vector
instance Num Vec2D where
  (+) (Vec2D a b) (Vec2D c d) = Vec2D (a + c) (b + d)
  (-) (Vec2D a b) (Vec2D c d) = Vec2D (a - c) (b - d)
  (*) (Vec2D a b) (Vec2D c d) = Vec2D (a * c) (b * d)
  abs (Vec2D a b) = Vec2D (abs a) (abs b)
  signum (Vec2D a b) = Vec2D (signum a) (signum b)
  fromInteger a = Vec2D (fromInteger a ::Double) (fromInteger a ::Double)

-- scalar vector operations
-- multiplication double vector
(.*) :: Double -> Vec2D -> Vec2D
(.*) x (Vec2D a b) = Vec2D (x*a) (x*b)

-- multiplication vector double
(*.) = flip (*.)

-- division double vector
(./) :: Double -> Vec2D -> Vec2D
(./) x (Vec2D a b) = Vec2D (x/a) (x/b)

-- division vector double
(/.) :: Vec2D -> Double -> Vec2D
(/.) (Vec2D a b) x = Vec2D (a/x) (b/x)

-- vector magnetude
magnetude :: Vec2D -> Double
magnetude (Vec2D a b) = sqrt(a*a + b*b)

-- celestial body type
data Body = Body {pos::Vec2D,mass::Double,mom::Vec2D} deriving Show

-- compute the acceleration between 2 bodies
acc :: Double ->  Body -> Body -> Vec2D
acc g (Body{pos=p1,mass=m1,mom=s1}) (Body{pos=p2,mass=_,mom=_}) | p1 == p2 =  zeroVec
acc g (Body{pos=p1,mass=m1,mom=_}) (Body{pos=p2,mass=m2,mom=_}) =
  let dist = (p2-p1)
  in (g*m2/(magnetude dist)^3) .* dist

-- compute new position and speed of a body in a certain universe of [Body]
computeStep :: Double -> Double -> [Body] -> Body -> Body
computeStep _ _ [] b = b
computeStep g dT bodies b@(Body{pos = p, mass = m, mom = s}) =
  let newSpeed =  (dT .* (sum (map (acc g b) bodies))) + s
      newPos = (dT .* newSpeed) + p
  in Body{pos=newPos, mass=m, mom = newSpeed}

-- simulate an iteration of a certain universe of [Body]
simulate :: Double -> Double -> [Body] -> [Body]
simulate g dT bodies = map (computeStep g dT bodies) bodies
