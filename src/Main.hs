{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Phys

import Data.List
import System.IO

import Control.Monad

import Foreign.C.Types
import SDL.Vect
import SDL.Primitive
import qualified SDL

import GHC.Word

---
--- RENDERING FUNCTION AND GRAPHICAL CONSTANTS
---

-- screen information
screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

cx, cy :: Double
(cx, cy) = (400, 300)


-- few colors
black :: SDL.Primitive.Color
black = V4 0 0 0 255

white :: SDL.Primitive.Color
white = V4 255 255 255 255


-- function to render all bodies
renderBodies :: SDL.Renderer -> [Body] -> IO ()
renderBodies r [] = do return ()
renderBodies r (Body{pos=(Vec2D x y), mass=mass, mom=_}:rest) = do
  let ratio = round (5*(mass/100))
  let radius = if  ratio > 0 then ratio else 1
  SDL.Primitive.fillCircle r (V2 (round (x + cx)) (round (y + cy))) radius white
  SDL.Primitive.circle r (V2 (round (x + cx)) (round (y + cy))) radius white
  renderBodies r rest
  return ()

-- animate the scene
animateScene :: SDL.Renderer -> GHC.Word.Word32 -> [[Body]] -> IO ()
animateScene _ _ [] = do return ()
animateScene r n (bs:rest) = do
    -- wait n ms
    SDL.delay n
    events <- SDL.pollEvents
    -- check if quit action performed
    let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
    -- clean scene
    SDL.rendererDrawColor r SDL.$= black
    SDL.clear r
    -- display all bodies
    renderBodies r bs
    -- show  the scene
    SDL.present r
    -- die or continue!
    if quit then  (return ()) else (animateScene r n rest)


---
--- PURE SIMULATION MAGIC
---

-- initial univese status
initUniverse :: [Body]
initUniverse = [Phys.Body Phys.zeroVec 100 Phys.zeroVec,
                Phys.Body (Phys.Vec2D 0 100) 0.1 (Phys.Vec2D 1 0),
                Phys.Body (Phys.Vec2D 0 200) 0.1 (Phys.Vec2D 0.7 0),
                Phys.Body (Phys.Vec2D 0 300) 0.1 (Phys.Vec2D 0.5 0)
               ]

-- Gravitational Constant
g = 1
-- Simulation interval
dT = 2e-1

-- infinite simulation
simulation = iterate (Phys.simulate g dT) initUniverse


---
--- MAIN ENTRY POINT
---

main :: IO ()
main = do
  -- initialize SDL
  SDL.initialize [SDL.InitVideo]
  -- create Window
  window <- SDL.createWindow "N-Body Simulation" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  -- create Renderer
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- show Windo
  SDL.showWindow window

  -- every N ms display a new step of the simulation
  animateScene renderer 2 simulation
  -- clean everything up
  SDL.destroyWindow window
  SDL.quit



