{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Phys

import Data.List
import Data.IORef
import System.IO

import Control.Monad

import Foreign.C.Types
import SDL.Vect
import SDL.Primitive
import qualified SDL

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

-- Gravitational Constant
g = 1
-- Simulation interval
dT = 1e-1

-- function to render all bodies
renderBodies :: SDL.Renderer -> [Body] -> IO ()
renderBodies r [] = do return ()
renderBodies r (Body{pos=(Vec2D x y), mass=mass, mom=_}:rest) = do
  let ratio = round (5*(mass/100))
  let radius = if  ratio > 0 then ratio else 1
  SDL.Primitive.fillCircle r (V2 (round (x + cx)) (round (y + cy))) radius white
  SDL.Primitive.smoothCircle r (V2 (round (x + cx)) (round (y + cy))) radius white
  renderBodies r rest
  return ()


-- function to render the scene (clean, render body)
renderScene :: SDL.Renderer -> [Body] -> IO ()
renderScene _ [] = do return ()
renderScene r bs = do
    -- clean scene
    SDL.rendererDrawColor r SDL.$= black
    SDL.clear r
    -- display all bodies
    renderBodies r bs
    -- show scene
    SDL.present r
    return ()


-- initial univese status
initUniverse :: [Body]
initUniverse = [Phys.Body Phys.zeroVec 100 Phys.zeroVec,
                Phys.Body (Phys.Vec2D 0 100) 0.1 (Phys.Vec2D 1 0),
                Phys.Body (Phys.Vec2D 0 200) 0.1 (Phys.Vec2D 0.7 0),
                Phys.Body (Phys.Vec2D 0 300) 0.1 (Phys.Vec2D 0.5 0)
               ]

-- infinit simulation
simulation = iterate  (Phys.simulate g dT . (Phys.simulate g dT)) initUniverse


-- Entry point
main :: IO ()
main = do
  -- initialize SDL
  SDL.initialize [SDL.InitVideo]
  -- create Window
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  -- create Renderer
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- show Windo
  SDL.showWindow window

  -- prepare to render all infinite simulation
  let displayedStates = fmap (renderScene renderer) simulation

  -- every 10ms display a new step of the simulation
  sequence_ $ intersperse (SDL.delay 10) displayedStates



