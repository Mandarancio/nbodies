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
renderAll :: SDL.Renderer -> [Body] -> IO ()
renderAll r [] = do return ()
renderAll r (Body{pos=(Vec2D x y), mass=_, mom=_}:rest) = do
  SDL.Primitive.fillCircle r (V2 (round (x + cx)) (round (y + cy))) 5 white
  SDL.Primitive.smoothCircle r (V2 (round (x + cx)) (round (y + cy))) 5 white
  renderAll r rest
  return ()


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

  -- initialize bodies
  bodies <- newIORef [Phys.Body Phys.zeroVec 100 Phys.zeroVec, Phys.Body (Phys.Vec2D 0 100) 0.1 (Phys.Vec2D 1 0),  Phys.Body (Phys.Vec2D 0 200) 0.1 (Phys.Vec2D 0.7 0)]
  -- main loop
  let
    loop = do
      -- retrive events
      events <- SDL.pollEvents
      -- check if quit action performed
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

      -- clear screen
      SDL.rendererDrawColor renderer SDL.$= black
      SDL.clear renderer

      -- update bodies position and speed
      modifyIORef bodies (Phys.simulate g dT)
      -- render bodies
      readIORef bodies >>= (renderAll renderer)

      -- display renderer
      SDL.present renderer
      unless quit loop
  loop

  -- delete and quit
  SDL.destroyWindow window
  SDL.quit

