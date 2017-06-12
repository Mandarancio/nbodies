{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Phys

import Data.List
import Data.IORef
import System.IO



import Control.Monad
import Control.Monad.State

import Foreign.C.Types
import SDL.Vect
import SDL.Primitive
import qualified SDL
-- import Control.Monad.IO.Class (MonadIO, liftIO)


screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

cx, cy :: Double
(cx, cy) = (400, 300)

black :: SDL.Primitive.Color
black = V4 0 0 0 255

white :: SDL.Primitive.Color
white = V4 255 255 255 255

count :: State Int Int
count = do
    c <- get
    put (c+1)
    return (c+1)


g = -1
dT = 1e-1


renderAll :: SDL.Renderer -> [Body] -> IO ()
renderAll r [] = do return ()
renderAll r (Body{pos=(Vec2D x y), mass=_, mom=_}:rest) = do
  SDL.Primitive.fillCircle r (V2 (round (x + cx)) (round (y + cy))) 5 white
  SDL.Primitive.smoothCircle r (V2 (round (x + cx)) (round (y + cy))) 5 white
  renderAll r rest
  return ()

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window
  ref <- newIORef [Phys.Body Phys.zeroVec 100 Phys.zeroVec, Phys.Body (Phys.Vec2D 0 100) 0.1 (Phys.Vec2D 1 0),  Phys.Body (Phys.Vec2D 0 200) 0.1 (Phys.Vec2D 0.7 0)]

  let
    loop = do
      events <- SDL.pollEvents
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
      SDL.rendererDrawColor renderer SDL.$= black
      SDL.clear renderer
      -- bodies <- Phys.simulate g dT bodies
      modifyIORef ref (Phys.simulate g dT)
      readIORef ref >>= (renderAll renderer)


      -- SDL.Primitive.fillCircle renderer (V2 100 100) 5 white
      SDL.present renderer

      unless quit loop
  loop

  SDL.destroyWindow window
  SDL.quit

