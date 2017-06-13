{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Phys
import ConfLoader
import BarnesHut

import Data.List
import System.IO
import System.Environment

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

green :: SDL.Primitive.Color
green = V4 0 255 0 125


-- render a QBox
renderQBox :: SDL.Renderer -> QBox -> IO()
renderQBox r (QB (Vec2D x y) s)= do
  let plt = V2 (round (x + cx)) (round (y + cy))
  let pbr = V2 (round (x + s + cx)) (round (y + s + cy))
  SDL.Primitive.rectangle r plt pbr green
  return ()

-- render QTree
renderTree :: SDL.Renderer -> QTree -> IO()
renderTree r (QE box) = renderQBox r box
renderTree r (QL box _) = renderQBox r box
renderTree r (QN box _ _ tl bl tr br) = do
  renderQBox r box
  renderTree r tl
  renderTree r bl
  renderTree r tr
  renderTree r br
  return ()


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
    let root = createRoot bs
    let tree = fillTree root bs
    renderTree r tree
    -- show  the scene
    SDL.present r
    -- die or continue!
    if quit then  (return ()) else (animateScene r n rest)




---
--- MAIN ENTRY POINT
---

main :: IO ()
main = do
  -- retrive args
  args <- getArgs
  let cfg_path = if (length args) == 1 then
                    args !! 0
                 else
                    error "Please specify the configuration file!"
  ---
  --- LOAD CONFIGURATION
  ---
  cfg <- loadCfg cfg_path
  -- retrive bodies position
  let initUniverse = bodies cfg

  ---
  --- PURE SIMULATION MAGIC
  ---

  -- infinite simulation baby!
  let simulation = iterate (BarnesHut.simulate (g cfg) (dT cfg)) initUniverse

  -- initialize SDL
  SDL.initialize [SDL.InitVideo]
  -- create Window
  window <- SDL.createWindow "N-Body Simulation" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  -- create Renderer
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- show Windo
  SDL.showWindow window

  -- every N ms display a new step of the simulation
  animateScene renderer 3 simulation
  -- clean everything up
  SDL.destroyWindow window
  SDL.quit



