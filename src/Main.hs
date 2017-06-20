{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Phys
import ConfLoader
import BarnesHut
import Renderer

import Data.List
import System.IO
import System.Environment

import Control.Monad

import qualified SDL

import GHC.Word



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
  let simulation = iterate ((if (bhMode cfg) then BarnesHut.simulate else Phys.simulate) (g cfg) (dT cfg)) initUniverse

  -- initialize SDL
  SDL.initialize [SDL.InitVideo]
  -- create Window
  window <- SDL.createWindow "N-Body Simulation" SDL.defaultWindow { SDL.windowInitialSize = screenSize }
  -- create Renderer
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- show Windo
  SDL.showWindow window

  -- every N ms display a new step of the simulation
  animateScene renderer (scale cfg) (qtree cfg) simulation
  -- clean everything up
  SDL.destroyWindow window
  SDL.quit



