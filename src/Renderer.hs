
---
--- RENDERING FUNCTION AND GRAPHICAL CONSTANTS
---

module Renderer ( screenSize
                , animateScene
                ) where

import Phys
import BarnesHut

import System.IO
import Foreign.C.Types

import SDL.Vect
import SDL.Primitive
import qualified SDL

-- screen information
screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

screenSize = V2 screenWidth screenHeight

cx, cy :: Double
(cx, cy) = (400, 300)


-- few colors
black :: SDL.Primitive.Color
black = V4 0 0 0 255

white :: SDL.Primitive.Color
white = V4 255 255 255 255

green :: SDL.Primitive.Color
green = V4 0 255 0 50


-- render a QBox
renderQBox :: SDL.Renderer -> Double -> QBox -> IO()
renderQBox _ scale (QB _ s) | (s*scale) <= 1  = return ()
renderQBox r scale (QB (Vec2D x y) s)= do
  let plt = V2 (round (x * scale + cx)) (round (y * scale + cy))
  let pbr = V2 (round (x * scale + s * scale + cx)) (round (y * scale + s *scale + cy))
  SDL.Primitive.rectangle r plt pbr green
  return ()

-- render QTree
renderTree :: SDL.Renderer -> Double -> QTree -> IO()
renderTree r scale (QE box) = renderQBox r scale box
renderTree r scale (QL box _) = renderQBox r scale box
renderTree r scale (QN box _ _ tl bl tr br) = do
  renderQBox r scale box
  renderTree r scale tl
  renderTree r scale bl
  renderTree r scale tr
  renderTree r scale br
  return ()

-- create and render tree
renderDebugTree :: SDL.Renderer -> Double -> Bool -> [Body] -> IO ()
renderDebugTree _ _ False _ = return ()
renderDebugTree r scale True bs = do
  let root = createRoot bs
  let tree = fillTree root bs
  renderTree r scale tree


-- draw cross function
xcross :: SDL.Renderer -> V2 CInt -> Color -> IO ()
xcross r (V2 x y) color= do
  SDL.Primitive.pixel r (V2 x y) color
  SDL.Primitive.pixel r (V2 (x + 1) y) color
  SDL.Primitive.pixel r (V2 (x - 1) y) color
  SDL.Primitive.pixel r (V2 x (y + 1)) color
  SDL.Primitive.pixel r (V2 x (y - 1)) color
  return ()

-- function to render all bodies
renderBodies :: SDL.Renderer -> Double -> [Body] -> IO ()
renderBodies _ _ [] = do return ()
renderBodies r scale (Body{pos=(Vec2D x y), mass=mass, mom=_}:rest) | (5*(mass/100)) > 1 = do
  let center = (V2 (round (x * scale + cx)) (round (y * scale + cy)))
  let ratio = (5*(mass/100))
  (SDL.Primitive.fillCircle r center (round ratio) white)
  (SDL.Primitive.circle r center (round ratio) white)
  renderBodies r scale rest
renderBodies r scale (Body{pos=(Vec2D x y), mass=mass, mom=_}:rest) = do
  let center = (V2 (round (x * scale + cx)) (round (y * scale + cy)))
  xcross r center white
  renderBodies r scale rest


-- animate the scene
animateScene :: SDL.Renderer ->  Double -> Bool -> [[Body]] -> IO ()
animateScene _ _ _ [] = do return ()
animateScene r scale qtree (bs:rest) = do
    events <- SDL.pollEvents
    -- check if quit action performed
    let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
    -- clean scene
    SDL.rendererDrawColor r SDL.$= black
    SDL.clear r

    -- check rescale factor
    let rescale = if scale <= 0 then (cy/(usize (b2vs bs))) else scale

    -- display Quad tree  *if needed*
    renderDebugTree r rescale qtree bs
    -- display all bodies
    renderBodies r rescale bs

    -- show  the scene
    SDL.present r
    -- die or continue!
    if quit then  (return ()) else (animateScene r scale qtree rest)


