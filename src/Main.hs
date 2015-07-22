{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Linear
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import Control.Wire
import Prelude hiding (id, (.), until, ceiling, floor)
import FRP.Netwire

import Extra (manageWires)


---------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------
screenW, screenH, scrollSpeed :: Num a => a
screenW = 800
screenH = 600
scrollSpeed = 240
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Data Types
data Assets = Assets {heli :: SDL.Surface 
                     ,wallColor :: SDL.Pixel
                     ,smoke :: SDL.Surface}

data Particle = SmokePuff (V2 Double)

-- Tuple of Game is basically a snapshot in time of our scene.
-- The render function below knows how to take such a tuple and 
-- put it on the screen.
type Game =
     (V2 Double,       -- player position 
      Double,          -- camera x-coordinate (scroll)
      ([Rect], [Rect]),-- level (ceiling, floor)
      [Particle]
      )
      

-- Simplified type sig for Netwire Wire
type Wire' a b = forall s. forall m. forall t. 
              (HasTime t s, Monad m, Fractional t, Monoid s) 
              => Wire s () m a b
--------------------------------------------------------------------------------              


--------------------------------------------------------------------------------
-- Rectangles
--------------------------------------------------------------------------------
data Rect = Rect (V2 Double) (V2 Double)

mkRect :: Double -> Double -> Double -> Double -> Rect
mkRect x y w h = Rect (V2 x y) (V2 w h)

unRect :: Rect -> ((Double, Double), (Double, Double))
unRect (Rect (V2 x y) (V2 w h)) = ((x,y),(w,h))


contains :: V2 Double -> Rect -> Bool
contains (V2 px py) r = 
         let ((x,y),(w,h)) = unRect r
         in x <= px && px <= (x+w)
         && y <= py && py <= (y+h)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Camera movement
--------------------------------------------------------------------------------
scroll :: Fractional t => Wire' a t
scroll = arr realToFrac . time * scrollSpeed
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Level generation
--------------------------------------------------------------------------------
accumList :: Wire' (Event a) (Event [a])
accumList = accumE (flip (:)) []

rects :: (Double, Double) -> (Double -> Double -> Rect) -> Wire' a [Rect]
rects interval toRect = hold . accumList . rect toRect . stdNoiseR 1 interval rndSeed
  where rndSeed = 1234

rect :: (Double -> Double -> Rect) -> Wire' (Event Double) (Event Rect)
rect toRect = proc ed -> do
     f <- arr toRect . (scroll+screenW) -< ()
     returnA -< fmap f ed

level :: Wire' a ([Rect], [Rect])
level = ceiling &&& floor
  where ceiling = rects (0, 200) (\x h -> mkRect x 0 scrollSpeed h)
        floor   = rects (400, 600) (\x y -> mkRect x y scrollSpeed 200)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Keyboard input
--------------------------------------------------------------------------------
keyDown :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyDown k = became $ \e -> case e of
   SDL.KeyDown (SDL.Keysym keysym _ _) -> keysym == k; _ -> False
   
keyUp :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyUp k = became $ \e -> case e of
   SDL.KeyUp (SDL.Keysym keysym _ _) -> keysym == k; _ -> False
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Velocity and player positioning
--------------------------------------------------------------------------------
velocity :: Wire' SDL.Event (V2 Double)
velocity = fall . until . pressSpace
           --> goUp . until . releaseSpace
           --> velocity
 where 
 fall = integral (V2 scrollSpeed 100) . pure (V2 0 800)
 pressSpace = pure () &&& keyDown SDL.SDLK_SPACE
 goUp = pure (V2 scrollSpeed (-200))
 releaseSpace = pure () &&& keyUp SDL.SDLK_SPACE


position :: Wire' SDL.Event (V2 Double)
position = integral (V2 300 300) . velocity 
  

isColliding :: Wire' (V2 Double, ([Rect],[Rect])) Bool
isColliding = arr (\(p, (c, f)) -> any (contains p) (c++f))


smokeTrail :: Wire' (V2 Double) [Particle]
smokeTrail = manageWires . periodic 0.09 . arr (\p -> for 0.5 . pure (SmokePuff p))
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Game Wire, when this inhibits, game is over
--------------------------------------------------------------------------------
game :: Wire' SDL.Event Game
game = proc e -> do
     playerPos <- position -< e
     cameraPos <- scroll -< ()
     world <- level -< ()
     collide <- isColliding -< (playerPos, world)
     when(==False) -< collide
     ps <- smokeTrail -< playerPos
     returnA -< (playerPos, cameraPos, world, ps)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------
loadAssets :: SDL.Surface -> IO Assets
loadAssets screen = do
  h <- IMG.load "assets/heli.png"
  c <- IMG.load "assets/cloud.png"  
  wall <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 80 40 30 255
  return $ Assets h wall c


main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  SDL.setCaption "FRPCopter" ""
  screen <- SDL.setVideoMode screenW screenH 32 [SDL.HWSurface]
  as <- loadAssets screen
  go as screen clockSession_ game
  SDL.quit
  
  -- go is our "main loop"
  where go as scr s w = do
     
          -- get system events (if any)
          e <- SDL.pollEvent
          
          -- see call to go above, but our value `s` here is our time (clockSession_)
          -- this is the call that figures out how long (seconds) since the last call
          -- to stepSession, hence we can get a `ds` which is our time delta, as well
          -- as `s'`, the current time.
          (ds, s') <- stepSession s
          
          -- Since Wires are locally stateful, we can "step" the `game` wire using our
          -- `ds` (again, no need to pass `s` since `game` "knows where it is" in time)
          -- `w` is the game wire (see first call to `go` in `main`).
          (res, w') <- stepWire w ds (Right e)

          -- We inspect the result of stepping the wire, if Left this indicates that
          -- we had inhibition, and we decide to quit the game.
          -- If right, we render the Game tuple, and loop back to `go`, using our new
          -- session (current time) `s'` as well as the updated ("stepped") game wire
          -- `w'` that we got from `stepWire`.
          case res of
            Left _ -> putStrLn "qutting"
            Right gm -> do render scr as gm
                           SDL.flip scr
                           go as scr s' w'

render :: SDL.Surface -> Assets -> Game -> IO ()
render screen assets (playerPos, dx, (c,f), ps) = do
    clearScreen
    renderLevel
    renderPlayer
    renderParticles
  where
    renderParticles =
      CM.forM_ ps $ \ (SmokePuff (V2 x y)) -> do
        (SDL.Rect _ _ w h) <- SDL.getClipRect (smoke assets)
        SDL.blitSurface (smoke assets) Nothing screen (Just $ SDL.Rect (round (x - dx) - w) (round y) w h)
    clearScreen = 
      SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 180 87 40 >>=
      SDL.fillRect screen Nothing
    rectToSDLRect p =
      let ((x,y), (w, h)) = unRect p
      in SDL.Rect (round (x - dx)) (round y) (round w) (round h)
    blitRects rs fill = do
      CM.forM_ (map rectToSDLRect rs) $ \r -> do
        flip (SDL.fillRect screen) fill (Just r)
    renderPlayer = do
      let (V2 px py) = playerPos
      r@(SDL.Rect _ _ w h) <- SDL.getClipRect (heli assets)
      CM.void $
        SDL.blitSurface  ((heli assets)) (Just r) screen
          (Just $ SDL.Rect (round (px-dx) ) (round py ) w h)
    renderLevel = do
      blitRects c (wallColor assets)
      blitRects f (wallColor assets)
--------------------------------------------------------------------------------
