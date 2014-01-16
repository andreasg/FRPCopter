{-# LANGUAGE FlexibleContexts #-}
module Main where

import Linear
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import Control.Wire
import Prelude hiding (id, (.), until, ceiling, floor)
import FRP.Netwire

import Extra

--------------------------------------------------------------------------------
-- params
--------------------------------------------------------------------------------
screenW, screenH, scrollSpeed :: Num a => a
screenW = 800
screenH = 600
scrollSpeed = 240
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- data
--------------------------------------------------------------------------------
type Wire' a b = (HasTime t s,  Monad m, Fractional t, Monoid s) => Wire s () m a b

type Game = (V2 Double, Double, ([Rect], [Rect]))

data Assets = Assets { heli :: SDL.Surface, wallColor :: SDL.Pixel, smoke :: SDL.Surface }

data Rect = Rect (V2 Double) (V2 Double)

mkRect :: Double -> Double -> Double -> Double -> Rect
mkRect x y w h = Rect (V2 x y) (V2 w h)

unRect :: Rect -> ((Double, Double), (Double, Double))
unRect (Rect (V2 x y) (V2 w h)) = ((x,y), (w, h))

contains :: V2 Double -> Rect -> Bool
contains (V2 x y) (Rect (V2 x' y') (V2 w h)) =
    x'    <= x && x <= (x'+w)
    && y' <= y && y <= (y'+h)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--  level generation
--------------------------------------------------------------------------------
events :: (Double, Double) -> Wire' a (Event Double, Event Double)
events interv = stdNoiseR 1 interv 1234 &&& periodic 1 . (scroll+screenW)

ceiling :: Wire' a [Rect]
ceiling = obst (\(y,x) -> mkRect x 0 scrollSpeed y)  (0, 200)

floor :: Wire' a [Rect]
floor = obst (\(y,x) -> mkRect x y scrollSpeed 200)  (400, 600)

obst :: ((Double,Double) -> Rect) -> (Double, Double) -> Wire' a [Rect]
obst toRect interv =  arr (map toRect . uncurry zip )
                      . (second (hold . accumList) . first (hold . accumList))
                      . events interv

accumList :: Wire' (Event a) (Event [a])
accumList = accumE (flip (:)) []

level :: Wire' a ([Rect], [Rect])
level  = ceiling &&& floor
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- input events
--------------------------------------------------------------------------------
keyDown :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyDown k = became $ \e -> case e of
  SDL.KeyDown (SDL.Keysym key _ _) -> k == key; _ -> False

keyUp :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyUp k = became $ \e -> case e of
  SDL.KeyUp (SDL.Keysym key _ _) -> k == key; _ -> False

command :: (HasTime t s, Monoid e, Monad m) => SDL.SDLKey -> Wire s e m SDL.Event ()
command k = between . arr (\(on,off) -> ((), on, off)) . (keyDown k &&& keyUp k)

up :: Wire' SDL.Event ()
up = command SDL.SDLK_SPACE
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- positioning
--------------------------------------------------------------------------------
velocity :: Wire'  SDL.Event (V2 Double)
velocity = integral (V2 scrollSpeed 100) . pure (V2 0 800) . until . pressSpace
           --> pure (V2 scrollSpeed (-100)) . up --> velocity
  where pressSpace = pure () &&& keyDown SDL.SDLK_SPACE

position :: Wire' SDL.Event (V2 Double)
position = integral (V2 200 200) . velocity
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- game
--------------------------------------------------------------------------------
scroll :: Fractional t => Wire' a t
scroll = arr realToFrac . time*scrollSpeed

isColliding :: Wire' (V2 Double, [Rect]) Bool
isColliding = arr $ \(r, rs) -> any (contains r) rs

game :: Wire' SDL.Event Game
game = proc e -> do
  (camera, playerPos) <- scroll &&& position -< e
  (c,f)               <- level  -< ()
  collide             <- isColliding -< (playerPos, (c++f))
  when (==False) --> inhibit () -< collide
  returnA -< (playerPos,  camera, (c,f))
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
  where go as scr s w = do
          e <- SDL.pollEvent
          (ds, s') <- stepSession s
          (res, w') <- stepWire w ds (Right e)
          case res of
            Left _ -> putStrLn "qutting"
            Right gm -> do render scr as gm
                           SDL.flip scr
                           go as scr s' w'

render :: SDL.Surface -> Assets -> Game -> IO ()
render screen assets (playerPos, dx, (c,f)) = do
    clearScreen
    renderLevel
    renderPlayer
--    renderParticles
  where
--    renderParticles =
--      CM.forM_ ps $ \ (SmokePuff (V2 x y)) -> do
--        (SDL.Rect _ _ w h) <- SDL.getClipRect (smoke assets)
--        SDL.blitSurface (smoke assets) Nothing screen (Just $ SDL.Rect (round (x - dx) - w) (round y) w h)
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



--------------------------------------------------------------------------------
-- Extra
--------------------------------------------------------------------------------
smokeTrail :: Wire' (V2 Double) [Particle]
smokeTrail = manageWires . periodic (0.09) . arr (\x -> for 0.3 . pure (SmokePuff x))


animation :: Int -> Double -> Wire' a Int
animation n fps = hold . periodicList (1 / realToFrac fps) (cycle [0..n-1])
