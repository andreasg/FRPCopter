{-# LANGUAGE FlexibleContexts #-}
module Main where

import Linear
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import Control.Wire
import Prelude hiding (id, (.), until, ceiling, floor)
import FRP.Netwire


--------------------------------------------------------------------------------
-- data
--------------------------------------------------------------------------------
type Wire' a b = (HasTime t s,  Monad m, Fractional t, Monoid s) => Wire s () m a b

type Game = (V2 Double, Double, ([Rect], [Rect]))

data Assets = Assets { heli :: SDL.Surface, wallColor :: SDL.Pixel }

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
-- params
--------------------------------------------------------------------------------
screenW, screenH, scrollSpeed :: Num a => a
screenW = 800
screenH = 600
scrollSpeed = 240
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--  level generation
--------------------------------------------------------------------------------
events :: (Double, Double) -> Wire' a (Event Double, Event Double)
events interv = stdNoiseR 1 interv 1234 &&& periodic 1 . (arr realToFrac) . (scrollSpeed*time+screenW)

ceiling :: Wire' a [Rect]
ceiling = obst (\(y,x) -> mkRect x 0 scrollSpeed y)  (0, 200)

floor :: Wire' a [Rect]
floor = obst (\(y,x) -> mkRect x y scrollSpeed 200)  (400, 600)

obst :: ((Double,Double) -> Rect) -> (Double, Double) -> Wire' a [Rect]
obst f interv =  arr (map f . uncurry zip ) . (second (hold . accumE (flip (:)) []) . first (hold . accumE (flip (:)) []) . events interv)

levelW :: Wire' a ([Rect], [Rect])
levelW  = ceiling &&& floor 
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

upAction :: (Monoid e, Monad m) => Wire s e m SDL.Event (Event SDL.Event)
upAction = keyDown SDL.SDLK_SPACE

quit :: (Monoid e, Monad m) => Wire s e m SDL.Event SDL.Event
quit = hold . keyDown  SDL.SDLK_ESCAPE

up :: Wire' SDL.Event ()
up = command SDL.SDLK_SPACE
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- positioning
--------------------------------------------------------------------------------
upVec :: Wire' SDL.Event (V2 Double)
upVec = pure (V2 0 (-300)) . up <|> pure (V2 0 0)

gravityVector :: Wire'  SDL.Event (V2 Double)
gravityVector = mkPureN $ \_ -> do
  (Right (V2 0 0), (integral (V2 0 100) . pure (V2 0 100) . (until . (pure () &&& upAction)))
                   --> pure (V2 0 0) . up --> gravityVector)

velocity ::  Wire' SDL.Event (V2 Double)
velocity = proc cmd -> do
  up' <- upVec -< cmd
  returnA -< (V2 scrollSpeed 0) ^+^ up'

position :: Wire' SDL.Event (V2 Double)
position = integral (V2 200 200) . (arr (uncurry (^+^))) . (gravityVector  &&& velocity)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- game
--------------------------------------------------------------------------------
scroll :: Fractional t => Wire' a t
scroll = arr realToFrac . (time*scrollSpeed)

isColliding :: Arrow a => a (V2 Double, [Rect]) Bool
isColliding = arr $ \(r, rs) -> any (contains r) rs

game :: Wire' SDL.Event Game
game = proc e -> do
  playerPos <- position -< e
  camera    <- scroll -< e
  (c,f)     <- levelW -< ()
  collide   <- isColliding -< (playerPos, (c++f))
  first (when (==False)) --> second stopGame -< (collide, camera)
  returnA -< (playerPos,  camera, (c,f))
  where stopGame = mkPureN $ \_ -> (Left (), mkConst (Left ()))
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------
loadAssets :: SDL.Surface -> IO Assets
loadAssets screen = do
  h <- IMG.load "assets/heli.png"
  wall <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 80 40 30 255
  return $ Assets h wall

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
  CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 180 87 40 >>= SDL.fillRect screen Nothing
  renderLevel
  renderPlayer

  where
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
