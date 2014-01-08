{-# LANGUAGE FlexibleContexts #-}

module Main where

import FRPCopter.Types
import FRPCopter.Level

import Linear
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLPrim
import qualified Graphics.UI.SDL.Image as IMG
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as CMR
import Control.Wire
import Prelude hiding (id, (.), until)
import FRP.Netwire
import Control.Monad.Random


--------------------------------------------------------------------------------
defaultGameParams :: GameParams
defaultGameParams = GameParams {
    scrollSpeed = 240
  , segmentLength = (0.05,0.7)
  , ceilingRange = (0, screenH / 3)
  , floorRange = (screenH - (screenH / 3),  screenH)
  , outOfRangeLimit = 100
  , obsticleRange = (screenH / 3, screenH - (screenH / 3))
  , obsticleHeights = (10, 60)
  , scrH = screenH
  , scrW = screenW
  , bpp = 32
  , accel = 240
  , initGravity = 200
  , gravityForce = 2500
  }
  where
    screenW :: Num a => a
    screenW = 800
    screenH :: Num a => a
    screenH = 600


--------------------------------------------------------------------------------
keyDown :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyDown k = became $ \e -> case e of
  SDL.KeyDown (SDL.Keysym key _ _) -> k == key; _ -> False

keyUp :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyUp k = became $ \e -> case e of
  SDL.KeyUp (SDL.Keysym key _ _) -> k == key; _ -> False

command :: (Monoid e, Monad m) => SDL.SDLKey -> Wire s e m SDL.Event ()
command k = between . arr (\(on,off) -> ((), on, off)) . (keyDown k &&& keyUp k)

upAction :: (Monoid e, Monad m) => Wire s e m SDL.Event (Event SDL.Event)
upAction = keyDown SDL.SDLK_UP <& keyDown SDL.SDLK_SPACE

quit :: (Monoid e, Monad m) => Wire s e m SDL.Event ()
quit = command SDL.SDLK_ESCAPE

up :: (Monoid e, Monad m) => Wire s e m SDL.Event ()
up = command SDL.SDLK_UP <|> command SDL.SDLK_SPACE

westVec :: (MonadReader GameParams m, Monoid e) => Wire s e m SDL.Event (V2 Double)
westVec = directionVec (\a -> V2 (-a) 0) (command SDL.SDLK_LEFT)

eastVec :: (MonadReader GameParams m, Monoid e) => Wire s e m SDL.Event (V2 Double)
eastVec = directionVec (\a -> V2 a 0) (command SDL.SDLK_RIGHT)


upVec :: (MonadReader GameParams m, Monoid e) => Wire s e m SDL.Event (V2 Double)
upVec = directionVec (\a -> V2 0 (-a)) up

directionVec :: (MonadReader GameParams m, Monoid e) =>
     (Double -> V2 Double) -> Wire s e m a b -> Wire s e m a (V2 Double)
directionVec toV c = mkGenN $ \_ -> do
  gp <- CMR.ask
  return (Right neutral, pure (toV (accel gp)) . c <|> pure neutral)
  where neutral = V2 0 0

--------------------------------------------------------------------------------
main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  SDL.setCaption "FRPCopter" ""
  let gp = defaultGameParams
  screen <- SDL.setVideoMode (scrW gp) (scrH gp) (bpp gp) [SDL.HWSurface]
  g <- getStdGen
  heli <- IMG.load "assets/heli.png"
  (SDL.Rect _ _ w h)  <- SDL.getClipRect heli
  go g screen clockSession_ (game (V2 (fromIntegral w) (fromIntegral h))) heli
  SDL.quit
  where
  go g screen s w heli = do
    evt <- SDL.pollEvent
    (ds, s') <- stepSession s
    let ((game', w'), g') = runIdentity
                             . flip runRandT g
                             . flip CMR.runReaderT defaultGameParams
                             . runGameState
                             $ stepWire w ds (Right evt)
    case game' of
      Left  _      -> go g' screen s' w' heli
      Right Ending -> putStrLn "quitting"
      Right stuff@(Running (_,_,(V2 px _)) running) -> do
        clear screen
        render screen heli stuff
        SDL.flip screen
        if running
          then go g' screen s' w' heli
          else do putStrLn ("game over" :: String)
                  putStrLn ("distance covered: " ++ show (round px :: Int))
                  SDL.delay 1000
  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40
            >>= SDL.fillRect s Nothing

render :: SDL.Surface -> SDL.Surface -> Game -> IO ()
render _ _ Ending = return ()
render screen heli (Running stuff _) = render' stuff
  where
    render' (x, (o,(c, f)), (V2 px py)) = do
      green <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 0 255 0 255
      red   <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 255 0 0 255

      CM.forM_ (map (rectToSDLRect x) c) $ flip (SDL.fillRect screen) green . Just
      CM.forM_ (map (rectToSDLRect x) f) $ flip (SDL.fillRect screen) green . Just
      CM.forM_ (map (rectToSDLRect x) o) $ flip (SDL.fillRect screen) red . Just
      r@(SDL.Rect _ _ w h) <- SDL.getClipRect heli
      CM.void $
        SDL.blitSurface  heli (Just r) screen
        (Just $ SDL.Rect (round (px-x) ) (round py ) w h)
    rectToSDLRect dx (Rect (V2 x y) (V2 w h)) =
      SDL.Rect (round (x - dx)) (round y) (round w) (round h)


--------------------------------------------------------------------------------
game :: (HasTime t s, Fractional t, MonadReader GameParams m, MonadRandom m)
     => V2 Double -> Wire s () m SDL.Event Game
game (V2 pw ph) = pure Ending . quit <|>  (mkGenN $ \_ -> CMR.ask >>= \gp -> return (Left (), run gp))
        where
        run gp  = proc e -> do sp <- (scroll - (scrW gp)) -< ()
                               (o, (c, l))  <- level -< ()
                               p@(V2 px py) <- position -< e
                               let frame    = Rect (V2 sp 0) (V2 (scrW gp) (scrH gp))
                                   player = Rect (V2 px py) (V2 pw  ph)
                               collide <- isColliding -< (player, (o ++ c ++ l))
                               within  <- isColliding -< (player, [frame])
                               returnA -< Running (sp, (o, (c, l)), p) (not collide && within)

isColliding :: Arrow a => a (Rect, [Rect]) Bool
isColliding = arr $ \(r, rs) -> any (overlapping r) rs


--------------------------------------------------------------------------------
acceleration :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
                => Wire s e m SDL.Event (V2 Double)
acceleration= proc cmd -> do
  base   <- baseAcceleration -< ()
  up'    <- upVec   -< cmd
  east   <- eastVec -< cmd
  west   <- westVec -< cmd
  returnA -< foldl1 (^+^) [base,up',east,west]


gravityVector :: (HasTime t s, Monoid e
                 ,MonadRandom m, MonadReader GameParams m) =>
                 Wire s e m SDL.Event (V2 Double)
gravityVector = mkGenN $ \_ -> do
  gp <- CMR.ask
  return (Right (V2 0 0), (integral (V2 0 (initGravity gp))
                           . pure (V2 0 (gravityForce gp))
                           . (until . (pure () &&& upAction)))
                --> pure (V2 0 0) . up
                --> gravityVector)

baseAcceleration :: (Monoid e, MonadRandom m, CMR.MonadReader GameParams m) =>
                    Wire s e m a (V2 Double)
baseAcceleration = mkGen_ $ \_ ->
  do gp <- CMR.ask
     return . Right $ V2 (scrollSpeed gp) 0

position :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
            => Wire s e m SDL.Event (V2 Double)
position = integral (V2 100 100) . (arr (uncurry (^+^)))
           . (gravityVector  &&& acceleration)
--------------------------------------------------------------------------------
