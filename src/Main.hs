{-# LANGUAGE FlexibleContexts #-}

module Main where

import FRPCopter.Types
import FRPCopter.Level

import Linear
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLPrim
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

up, down, east, west, quit :: (Monoid e, Monad m) => Wire s e m SDL.Event ()
up = command SDL.SDLK_UP <|> command SDL.SDLK_SPACE
down = command SDL.SDLK_DOWN
east = command SDL.SDLK_RIGHT
west = command SDL.SDLK_LEFT
quit = command SDL.SDLK_ESCAPE


--------------------------------------------------------------------------------
main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  SDL.setCaption "FRPCopter" ""
  let gp = defaultGameParams
  screen <- SDL.setVideoMode (scrW gp) (scrH gp) (bpp gp) [SDL.HWSurface]
  g <- getStdGen
  go g screen clockSession_ game
  SDL.quit
  where
  go g screen s w = do
    evt <- SDL.pollEvent
    (ds, s') <- stepSession s
    let ((game', w'), g') = runIdentity
                             . flip runRandT g
                             . flip CMR.runReaderT defaultGameParams
                             . runGameState
                             $ stepWire w ds (Right evt)
    case game' of
      Left  _      -> go g' screen s' w'
      Right Ending -> putStrLn "quitting"
      Right stuff@(Running (_,_,(V2 px _)) running) -> do
        clear screen
        render screen stuff
        SDL.flip screen
        if running
          then go g' screen s' w'
          else do putStrLn ("game over" :: String)
                  putStrLn ("distance covered: " ++ show (round px :: Int))
                  SDL.delay 1000

  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40
            >>= SDL.fillRect s Nothing

render :: SDL.Surface -> Game -> IO ()
render _ Ending = return ()
render screen (Running stuff _) = render' stuff
  where
    render' (x, (o,(c, f)), (V2 px py)) = do
      green <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 0 255 0 255
      red   <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 255 0 0 255
      white <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 255 255 255 255
      SDLPrim.circle screen (round (px - x)) (round py) 10 white
      CM.forM_ (map (rectToSDLRect x) c) $ flip (SDL.fillRect screen) green . Just
      CM.forM_ (map (rectToSDLRect x) f) $ flip (SDL.fillRect screen) green . Just
      CM.forM_ (map (rectToSDLRect x) o) $ flip (SDL.fillRect screen) red . Just
    rectToSDLRect dx (Rect (V2 x y) (V2 w h)) =
      SDL.Rect (round (x - dx)) (round y) (round w) (round h)


--------------------------------------------------------------------------------
game :: (HasTime t s, Fractional t, MonadReader GameParams m, MonadRandom m)
     => Wire s () m SDL.Event Game
game = pure Ending . quit <|>  (mkGenN $ \_ -> CMR.ask >>= \gp ->  return (Left (), run gp))
        where
        run gp  = proc e -> do sp <- (scroll - (scrW gp)) -< ()
                               (o, (c, l))  <- level -< ()
                               p <- position -< (e, 300)
                               collide <- isColliding -< (p, (o ++ c ++ l))
                               returnA -< Running (sp, (o, (c, l)), p) (not collide)

isColliding :: Arrow a => a (V2 Double, [Rect]) Bool
isColliding = arr $ \(pos, rs) -> any (contains pos) rs


--------------------------------------------------------------------------------
acceleration :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
                => Wire s e m (SDL.Event, Double) (V2 Double)
acceleration= proc (cmd, acc) -> do
  base   <- baseAcceleration -< ()
  up'     <- arr (V2 0 . negate . snd) . first up        <|> neutral -< (cmd, acc)
  down'   <- arr (V2 0 . snd) . first down               <|> neutral -< (cmd, acc)
  east'   <- arr (flip V2 0 . snd) . first east          <|> neutral -< (cmd, acc)
  west'   <- arr (flip V2 0 . negate . snd) . first west <|> neutral -< (cmd, acc)
  returnA -< foldl1 (^+^) [base,up',down',east',west']
  where neutral = pure $ V2 0 0

gravityVector :: (HasTime t s, Monoid e
                 ,MonadRandom m, MonadReader GameParams m) =>
                 Wire s e m (SDL.Event, Double) (V2 Double)
gravityVector = (integral (V2 0 100) . pure (V2 0 2000)
                   . first (until . (pure () &&& upAction)))
                --> pure (V2 0 0) . first up
                --> gravityVector

baseAcceleration :: (Monoid e, MonadRandom m, CMR.MonadReader GameParams m) =>
                    Wire s e m a (V2 Double)
baseAcceleration = mkGen_ $ \_ ->
  do gp <- CMR.ask
     return . Right $ V2 (scrollSpeed gp) 0

position :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
            => Wire s e m (SDL.Event, Double) (V2 Double)
position = integral (V2 100 100) . (arr (uncurry (^+^)))
           . (gravityVector  &&& acceleration)
--------------------------------------------------------------------------------
