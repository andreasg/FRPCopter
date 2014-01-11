{-# LANGUAGE FlexibleContexts #-}
module Main where

import FRPCopter.Types
import FRPCopter.Level

import Linear
import Data.Maybe (catMaybes)
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as CMR
import Control.Wire
import Control.Wire.Unsafe.Event
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
  , initGravity = 0
  , gravityForce = 1000
  }
  where
    screenW :: Num a => a
    screenW = 800
    screenH :: Num a => a
    screenH = 600
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Wires for user input and keybindings.
--------------------------------------------------------------------------------
keyDown :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyDown k = became $ \e -> case e of
  SDL.KeyDown (SDL.Keysym key _ _) -> k == key; _ -> False

keyUp :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyUp k = became $ \e -> case e of
  SDL.KeyUp (SDL.Keysym key _ _) -> k == key; _ -> False

-- | Produces only while given key is pressed.
command :: (Monoid e, Monad m) => SDL.SDLKey -> Wire s e m SDL.Event ()
command k = between . arr (\(on,off) -> ((), on, off)) . (keyDown k &&& keyUp k)

upAction :: (Monoid e, Monad m) => Wire s e m SDL.Event (Event SDL.Event)
upAction = keyDown SDL.SDLK_UP <& keyDown SDL.SDLK_SPACE

quit :: (Monoid e, Monad m) => Wire s e m SDL.Event SDL.Event
quit = hold . keyDown  SDL.SDLK_ESCAPE

up :: (Monoid e, Monad m) => Wire s e m SDL.Event ()
up = command SDL.SDLK_UP <|> command SDL.SDLK_SPACE


--------------------------------------------------------------------------------
-- Acceleration-vectors.
--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  SDL.setCaption "FRPCopter" ""
  let gp = defaultGameParams
  screen <- SDL.setVideoMode (scrW gp) (scrH gp) (bpp gp) [SDL.HWSurface]
  g      <- getStdGen
  heli   <- IMG.load "assets/heli.png"
  cloud  <- IMG.load "assets/cloud.png"
  (SDL.Rect _ _ w h)  <- SDL.getClipRect heli
  go g screen clockSession_ (game (V2 (fromIntegral w) (fromIntegral h))) heli cloud
  SDL.quit
  where
  go g screen s w heli cloud = do
    evt <- SDL.pollEvent
    (ds, s') <- stepSession s
    let ((game', w'), g') = runIdentity
                             . flip runRandT g
                             . flip CMR.runReaderT defaultGameParams
                             . runGameState
                             $ stepWire w ds (Right evt)
    case game' of
      Left  _      -> go g' screen s' w' heli cloud
      Right Ending -> putStrLn "quitting"
      Right stuff -> do
        clear screen
        render screen heli cloud stuff
        SDL.flip screen
        if running stuff
          then go g' screen s' w' heli cloud
          else do putStrLn ("game over" :: String)
                  putStrLn ("distance covered: " ++ show ( (playerPos $ stuff)))
                  SDL.delay 1000
  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40
            >>= SDL.fillRect s Nothing


render :: SDL.Surface -> SDL.Surface -> SDL.Surface -> Game -> IO ()
render _ _ _ Ending = return ()
render screen heli cloud stuff = render' stuff
  where
    render' g = do
      green <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 0 255 0 255
      red   <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 255 0 0 255
      let x = cameraPos g
      CM.forM_ (map (rectToSDLRect x) (ceilingRects . level $ g)) $
        flip (SDL.fillRect screen) green . Just
      CM.forM_ (map (rectToSDLRect x) (floorRects . level $ g)) $
        flip (SDL.fillRect screen) green . Just
      CM.forM_ (map (rectToSDLRect x) (obsticleRects . level $ g)) $
        flip (SDL.fillRect screen) red . Just

      r@(SDL.Rect _ _ w h) <- SDL.getClipRect heli
      cld@(SDL.Rect _ _ cw ch) <- SDL.getClipRect cloud

      CM.forM_ (particles g) $ \(SmokePuff sp) ->
        let (cx, cy) = unPoint sp
        in SDL.blitSurface cloud (Just cld) screen
             (Just $ SDL.Rect (round (cx-x)-cw) (round cy) cw ch)
      let (px, py) = unPoint . playerPos $ g
      CM.void $
        SDL.blitSurface  heli (Just r) screen
          (Just $ SDL.Rect (round (px-x) ) (round py ) w h)

    rectToSDLRect dx p =
      let ((x,y), (w, h)) = unRect p
      in SDL.Rect (round (x - dx)) (round y) (round w) (round h)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
game :: (HasTime t s, Fractional t, MonadReader GameParams m, MonadRandom m)
     => V2 Double -> Wire s () m SDL.Event Game
game (V2 pw ph) = pure Ending . quit <|>
                  (mkGenN $ \_ -> CMR.ask >>= \gp -> return (Left (), run gp))
        where
        run gp  = proc e -> do sp <- (scroll - (scrW gp)) -< ()
                               p <- position -< e
                               let (px,py) = unPoint p
                               let frame    = mkRect sp 0 (scrW gp) (scrH gp)
                                   player = mkRect px py pw  ph
                               l  <- levelW -< ()
                               collide      <- isColliding -< (player, levelRects l)
                               within       <- isColliding -< (player, [frame])
                               smoke        <- smokeTrail  -< p
                               returnA -< Running {
                                  cameraPos = sp
                                 ,level = l
                                 ,particles = smoke
                                 ,playerPos = p
                                 ,running = not collide && within}
--------------------------------------------------------------------------------                               


--------------------------------------------------------------------------------
isColliding :: Arrow a => a (Rect, [Rect]) Bool
isColliding = arr $ \(r, rs) -> any (overlapping r) rs
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
simpleGravityVector :: (HasTime t s, Monoid e
                 ,MonadRandom m, MonadReader GameParams m) =>
                 Wire s e m SDL.Event (V2 Double)
simpleGravityVector = mkGenN $ \_ -> do
  gp <- CMR.ask
  return (Right (V2 0 0), (integral (V2 0 (initGravity gp))
                           . pure (V2 0 (gravityForce gp))
                           . (until . (pure () &&& upAction)))
                --> pure (V2 0 0) . up
                --> gravityVector)

-- | Higher complexity than simpleGravityVector, but allows
-- for smooth breaking of fall. (Requires us to keep the
-- previous value from our falling vector as this is
-- unavailable as soon as the wire inhibits.)
gravityVector :: (HasTime t s, Monoid e
                 ,MonadRandom m, MonadReader GameParams m) =>
                 Wire s e m SDL.Event (V2 Double)
gravityVector = mkGenN $ \_ ->
  CMR.ask >>= \gp -> return (Right (V2 0 0)
                            ,go gp (V2 0 0) (fallWire gp) (holdWire gp (V2 0 0)))
  where
        go :: (HasTime t s, Monad m, Monoid e) =>
              GameParams ->
              V2 Double ->
              (Wire s e m SDL.Event (V2 Double)) ->
              (Wire s e m SDL.Event (V2 Double)) ->
              Wire s e m SDL.Event (V2 Double)
        go gp acc fw hw = mkGen $ \ds ev -> do
          (h, hw') <- stepWire hw ds (Right ev)
          case h of
            Right acc' -> return (Right acc', go gp acc' (fallWire gp) hw')
            Left _     -> do (f, fw') <- stepWire fw ds (Right ev)
                             case f of
                               Right acc' ->
                                 return (Right acc'
                                        ,go gp acc' fw' (holdWire gp acc') )
                               Left _     ->
                                 return (Right acc
                                        ,go gp acc fw' hw' )

-- | Accelerate falling.
fallWire :: (HasTime t s, Monoid e, Monad m) =>
            GameParams -> Wire s e m SDL.Event (V2 Double)
fallWire gp = integral (V2 0 (initGravity gp))
              . pure (V2 0 (gravityForce gp))
              . (until . (pure () &&& upAction))


-- | Accelerate with (-2)g until 0.0 and hold.
holdWire :: (HasTime t s, Monoid e, Monad m) =>
            GameParams -> V2 Double -> Wire s e m SDL.Event (V2 Double)
holdWire gp v0 = arr (\(x,y) -> max x y)
          . (integral v0 . pure (V2 0 (negate $ 2*gravityForce gp)) &&& pure (V2 0 0))
          . up
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
acceleration :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
                => Wire s e m SDL.Event (V2 Double)
acceleration = proc cmd -> do
  (base, (up', (east, west))) <-
    baseAcceleration &&& upVec &&& eastVec &&& westVec -< cmd
  returnA -< foldl1 (^+^) [base,up',east,west]

baseAcceleration :: (Monoid e, MonadRandom m, CMR.MonadReader GameParams m) =>
                    Wire s e m a (V2 Double)
baseAcceleration = mkGen_ $ \_ ->
  CMR.ask >>= \gp -> return . Right $ V2 (scrollSpeed gp) 0

position :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
            => Wire s e m SDL.Event Point
position = arr Point . integral (V2 100 100) . (arr (uncurry (^+^)))
           . (gravityVector  &&& acceleration)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Manages a set of wires. When a wire inhibits, it's removed from
-- the set. On Event, the wire is added to the set. 
wires :: (Monad m, Monoid e, HasTime t s, Monoid s) =>
         Wire s e m (Event (Wire s e m () a)) [a]
wires = go []
  where go ws =
          mkGen $ \ds ev -> do
            let nw = event ws (flip (:) ws) ev
            ws' <- CM.liftM catMaybes . CM.forM nw $ \w -> do
              (r, w') <- stepWire w ds $ Right ()
              return $ either (const Nothing) (\b -> Just (b,w')) r
            return (Right (map fst ws'), go (map snd ws'))

smokeTrail :: (HasTime t s, Fractional t, Monad m) =>
              Wire s () m Point [Particle]
smokeTrail = wires . periodic (0.09) . arr (\x -> for 1 . pure (SmokePuff x))
--------------------------------------------------------------------------------
