{-# LANGUAGE FlexibleContexts #-}
module Main where

import FRPCopter.Types
import FRPCopter.Level

import Linear
import Data.Maybe (catMaybes)
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import qualified Graphics.UI.SDL.TTF as TTF
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as CMR
import Control.Wire
import Control.Wire.Unsafe.Event
import Prelude hiding (id, (.), until)
import FRP.Netwire
import Control.Monad.Random
import Data.Fixed (mod')

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
  , bgWidth = undefined
  , playerSize = undefined
  }
  where
    screenW :: Num a => a
    screenW = 1024
    screenH :: Num a => a
    screenH = 600
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Wires for user input and keybindings.
--------------------------------------------------------------------------------
-- | Generate event when the key is pressed.
keyDown :: SDL.SDLKey -> Wire s e m SDL.Event (Event SDL.Event)
keyDown k = became $ \e -> case e of
  SDL.KeyDown (SDL.Keysym key _ _) -> k == key; _ -> False


-- | Produce event when the key is released.
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
  TTF.init
  SDL.setCaption "FRPCopter" ""
  let gp' = defaultGameParams
  screen <- SDL.setVideoMode (scrW gp') (scrH gp') (bpp gp') [SDL.HWSurface]
  g      <- getStdGen
  fn     <- TTF.openFont "assets/Ubuntu-R.ttf" 28
  heli   <- IMG.load "assets/heli.png"
  cloud  <- IMG.load "assets/cloud.png"
  background <- IMG.load "assets/bg.png"
  (SDL.Rect _ _ w h)  <- SDL.getClipRect heli
  (SDL.Rect _ _ bgw _) <- SDL.getClipRect background
  let gp = defaultGameParams { bgWidth = fromIntegral bgw, playerSize = V2 (fromIntegral w) (fromIntegral h) }
  go gp g screen clockSession_ game heli cloud background fn
  SDL.quit
  where
  go gp g screen s w heli cloud background fn = do
    evt <- SDL.pollEvent
    (ds, s') <- stepSession s
    let ((game', w'), g') = runIdentity
                             . flip runRandT g
                             . flip CMR.runReaderT gp
                             . runGameState
                             $ stepWire w ds (Right evt)
    case game' of
      Left  _      -> go gp g' screen s' w' heli cloud background fn
      Right Ending -> putStrLn "quitting"
      Right stuff -> do
        clear screen
        render fn screen heli cloud background stuff 
        SDL.flip screen
        if running stuff
          then go gp g' screen s' w' heli cloud background fn
          else do putStrLn ("game over" :: String)
                  SDL.delay 2000
  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40
            >>= SDL.fillRect s Nothing


render :: TTF.Font -> SDL.Surface -> SDL.Surface -> SDL.Surface -> SDL.Surface -> Game -> IO ()
render _ _ _ _ _ Ending = return ()
render fn screen heli cloud background stuff = render' stuff
  where
    render' g = do
      let bgS = bgSlice g
      sc@(SDL.Rect _ _ scw _) <- SDL.getClipRect screen
      case bgS of
        (r0, Nothing) -> CM.void $ SDL.blitSurface background (Just (rectToSDLRect 0 r0)) screen (Just sc)
        (r0, Just r1) -> do
          let (Rect _ (V2 w0 h0)) = r0
              (Rect _ (V2 w1 h1)) = r1
          CM.void $ SDL.blitSurface background (Just (rectToSDLRect 0 r0)) screen (Just $ SDL.Rect 0 0 (round w0) (round h0))
          CM.void $ SDL.blitSurface background (Just (rectToSDLRect 0 r1)) screen (Just $ SDL.Rect (round w0) 0 (round w1) (round h1))


      border <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 50 50 50 200
      wallColor <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 150 80 30 255
      red   <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 160 20 20 255
      let x = cameraPos g
      CM.forM_ (map (rectToSDLRect x) (ceilingRects . level $ g)) $ \r -> do
        flip (SDL.fillRect screen) border (Just (grow 1 r))
        flip (SDL.fillRect screen) red (Just r)
      CM.forM_ (map (rectToSDLRect x) (floorRects . level $ g)) $ \r -> do
        flip (SDL.fillRect screen) border (Just (grow 1 r))
        flip (SDL.fillRect screen) red (Just r)      
      CM.forM_ (map (rectToSDLRect x) (obsticleRects . level $ g)) $ \r -> do
        flip (SDL.fillRect screen) border (Just $ grow 1 r)
        flip (SDL.fillRect screen) wallColor (Just r)

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


      score <- TTF.renderTextBlended fn ("Distance: " ++ show (round px :: Int)) (SDL.Color 255 255 255)
      (SDL.Rect _ _ sw sh) <- SDL.getClipRect score
      CM.void $ SDL.blitSurface score Nothing screen (Just $ SDL.Rect (scw - sw - 15) 15 sw sh)

    rectToSDLRect dx p =
      let ((x,y), (w, h)) = unRect p
      in SDL.Rect (round (x - dx)) (round y) (round w) (round h)

grow :: Int -> SDL.Rect -> SDL.Rect
grow d (SDL.Rect x y w h) = SDL.Rect (x-d) (y-d) (w+d*2) (h+d*2)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
game :: (HasTime t s, Fractional t, MonadReader GameParams m, MonadRandom m)
     =>  Wire s () m SDL.Event Game
game = pure Ending . quit <|>
                  (mkGenN $ \_ -> CMR.ask >>= \gp -> return (Left (), run gp))
        where
        run gp  = proc e -> do sp <- (scroll - (scrW gp)) -< ()
                               p <- position -< e
                               let (px,py) = unPoint p
                               let frame    = mkRect sp 0 (scrW gp) (scrH gp)
                                   (V2 pw ph) = playerSize gp
                                   player = mkRect px py pw  ph
                               l  <- levelW -< ()
                               collide      <- isColliding -< (player, levelRects l)
                               within       <- isColliding -< (player, [frame])
                               smoke        <- smokeTrail  -< p
                               bgS          <- bg (scrW gp) (scrH gp) (bgWidth gp) -< ()
                               returnA -< Running {
                                  cameraPos = sp
                                 ,level = l
                                 ,particles = smoke
                                 ,playerPos = p
                                 ,running = not collide && within
                                 ,bgSlice = bgS}
-------------------------------------------------------------------------------                               


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



bg :: (HasTime t s, Fractional t, Monad m) => Double -> Double -> Double -> Wire s () m a (Rect, Maybe Rect)
bg w h bgW =
  (proc x' -> do
    let x = (realToFrac x') `mod'` bgW
    if (x+w) < bgW
       then returnA -< (mkRect x 0 w h, Nothing)
       else returnA -< (mkRect x 0 (bgW - x) h, Just $ mkRect 0 0 (w - (bgW - x)) h)) . (64*time)
