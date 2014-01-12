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
  , ceilingRange = (0, screenH / 4)
  , floorRange = (screenH - (screenH / 4),  screenH)
  , outOfRangeLimit = 100
  , obsticleRange = (screenH / 3, screenH - (screenH / 3))
  , obsticleHeights = (10, 60)
  , scrH = screenH
  , scrW = screenW
  , bpp = 32
  , accel = 240
  , initGravity = 0
  , gravityForce = 500
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


-- | Produces only while given key is pressed. Messy because the
--   `Control.Wire.Interval.between` was causing memory-leaks.
command :: (HasTime t s, Monoid e, Monad m) => SDL.SDLKey -> Wire s e m SDL.Event ()
command k = go (keyUp k) (keyDown k) False
 where go upw' downw' active = mkGen $ \ds e -> seq ds $ do
         (up', upw)    <- stepWire upw'   ds (Right e)
         (down, downw) <- stepWire downw' ds (Right e)
         return $ if active
          then case up' of
           Left _ -> (Right (), go upw downw active)
           Right ev -> let (ans, nxt) = event (Right (), True)
                                              (const (Left mempty, False)) ev
                       in (ans, go upw downw nxt)
          else case down of
            Left _ -> (Left mempty, go upw downw active)
            Right ev -> let (ans, nxt) = event (Left mempty, False)
                                               (const (Right (), True)) ev
                        in (ans, go upw downw nxt)

upAction :: (Monoid e, Monad m) => Wire s e m SDL.Event (Event SDL.Event)
upAction = keyDown SDL.SDLK_SPACE

quit :: (Monoid e, Monad m) => Wire s e m SDL.Event SDL.Event
quit = hold . keyDown  SDL.SDLK_ESCAPE

up :: (HasTime t s, Monoid e, Monad m) => Wire s e m SDL.Event ()
up = command SDL.SDLK_SPACE


--------------------------------------------------------------------------------
-- Acceleration-vectors.
--------------------------------------------------------------------------------
westVec :: (HasTime t s, MonadReader GameParams m, Monoid e) =>
           Wire s e m SDL.Event (V2 Double)
westVec = directionVec (\a -> V2 (-a) 0) (command SDL.SDLK_LEFT)

eastVec :: (HasTime t s, MonadReader GameParams m, Monoid e) =>
           Wire s e m SDL.Event (V2 Double)
eastVec = directionVec (\a -> V2 a 0) (command SDL.SDLK_RIGHT)

upVec :: (HasTime t s, MonadReader GameParams m, Monoid e) =>
         Wire s e m SDL.Event (V2 Double)
upVec = directionVec (\a -> V2 0 (-a)) up

directionVec :: (HasTime t s, MonadReader GameParams m, Monoid e) =>
     (Double -> V2 Double)  ->
     Wire s e m SDL.Event b ->
     Wire s e m SDL.Event (V2 Double)
directionVec toV c = mkGenN $ \_ -> do
  gp <- CMR.ask
  return $  (Right neutral, pure (toV (accel gp)) . c <|> pure neutral)
  where neutral = V2 0 0
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- loading of assets
--------------------------------------------------------------------------------
loadAssets :: SDL.Surface -> IO Assets
loadAssets screen = do
  fn     <- TTF.openFont "assets/Ubuntu-R.ttf" 28
  heli0   <- IMG.load "assets/heli0.png"
  heli1   <- IMG.load "assets/heli1.png"
  cld  <- IMG.load "assets/cloud.png"
  bground <- IMG.load "assets/bg.png"
  border <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 50 50 50 200
  wall <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 150 80 30 255
  red   <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 160 20 20 255
  return $ Assets fn bground [heli0, heli1] cld border wall red
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------
main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  TTF.init
  SDL.setCaption "FRPCopter" ""
  let gp' = defaultGameParams
  screen <- SDL.setVideoMode (scrW gp') (scrH gp') (bpp gp') [SDL.HWSurface]
  g      <- getStdGen
  assets <- loadAssets screen
  (SDL.Rect _ _ w h)  <- SDL.getClipRect (head . heli $ assets)
  (SDL.Rect _ _ bgw _) <- SDL.getClipRect (background assets)
  let gp = defaultGameParams { bgWidth = fromIntegral bgw, playerSize = V2 (fromIntegral w) (fromIntegral h) }
  go gp g screen clockSession_ (runGame 0) assets
  SDL.quit
  where
  go gp g screen s w assets = do
    evt <- SDL.pollEvent
    (ds, s') <- stepSession s
    let ((game', w'), g') = runIdentity
                             . flip runRandT g
                             . flip CMR.runReaderT gp
                             . runGameState
                             $ stepWire w ds (Right evt)
    case game' of
      Left  _ -> go gp g' screen s' w' assets
      Right Ending -> putStrLn "quitting"
      Right stuff -> do
        clear screen
        render screen assets stuff
        SDL.flip screen
        go gp g' screen s' w' assets
  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40
            >>= SDL.fillRect s Nothing

--------------------------------------------------------------------------------
-- rendering
--------------------------------------------------------------------------------
drawText :: SDL.Surface -> TTF.Font -> String -> (Int, Int) -> SDL.Color -> IO Bool
drawText screen fn txt (x,y) c = do
  srf <- TTF.renderTextBlended fn txt c
  (SDL.Rect _ _ w h) <- SDL.getClipRect srf
  SDL.blitSurface srf Nothing screen (Just $ SDL.Rect x y w h)

render :: SDL.Surface -> Assets -> Game -> IO ()
render _ _ Ending = return ()
render screen assets (MainMenu score) = do
  sc <- SDL.getClipRect screen
  SDL.blitSurface (background assets) (Just sc) screen (Just sc)
  let white = SDL.Color 255 255 255
      grey = SDL.Color 140 140 140
      fnt = font assets
  drawText screen fnt "welcome to FRPCopter!" (380, 100) white
  drawText screen fnt "press Space to play" (390, 200) grey
  drawText screen fnt "(or Escape to quit)" (394, 230)  grey
  CM.when (score > 0) . CM.void $ drawText screen fnt
    ("Score: " ++ show (round score :: Int)) (415, 330) (SDL.Color 50 255 50)
render screen assets g = do
        let dx = cameraPos g
        renderBackground
        renderLevel dx
        renderParticles dx
        renderPlayer dx
        renderScore dx
  where
    rectToSDLRect dx p =
      let ((x,y), (w, h)) = unRect p
      in SDL.Rect (round (x - dx)) (round y) (round w) (round h)

    blitRects dx rs fill border = do
      CM.forM_ (map (rectToSDLRect dx) rs) $ \r -> do
        flip (SDL.fillRect screen) border (Just (grow 1 r))
        flip (SDL.fillRect screen) fill (Just r)

    renderParticles dx = do
      (SDL.Rect _ _ cw ch) <- SDL.getClipRect (cloud assets)
      CM.forM_ (particles g) $ \(SmokePuff sp) ->
        let (cx, cy) = unPoint sp
        in SDL.blitSurface (cloud assets) Nothing screen
             (Just $ SDL.Rect (round (cx-dx)-cw) (round cy) cw ch)

    renderPlayer dx = do
      let (px, py) = unPoint . playerPos $ g
      r@(SDL.Rect _ _ w h) <- SDL.getClipRect ((heli assets) !! playerFrame g)
      CM.void $
        SDL.blitSurface  ((heli assets) !! playerFrame g) (Just r) screen
          (Just $ SDL.Rect (round (px-dx) ) (round py ) w h)

    renderLevel dx = do
      blitRects dx (ceilingRects  . level $ g) (redColor assets)  (borderColor assets)
      blitRects dx (floorRects    . level $ g) (redColor assets)  (borderColor assets)
      blitRects dx (obsticleRects . level $ g) (wallColor assets) (borderColor assets)

    renderScore dx = do
      (SDL.Rect _ _ scw _) <- SDL.getClipRect screen
      CM.void $
        drawText screen (font assets) ("Distance: " ++ show (round dx :: Int))
                 (scw - 15 - 200, 15) (SDL.Color 255 255 255)

    renderBackground = do
      let bgS = bgSlice g
      sc <- SDL.getClipRect screen
      case bgS of
        (r0, Nothing) -> CM.void $ SDL.blitSurface (background assets)
                         (Just (rectToSDLRect 0 r0)) screen (Just sc)
        (r0, Just r1) -> do
          let (Rect _ (V2 w0 h0)) = r0
              (Rect _ (V2 w1 h1)) = r1
          CM.void $ SDL.blitSurface (background assets) (Just (rectToSDLRect 0 r0))
            screen (Just $ SDL.Rect 0 0 (round w0) (round h0))
          CM.void $ SDL.blitSurface (background assets) (Just (rectToSDLRect 0 r1))
            screen (Just $ SDL.Rect (round w0) 0 (round w1) (round h1))


grow :: Int -> SDL.Rect -> SDL.Rect
grow d (SDL.Rect x y w h) = SDL.Rect (x-d) (y-d) (w+d*2) (h+d*2)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- game
--------------------------------------------------------------------------------
runGame :: (HasTime t s, MonadRandom m,  MonadReader GameParams m, Fractional t) =>
           Integer -> Wire s GameOver m SDL.Event Game
runGame score = pure Ending . quit <|>  go (startGame score)
  where startGame s = (until . (pure (MainMenu (fromInteger s)) &&& upAction)) --> game
        go w = mkGen $ \ds ev -> seq ds $ do
          (o, w') <- stepWire w ds (Right ev)
          case o of
            Left (GameOver s) -> return $  (Left (GameOver s), go (startGame s))
            Right g -> return $  (Right g, go w')


game :: (HasTime t s, Fractional t, MonadReader GameParams m, MonadRandom m)
     =>  Wire s GameOver m SDL.Event Game
game = mkGenN $ \_ -> CMR.ask >>= \gp -> return (Right (MainMenu 0), run gp)
        where
        run gp  = proc e -> do camX <- (scroll - (scrW gp)) -< ()
                               p  <- position -< e
                               let (px,py) = unPoint p
                               let (V2 pw ph) = playerSize gp
                                   player = mkRect px py pw  ph
                               l  <- levelW -< ()
                               collide      <- isColliding -< (player, levelRects l)
                               smoke        <- smokeTrail  -< p
                               bgS          <- bg (scrW gp) (scrH gp) (bgWidth gp) -< ()
                               playerFrame'  <- animation 2 5 -< ()
                               first (when (==False)) --> second stopGame -<
                                 (collide || px < camX || (scrW gp + camX) < px || py < 0 || (py+ph) > (scrH gp), camX)
                               returnA -< Running {
                                  cameraPos = camX
                                 ,level = l
                                 ,particles = smoke
                                 ,playerPos = p
                                 ,bgSlice = bgS
                                 ,playerFrame = playerFrame' }
        stopGame = mkPureN $ \points -> (Left (GameOver (round points))
                                        , mkConst (Left (GameOver $ round points)))
-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
isColliding :: Arrow a => a (Rect, [Rect]) Bool
isColliding = arr $ \(r, rs) -> any (overlapping r) rs
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- gravity
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
                --> simpleGravityVector)

-- | Higher complexity than simpleGravityVector, but allows
-- for smooth breaking of fall. (Requires us to keep the
-- previous value from our falling vector as this is
-- unavailable as soon as the wire inhibits.)
gravityVector :: (HasTime t s, Monoid e
                 ,MonadRandom m, MonadReader GameParams m) =>
                 Wire s e m SDL.Event (V2 Double)
gravityVector = mkGenN $ \_ ->
  CMR.ask >>= \gp ->
    return $
      (Right (V2 0 0)
      ,go gp (V2 0 0) (fallWire gp (V2 0 $ initGravity gp))
                      (holdWire gp (V2 0 0)))
  where
        go :: (HasTime t s, Monad m, Monoid e) =>
              GameParams -> V2 Double ->
              (Wire s e m SDL.Event (V2 Double)) ->
              (Wire s e m SDL.Event (V2 Double)) ->
              Wire s e m SDL.Event (V2 Double)
        go gp acc fw hw = mkGen $ \ds ev ->
          seq ds $ do
           (h, hw') <- stepWire hw ds (Right ev)
           case h of
            Right acc' -> return (Right acc', go gp acc' (fallWire gp acc') hw')
            Left _     -> do (f, fw') <- stepWire fw ds (Right ev)
                             case f of
                               Right acc' ->
                                 return $ lstrict (Right acc'
                                        ,go gp acc' fw' (holdWire gp acc') )
                               Left _     ->
                                 return $ lstrict (Right acc
                                        ,go gp acc fw' hw' )

-- | Accelerate falling.
fallWire :: (HasTime t s, Monoid e, Monad m) =>
            GameParams -> V2 Double -> Wire s e m SDL.Event (V2 Double)
fallWire gp v0 = arr (\(x,y) -> min x y)
               . (integral v0
                  . pure (V2 0 (gravityForce gp))
                    &&& pure (V2 0 (gravityForce gp)))
               . (until . (pure () &&& upAction))


-- | Accelerate with (-1.2)g until 0.0 and hold.
holdWire :: (HasTime t s, Monoid e, Monad m) =>
            GameParams -> V2 Double -> Wire s e m SDL.Event (V2 Double)
holdWire gp v0 = arr (\(x,y) -> max x y)
          . (integral v0 . pure (V2 0 (negate $ 1.2*gravityForce gp))
                           &&& pure (V2 0 0))
          . up
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- positioning
--------------------------------------------------------------------------------
acceleration :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
                => Wire s e m SDL.Event (V2 Double)
acceleration = proc cmd -> do
  (base, (up', (east, west))) <-
    baseAcceleration &&& upVec &&& eastVec &&& westVec -< cmd
  returnA -< base ^+^ up' ^+^ east ^+^ west

baseAcceleration :: (Monoid e, MonadRandom m, CMR.MonadReader GameParams m) =>
                    Wire s e m a (V2 Double)
baseAcceleration = mkGenN $ \_ ->
  CMR.ask >>= \gp -> return $ (Right $ V2 (scrollSpeed gp) 0
                                      , mkConst (Right (V2 (scrollSpeed gp) 0)))

position :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
            => Wire s e m SDL.Event Point
position = arr Point . integral (V2 300 300) . (arr (uncurry (^+^)))
           . (gravityVector  &&& acceleration)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- particles
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
            return $ lstrict (Right (map fst ws'), go (map snd ws'))

-- | for, but with a random duration.
for' :: (HasTime t s, Fractional t, Monoid e, MonadRandom m) =>
        (Double, Double) -> Wire s e m a a
for' interv  = mkGenN $ \a -> do
  t <- getRandomR interv
  return (Right a, go (realToFrac t))
  where go t' = mkPure $ \ds x ->
                  let t = t' - dtime ds in
                  seq t $
                   if t <= 0
                    then (Left mempty, mkEmpty)
                    else (Right x, go t)

smokeTrail :: (HasTime t s, Fractional t, MonadRandom m, Monoid e) =>
              Wire s e m Point [Particle]
smokeTrail = wires . periodic (0.09) . arr (\x -> for' (0.25,1.5)
                                                  . pure (SmokePuff x))
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- parallax scrolling background
--------------------------------------------------------------------------------
bg :: (HasTime t s, Fractional t, Monoid e, Monad m) =>
      Double -> Double -> Double -> Wire s e m a (Rect, Maybe Rect)
bg w h bgW =
  (proc x' -> do
    let x = (realToFrac x') `mod'` bgW
    if (x+w) < bgW
       then returnA -< (mkRect x 0 w h, Nothing)
       else returnA -< (mkRect x 0 (bgW - x) h
                       ,Just $ mkRect 0 0 (w - (bgW - x)) h)) . (32*time)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- animation-ticker
--------------------------------------------------------------------------------
animation :: (HasTime t s, Monad m, Monoid e, Fractional t) =>
             Int -> Double -> Wire s e m a Int
animation frames fps = go 0
  where go n = for dt . seq n (pure n)
               --> go ((n+1) `mod` frames)
        dt = 1 / realToFrac fps
--------------------------------------------------------------------------------
