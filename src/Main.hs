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
import Data.Time
import Debug.Trace

--------------------------------------------------------------------------------
defaultGameParams :: GameParams
defaultGameParams = GameParams {
    scrollSpeed = 200
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
  heli0   <- IMG.load "assets/heli0.png"
  heli1   <- IMG.load "assets/heli1.png"  
  
  cloud  <- IMG.load "assets/cloud.png"
  background <- IMG.load "assets/bg.png"
  (SDL.Rect _ _ w h)  <- SDL.getClipRect heli0
  (SDL.Rect _ _ bgw _) <- SDL.getClipRect background
  let gp = defaultGameParams { bgWidth = fromIntegral bgw, playerSize = V2 (fromIntegral w) (fromIntegral h) }
  go gp g screen clockSession_ (runGame 0) [heli0, heli1] cloud background fn
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
      Left  _ -> go gp g' screen s' w' heli cloud background fn
      Right Ending -> putStrLn "quitting"
      Right stuff -> do
        clear screen
        render fn screen heli cloud background stuff 
        SDL.flip screen
        go gp g' screen s' w' heli cloud background fn
  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40
            >>= SDL.fillRect s Nothing


drawText :: SDL.Surface -> TTF.Font -> String -> (Int, Int) -> SDL.Color -> IO Bool
drawText screen fn txt (x,y) c = do
  srf <- TTF.renderTextBlended fn txt c
  (SDL.Rect _ _ w h) <- SDL.getClipRect srf
  SDL.blitSurface srf Nothing screen (Just $ SDL.Rect x y w h)


--------------------------------------------------------------------------------
render :: TTF.Font -> SDL.Surface -> [SDL.Surface] -> SDL.Surface -> SDL.Surface -> Game -> IO ()
render _ _ _ _ _ Ending = return ()
render fn screen _ _ background (MainMenu score) = do
  sc <- SDL.getClipRect screen
  SDL.blitSurface background (Just sc) screen (Just sc)
  let white = SDL.Color 255 255 255
      grey = SDL.Color 140 140 140
  drawText screen fn "welcome to FRPHeli!" (380, 100) white
  drawText screen fn "press Space to play" (390, 200) grey
  drawText screen fn "(or Escape to quit)" (394, 230)  grey
  CM.when (score > 0) . CM.void $ drawText screen fn
    ("Score: " ++ show (round score :: Int)) (415, 330) (SDL.Color 50 255 50)
render fn screen hs cloud background stuff = render' stuff
  where
    render' g = do

      -- background
      let bgS = bgSlice g
      sc@(SDL.Rect _ _ scw _) <- SDL.getClipRect screen
      case bgS of
        (r0, Nothing) -> CM.void $ SDL.blitSurface background (Just (rectToSDLRect 0 r0)) screen (Just sc)
        (r0, Just r1) -> do
          let (Rect _ (V2 w0 h0)) = r0
              (Rect _ (V2 w1 h1)) = r1
          CM.void $ SDL.blitSurface background (Just (rectToSDLRect 0 r0)) screen (Just $ SDL.Rect 0 0 (round w0) (round h0))
          CM.void $ SDL.blitSurface background (Just (rectToSDLRect 0 r1)) screen (Just $ SDL.Rect (round w0) 0 (round w1) (round h1))

      -- colors
      border <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 50 50 50 200
      wallColor <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 150 80 30 255
      red   <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 160 20 20 255
      let x = cameraPos g

      -- Ceiling, floor, obsticles
      blitRects x (ceilingRects . level $ g) red border
      blitRects x (floorRects . level $ g) red border
      blitRects x (obsticleRects . level $ g) wallColor border      

        
      r@(SDL.Rect _ _ w h) <- SDL.getClipRect (hs !! playerFrame g)
      cld@(SDL.Rect _ _ cw ch) <- SDL.getClipRect cloud

      -- particles
      CM.forM_ (particles g) $ \(SmokePuff sp) ->
        let (cx, cy) = unPoint sp
        in SDL.blitSurface cloud (Just cld) screen
             (Just $ SDL.Rect (round (cx-x)-cw) (round cy) cw ch)

      -- player
      let (px, py) = unPoint . playerPos $ g
      CM.void $
        SDL.blitSurface  (hs !! playerFrame g) (Just r) screen
          (Just $ SDL.Rect (round (px-x) ) (round py ) w h)

      -- score
      score <- TTF.renderTextBlended fn ("Distance: " ++ show (round x :: Int)) (SDL.Color 255 255 255)
      (SDL.Rect _ _ sw sh) <- SDL.getClipRect score
      CM.void $ SDL.blitSurface score Nothing screen (Just $ SDL.Rect (scw - sw - 15) 15 sw sh)

    rectToSDLRect dx p =
      let ((x,y), (w, h)) = unRect p
      in SDL.Rect (round (x - dx)) (round y) (round w) (round h)

    blitRects dx rs fill border = do
      CM.forM_ (map (rectToSDLRect dx) rs) $ \r -> do
        flip (SDL.fillRect screen) border (Just (grow 1 r))
        flip (SDL.fillRect screen) fill (Just r)

grow :: Int -> SDL.Rect -> SDL.Rect
grow d (SDL.Rect x y w h) = SDL.Rect (x-d) (y-d) (w+d*2) (h+d*2)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
runGame :: (HasTime t s, MonadRandom m,  MonadReader GameParams m, Fractional t) =>
           Integer -> Wire s GameOver m SDL.Event Game
runGame score = pure Ending . quit <|>
                go ((until . (pure (MainMenu (fromInteger score)) &&& upAction)) --> game)
  where go w = mkGen $ \ds ev -> do
          (o, w') <- stepWire w ds (Right ev)
          case o of
            Left (GameOver s) -> return (Left (GameOver s), runGame s . after 2)
            Right g -> return (Right g, go w')


game :: (HasTime t s, Fractional t, MonadReader GameParams m, MonadRandom m)
     =>  Wire s GameOver m SDL.Event Game
game = mkGenN $ \_ -> CMR.ask >>= \gp -> return (Right (MainMenu 10), run gp)
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
                               pframe <- animation 2 5 -< ()                               
                               first (when (==False)) --> for 2 . second stopGame -<
                                 (collide || px < sp || (scrW gp + sp) < px || py < 0 || (py+ph) > (scrH gp), sp)
                               returnA -< Running {
                                  cameraPos = sp
                                 ,level = l
                                 ,particles = smoke
                                 ,playerPos = p
                                 ,running = not collide && within
                                 ,bgSlice = bgS
                                 ,playerFrame = pframe }
        stopGame = mkPureN $ \points -> (Left (GameOver (round points))
                                        , mkConst (Left (GameOver $ round points)))
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
                --> simpleGravityVector)

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
          . (integral v0 . pure (V2 0 (negate $ 1.2*gravityForce gp)) &&& pure (V2 0 0))
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
position = arr Point . integral (V2 300 300) . (arr (uncurry (^+^)))
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

-- | for, but with a random duration.
for' :: (HasTime t s, Fractional t, Monoid e, MonadRandom m) =>
        (Double, Double) -> Wire s e m a a
for' interv  = mkGenN $ \a -> do
  t <- getRandomR interv
  return (Right a, go (realToFrac t))
  where go t' = mkPure $ \ds x ->
                  let t = t' - dtime ds in
                  if t <= 0
                   then (Left mempty, mkEmpty)
                   else (Right x, go t)

smokeTrail :: (HasTime t s, Fractional t, MonadRandom m, Monoid e) =>
              Wire s e m Point [Particle]
smokeTrail = wires . periodic (0.09) . arr (\x -> for' (0.25,1.5) . pure (SmokePuff x))                        
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
bg :: (HasTime t s, Fractional t, Monoid e, Monad m) =>
      Double -> Double -> Double -> Wire s e m a (Rect, Maybe Rect)
bg w h bgW =
  (proc x' -> do
    let x = (realToFrac x') `mod'` bgW
    if (x+w) < bgW
       then returnA -< (mkRect x 0 w h, Nothing)
       else returnA -< (mkRect x 0 (bgW - x) h
                       ,Just $ mkRect 0 0 (w - (bgW - x)) h)) . (64*time)
--------------------------------------------------------------------------------  


--------------------------------------------------------------------------------
animation :: (HasTime t s, Monad m, Monoid e, Fractional t) =>
             Int -> Double -> Wire s e m a Int
animation frames fps = go 0
 where go n = mkPureN $ \_ -> (Right n, for (1/ realToFrac fps) . pure n
                                        --> go ((n+1) `mod` frames))
--------------------------------------------------------------------------------
