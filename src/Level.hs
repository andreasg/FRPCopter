{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Level where

import Params
import Control.Wire
import System.Random
import Prelude hiding ((.), id, floor, ceiling)
import Control.Wire.Unsafe.Event
import Data.Fixed (mod')
import Control.Monad.Reader (ReaderT, MonadReader, ask)
import Control.Monad.Random
import Control.Monad.Identity
import Linear (V2(..))

class CanContain a where
  contains :: V2 Double -> a -> Bool

data Rect = Rect (V2 Double) (V2 Double) deriving (Show)

instance CanContain Rect where
  contains (V2 x y) (Rect (V2 x' y') (V2 w h)) = x' <= x && x <= (x'+w) && y' <= y && y <= (y'+h)


newtype GameState g a = GameState { runGameState :: ReaderT GameParams (RandT g Identity) a }
                        deriving (Monad, MonadReader GameParams, MonadRandom)

events :: (HasTime t s, Fractional t, MonadRandom m, Monoid e) =>
     (Double, Double) -> Wire s e m Double (Event (Double, Double))
events interv = go (0 :: Double) 0
  where go tt t = do
          mkGen $ \ds xcord -> do
            let dt = t - dtime ds
            (nw :: Double) <- getRandomR interv
            let nextWait = n nw
            return $ if dt >  0
                      then (Right NoEvent, go tt dt)
                      else (Right (Event (fromInteger . round $ xcord, nextWait)) , go (nextWait) (mod' dt ( nextWait)))


level :: (Fractional t, Monoid e, HasTime t s, MonadReader GameParams m, MonadRandom m) => Wire s e m a ([Rect], ([Rect], [Rect]))
level = scroll >>> obsticles &&& ceiling &&& floor


floor :: (MonadRandom m, HasTime t s, Fractional t, Monoid e, MonadReader GameParams m) => Wire s e m Double [Rect]
floor = mkGenN $ \_ -> do
  sl <- liftM segmentLength ask
  return (Right [], construct . (onEventM floorTile) . events sl)
  where floorTile (x, dt) = do
          gp <- ask  
          (y :: Double) <- getRandomR (floorRange gp)
          return $ Rect (V2 (n x) (n y)) (V2 (n $ dt* (scrollSpeed gp)) (n $ screenH - y))


ceiling :: (MonadRandom m, HasTime t s, Fractional t, Monoid e, MonadReader GameParams m) => Wire s e m Double [Rect]
ceiling = mkGenN $ \_ -> do
  sl <- liftM segmentLength ask
  return (Right [], construct . onEventM ceilingTile . events sl)
  where ceilingTile (x, dt) = do
          gp <- ask
          (y :: Double) <- getRandomR (ceilingRange gp)
          return $ Rect (V2 x 0) (V2 (n $ dt*(scrollSpeed gp)) (n y))


obsticles :: (MonadRandom m, HasTime t s, Fractional t, Monoid e, MonadReader GameParams m) => Wire s e m Double [Rect]
obsticles = mkGenN $ \_ -> do
  gp <- ask
  return (Right [], construct . onEventM (toRect (0,0) (obsticleRange gp) (obsticleHeights gp) (10,100)) . events (0.75, 1.75))


scroll :: (Fractional b, HasTime b1 s, Monoid e, MonadReader GameParams m) => Wire s e m a b
scroll =
  mkGenN $ \_ -> do
    gp <- ask
    return $ (Left mempty, arr realToFrac .  (time * pure (scrollSpeed gp)) + screenW)


toRect :: (Monad m, MonadRandom m) => (Double, Double) -> (Double, Double) -> (Double, Double)
       -> (Double, Double) -> (Double, Double) -> m Rect
toRect xcord ycord width height (dx, _)= do
    x <- getRandomR xcord
    y <- getRandomR ycord
    w <- getRandomR width
    h <- getRandomR height
    return $ Rect (V2 (n (x+dx)) (n y)) (V2 (n w) (n h))


construct :: (Monad m, Monoid e) => Wire s e m (Event Rect) [Rect]
construct = hold . accumE append []
 where append xs x@(Rect (V2 x0 _) _ ) = x : filter (\(Rect (V2 x1 _) _ ) -> x1 >= x0 - screenW - outOfRangeThreshhold) xs
       outOfRangeThreshhold = 1000

n :: (Fractional b, RealFrac a) => a -> b
n x = (fromInteger . round $ x*10) / 10.0
