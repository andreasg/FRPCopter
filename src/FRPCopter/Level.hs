{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FRPCopter.Level (levelW, GameState(..), Rect(..), CanContain(..), scroll) where

import FRPCopter.Types

import Control.Wire
import Prelude hiding ((.), id, floor, ceiling)
import Control.Wire.Unsafe.Event
import Data.Fixed (mod')
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Random
import Control.Monad.Identity
import Linear (V2(..))


--------------------------------------------------------------------------------
events :: (HasTime t s, Fractional t, MonadRandom m, Monoid e) =>
     (Double, Double) -> Wire s e m Double (Event (Double, Double))
events interv = go (0 :: Double) 0
  where go tt t =
          mkGen $ \ds xcord -> do
            let dt = t - dtime ds
            (nw :: Double) <- getRandomR interv
            let nextWait = n nw
            return $ if dt >  0
                     then (Right NoEvent, go tt dt)
                     else (Right (Event (fromInteger . round $ xcord, nextWait))
                          ,go nextWait (mod' dt nextWait))


--------------------------------------------------------------------------------
levelW :: (Fractional t, Monoid e, HasTime t s
         ,MonadReader GameParams m, MonadRandom m) =>
         Wire s e m a Level
levelW = scroll >>> obsticles &&& ceiling &&& floor >>>
         arr (\(o, (c,f)) -> Level { obsticleRects = o
                                   , ceilingRects  = c
                                   , floorRects    = f})


--------------------------------------------------------------------------------
floor :: (MonadRandom m, HasTime t s, Fractional t
         ,Monoid e, MonadReader GameParams m) =>
         Wire s e m Double [Rect]
floor = mkGenN $ \_ -> do
  sl <- liftM segmentLength ask
  w  <- liftM scrW ask
  return (Right [], construct w . onEventM floorTile . events sl)
  where floorTile (x, dt) = do
          gp <- ask
          (y :: Double) <- getRandomR (floorRange gp)
          return $ Rect (mkPoint (n x -1)                    (n y))
                        (V2 (n $ 1 + dt* scrollSpeed gp) (n $ scrH gp - y))

ceiling :: (MonadRandom m, HasTime t s, Fractional t, Monoid e
           ,MonadReader GameParams m)
           => Wire s e m Double [Rect]
ceiling = mkGenN $ \_ -> do
  sl <- liftM segmentLength ask
  w  <- liftM scrW ask
  return (Right [], construct w  . onEventM ceilingTile . events sl)
  where ceilingTile (x, dt) = do
          gp <- ask
          (y :: Double) <- getRandomR (ceilingRange gp)
          return $ Rect (mkPoint (x-1) 0) (V2 (n $ 1+ dt * scrollSpeed gp) (n y))

obsticles :: (MonadRandom m, HasTime t s, Fractional t
             ,Monoid e, MonadReader GameParams m) =>
             Wire s e m Double [Rect]
obsticles = mkGenN $ \_ -> do
  gp <- ask
  return (Right [], construct (scrW gp)
                    . onEventM (toRect (0,0) (obsticleRange gp)
                                (obsticleHeights gp) (10,100))
                    . events (0.75, 1.75))


--------------------------------------------------------------------------------
scroll :: (Fractional b, HasTime b1 s, Monoid e, MonadReader GameParams m) =>
          Wire s e m a b
scroll =
  mkGenN $ \_ -> do
    gp <- ask
    return (Left mempty, arr realToFrac . (time*pure (scrollSpeed gp))+scrW gp)


--------------------------------------------------------------------------------
toRect :: (Monad m, MonadRandom m) =>
          (Double, Double) -> (Double, Double) -> (Double, Double) ->
          (Double, Double) -> (Double, Double) -> m Rect
toRect xcord ycord width height (dx, _)= do
    x <- getRandomR xcord
    y <- getRandomR ycord
    w <- getRandomR width
    h <- getRandomR height
    return $ mkRect (n (x+dx)) (n y) (n w) (n h)


--------------------------------------------------------------------------------
construct :: (Monad m, Monoid e) =>
             Double -> Wire s e m (Event Rect) [Rect]
construct lim  = hold . accumE append []
 where outOfRangeThreshhold = 1000
       append xs x@(Rect ((Point (V2 x0 _))) _ ) =
        x : filter (\(Rect (Point (V2 x1 _))_) ->x1>=x0-lim-outOfRangeThreshhold) xs


--------------------------------------------------------------------------------
n :: (Fractional b, RealFrac a) => a -> b
n x = (fromInteger . round $ x*10) / 10.0
