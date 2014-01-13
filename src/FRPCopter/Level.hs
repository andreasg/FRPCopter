{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FRPCopter.Level (levelW, GameState(..), Rect(..), CanContain(..), scroll, events) where

import FRPCopter.Types

import FRP.Netwire
import Control.Wire
import Prelude hiding ((.), id, floor, ceiling)
import Control.Monad.Reader (MonadReader, ask)
--------------------------------------------------------------------------------
events :: (HasTime t s, Monad m, Monoid e) => (Double, Double) -> Wire s e m a (Event Double, Event Double)
events interv = stdNoiseR 1 interv 1234 &&& periodic 1 . (arr realToFrac) . (240*time+1240)


ceiling :: (HasTime t s, Monad m, Monoid e) =>  Wire s e m a [Rect]
ceiling = obst (\(y,x) -> mkRect x 0 240 y)  (0, 200)

floor :: (HasTime t s, Monad m, Monoid e) =>  Wire s e m a [Rect]
floor = obst (\(y,x) -> mkRect x y 240 200)  (400, 600)

obst :: (HasTime t s, Monad m, Monoid e) => ((Double,Double) -> Rect) -> (Double, Double) -> Wire s e m a [Rect]
obst f interv =  arr (map f . uncurry zip ) . (second (hold . accumE (flip (:)) []) . first (hold . accumE (flip (:)) []) . events interv)

levelW :: (Monoid e, HasTime t s, Monad m) => Wire s e m a Level
levelW  =
  ceiling &&& floor >>>
           arr (\(c,f) -> Level { obsticleRects = []
                                   , ceilingRects  = c
                                   , floorRects    = f})
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
scroll :: (Fractional b, HasTime t s, Monoid e, MonadReader GameParams m) =>
          Wire s e m a b
scroll =
  mkGenN $ \_ -> do
    gp <- ask
    return (Right (scrW gp), arr realToFrac . (time*pure (scrollSpeed gp))+scrW gp)
--------------------------------------------------------------------------------
