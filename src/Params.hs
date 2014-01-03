module Params where

import Control.Wire (HasTime(), Wire, pure)

screenW :: Num a => a
screenW = 800

screenH :: Num a => a
screenH = 600

data GameParams = GameParams {
    scrollSpeed :: (HasTime t s, Monad m) => Wire s e m a t
  , segmentLength :: (Integer, Integer)
  , ceilingRange :: (Double, Double)
  , floorRange :: (Double, Double)
  , outOfRangeLimit :: Num a => a
 }

defaultGameParams :: GameParams
defaultGameParams = GameParams {
    scrollSpeed = pure 60
  , segmentLength = (1,2)
  , ceilingRange = (0.0, screenH / 2.0)
  , floorRange = (screenH / 2.0,  screenH)
  , outOfRangeLimit = 100
  }
