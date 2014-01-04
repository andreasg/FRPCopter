module Params where

screenW :: Num a => a
screenW = 800

screenH :: Num a => a
screenH = 600

bpp :: Num a => a
bpp = 32

data GameParams = GameParams {
    scrollSpeed :: Num a => a  -- (HasTime t s, Monad m) => Wire s e m a t
  , segmentLength :: (Double, Double)
  , ceilingRange :: (Double, Double)
  , floorRange :: (Double, Double)
  , outOfRangeLimit :: Num a => a
  , gravity :: Double
 }

defaultGameParams :: GameParams
defaultGameParams = GameParams {
    scrollSpeed = 240
  , segmentLength = (0.05,0.3)
  , ceilingRange = (0.0, screenH / 2.0)
  , floorRange = (screenH / 2.0,  screenH)
  , outOfRangeLimit = 100
  , gravity = (9)
  }
