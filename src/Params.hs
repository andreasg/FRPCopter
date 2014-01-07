module Params where

--------------------------------------------------------------------------------
screenW :: Num a => a
screenW = 800

screenH :: Num a => a
screenH = 600

bpp :: Num a => a
bpp = 32

--------------------------------------------------------------------------------
data GameParams = GameParams {
    scrollSpeed :: Num a => a
  , segmentLength :: (Double, Double)
  , ceilingRange :: (Double, Double)
  , floorRange :: (Double, Double)
  , outOfRangeLimit :: Num a => a
  , obsticleRange :: (Double, Double)
  , obsticleHeights :: (Double, Double)
 }

defaultGameParams :: GameParams
defaultGameParams = GameParams {
    scrollSpeed = 240
  , segmentLength = (0.05,0.7)
  , ceilingRange = (0.0, screenH / 3.0)
  , floorRange = (screenH - (screenH / 3.0),  screenH)
  , outOfRangeLimit = 100
  , obsticleRange = (screenH / 3.0, screenH - (screenH / 3.0))
  , obsticleHeights = (10, 60)
  }
