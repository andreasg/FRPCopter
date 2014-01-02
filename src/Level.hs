module Level (levelBorders) where

import Params
import Control.Wire
import System.Random
import Prelude hiding ((.), id)

-- delay-range between generating new segments.
segmentInterval :: (Integer, Integer)
segmentInterval = (1, 3)

floorInterval :: (Double, Double)
floorInterval = (fromIntegral screenH / 2.0,  fromIntegral screenH)

ceilingInterval :: (Double, Double)
ceilingInterval = (0.0, fromIntegral screenH / 2.0)

levelBorders :: (HasTime t s, Monad m, RandomGen g) => g -> Wire s () m a (Double, Double)
levelBorders g = 
  let (g0, g1) = split g
  in nextFloorHeight ceilingInterval g0 1 &&& nextFloorHeight floorInterval g1 1

nextFloorHeight :: (RandomGen g, Monad m, HasTime t s) => (Double, Double) -> g -> t -> Wire s () m a Double
nextFloorHeight interv g t' =
  let (x' :: Double, g0) = randomR interv g
      x :: Double = fromIntegral $ (round x' :: Integer)
      (nextWait :: Integer, g1) = randomR segmentInterval g0
  in (mkPureN $ \_ -> (Right x, (for t' . (pure x)) --> nextFloorHeight interv g1 (fromInteger nextWait)))
