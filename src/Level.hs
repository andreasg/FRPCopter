{-# LANGUAGE NoMonomorphismRestriction #-}
module Level where

import Params
import Control.Wire
import System.Random
import Prelude hiding ((.), id, floor, ceiling)
import Control.Wire.Unsafe.Event
import Data.Fixed (mod')
import Control.Monad.Reader

nextCoord
  :: (HasTime t s, RandomGen g, Monoid e, Monad m) =>
     (Integer, Integer) -> (Double, Double) -> g -> Wire s e m Double (Event (Double, Double))
nextCoord interv valueRange gen =
  let (x :: Double, g') = randomR valueRange gen
      (t :: Integer, g'') = randomR interv g'
      y = fromInteger t
  in mkSFN $ \xcord -> (Event (xcord, round' x), go g'' y y)
  where
  go g t 0 = go g t t
  go g t t' =
          mkSF $ \ds xcord ->
           let dt = t' -  dtime ds
           in if dt <= 0
              then let (x, g') = randomR valueRange g
                       (t'' :: Integer, g'') = randomR interv g'
                       y = fromInteger t''
                   in (Event (xcord, round' x), go g'' y (dt `mod'` y))
              else (NoEvent, go g t dt)
  round' = fromInteger . round


newtype Ceiling = Ceiling  {
    unCeiling :: [(Double, Double)]
  } deriving (Show)

             
newtype Floor = Floor {
    unFloor :: [(Double, Double)]
  } deriving (Show)

outOfRangeThreshhold = 1000

level :: (Monoid e, HasTime t s, RandomGen g) => g -> Wire s e (Reader GameParams) a (Ceiling, Floor)
level gen =
  let (g0, g1) = split gen
  in mkGenN $ \_ -> do
    gs <- ask
    return (Left mempty, arr Ceiling . accum . bounds gs ceilingRange g0 &&& arr Floor . accum .  bounds gs floorRange g1)
  where
  accum = hold . accumE mrg []
  mrg xs x@(x0,_) = x : filter (\(x1,_) -> x1 >= x0 - screenW - outOfRangeThreshhold) xs
  bounds gp range g = 
        nextCoord (segmentLength gp) (range gp) g
               . arr ((+) screenW
                      . fromInteger
                      . round
                      . (realToFrac ::  Real a => a -> Double))
               . (time * scrollSpeed gp)

