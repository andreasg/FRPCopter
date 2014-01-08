module FRPCopter.Types where

import Linear
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Random
import Control.Monad.Identity


--------------------------------------------------------------------------------
data GameParams = GameParams {
    scrollSpeed :: Num a => a
  , segmentLength :: (Double, Double)
  , ceilingRange :: (Double, Double)
  , floorRange :: (Double, Double)
  , outOfRangeLimit :: Num a => a
  , obsticleRange :: (Double, Double)
  , obsticleHeights :: (Double, Double)
  , scrW :: Num a => a
  , scrH :: Num a => a
  , bpp :: Int
  , accel :: Double
  , gravityForce :: Double
  , initGravity :: Double    
 }


--------------------------------------------------------------------------------
data Game = Running (Double, ([Rect], ([Rect], [Rect])), V2 Double) Bool
          | Ending


--------------------------------------------------------------------------------
class CanContain a where
  contains :: V2 Double -> a -> Bool
  overlapping :: a -> a -> Bool


--------------------------------------------------------------------------------
data Rect = Rect (V2 Double) (V2 Double)
          deriving (Show)

instance CanContain Rect where
  contains (V2 x y) (Rect (V2 x' y') (V2 w h)) =    x' <= x && x <= (x'+w)
                                                 && y' <= y && y <= (y'+h)
  overlapping r0@(Rect (V2 x0 y0) (V2 w0 h0))
              r1@(Rect (V2 x1 y1) (V2 w1 h1)) = contains (V2 x0 y0) r1 ||
                                                contains (V2 (x0) (y0+h0)) r1 ||
                                                contains (V2 (x0+w0) (y0)) r1 ||
                                                contains (V2 (x0+w0) (y0+h0)) r1 ||

                                                contains (V2 x1 y1) r0 ||
                                                contains (V2 (x1) (y1+h1)) r0 ||
                                                contains (V2 (x1+w1) (y1)) r0 ||                                                
                                                contains (V2 (x1+w1) (y1+h1)) r0


--------------------------------------------------------------------------------
newtype GameState g a = GameState {
  runGameState :: ReaderT GameParams (RandT g Identity) a
  } deriving (Monad, MonadReader GameParams, MonadRandom)
