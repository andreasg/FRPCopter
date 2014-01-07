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
 }


--------------------------------------------------------------------------------
data Game = Running (Double, ([Rect], ([Rect], [Rect])), V2 Double) Bool
          | Ending


--------------------------------------------------------------------------------
class CanContain a where
  contains :: V2 Double -> a -> Bool


--------------------------------------------------------------------------------
data Rect = Rect (V2 Double) (V2 Double)
          deriving (Show)

instance CanContain Rect where
  contains (V2 x y) (Rect (V2 x' y') (V2 w h)) =    x' <= x && x <= (x'+w)
                                                 && y' <= y && y <= (y'+h)


--------------------------------------------------------------------------------
newtype GameState g a = GameState {
  runGameState :: ReaderT GameParams (RandT g Identity) a
  } deriving (Monad, MonadReader GameParams, MonadRandom)
