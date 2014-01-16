module Extra where

import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Maybe (catMaybes)
import Linear
import qualified Control.Monad as CM
import Prelude hiding ((.))

manageWires :: (Monad m, Monoid e, HasTime t s, Monoid s) =>
         Wire s e m (Event (Wire s e m () a)) [a]
manageWires = go []
  where go ws =
          mkGen $ \ds ev -> do
            let nw = event ws (flip (:) ws) ev
            ws' <- CM.liftM catMaybes . CM.forM nw $ \w -> do
              (r, w') <- stepWire w ds $ Right ()
              return $ either (const Nothing) (\b -> Just (b,w')) r
            return $ lstrict (Right (map fst ws'), go (map snd ws'))


data Particle = SmokePuff (V2 Double)
