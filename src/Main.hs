{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Level
import Params

import Linear
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLPrim
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as CMR
import Control.Wire
import Prelude hiding (id, (.))
import FRP.Netwire
import Control.Monad.Random
import qualified Data.Set as Set


--------------------------------------------------------------------------------
data Direction = Up | Down | East | West deriving (Eq, Show, Ord)

data UserCommand = Accelerate Direction | QuitGame
  deriving (Show, Eq, Ord)

parseEvent :: Set.Set UserCommand -> IO (Set.Set UserCommand)
parseEvent active = do
  event <- SDL.pollEvent
  return $ case event of
    SDL.KeyDown (SDL.Keysym key _ _)  ->
      case key of
        SDL.SDLK_SPACE  -> Set.insert (Accelerate Up) active
        SDL.SDLK_UP     -> Set.insert (Accelerate Up) active
        SDL.SDLK_LEFT   -> Set.insert (Accelerate West) active
        SDL.SDLK_RIGHT  -> Set.insert (Accelerate East) active
        SDL.SDLK_DOWN   -> Set.insert (Accelerate Down) active
        SDL.SDLK_ESCAPE -> Set.insert QuitGame active
        _               -> active
    SDL.KeyUp (SDL.Keysym key _ _)  ->
      case key of
        SDL.SDLK_SPACE  -> Set.delete (Accelerate Up) active
        SDL.SDLK_UP     -> Set.delete (Accelerate Up) active
        SDL.SDLK_LEFT   -> Set.delete (Accelerate West) active
        SDL.SDLK_RIGHT  -> Set.delete (Accelerate East) active
        SDL.SDLK_DOWN   -> Set.delete (Accelerate Down) active
        SDL.SDLK_ESCAPE -> Set.delete QuitGame active
        _               -> active
    SDL.Quit -> Set.insert QuitGame active
    _        -> active


--------------------------------------------------------------------------------
main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  SDL.setCaption "FRPCopter" ""
  screen <- SDL.setVideoMode screenW screenH 32 [SDL.HWSurface]
  g <- getStdGen
  go g Set.empty screen clockSession_ game
  SDL.quit
  where
  go g cmds' screen s w = do
    green <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 0 255 0 255
    red   <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 255 0 0 255
    white <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 255 255 255 255
    cmds  <- parseEvent cmds'
    (ds, s') <- stepSession s
    let ((continue, w'), g') = runIdentity
                             . flip runRandT g
                             . flip CMR.runReaderT defaultGameParams
                             . runGameState
                             $ stepWire w ds (Right cmds)
    case continue of
      Left _ -> go g' cmds screen s' w'
      Right Nothing -> putStrLn "quitting"
      Right (Just (x, (o,(c, f)), (V2 px py))) -> do
        clear screen
        SDLPrim.circle screen (round (px - x)) (round py) 10 white
        CM.forM (map (rectToSDLRect x) c) $ flip (SDL.fillRect screen) green . Just
        CM.forM (map (rectToSDLRect x) f) $ flip (SDL.fillRect screen) green . Just
        CM.forM (map (rectToSDLRect x) o) $ flip (SDL.fillRect screen) red . Just
        SDL.flip screen
        go g' cmds screen s' w'
  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40
            >>= SDL.fillRect s Nothing


rectToSDLRect :: Double -> Rect -> SDL.Rect
rectToSDLRect dx (Rect (V2 x y) (V2 w h)) = SDL.Rect (round (x - dx)) (round y)
                                                     (round w)        (round h)


--------------------------------------------------------------------------------
game :: (HasTime t s, Fractional t, MonadReader GameParams m, MonadRandom m)
     => Wire s () m (Set.Set UserCommand) (Maybe (Double
                                                 ,([Rect], ([Rect], [Rect]))
                                                 ,V2 Double))
game = pure Nothing . onUserCommand QuitGame <|> run
        where
        run  = proc cmds -> do sp <- (scroll - screenW) -< ()
                               (o, (c, l))  <- level -< ()
                               p <- position -< (cmds, 300)
                               collide <- isColliding -< (p, (o ++ c ++ l))
                               if not collide
                                   then returnA -< Just (sp, (o, (c, l)), p)
                                   else returnA -< Nothing

isColliding :: Arrow a => a (V2 Double, [Rect]) Bool
isColliding = arr $ \(pos, rs) -> any (contains pos) rs


--------------------------------------------------------------------------------

onUserCommand :: (Monoid e, Monad m) =>
                 UserCommand ->
                 Wire s e m (Set.Set UserCommand) (Set.Set UserCommand)
onUserCommand c = fmap snd $ (when (\cmds -> c `Set.member` cmds)) &&& id

unlessUserCommand :: (Monoid e, Monad m) =>
                     UserCommand
                     -> Wire s e m (Set.Set UserCommand) (Set.Set UserCommand)
unlessUserCommand c = fmap snd $ (unless (\cmds -> c `Set.member` cmds)) &&& id


--------------------------------------------------------------------------------
acceleration :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
                => Wire s e m (Set.Set UserCommand, Double) (V2 Double)
acceleration= proc (cmd, acc) -> do
  base   <- baseAcceleration -< ()
  up     <- arr (V2 0 . negate . snd) . on Up        <|> neutral -< (cmd, acc)
  down   <- arr (V2 0 . snd) . on Down               <|> neutral -< (cmd, acc)
  east   <- arr (flip V2 0 . snd) . on East          <|> neutral -< (cmd, acc)
  west   <- arr (flip V2 0 . negate . snd) . on West <|> neutral -< (cmd, acc)
  returnA -< foldl1 (^+^) [base,up,down,east,west]
  where neutral = pure $ V2 0 0
        on c = first (onUserCommand (Accelerate c))

gravityVector :: (HasTime t s, Monoid e
                 ,MonadRandom m, MonadReader GameParams m) =>
                 Wire s e m (Set.Set UserCommand, Double) (V2 Double)
gravityVector = (integral (V2 0 100) . pure (V2 0 2000)
                 . first (unlessUserCommand (Accelerate Up)))
                -->  pure (V2 0 0) . first (onUserCommand (Accelerate Up))
                --> gravityVector

baseAcceleration :: (Monoid e, MonadRandom m, CMR.MonadReader GameParams m) =>
                    Wire s e m a (V2 Double)
baseAcceleration = mkGen_ $ \_ ->
  do gp <- CMR.ask
     return . Right $ V2 (scrollSpeed gp) 0

position :: (HasTime t s, Monoid e, MonadRandom m, MonadReader GameParams m)
            => Wire s e m (Set.Set UserCommand, Double) (V2 Double)
position = integral (V2 100 100) . (arr (uncurry (^+^)))
           . (gravityVector &&& acceleration)
--------------------------------------------------------------------------------
