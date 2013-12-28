
newtype Coord = Coord { unCoord :: V2 Int }
              deriving (Show, Eq, Ord)

newtype Color = Color { unColor :: SDL.Color }

instance Show Color where
  show (Color (SDL.Color r g b)) = "Color " ++ show (r,g,b)

instance Eq Color where
  (Color (SDL.Color r g b)) == (Color (SDL.Color r' g' b')) =
    r == r' && g == g' && b == b'

data Block = Block Color
           deriving (Show, Eq)

data Shape = I | J | L | O | S | T | Z
           deriving (Show, Eq)                    

data Figure = Figure Shape [[Maybe Block]]
            deriving (Show, Eq)

blue = Color $ SDL.Color 0 0 255
red  = Color $ SDL.Color 255 0 0 

iFig color = [[Nothing, Just (Block color), Nothing, Nothing]
             ,[Nothing, Just (Block color), Nothing, Nothing]
             ,[Nothing, Just (Block color), Nothing, Nothing]
             ,[Nothing, Just (Block color), Nothing, Nothing]]

jFig color = [[Nothing         , Just (Block color), Nothing ]
             ,[Nothing         , Just (Block color), Nothing ]
             ,[Just (Block red), Just (Block color), Nothing ]]

iFigure = Figure I (iFig blue)
jFigure = Figure J (jFig red)


printFig = putStrLn . unlines . map (map (\c -> if c == Nothing then ' ' else '#'))


rot :: Figure -> Direction -> Figure
rot (Figure s rows) dir =
  let rotF = case dir of
        East -> reverse . map (reverse . extractColumn rows)
        West -> map (extractColumn rows)
  in Figure s (rotF columns)
 where
   columns = reverse [0 .. length rows - 1]
   extractColumn [] _ = []
   extractColumn (r:rs) c = r !! c : extractColumn rs c       

rgbColor :: Int -> Int -> Int -> Color
rgbColor r g b = Color $ SDL.Color (fromIntegral r)
                                   (fromIntegral g)
                                   (fromIntegral b)

mkCoord :: Int -> Int -> Coord
mkCoord x y = Coord (V2 x y)

data Well = Well { unWell :: Matrix (Maybe Figure) }
          deriving (Show)

data Tetris = Tetris {
      activeFigure :: Figure
    , gameState :: GameState
    , well :: Well
    , pressedKeys :: [SDL.Keysym]}
            deriving (Show)

data GameState = Running
               | Paused
               | Stopped
               deriving (Show)

newtype TetrisEnv a = TetrisEnv { unGameEnv :: Reader Tetris a }
                      deriving (Monad, CMR.MonadReader Tetris)

data Direction = West | East


data UserCommand = DropFigure | PauseGame | QuitGame | NewGame | MoveFigure Direction

-- create a wire that inhibits on other than specific key-events
userCommand :: SDL.Keysym -> Wire s () m a a 
userCommand k = case k of
  _ -> undefined

pause :: Monoid e => Wire s e m a a
pause = undefined

moveFigure :: (Monoid e, HasTime t s) =>
              Wire s e TetrisEnv Direction a
moveFigure = undefined

dropFigure :: (Monoid e, HasTime t s) =>
              Wire s e TetrisEnv a a
dropFigure = undefined

rotateFigure :: (Monoid e, HasTime t s) => Wire s e TetrisEnv Direction a
rotateFigure = undefined
--  mkGenN $ \dir -> do
--    env <- CMR.ask
--    let rotated = rot (activeFigure env) dir
--    return (Right undefined, rotateFigure)
    
