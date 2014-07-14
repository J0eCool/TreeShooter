{-# LANGUAGE RecursiveDo, TemplateHaskell #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Lens
import Data.IORef
import FRP.Elerea.Param
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

import Util.Vector

data InputState = InputState
    { _isClosed :: Bool
    , _mouseClicked :: Bool
    }
makeLenses ''InputState
newInputState :: InputState
newInputState = InputState False False

data WorldState = WorldState
    { _timeElapsed :: Double
    , _inputState :: InputState
    }
makeLenses ''WorldState
newWorldState :: WorldState
newWorldState = WorldState 0 newInputState

initGL :: IO ()
initGL = do
    clearColor $= Color4 0 0 0 1
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    cullFace $= Just Back

driveNetwork :: (a -> IO (IO b)) -> IO (Maybe a) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join (network dt)
            driveNetwork network driver
        Nothing -> return ()

readInput :: IORef WorldState -> IO (Maybe Double)
readInput worldState = do
    dt <- get GLFW.time
    GLFW.time $= 0

    threadDelay $ floor $ 1000000 * (1 - dt) / 60

    world <- readIORef worldState

    m <- (== Press) <$> getMouseButton ButtonLeft
    writeIORef worldState
        (world & inputState . mouseClicked .~ m)

    let c = world ^. inputState . isClosed
    k <- (== Press) <$> getKey ESC
    return (if c || k then Nothing else Just dt)

renderRect :: Vec2 -> Vec2 -> IO ()
renderRect (Vec2 x y) (Vec2 w h) =
    renderPrimitive Quads $ mapM_ vert points
    where
        (hw, hh) = (w / 2, h / 2)
        vert (x, y) = vertex $ Vertex3 x y (0 :: GLfloat)
        points =
            [ (x - hw, y - hh)
            , (x + hw, y - hh)
            , (x + hw, y + hh)
            , (x - hw, y + hh)
            ]

render :: IO WorldState -> IO ()
render world = do
    t <- realToFrac <$> (^. timeElapsed) <$> world
    m <- (^. inputState . mouseClicked) <$> world
    clear [ColorBuffer]

    let
        setC r g b = color $ Color3 r g (b :: GLfloat)
        pos = 0.4 .*/ unitVec t
        size = Vec2 1 (abs . cos $ t) /*. 0.3
    if m then setC 1 0 0 else setC 1 1 (mag size)
    renderRect pos size

    flush
    swapBuffers

main = do
    let (winWidth, winHeight) = (800, 800)

    initialize
    openWindow (Size winWidth winHeight)
        [DisplayRGBBits 8 8 8, DisplayAlphaBits 8] Window
    windowTitle $= "Ello boyo"

    world <- newIORef $ newWorldState
    windowCloseCallback $= do
        w <- readIORef world
        writeIORef world
            (w & inputState . isClosed .~ True)
            >> return True
    initGL

    game <- start $ do


        let acc dt world = do
            w <- world
            return $ w & timeElapsed %~ (+ dt)
        signal <- stateful (readIORef world) acc >>= delay (readIORef world)

        return $ render <$> signal

    driveNetwork game (readInput world)

    closeWindow
