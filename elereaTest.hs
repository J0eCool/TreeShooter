{-# LANGUAGE RecursiveDo #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.IORef
import FRP.Elerea.Param
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

data InputState = InputState
    { isClosed :: Bool
    , mouseClicked :: Bool
    }
newInputState :: InputState
newInputState = InputState False False

data WorldState = WorldState
    { timeElapsed :: Double
    , inputState :: InputState
    }
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
    threadDelay 0

    world <- readIORef worldState

    let c = isClosed . inputState $ world

    dt <- get GLFW.time
    GLFW.time $= 0
    -- let dt = 0.16

    m <- (== Press) <$> getKey ENTER
    writeIORef worldState
        (world {inputState=((inputState world) {mouseClicked=m})})

    k <- (== Press) <$> getKey ESC
    return (if c || k then Nothing else Just dt)

renderRect :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderRect x y w h = renderPrimitive Quads $ mapM_ vert points
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
    t <- realToFrac <$> timeElapsed <$> world
    m <- mouseClicked <$> inputState <$> world
    clear [ColorBuffer]

    let setC r g b = color $ Color3 r g (b :: GLfloat)
    if m then setC 1 0 0 else setC 1 1 1
    renderRect (cos t) (sin t) 0.3 0.3

    flush
    swapBuffers

main = do
    let (winWidth, winHeight) = (800, 800)

    initialize
    openWindow (Size winWidth winHeight) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8] Window
    windowTitle $= "Ello boyo"

    world <- newIORef $ newWorldState
    windowCloseCallback $= do
        w <- readIORef world
        (writeIORef world
            (w {inputState=((inputState w) {isClosed=True})})
            >> return True)
    initGL


    -- game <- start $ stateful 0 (+)
    -- forM (take 10 [1,1..]) game >>= print

    game <- start $ do


        let acc dt world = do
            w <- world
            return w {timeElapsed = timeElapsed w + dt}
        signal <- stateful (readIORef world) acc >>= delay (readIORef world)

        return $ render <$> signal

    driveNetwork game (readInput world)

    closeWindow
