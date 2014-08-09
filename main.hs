{-# LANGUAGE RecursiveDo, TemplateHaskell #-}

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.IORef
import FRP.Elerea.Param
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

import Engine.Entity
import Engine.Input
import Engine.Rendering
import Engine.WorldState
import Util.Vector

initGL = do
	clearColor $= Color4 0 0 0 1
	blend $= Enabled
	blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
	cullFace $= Just Back

driveNetwork network driver = do
	output <- driver
	case output of
		Just output -> do
			join (network output)
			driveNetwork network driver
		Nothing -> return ()

render world = do
	w <- world
	let
		t = realToFrac $ (^. timeElapsed) $ w
		m = (^. inputState . mouseClicked) $ w

	clear [ColorBuffer]
	loadIdentity

	let
		setC r g b = color $ Color3 r g (b :: GLfloat)
		pos = 0.4 .*/ unitVec t
		size = Vec2 1 (abs . cos $ t) /*. 0.3
	setC 0 0 1
	renderRect pos size

	if m then setC 1 0 0 else setC 1 1 (mag size)
	let p = w ^. player
	p ^. onDraw $ p

	scale 0.003 0.003 (1 :: GLfloat)
	color $ Color4 1 1 1 (1 :: GLfloat)
	renderString Fixed8x16 $ "Woah hi dere"

	flush
	swapBuffers

update worldState = do
	let world = readIORef worldState
	signal <- stateful world worldUpdate

	return $ render <$> signal

main = do
	let (winWidth, winHeight) = (800, 800)

	initialize
	openWindow (Size winWidth winHeight)
		[DisplayRGBBits 8 8 8, DisplayAlphaBits 8] Window
	windowTitle $= "Ello boyo"

	world <- newIORef $ newWorldState
	w <- readIORef world
	writeIORef world $ w {_player =
		Entity (Vec2 0.5 0) (Vec2 0.25 0.4) vZero drawEnt}
	windowCloseCallback $= do
		w <- readIORef world
		writeIORef world
			(w & inputState . isClosed .~ True)
		return True
	initGL

	game <- start $ update world

	driveNetwork game (inputDriver world)

	closeWindow
