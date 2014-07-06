import Graphics.UI.GLUT
import Data.IORef

data Ball a = Ball (a, a) a Bool

main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Oi"
	worldState <- newIORef (0.0, 0.0, (Ball (0.0, 0.0) 0.0 True))
	keyState <- newIORef (False, False)
	displayCallback $= display worldState
	idleCallback $= Just (update worldState keyState)
	keyboardMouseCallback $= Just (input keyState)
	mainLoop

drawRect x y w h = do
	renderPrimitive Quads $
		mapM_ (\ (x, y, z) -> vertex $ Vertex3 x y z) points
	where
		points :: [(GLfloat, GLfloat, GLfloat)]
		hw = w / 2
		hh = h / 2
		points = [(x - hw, y - hh, 0), (x + hw, y - hh, 0), (x + hw, y + hh, 0), (x - hw, y + hh, 0)]

stageWidth = 0.8
stageDim = 0.1
paddleWidth = 0.3
paddleHeight = 0.05
paddleDist = 0.8
ballDim = 0.05

--display :: IORef GLfloat -> DisplayCallback
display worldState = do
	(playerPos, opponentPos, (Ball (ballX, ballY) _ _)) <- get worldState
	clear [ColorBuffer]
	loadIdentity

	-- draw ball
	drawRect ballX ballY ballDim ballDim

	-- draw walls
	drawRect (-stageWidth) 0 stageDim 2.0
	drawRect stageWidth 0 stageDim 2.0

	-- draw paddles
	drawRect opponentPos paddleDist paddleWidth paddleHeight -- opponent
	drawRect playerPos (-paddleDist) paddleWidth paddleHeight -- player

	flush

clamp t lo hi
	| t < lo = lo
	| t > hi = hi
	| otherwise = t

--updateItem index list

--update :: IORef GLfloat -> IORef (Bool, Bool) -> IdleCallback
update worldState keyState = do
	ks <- get keyState

	-- update player
	case ks of
		(True, False) -> worldState $~! \(x, y, b) -> (clamp (x - playerSpeed) paddleLeftBound paddleRightBound, y, b)
		(False, True) -> worldState $~! \(x, y, b) -> (clamp (x + playerSpeed) paddleLeftBound paddleRightBound, y, b)
		_ -> worldState $~! \x -> x

	-- update enemy
	worldState $~! \(x, y, (Ball (bx, by) v t)) -> (x, if y > bx then y - opponentSpeed else y + opponentSpeed, (Ball (bx, by) v t))

	-- update ball
	worldState $~! \(x, y, (Ball (bx, by) v t)) ->
		let
			padPos = if t then y else x
			d = (bx - padPos) / paddleWidth
			didCollidePaddle = (if t then by > paddleDist else by < (-paddleDist)) && abs d < 1.0
			didLeaveScreen = abs by > 1.0
			didCollideWall = bx > ballRightBound || bx < ballLeftBound
			newV = case (didCollideWall, didCollidePaddle, didLeaveScreen) of
				(True, _, _) -> -v
				(_, True, _) -> v + d / 2
				(_, _, True) -> v / 2
				_ -> v
			newT = if didCollidePaddle || didLeaveScreen then not t else t
			dX = newV * ballSpeed
			dY = if newT then ballSpeed else (-ballSpeed)
			newPos = if didLeaveScreen
				then (0.0, if t then 0.7 * paddleDist else (- 0.7) * paddleDist)
				else (bx + dX, by + dY)
		in (x, y, (Ball newPos newV newT))

	postRedisplay Nothing
	where
		playerSpeed = 0.0002
		opponentSpeed = 0.0001
		ballSpeed = 0.0003
		left = (stageDim / 2) - stageWidth
		right = stageWidth - (stageDim / 2)
		paddleLeftBound = (paddleWidth / 2) + left
		paddleRightBound = right - (paddleWidth / 2)
		ballLeftBound = (ballDim / 2) + left
		ballRightBound = right - (ballDim / 2)

--input :: IORef (Bool, Bool) -> KeyboardMouseCallback
input keyState key Down _ _ = case key of
	(SpecialKey KeyLeft) -> keyState $~! \(x, y) -> (True, y)
	(SpecialKey KeyRight) -> keyState $~! \(x, y) -> (x, True)
	_ -> return ()
input keyState key Up _ _ = case key of
	(SpecialKey KeyLeft) -> keyState $~! \(x, y) -> (False, y)
	(SpecialKey KeyRight) -> keyState $~! \(x, y) -> (x, False)
	_ -> return ()
--input _ _ _ _ _ = return ()
