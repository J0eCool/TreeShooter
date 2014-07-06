import Graphics.UI.GLUT
import Data.IORef

--data Entity = Entity pos size vel
data Entity = Entity (GLfloat, GLfloat) (GLfloat, GLfloat) (GLfloat, GLfloat)

stageWidth = 0.8
stageDim = 0.1
paddleWidth = 0.3
paddleHeight = 0.05
paddleDist = 0.8
ballDim = 0.05

main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Oi"
	worldState <- newIORef (
		(Entity (0.0, (-paddleDist)) (paddleWidth, paddleHeight) (0.0, 0.0)),
		(Entity (0.0, paddleDist) (paddleWidth, paddleHeight) (0.0, 0.0)),
		(Entity (0.0, 0.0) (ballDim, ballDim) (0.0, 1.0)))
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

drawEntity (Entity (px, py) (sx, sy) _) = drawRect px py sx sy

--display :: IORef GLfloat -> DisplayCallback
display worldState = do
	(player, opponent, ball) <- get worldState
	clear [ColorBuffer]
	loadIdentity

	mapM_ drawEntity [player, opponent, ball]

	-- draw walls
	drawRect (-stageWidth) 0 stageDim 2.0
	drawRect stageWidth 0 stageDim 2.0

	flush

clamp t lo hi
	| t < lo = lo
	| t > hi = hi
	| otherwise = t

getXPos (Entity (x, _) _ _) = x

--update :: IORef GLfloat -> IORef (Bool, Bool) -> IdleCallback
update worldState keyState = do
	ks <- get keyState

	-- update player
	case ks of
		(True, False) -> worldState $~! \(Entity (x, y) a b, op, ball) -> (Entity (clamp (x - playerSpeed) paddleLeftBound paddleRightBound, y) a b, op, ball)
		(False, True) -> worldState $~! \(Entity (x, y) a b, op, ball) -> (Entity (clamp (x + playerSpeed) paddleLeftBound paddleRightBound, y) a b, op, ball)
		_ -> worldState $~! \x -> x

	-- update enemy
	worldState $~! \(pl, Entity (x, y) s v, (Entity (bx, by) bv t)) -> (pl, Entity (if x > bx then x - opponentSpeed else x + opponentSpeed, y) s v, (Entity (bx, by) bv t))

	-- update ball
	worldState $~! \(x, y, (Entity (bx, by) s (vx, vy))) ->
		let
			t = vy > 0.0
			padPos = if t then getXPos y else getXPos x
			d = (bx - padPos) / paddleWidth
			didCollidePaddle = (if t then by > paddleDist else by < (-paddleDist)) && abs d < 1.0
			didLeaveScreen = abs by > 1.0
			didCollideWall = bx > ballRightBound || bx < ballLeftBound
			newV = case (didCollideWall, didCollidePaddle, didLeaveScreen) of
				(True, _, _) -> -vx
				(_, True, _) -> vx + d / 2
				(_, _, True) -> vx / 2
				_ -> vx
			newT = if didCollidePaddle || didLeaveScreen then (-1) * vy else vy
			dX = newV * ballSpeed
			dY = if newT > 0 then ballSpeed else (-ballSpeed)
			newPos = if didLeaveScreen
				then (0.0, if t then 0.7 * paddleDist else (- 0.7) * paddleDist)
				else (bx + dX, by + dY)
		in (x, y, (Entity newPos s (newV, newT)))

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
