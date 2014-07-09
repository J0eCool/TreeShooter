import Graphics.UI.GLUT
import Data.IORef

data WorldState = WorldState 
	{ entities :: [Entity]
	, keyState :: (Bool, Bool)
	}

data Vec2 = Vec2 { x :: GLfloat, y :: GLfloat }

data Entity = Entity
	{ pos :: Vec2
	, size :: Vec2
	, vel :: Vec2
	, onDraw :: Entity -> IO ()
	, onUpdate :: Entity -> WorldState -> Entity
	}

main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Oi"
	worldState <- newIORef $ WorldState [
		Entity (Vec2 0.0 (-paddleDist)) (Vec2 paddleWidth paddleHeight) (Vec2 0.0 0.0) (drawEntityColor 0 1 1) updatePlayer,
		Entity (Vec2 0.0 paddleDist) (Vec2 paddleWidth paddleHeight) (Vec2 0.0 0.0) drawEntity updateOpponent,
		Entity (Vec2 0.0 0.0) (Vec2 ballDim ballDim) (Vec2 0.0 1.0) drawEntity updateBall
		] (False, False)
	displayCallback $= display worldState
	idleCallback $= Just (update worldState)
	keyboardMouseCallback $= Just (input worldState)
	mainLoop

stageWidth = 0.8
stageDim = 0.1
paddleWidth = 0.3
paddleHeight = 0.05
paddleDist = 0.8
ballDim = 0.05

drawRect :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRect x y w h = do
	renderPrimitive Quads $
		mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points
	where
		points :: [(GLfloat, GLfloat, GLfloat)]
		hw = w / 2
		hh = h / 2
		points = [(x - hw, y - hh, 0), (x + hw, y - hh, 0), (x + hw, y + hh, 0), (x - hw, y + hh, 0)]

drawEntity :: Entity -> IO ()
drawEntity e = drawRect px py sx sy
	where
		Vec2 px py = pos e
		Vec2 sx sy = size e

drawEntityColor :: GLfloat -> GLfloat -> GLfloat -> Entity -> IO ()
drawEntityColor r g b e = do
	color $ Color3 r g b
	drawEntity e
	color $ Color3 1 1 (1 :: GLfloat)

--display :: IORef [Entity] -> DisplayCallback
display worldState = do
	WorldState world _ <- readIORef worldState
	clear [ColorBuffer]
	loadIdentity

	mapM_ (\e -> (onDraw e) e) world

	-- draw walls
	drawRect (-stageWidth) 0 stageDim 2.0
	drawRect stageWidth 0 stageDim 2.0

	flush

clamp t lo hi
	| t < lo = lo
	| t > hi = hi
	| otherwise = t

getXPos = x . pos

playerSpeed = 0.0002
opponentSpeed = 0.0001
ballSpeed = 0.0003
left = (stageDim / 2) - stageWidth
right = stageWidth - (stageDim / 2)
paddleLeftBound = (paddleWidth / 2) + left
paddleRightBound = right - (paddleWidth / 2)
ballLeftBound = (ballDim / 2) + left
ballRightBound = right - (ballDim / 2)

updateNull entity worldState = entity

--getInputState entity =


updatePlayer entity worldState = newEntity
	where
		input = keyState worldState
		Vec2 x y = pos entity
		newX =
			case input of
				(True, False) -> clamp (x - playerSpeed) paddleLeftBound paddleRightBound
				(False, True) -> clamp (x + playerSpeed) paddleLeftBound paddleRightBound
				_ -> x
		newEntity = entity { pos = Vec2 newX y }

updateOpponent entity worldState = newEntity
	where
		ball = (entities worldState) !! 2
		bx = x $ pos ball
		Vec2 ex ey = pos entity
		newX = if ex > bx then ex - opponentSpeed else ex + opponentSpeed
		newEntity = entity { pos = Vec2 newX ey }

updateBall entity worldState = newEntity
	where
		Vec2 bx by = pos entity
		Vec2 vx vy = vel entity
		ents = entities worldState
		t = vy > 0.0
		padPos = getXPos $ if t then (ents !! 1) else (ents !! 0)
		d = (bx - padPos) / paddleWidth
		didCollidePaddle = (if t then by > paddleDist else by < (-paddleDist)) && abs d < 1.0
		didLeaveScreen = abs by > 1.0
		didCollideWall = bx > ballRightBound || bx < ballLeftBound
		newVX = case (didCollideWall, didCollidePaddle, didLeaveScreen) of
			(True, _, _) -> -vx
			(_, True, _) -> vx + d / 2
			(_, _, True) -> vx / 2
			_ -> vx
		newVY = if didCollidePaddle || didLeaveScreen then (-1) * vy else vy
		newVel = Vec2 newVX newVY
		dX = newVX * ballSpeed
		dY = if newVY > 0 then ballSpeed else (-ballSpeed)
		newPos = if didLeaveScreen
			then Vec2 0.0 $ if t then 0.7 * paddleDist else (- 0.7) * paddleDist
			else Vec2 (bx + dX) (by + dY)
		newEntity = entity { pos = newPos } { vel = newVel }

update :: IORef WorldState -> IdleCallback
update worldState = do
	world <- readIORef worldState

	writeIORef worldState $ world { entities = map (\e -> onUpdate e e world) (entities world) }

	postRedisplay Nothing

--input :: IORef (Bool, Bool) -> KeyboardMouseCallback
input worldState key state _ _ = case key of
	(SpecialKey KeyLeft) -> worldState $~! \x -> x { keyState = (set, snd $ keyState x) }
	(SpecialKey KeyRight) -> worldState $~! \x -> x { keyState = (fst $ keyState x, set) }
	_ -> return ()
	where set = state == Down
--input _ _ _ _ _ = return ()
