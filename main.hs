import Graphics.UI.GLUT
import Data.IORef

data WorldState = WorldState 
	{ entities :: [Entity]
	, keyState :: (Bool, Bool)
	}

data Entity = Entity
	{ pos :: (GLfloat, GLfloat)
	, size :: (GLfloat, GLfloat)
	, vel :: (GLfloat, GLfloat)
	, onDraw :: Entity -> IO ()
	, onUpdate :: Entity -> (Bool, Bool) -> Entity
	}

main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Oi"
	worldState <- newIORef $ WorldState [
		Entity (0.0, (-paddleDist)) (paddleWidth, paddleHeight) (0.0, 0.0) (drawEntityColor 0 1 1) updatePlayer,
		Entity (0.0, paddleDist) (paddleWidth, paddleHeight) (0.0, 0.0) drawEntity updateNull,
		Entity (0.0, 0.0) (ballDim, ballDim) (0.0, 1.0) drawEntity updateNull
		] (False, False)
	displayCallback $= display worldState
	idleCallback $= Just (update worldState)
	keyboardMouseCallback $= Just (input worldState)
	mainLoop
	where

stageWidth = 0.8
stageDim = 0.1
paddleWidth = 0.3
paddleHeight = 0.05
paddleDist = 0.8
ballDim = 0.05
drawRect x y w h = do
	renderPrimitive Quads $
		mapM_ (\ (x, y, z) -> vertex $ Vertex3 x y z) points
	where
		points :: [(GLfloat, GLfloat, GLfloat)]
		hw = w / 2
		hh = h / 2
		points = [(x - hw, y - hh, 0), (x + hw, y - hh, 0), (x + hw, y + hh, 0), (x - hw, y + hh, 0)]

drawEntity e = drawRect px py sx sy
	where
		(px, py) = pos e
		(sx, sy) = size e

drawEntityColor r g b e = do
	color $ Color3 r g (b :: GLfloat)
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

getXPos = fst . pos

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


updatePlayer entity input = newEntity
	where
		(x, y) = pos entity
		newX =
			case input of
				(True, False) -> clamp (x - playerSpeed) paddleLeftBound paddleRightBound
				(False, True) -> clamp (x + playerSpeed) paddleLeftBound paddleRightBound
				_ -> x
		newEntity = entity { pos = (newX, y) }


--update :: WorldState -> IORef (Bool, Bool) -> IdleCallback
update worldState = do
	world <- readIORef worldState

	writeIORef worldState $ WorldState (map (\e -> onUpdate e e (keyState world)) (entities world)) (keyState world)

	---- update player
	--case ks of
	--	(True, False) -> worldState $~! \[Entity (x, y) a b d, op, ball] -> [Entity (clamp (x - playerSpeed) paddleLeftBound paddleRightBound, y) a b d, op, ball]
	--	(False, True) -> worldState $~! \[Entity (x, y) a b d, op, ball] -> [Entity (clamp (x + playerSpeed) paddleLeftBound paddleRightBound, y) a b d, op, ball]
	--	_ -> worldState $~! \x -> x

	---- update enemy
	--worldState $~! \[pl, Entity (x, y) s v d, (Entity (bx, by) bv t bd)] -> [pl, Entity (if x > bx then x - opponentSpeed else x + opponentSpeed, y) s v d, Entity (bx, by) bv t bd]

	---- update ball
	--worldState $~! \[x, y, (Entity (bx, by) s (vx, vy) bd)] ->
	--	let
	--		t = vy > 0.0
	--		padPos = if t then getXPos y else getXPos x
	--		d = (bx - padPos) / paddleWidth
	--		didCollidePaddle = (if t then by > paddleDist else by < (-paddleDist)) && abs d < 1.0
	--		didLeaveScreen = abs by > 1.0
	--		didCollideWall = bx > ballRightBound || bx < ballLeftBound
	--		newV = case (didCollideWall, didCollidePaddle, didLeaveScreen) of
	--			(True, _, _) -> -vx
	--			(_, True, _) -> vx + d / 2
	--			(_, _, True) -> vx / 2
	--			_ -> vx
	--		newT = if didCollidePaddle || didLeaveScreen then (-1) * vy else vy
	--		dX = newV * ballSpeed
	--		dY = if newT > 0 then ballSpeed else (-ballSpeed)
	--		newPos = if didLeaveScreen
	--			then (0.0, if t then 0.7 * paddleDist else (- 0.7) * paddleDist)
	--			else (bx + dX, by + dY)
	--	in [x, y, (Entity newPos s (newV, newT) bd)]

	postRedisplay Nothing
	--where

--input :: IORef (Bool, Bool) -> KeyboardMouseCallback
input worldState key Down _ _ = case key of
	(SpecialKey KeyLeft) -> worldState $~! \x -> x { keyState = (True, snd $ keyState x) }
	(SpecialKey KeyRight) -> worldState $~! \x -> x { keyState = (fst $ keyState x, True) }
	_ -> return ()
input worldState key Up _ _ = case key of
	(SpecialKey KeyLeft) -> worldState $~! \x -> x { keyState = (False, snd $ keyState x) }
	(SpecialKey KeyRight) -> worldState $~! \x -> x { keyState = (fst $ keyState x, False) }
	_ -> return ()
--input _ _ _ _ _ = return ()
