import Graphics.UI.GLUT
import Data.IORef

data WorldState = WorldState 
	{ entities :: [Entity]
	, heldKeys :: [Key]
	, pressedKeys :: [Key]
	, mousePos :: Vec2
	}

data Vec2 = Vec2
	{ x :: GLfloat
	, y :: GLfloat
	} deriving (Eq, Show)

(Vec2 x1 y1) /+ (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
(Vec2 x1 y1) /- (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
s /.* (Vec2 x y) = Vec2 (s * x) (s * y)
(Vec2 x1 y1) /* (Vec2 x2 y2) = Vec2 (x1 * x2) (y1 * y2)

data UpdateData = UpdateData
	{ newState :: Entity
	, toAdd :: [Entity]
	, toRemove :: [Entity]
	}

data Entity = Entity
	{ pos :: Vec2
	, size :: Vec2
	, vel :: Vec2
	, onDraw :: Entity -> IO ()
	, onUpdate :: Entity -> WorldState -> UpdateData
	}
instance Eq Entity where
	Entity p1 s1 v1 _ _ == Entity p2 s2 v2 _ _ = (p1 == p2) && (s1 == s2) && (v1 == v2)

main :: IO ()
main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Shooter"
	worldState <- newIORef $ WorldState [
		Entity (Vec2 0.0 0.0) (Vec2 shipDim shipDim) (Vec2 0.0 0.0) (drawEntityColor 0 1 0) updatePlayer
		] [] [] (Vec2 0 0)
	displayCallback $= display worldState
	idleCallback $= Just (update worldState)
	keyboardMouseCallback $= Just (input worldState)
	motionCallback $= Just (mouseMotion worldState)
	passiveMotionCallback $= Just (mouseMotion worldState)
	mainLoop

shipDim = 0.08

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

display :: IORef WorldState -> DisplayCallback
display worldState = do
	world <- readIORef worldState
	clear [ColorBuffer]
	loadIdentity

	mapM_ (\e -> (onDraw e) e) (entities world)

	flush

clamp :: (Ord a) => a -> a -> a -> a
clamp t lo hi
	| t < lo = lo
	| t > hi = hi
	| otherwise = t

updateNull :: Entity -> WorldState -> UpdateData
updateNull entity worldState = UpdateData entity [] []

updateVel :: Entity -> WorldState -> UpdateData
updateVel entity worldState = UpdateData newEntity [] []
	where newEntity = entity { pos = pos entity /+ vel entity }

updatePlayer :: Entity -> WorldState -> UpdateData
updatePlayer entity worldState = UpdateData newEntity added removed
	where
		m = mousePos worldState
		newPos = Vec2 2 (-2) /* (m /- Vec2 0.5 0.5)

		isHeld x = x `elem` heldKeys worldState
		isDown x = x `elem` pressedKeys worldState
		newSize = Vec2
			(if isHeld (Char 'a') then shipDim * 0.5 else shipDim)
			(if isHeld (MouseButton LeftButton) then 2 * shipDim else (if isHeld (Char 'g') then shipDim * 0.5 else shipDim))

		newEntity = entity { pos = newPos } { size = newSize }

		added = if isDown (MouseButton LeftButton)
			then [Entity newPos (Vec2 0.025 0.025) (Vec2 0 0.001) (drawEntityColor 1 0 0) updateVel]
			else []

		ents = entities worldState
		removed = if length ents > 5
			then [ents !! 1]
			else []

update :: IORef WorldState -> IdleCallback
update worldState = do
	world <- readIORef worldState

	windowTitle $= "Shooter " ++ (show $ length $ entities world)

	writeIORef worldState $ let
		compact updates = foldr (\(UpdateData e a r) (es, as, rs) -> (e:es, a ++ as, r ++ rs)) ([], [], []) updates
		(ents, adds, rems) = compact $ map (\e -> onUpdate e e world) (entities world)
		newEnts = filter (not . (`elem` rems)) (ents ++ adds)
		in world { entities = newEnts } { pressedKeys = []}

	postRedisplay Nothing

input :: IORef WorldState -> KeyboardMouseCallback
input worldState key state _ _ = do
	world <- readIORef worldState
	let
		f = if state == Down
			then \l -> key : l
			else \l -> filter (not . (== key)) l
		nowHeld = f $ heldKeys world
		f' = if state == Down
			then \l -> key : l
			else id
		nowPressed = f' $ pressedKeys world
	worldState $~! \w -> w { heldKeys = nowHeld } { pressedKeys = nowPressed }

mouseMotion :: IORef WorldState -> MotionCallback
mouseMotion worldState (Position x y) = do
	Size wx wy <- get windowSize
	let winPos = Vec2 ((fromIntegral x) / (fromIntegral wx)) ((fromIntegral y) / (fromIntegral wy))
	worldState $~! \w -> w { mousePos = winPos }
