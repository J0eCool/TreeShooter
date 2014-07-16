module Engine.Rendering where

import Graphics.Rendering.OpenGL

import Util.Vector

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
