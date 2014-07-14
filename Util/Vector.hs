module Util.Vector where

import Graphics.Rendering.OpenGL

data Vec2 = Vec2
    { x :: GLfloat
    , y :: GLfloat
    } deriving (Eq, Show)

(Vec2 x1 y1) /+/ (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
(Vec2 x1 y1) /-/ (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
s .*/ (Vec2 x y) = Vec2 (s * x) (s * y)
v /*. s = s .*/ v
(Vec2 x1 y1) /*/ (Vec2 x2 y2) = Vec2 (x1 * x2) (y1 * y2)
