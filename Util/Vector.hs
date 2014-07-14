module Util.Vector where

import Graphics.Rendering.OpenGL

data Vec2 = Vec2
    { x :: GLfloat
    , y :: GLfloat
    } deriving (Eq, Show)

-- Basic operations
(/+/) :: Vec2 -> Vec2 -> Vec2
(Vec2 x1 y1) /+/ (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

(/-/) :: Vec2 -> Vec2 -> Vec2
(Vec2 x1 y1) /-/ (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)

(.*/) :: GLfloat -> Vec2 -> Vec2
s .*/ (Vec2 x y) = Vec2 (s * x) (s * y)

(/*.) :: Vec2 -> GLfloat -> Vec2
v /*. s = s .*/ v

(/*/) :: Vec2 -> Vec2 -> Vec2
(Vec2 x1 y1) /*/ (Vec2 x2 y2) = Vec2 (x1 * x2) (y1 * y2)

-- Vector operations
mag2 :: Vec2 -> GLfloat
mag2 (Vec2 x y) = x^2 + y^2

mag :: Vec2 -> GLfloat
mag v = sqrt $ mag2 v

dot :: Vec2 -> Vec2 -> GLfloat
(Vec2 x1 y1) `dot` (Vec2 x2 y2) = x1 * x2 + y1 * y2

cross :: Vec2 -> Vec2 -> GLfloat
(Vec2 x1 y1) `cross` (Vec2 x2 y2) = x1 * y2 - y1 * x2

-- Constructors
unitVec :: (Real a, Floating a) => a -> Vec2
unitVec angle = Vec2 (aux cos) (aux sin)
    where aux f = (realToFrac . f $ angle)
