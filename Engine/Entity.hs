{-# LANGUAGE TemplateHaskell #-}

module Engine.Entity where

import Control.Lens

import Engine.Rendering
import Util.Vector

data Entity = Entity
    { _pos :: Vec2
    , _size :: Vec2
    , _vel :: Vec2
    , _onDraw :: Entity -> IO ()
    -- , _onUpdate :: Entity -> Entity
    }
makeLenses ''Entity
newEntity = Entity vZero vZero vZero drawEnt

drawEnt :: Entity -> IO ()
drawEnt entity = renderRect (entity ^. pos) (entity ^. size)
