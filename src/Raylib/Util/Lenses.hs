{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Lenses for raylib types
module Raylib.Util.Lenses where

import Control.Lens (Lens', lens)
import Raylib.Util.Lenses.TH (genLenses)
import qualified Raylib.Types as RL

$( genLenses
     [ -- Raylib.Types.Core
       ''RL.Matrix,
       ''RL.Color,
       ''RL.Rectangle,
       ''RL.VrDeviceInfo,
       ''RL.VrStereoConfig,
       ''RL.FilePathList,
       ''RL.AutomationEvent,
       ''RL.AutomationEventList,
       -- Raylib.Types.Core.Audio
       ''RL.Wave,
       ''RL.RAudioBuffer,
       ''RL.RAudioProcessor,
       ''RL.AudioStream,
       ''RL.Sound,
       ''RL.Music,
       -- Raylib.Types.Core.Camera
       ''RL.Camera3D,
       ''RL.Camera2D,
       -- Raylib.Types.Core.Models
       ''RL.Mesh,
       ''RL.Shader,
       ''RL.MaterialMap,
       ''RL.Material,
       ''RL.Transform,
       ''RL.BoneInfo,
       ''RL.Model,
       ''RL.ModelAnimation,
       ''RL.Ray,
       ''RL.RayCollision,
       ''RL.BoundingBox,
       -- Raylib.Types.Core.Text
       ''RL.GlyphInfo,
       ''RL.Font,
       -- Raylib.Types.Core.Textures
       ''RL.Image,
       ''RL.Texture,
       ''RL.RenderTexture,
       ''RL.NPatchInfo,
       -- Raylib.Types.Util.RLGL
       ''RL.RLVertexBuffer,
       ''RL.RLDrawCall,
       ''RL.RLRenderBatch,
       -- Raylib.Types.Util.GUI
       ''RL.GuiStyleProp
     ]
 )

_vector2'x :: Lens' RL.Vector2 Float
_vector2'x = lens RL.vector2'x (\x v -> x {RL.vector2'x = v})

_vector2'y :: Lens' RL.Vector2 Float
_vector2'y = lens RL.vector2'y (\x v -> x {RL.vector2'y = v})

_vector3'x :: Lens' RL.Vector3 Float
_vector3'x = lens RL.vector3'x (\x v -> x {RL.vector3'x = v})

_vector3'y :: Lens' RL.Vector3 Float
_vector3'y = lens RL.vector3'y (\x v -> x {RL.vector3'y = v})

_vector3'z :: Lens' RL.Vector3 Float
_vector3'z = lens RL.vector3'z (\x v -> x {RL.vector3'z = v})

_vector4'x :: Lens' RL.Vector4 Float
_vector4'x = lens RL.vector4'x (\x v -> x {RL.vector4'x = v})

_vector4'y :: Lens' RL.Vector4 Float
_vector4'y = lens RL.vector4'y (\x v -> x {RL.vector4'y = v})

_vector4'z :: Lens' RL.Vector4 Float
_vector4'z = lens RL.vector4'z (\x v -> x {RL.vector4'z = v})

_vector4'w :: Lens' RL.Vector4 Float
_vector4'w = lens RL.vector4'w (\x v -> x {RL.vector4'w = v})
