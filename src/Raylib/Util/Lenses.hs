{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Lenses for raylib types
module Raylib.Util.Lenses where

import Raylib.Internal.TH (genLenses)
import qualified Raylib.Types as RL

$( genLenses
     [ 
       -- Raylib.Types.Core
       ''RL.Vector2,
       ''RL.Vector3,
       ''RL.Vector4,
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
