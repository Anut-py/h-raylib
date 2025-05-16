{-# LANGUAGE PatternSynonyms #-}

module Main where

import Paths_h_raylib (getDataFileName)
import Raylib.Core (clearBackground)
import Raylib.Core.Models (drawGrid)
import Raylib.Core.Textures (loadTexture)
import Raylib.Types (Camera3D (Camera3D), CameraProjection (CameraPerspective), Color (Color), RLDrawMode (RLQuads), Rectangle (Rectangle), Texture (texture'height, texture'id, texture'width), Vector3, pattern Vector3)
import Raylib.Util (drawing, managed, mode3D, whileWindowOpen0, withWindow)
import Raylib.Util.Colors (rayWhite, white)
import Raylib.Util.RLGL (rlBegin, rlColor4ub, rlEnd, rlNormal3f, rlPopMatrix, rlPushMatrix, rlRotatef, rlScalef, rlSetTexture, rlTexCoord2f, rlTranslatef, rlVertex3f)
import Prelude hiding (length)

texturePath :: String
texturePath = "examples/basic-rlgl/assets/cubicmap_atlas.png"

main :: IO ()
main = do
  withWindow
    650
    400
    "raylib [rlgl] example - basic rlgl"
    60
    ( \window -> do
        let camera = Camera3D (Vector3 0 10 10) (Vector3 0 0 0) (Vector3 0 1 0) 45 CameraPerspective

        texture <- managed window $ loadTexture =<< getDataFileName texturePath

        whileWindowOpen0
          ( drawing
              ( do
                  clearBackground rayWhite
                  mode3D
                    camera
                    ( do
                        drawCubeTexture texture (Vector3 (-2) 2 0) 2 4 2 white
                        drawCubeTextureRec
                          texture
                          (Rectangle 0 (fromIntegral (texture'height texture) / 2.0) (fromIntegral (texture'width texture) / 2.0) (fromIntegral (texture'height texture) / 2.0))
                          (Vector3 2 1 0)
                          2
                          2
                          2
                          white
                        drawGrid 10 1
                    )
              )
          )
    )

drawCubeTexture :: Texture -> Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCubeTexture texture (Vector3 x y z) width height length (Color r g b a) = do
  rlSetTexture $ texture'id texture

  rlPushMatrix

  -- NOTE: Transformation is applied in inverse order (scale -> rotate -> translate)
  rlTranslatef (-1) 0 0
  rlRotatef 45 0 1 0
  rlScalef 1.5 1.5 1.5

  rlBegin RLQuads
  rlColor4ub r g b a

  -- Front Face
  rlNormal3f 0 0 1 -- Normal Pointing Towards Viewer
  rlTexCoord2f 0 0 >> rlVertex3f (x - width / 2) (y - height / 2) (z + length / 2) -- Bottom Left Of The Texture and Quad
  rlTexCoord2f 1 0 >> rlVertex3f (x + width / 2) (y - height / 2) (z + length / 2) -- Bottom Right Of The Texture and Quad
  rlTexCoord2f 1 1 >> rlVertex3f (x + width / 2) (y + height / 2) (z + length / 2) -- Top Right Of The Texture and Quad
  rlTexCoord2f 0 1 >> rlVertex3f (x - width / 2) (y + height / 2) (z + length / 2) -- Top Left Of The Texture and Quad

  -- Back Face
  rlNormal3f 0 0 (-1) -- Normal Pointing Away From Viewer
  rlTexCoord2f 1 0 >> rlVertex3f (x - width / 2) (y - height / 2) (z - length / 2) -- Bottom Right Of The Texture and Quad
  rlTexCoord2f 1 1 >> rlVertex3f (x - width / 2) (y + height / 2) (z - length / 2) -- Top Right Of The Texture and Quad
  rlTexCoord2f 0 1 >> rlVertex3f (x + width / 2) (y + height / 2) (z - length / 2) -- Top Left Of The Texture and Quad
  rlTexCoord2f 0 0 >> rlVertex3f (x + width / 2) (y - height / 2) (z - length / 2) -- Bottom Left Of The Texture and Quad

  -- Top Face
  rlNormal3f 0 1 0 -- Normal Pointing Up
  rlTexCoord2f 0 1 >> rlVertex3f (x - width / 2) (y + height / 2) (z - length / 2) -- Top Left Of The Texture and Quad
  rlTexCoord2f 0 0 >> rlVertex3f (x - width / 2) (y + height / 2) (z + length / 2) -- Bottom Left Of The Texture and Quad
  rlTexCoord2f 1 0 >> rlVertex3f (x + width / 2) (y + height / 2) (z + length / 2) -- Bottom Right Of The Texture and Quad
  rlTexCoord2f 1 1 >> rlVertex3f (x + width / 2) (y + height / 2) (z - length / 2) -- Top Right Of The Texture and Quad

  -- Bottom Face
  rlNormal3f 0 (-1) 0 -- Normal Pointing Down
  rlTexCoord2f 1 1 >> rlVertex3f (x - width / 2) (y - height / 2) (z - length / 2) -- Top Right Of The Texture and Quad
  rlTexCoord2f 0 1 >> rlVertex3f (x + width / 2) (y - height / 2) (z - length / 2) -- Top Left Of The Texture and Quad
  rlTexCoord2f 0 0 >> rlVertex3f (x + width / 2) (y - height / 2) (z + length / 2) -- Bottom Left Of The Texture and Quad
  rlTexCoord2f 1 0 >> rlVertex3f (x - width / 2) (y - height / 2) (z + length / 2) -- Bottom Right Of The Texture and Quad

  -- Right face
  rlNormal3f 1 0 0 -- Normal Pointing Right
  rlTexCoord2f 1 0 >> rlVertex3f (x + width / 2) (y - height / 2) (z - length / 2) -- Bottom Right Of The Texture and Quad
  rlTexCoord2f 1 1 >> rlVertex3f (x + width / 2) (y + height / 2) (z - length / 2) -- Top Right Of The Texture and Quad
  rlTexCoord2f 0 1 >> rlVertex3f (x + width / 2) (y + height / 2) (z + length / 2) -- Top Left Of The Texture and Quad
  rlTexCoord2f 0 0 >> rlVertex3f (x + width / 2) (y - height / 2) (z + length / 2) -- Bottom Left Of The Texture and Quad

  -- Left Face
  rlNormal3f (-1) 0 0 -- Normal Pointing Left
  rlTexCoord2f 0 0 >> rlVertex3f (x - width / 2) (y - height / 2) (z - length / 2) -- Bottom Left Of The Texture and Quad
  rlTexCoord2f 1 0 >> rlVertex3f (x - width / 2) (y - height / 2) (z + length / 2) -- Bottom Right Of The Texture and Quad
  rlTexCoord2f 1 1 >> rlVertex3f (x - width / 2) (y + height / 2) (z + length / 2) -- Top Right Of The Texture and Quad
  rlTexCoord2f 0 1 >> rlVertex3f (x - width / 2) (y + height / 2) (z - length / 2) -- Top Left Of The Texture and Quad
  rlEnd
  rlPopMatrix

  rlSetTexture 0

drawCubeTextureRec :: Texture -> Rectangle -> Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCubeTextureRec texture (Rectangle sX sY sW sH) (Vector3 x y z) width height length (Color r g b a) = do
  let texWidth = realToFrac $ texture'width texture :: Float
      texHeight = realToFrac $ texture'height texture :: Float

  rlSetTexture $ texture'id texture

  -- We calculate the normalized texture coordinates for the desired texture-source-rectangle
  -- It means converting from (tex.width, tex.height) coordinates to [0.0f, 1.0f] equivalent
  rlBegin RLQuads
  rlColor4ub r g b a

  -- Front face
  rlNormal3f 0 0 1
  rlTexCoord2f (sX / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x - width / 2) (y - height / 2) (z + length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x + width / 2) (y - height / 2) (z + length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) (sY / texHeight)
  rlVertex3f (x + width / 2) (y + height / 2) (z + length / 2)
  rlTexCoord2f (sX / texWidth) (sY / texHeight)
  rlVertex3f (x - width / 2) (y + height / 2) (z + length / 2)

  -- Back face
  rlNormal3f 0 0 (-1)
  rlTexCoord2f ((sX + sW) / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x - width / 2) (y - height / 2) (z - length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) (sY / texHeight)
  rlVertex3f (x - width / 2) (y + height / 2) (z - length / 2)
  rlTexCoord2f (sX / texWidth) (sY / texHeight)
  rlVertex3f (x + width / 2) (y + height / 2) (z - length / 2)
  rlTexCoord2f (sX / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x + width / 2) (y - height / 2) (z - length / 2)

  -- Top face
  rlNormal3f 0 1 0
  rlTexCoord2f (sX / texWidth) (sY / texHeight)
  rlVertex3f (x - width / 2) (y + height / 2) (z - length / 2)
  rlTexCoord2f (sX / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x - width / 2) (y + height / 2) (z + length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x + width / 2) (y + height / 2) (z + length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) (sY / texHeight)
  rlVertex3f (x + width / 2) (y + height / 2) (z - length / 2)

  -- Bottom face
  rlNormal3f 0 (-1) 0
  rlTexCoord2f ((sX + sW) / texWidth) (sY / texHeight)
  rlVertex3f (x - width / 2) (y - height / 2) (z - length / 2)
  rlTexCoord2f (sX / texWidth) (sY / texHeight)
  rlVertex3f (x + width / 2) (y - height / 2) (z - length / 2)
  rlTexCoord2f (sX / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x + width / 2) (y - height / 2) (z + length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x - width / 2) (y - height / 2) (z + length / 2)

  -- Right face
  rlNormal3f 1 0 0
  rlTexCoord2f ((sX + sW) / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x + width / 2) (y - height / 2) (z - length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) (sY / texHeight)
  rlVertex3f (x + width / 2) (y + height / 2) (z - length / 2)
  rlTexCoord2f (sX / texWidth) (sY / texHeight)
  rlVertex3f (x + width / 2) (y + height / 2) (z + length / 2)
  rlTexCoord2f (sX / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x + width / 2) (y - height / 2) (z + length / 2)

  -- Left face
  rlNormal3f (-1) 0 0
  rlTexCoord2f (sX / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x - width / 2) (y - height / 2) (z - length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) ((sY + sH) / texHeight)
  rlVertex3f (x - width / 2) (y - height / 2) (z + length / 2)
  rlTexCoord2f ((sX + sW) / texWidth) (sY / texHeight)
  rlVertex3f (x - width / 2) (y + height / 2) (z + length / 2)
  rlTexCoord2f (sX / texWidth) (sY / texHeight)
  rlVertex3f (x - width / 2) (y + height / 2) (z - length / 2)

  rlEnd

  rlSetTexture 0
