{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (when)
import Raylib.Core
  ( beginDrawing,
    beginMode3D,
    changeDirectory,
    clearBackground,
    closeWindow,
    disableCursor,
    endDrawing,
    endMode3D,
    getApplicationDirectory,
    getFrameTime,
    initWindow,
    isKeyDown,
    loadShader,
    setShaderValue,
    setTargetFPS,
    updateCamera,
  )
import Raylib.Core.Models (drawModel, drawSphere, genMeshCube, loadModelFromMesh)
import Raylib.Core.Text (drawText)
import Raylib.Types
  ( Camera3D (Camera3D, camera3D'position),
    CameraMode (CameraModeFirstPerson),
    CameraProjection (CameraPerspective),
    KeyboardKey (KeyH, KeyJ, KeyU, KeyY),
    ShaderUniformData
      ( ShaderUniformFloat,
        ShaderUniformVec3,
        ShaderUniformVec4
      ),
    Vector3 (Vector3),
    Vector4 (Vector4, vector4'w),
    vectorToColor,
  )
import Raylib.Util (whileWindowOpen_, setMaterialShader)
import Raylib.Util.Colors (black, orange, white)

assetsPath :: String
assetsPath = "../../../../../../../../../examples/basic-shaders/assets/"

main :: IO ()
main = do
  initWindow 1300 800 "raylib [core] example - basic shaders"
  setTargetFPS 60
  disableCursor
  _ <- changeDirectory =<< getApplicationDirectory

  let camera = Camera3D (Vector3 1 2 3) (Vector3 1 0 1) (Vector3 0 1 0) 45 CameraPerspective

  shader <- loadShader (Just $ assetsPath ++ "lighting.vert") (Just $ assetsPath ++ "lighting.frag")

  let pointLightPosition = Vector3 0 3 2
  let pointLightColor = Vector4 1 1 1 1
  let pointLightStrength = 1.0
  let specularStrength = 0.5
  let ambientLightColor = Vector4 1 1 0 1
  let ambientStrength = 0.1

  setShaderValue shader "pointLightPosition" (ShaderUniformVec3 pointLightPosition)
  setShaderValue shader "pointLightColor" (ShaderUniformVec4 pointLightColor)
  setShaderValue shader "pointLightStrength" (ShaderUniformFloat pointLightStrength)
  setShaderValue shader "specularStrength" (ShaderUniformFloat specularStrength)
  setShaderValue shader "ambientLightColor" (ShaderUniformVec4 ambientLightColor)
  setShaderValue shader "ambientStrength" (ShaderUniformFloat ambientStrength)

  cubeMesh <- genMeshCube 2 2 2
  cubeModel' <- loadModelFromMesh cubeMesh
  let cubeModel = setMaterialShader cubeModel' 0 shader

  whileWindowOpen_
    ( \(c, ls, ss) -> do
        beginDrawing
        clearBackground black

        beginMode3D c

        drawModel cubeModel (Vector3 0 0 0) 1 orange
        drawSphere pointLightPosition 0.25 $ vectorToColor (pointLightColor {vector4'w = ls / 3.0})

        endMode3D

        yDown <- isKeyDown KeyY
        hDown <- isKeyDown KeyH
        uDown <- isKeyDown KeyU
        jDown <- isKeyDown KeyJ
        frameTime <- getFrameTime

        let newLightStrength = clamp (ls + change * frameTime) 0 3
              where
                change
                  | yDown = 1
                  | hDown = -1
                  | otherwise = 0

        let newSpecularStrength = clamp (ss + change * frameTime) 0 2
              where
                change
                  | uDown = 1
                  | jDown = -1
                  | otherwise = 0

        when (yDown || hDown) $ setShaderValue shader "pointLightStrength" (ShaderUniformFloat newLightStrength)
        when (uDown || jDown) $ setShaderValue shader "specularStrength" (ShaderUniformFloat newSpecularStrength)

        drawText "Press the Y and H keys to increase and decrease the diffuse strength." 10 10 20 white
        drawText ("Current diffuse strength: " ++ take 4 (show newLightStrength)) 10 40 20 white

        drawText "Press the U and J keys to increase and decrease the specular strength." 10 80 20 white
        drawText ("Current specular strength: " ++ take 4 (show newSpecularStrength)) 10 110 20 white

        endDrawing

        newCam <- updateCamera c CameraModeFirstPerson
        setShaderValue shader "viewPos" (ShaderUniformVec3 $ camera3D'position newCam)

        return (newCam, newLightStrength, newSpecularStrength)
    )
    (camera, pointLightStrength, specularStrength)

  closeWindow

clamp :: Float -> Float -> Float -> Float
clamp x l h = min h (max x l)
