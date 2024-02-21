{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (unless, void, when)
import Numeric (showFFloat)
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
  )
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawModel, drawSphereWires, genMeshCube, genMeshPlane, genMeshSphere, loadModelFromMesh)
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
import Raylib.Util (inGHCi, setMaterialShader, whileWindowOpen_)
import Raylib.Util.Colors (black, blue, lightGray, orange, white)

assetsPath :: String
assetsPath = (if not inGHCi then "../../../../../../../../../../" else "./") ++ "examples/basic-shaders/assets/"

main :: IO ()
main = do
  window <- initWindow 1300 800 "raylib [shaders] example - basic shaders"
  setTargetFPS 60
  disableCursor
  unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

  let camera = Camera3D (Vector3 1 3 3) (Vector3 1 0 1) (Vector3 0 1 0) 45 CameraPerspective

  shader <- loadShader (Just $ assetsPath ++ "lighting.vert") (Just $ assetsPath ++ "lighting.frag") window

  let pointLightPosition = Vector3 0 3 2
  let pointLightColor = Vector4 1 1 1 1
  let pointLightStrength = 1.0
  let specularStrength = 0.5
  let ambientLightColor = Vector4 1 1 0 1
  let ambientStrength = 0.1

  setShaderValue shader "pointLightPosition" (ShaderUniformVec3 pointLightPosition) window
  setShaderValue shader "pointLightColor" (ShaderUniformVec4 pointLightColor) window
  setShaderValue shader "pointLightStrength" (ShaderUniformFloat pointLightStrength) window
  setShaderValue shader "specularStrength" (ShaderUniformFloat specularStrength) window
  setShaderValue shader "ambientLightColor" (ShaderUniformVec4 ambientLightColor) window
  setShaderValue shader "ambientStrength" (ShaderUniformFloat ambientStrength) window

  cubeMesh <- genMeshCube 2 2 2 window
  cubeModel' <- loadModelFromMesh cubeMesh window
  let cubeModel = setMaterialShader cubeModel' 0 shader

  sphereMesh <- genMeshSphere 0.5 32 32 window
  sphereModel' <- loadModelFromMesh sphereMesh window
  let sphereModel = setMaterialShader sphereModel' 0 shader

  planeMesh <- genMeshPlane 100 100 20 20 window
  planeModel' <- loadModelFromMesh planeMesh window
  let planeModel = setMaterialShader planeModel' 0 shader

  whileWindowOpen_
    ( \(c, ls, ss) -> do
        beginDrawing
        clearBackground black

        beginMode3D c

        drawModel cubeModel (Vector3 0 1 0) 1 orange
        drawModel sphereModel (Vector3 2 0.5 2) 1 blue
        drawModel planeModel (Vector3 0 0 0) 1 lightGray
        drawSphereWires pointLightPosition 0.25 12 12 $ vectorToColor (pointLightColor {vector4'w = ls / 3})

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

        when (yDown || hDown) $ setShaderValue shader "pointLightStrength" (ShaderUniformFloat newLightStrength) window
        when (uDown || jDown) $ setShaderValue shader "specularStrength" (ShaderUniformFloat newSpecularStrength) window

        drawText "Press the Y and H keys to increase and decrease the diffuse strength." 10 10 20 white
        drawText ("Current diffuse strength: " ++ showFFloat (Just 2) newLightStrength "") 10 40 20 white

        drawText "Press the U and J keys to increase and decrease the specular strength." 10 80 20 white
        drawText ("Current specular strength: " ++ showFFloat (Just 2) newSpecularStrength "") 10 110 20 white

        endDrawing

        newCam <- updateCamera c CameraModeFirstPerson
        setShaderValue shader "viewPos" (ShaderUniformVec3 $ camera3D'position newCam) window

        return (newCam, newLightStrength, newSpecularStrength)
    )
    (camera, pointLightStrength, specularStrength)

  closeWindow window

clamp :: Float -> Float -> Float -> Float
clamp x l h = min h (max x l)
