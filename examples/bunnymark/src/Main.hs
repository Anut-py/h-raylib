{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}

module Main where

import Control.Monad (replicateM, forM_, void, unless, when)
import Raylib.Core
  ( getScreenHeight
  , getScreenWidth
  , initWindow
  , isMouseButtonDown
  , setTargetFPS
  , windowShouldClose
  , clearBackground
  , changeDirectory
  , getApplicationDirectory
  , getFrameTime
  , beginDrawing
  , endDrawing
  , c'getMouseX
  , c'getMouseY
  , c'getRandomValue
  , isKeyDown
  )
import Raylib.Core.Textures (c'loadTexture, c'drawTexture, c'unloadTexture)
import Raylib.Types.Core (Color (Color), MouseButton (MouseButtonLeft), KeyboardKey (KeyLeftShift))
import Raylib.Types.Core.Textures (Texture (texture'width, texture'height))
import Raylib.Util (raylibApplication, inGHCi, rlFree)
import Raylib.Util.Colors (rayWhite, black, green, maroon)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Text (drawText, drawFPS)
import Foreign (Ptr, malloc, poke, peek, free)
import Foreign.C (withCString)

-- Remove one ../ when disabling optimization
texPath :: String
texPath = (if not inGHCi then "../../../../../../../../../../" else "./") ++ "examples/bunnymark/assets/wabbit_alpha.png"

maxBunnies :: Int
maxBunnies = 500000 -- 500K bunnies limit

data Bunny = Bunny
  { px :: Float,
    py :: Float,
    sx :: Float,
    sy :: Float,
    color :: Ptr Color
  } deriving (Show, Eq)

data AppState = AppState
  { texBunny :: Ptr Texture,
    halfTexWidth :: Float,
    halfTexHeight :: Float,
    bunnies :: [Bunny]
  } deriving (Show, Eq)

startup :: IO AppState
startup = do
  _ <- initWindow 800 450 "raylib [textures] example - bunnymark"
  setTargetFPS 60
  unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)
  texPtr <- withCString texPath c'loadTexture
  tex <- peek texPtr
  return (
    AppState
    { texBunny = texPtr
    , bunnies = []
    , halfTexWidth = fromIntegral (texture'width tex) / 2
    , halfTexHeight = fromIntegral (texture'height tex) / 2
    })

mainLoop :: AppState -> IO AppState
mainLoop state = do
  screenWidth <- getScreenWidth
  screenHeight <- getScreenHeight

  beginDrawing
  clearBackground rayWhite
  forM_ (bunnies state) (\b -> c'drawTexture (texBunny state) (floor (px b)) (floor (py b)) (color b))
  drawRectangle 0 0 screenWidth 40 black
  drawText ("bunnies: " ++ show (length (bunnies state))) 120 10 20 green
  drawText ("batched draw calls: " ++ show (1 + length (bunnies state) `div` 8192)) 320 10 20 maroon
  drawFPS 10 10
  endDrawing

  let
    updatedBunnies =
        map
          ( \b ->
              let px' = px b + sx b
                  py' = py b + sy b
                  adjX = px' + halfTexWidth state
                  adjY = py' + halfTexHeight state
                  sx' = if adjX > fromIntegral screenWidth || adjX < 0 then -(sx b) else sx b
                  sy' = if adjY > fromIntegral screenHeight || adjY < 40 then -(sy b) else sy b
               in b {px = px', py = py', sx = sx', sy = sy'}
          )
          (bunnies state)
  addBunnies updatedBunnies
  where addBunnies updatedBunnies =
          do
            lDown <- isMouseButtonDown MouseButtonLeft
            if lDown
              then do
                frameTime <- getFrameTime
                mx <- realToFrac <$> c'getMouseX
                my <- realToFrac <$> c'getMouseY
                newBunnies <-
                  replicateM
                    (min (round (5000 * frameTime)) (maxBunnies - length updatedBunnies))
                    ( do
                        xSpeed <- realToFrac <$> c'getRandomValue (-250) 250
                        ySpeed <- realToFrac <$> c'getRandomValue (-250) 250
                        r <- fromIntegral <$> c'getRandomValue 50 240
                        g <- fromIntegral <$> c'getRandomValue 80 240
                        b <- fromIntegral <$> c'getRandomValue 100 240
                        ptr <- malloc
                        poke ptr (Color r g b 255)
                        return $ Bunny {px = mx, py = my, sx = xSpeed / 60, sy = ySpeed / 60, color = ptr}
                    )
                shiftDown <- isKeyDown KeyLeftShift
                when shiftDown (print newBunnies)
                return $ state {bunnies = updatedBunnies ++ newBunnies}
              else return $ state {bunnies = updatedBunnies}

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown state = do
  c'unloadTexture (texBunny state) 
  free (texBunny state)
  forM_ (bunnies state) (\(Bunny {color = c}) -> peek c >>= (`rlFree` c))

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
