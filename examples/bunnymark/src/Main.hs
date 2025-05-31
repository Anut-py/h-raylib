{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

-- Writing performant h-raylib code requires the use of pointers. However,
-- most h-raylib functions work fine with pointers.

module Main where

import Control.Monad (forM_, when)
import Foreign
  ( ForeignPtr,
    Ptr,
    Storable (alignment, peek, poke, sizeOf),
    advancePtr,
    mallocForeignPtrArray,
    peek,
    plusPtr,
    poke,
    withForeignPtr,
  )
import Foreign.C (CFloat)
import Paths_h_raylib (getDataFileName)
import Raylib.Core
  ( beginDrawing,
    getMouseX,
    getMouseY,
    getRandomValue,
    clearBackground,
    closeWindow,
    endDrawing,
    getFrameTime,
    getScreenHeight,
    getScreenWidth,
    initWindow,
    isMouseButtonDown,
    setTargetFPS,
    windowShouldClose,
  )
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Text (drawFPS, drawText)
import Raylib.Core.Textures (drawTexture, loadTexture)
import Raylib.Types (Color (Color), MouseButton (MouseButtonLeft), Texture, p'texture'height, p'texture'width)
import Raylib.Util (WindowResources, managed, raylibApplication)
import Raylib.Util.Colors (black, green, maroon, rayWhite)

texPath :: String
texPath = "examples/bunnymark/assets/wabbit_alpha.png"

maxBunnies :: Int
maxBunnies = 500000 -- 500K bunnies limit

-- Strict fields
data Bunny = Bunny
  { px :: !Float,
    py :: !Float,
    sx :: !Float,
    sy :: !Float,
    color :: !Color
  }
  deriving (Show, Eq)

cfs :: Int
cfs = sizeOf (0 :: CFloat)

cls :: Int
cls = sizeOf (undefined :: Color)

instance Storable Bunny where
  alignment _ = min cfs cls
  sizeOf _ = 4 * cfs + cls

  -- We do not peek/poke entire bunnies in this example, so the code below is never called
  -- (we advance the pointers using the `p'*` functions to directly access the fields)
  peek _p = do
    _px <- realToFrac <$> peek (p'px _p)
    _py <- realToFrac <$> peek (p'py _p)
    _sx <- realToFrac <$> peek (p'sx _p)
    _sy <- realToFrac <$> peek (p'sy _p)
    _color <- peek (p'color _p)
    return $ Bunny {px = _px, py = _py, sx = _sx, sy = _sy, color = _color}
  poke _p (Bunny _px _py _sx _sy _color) = do
    poke (p'px _p) (realToFrac _px)
    poke (p'py _p) (realToFrac _py)
    poke (p'sx _p) (realToFrac _sx)
    poke (p'sy _p) (realToFrac _sy)
    poke (p'color _p) _color
    return ()

p'px :: Ptr Bunny -> Ptr CFloat
p'px = (`plusPtr` (0 * cfs))

p'py :: Ptr Bunny -> Ptr CFloat
p'py = (`plusPtr` (1 * cfs))

p'sx :: Ptr Bunny -> Ptr CFloat
p'sx = (`plusPtr` (2 * cfs))

p'sy :: Ptr Bunny -> Ptr CFloat
p'sy = (`plusPtr` (3 * cfs))

p'color :: Ptr Bunny -> Ptr Color
p'color = (`plusPtr` (4 * cfs))

data AppState = AppState
  { texBunny :: !(ForeignPtr Texture),
    halfTexWidth :: !CFloat,
    halfTexHeight :: !CFloat,
    -- Store the bunnies as a pointer (C-style array) because Haskell linked lists are extremely slow.
    -- For actual applications a `vector` is probably more practical.
    bunnies :: !(ForeignPtr Bunny),
    bunniesCount :: !Int,
    window :: !WindowResources
  }

startup :: IO AppState
startup = do
  wr <- initWindow 800 450 "raylib [textures] example - bunnymark"
  setTargetFPS 60
  texPath' <- getDataFileName texPath
  -- Use `withForeignPtr` and `peek` when you need to access the underlying fields.
  -- You don't need to worry about freeing/unloading it because `managed` takes
  -- care of that automatically.
  texPtr <- managed wr $ loadTexture texPath'

  withForeignPtr
    texPtr
    (\p -> do
      -- This could be rewritten as
      --   tex <- peek p
      --   let tWidth = texture'width tex
      --   ...
      -- but the code below is faster as it doesn't have to load the entire structure into Haskell
      tWidth <- peek (p'texture'width p)
      tHeight <- peek (p'texture'height p)
      bunniesPtr <- mallocForeignPtrArray maxBunnies

      return
        ( AppState
            { texBunny = texPtr,
              bunnies = bunniesPtr,
              halfTexWidth = fromIntegral tWidth / 2,
              halfTexHeight = fromIntegral tHeight / 2,
              bunniesCount = 0,
              window = wr
            }
        )
    )

mainLoop :: AppState -> IO AppState
mainLoop state = do
  withForeignPtr (bunnies state) $ \bptr -> do
    screenWidth <- getScreenWidth
    screenHeight <- getScreenHeight

    beginDrawing
    clearBackground rayWhite
    forM_
      [0 .. (bunniesCount state - 1)]
      ( \(!i) ->
          do
            -- Advancing the array pointer to get a pointer to a bunny
            let bunny = advancePtr bptr i
            -- Advancing the bunny pointer to access the fields
            _px <- peek $ p'px bunny
            _py <- peek $ p'py bunny
            drawTexture (texBunny state) (floor _px) (floor _py) =<< peek (p'color bunny)
      )
    drawRectangle 0 0 screenWidth 40 black
    drawText ("bunnies: " ++ show (bunniesCount state)) 120 10 20 green
    drawText ("batched draw calls: " ++ show (1 + (bunniesCount state `div` 8192))) 320 10 20 maroon
    drawFPS 10 10
    endDrawing

    forM_
      [0 .. (bunniesCount state - 1)]
      ( \(!i) ->
          do
            -- Same thing as before, but reading (`peek`) _and_ writing (`poke`)
            let bunny = advancePtr bptr i
            _px <- peek $ p'px bunny
            _py <- peek $ p'py bunny
            _sx <- peek $ p'sx bunny
            _sy <- peek $ p'sy bunny
            let px' = _px + _sx
                py' = _py + _sy
                adjX = px' + halfTexWidth state
                adjY = py' + halfTexHeight state
            poke (p'px bunny) px'
            poke (p'py bunny) py'
            when (adjX > fromIntegral screenWidth || adjX < 0) $ poke (p'sx bunny) (-_sx)
            when (adjY > fromIntegral screenHeight || adjY < 40) $ poke (p'sy bunny) (-_sy)
      )

    do
      lDown <- isMouseButtonDown MouseButtonLeft
      if lDown
        then do
          frameTime <- getFrameTime
          let newBunnies = min (round (10000 * frameTime)) (maxBunnies - bunniesCount state)
          mx <- getMouseX
          my <- getMouseY
          forM_
            [bunniesCount state .. (bunniesCount state + newBunnies - 1)]
            ( \(!i) ->
                do
                  -- Creating elements uses `poke`, just like writing
                  let bunny = advancePtr bptr i
                  xSpeed <- (/ 60) . fromIntegral <$> getRandomValue (-250) 250
                  ySpeed <- (/ 60) . fromIntegral <$> getRandomValue (-250) 250
                  r <- fromIntegral <$> getRandomValue 50 240
                  g <- fromIntegral <$> getRandomValue 80 240
                  b <- fromIntegral <$> getRandomValue 100 240

                  poke (p'px bunny) mx
                  poke (p'py bunny) my
                  poke (p'sx bunny) xSpeed
                  poke (p'sy bunny) ySpeed
                  poke (p'color bunny) (Color r g b 255)
            )
          return $ state {bunniesCount = bunniesCount state + newBunnies}
        else return state

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown state = closeWindow (Just (window state))

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
