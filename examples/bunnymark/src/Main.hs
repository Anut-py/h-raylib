{-# OPTIONS -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

-- Writing performant h-raylib code requires the use of pointers and other
-- un-Haskelly functionality. Unfortunately, this cannot be avoided.

module Main where

import Control.Monad (forM_, unless, void, when)
import Foreign
  ( Ptr,
    Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    advancePtr,
    callocArray,
    free,
    malloc,
    nullPtr,
    peek,
    plusPtr,
    poke,
  )
import Foreign.C (CFloat, withCString)
import Raylib.Core
  ( beginDrawing,
    c'getMouseX,
    c'getMouseY,
    c'getRandomValue,
    changeDirectory,
    clearBackground,
    endDrawing,
    getApplicationDirectory,
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
import Raylib.Core.Textures (c'drawTexture, c'loadTexture, c'unloadTexture)
import Raylib.Types (Color (Color), MouseButton (MouseButtonLeft), Texture, p'texture'height, p'texture'width)
import Raylib.Util (inGHCi, raylibApplication)
import Raylib.Util.Colors (black, green, maroon, rayWhite)

texPath :: String
texPath = (if not inGHCi then "../../../../../../../../../../" else "./") ++ "examples/bunnymark/assets/wabbit_alpha.png"

maxBunnies :: Int
maxBunnies = 500000 -- 500K bunnies limit

-- Strict fields
data Bunny = Bunny
  { px :: !Float,
    py :: !Float,
    sx :: !Float,
    sy :: !Float,
    color :: !(Ptr Color) -- Store colors as pointers to avoid reallocating memory each time
  }
  deriving (Show, Eq)

cfs :: Int
cfs = sizeOf (0 :: CFloat)

nps :: Int
nps = sizeOf nullPtr

instance Storable Bunny where
  alignment _ = min cfs nps
  sizeOf _ = 4 * cfs + nps

  -- We do not peek/poke entire bunnies in this example, so the code below is never called
  -- (we advance the pointers using the `p'*` functions to directly access the fields)
  peek _p = do
    _px <- realToFrac <$> (peekByteOff _p (0 * cfs) :: IO CFloat)
    _py <- realToFrac <$> (peekByteOff _p (1 * cfs) :: IO CFloat)
    _sx <- realToFrac <$> (peekByteOff _p (2 * cfs) :: IO CFloat)
    _sy <- realToFrac <$> (peekByteOff _p (3 * cfs) :: IO CFloat)
    _color <- peekByteOff _p (4 * cfs) :: IO (Ptr Color)
    return $ Bunny {px = _px, py = _py, sx = _sx, sy = _sy, color = _color}
  poke _p (Bunny _px _py _sx _sy _color) = do
    pokeByteOff _p (0 * cfs) (realToFrac _px :: CFloat)
    pokeByteOff _p (1 * cfs) (realToFrac _py :: CFloat)
    pokeByteOff _p (2 * cfs) (realToFrac _sx :: CFloat)
    pokeByteOff _p (3 * cfs) (realToFrac _sy :: CFloat)
    pokeByteOff _p (4 * cfs) _color
    return ()

p'px :: Ptr Bunny -> Ptr CFloat
p'px = (`plusPtr` (0 * cfs))

p'py :: Ptr Bunny -> Ptr CFloat
p'py = (`plusPtr` (1 * cfs))

p'sx :: Ptr Bunny -> Ptr CFloat
p'sx = (`plusPtr` (2 * cfs))

p'sy :: Ptr Bunny -> Ptr CFloat
p'sy = (`plusPtr` (3 * cfs))

p'color :: Ptr Bunny -> Ptr (Ptr Color)
p'color = (`plusPtr` (4 * cfs))

data AppState = AppState
  { texBunny :: !(Ptr Texture),
    halfTexWidth :: !CFloat,
    halfTexHeight :: !CFloat,
    bunnies :: !(Ptr Bunny), -- Store the bunnies as a pointer because Haskell linked lists are extremely slow
    bunniesCount :: !Int
  }
  deriving (Show, Eq)

startup :: IO AppState
startup = do
  _ <- initWindow 800 450 "raylib [textures] example - bunnymark"
  setTargetFPS 60
  unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)
  texPtr <- withCString texPath c'loadTexture
  -- Use `peek` when you need to access the underlying fields

  -- This could be rewritten as
  --   tex <- peek texPtr
  --   let tWidth = texture'width tex
  --   ...
  -- but the code below is faster as it doesn't have to load the entire structure into Haskell
  tWidth <- peek (p'texture'width texPtr)
  tHeight <- peek (p'texture'height texPtr)
  bunniesPtr <- callocArray maxBunnies
  return
    ( AppState
        { texBunny = texPtr,
          bunnies = bunniesPtr,
          halfTexWidth = fromIntegral tWidth / 2,
          halfTexHeight = fromIntegral tHeight / 2,
          bunniesCount = 0
        }
    )

mainLoop :: AppState -> IO AppState
mainLoop state = do
  screenWidth <- getScreenWidth
  screenHeight <- getScreenHeight

  beginDrawing
  clearBackground rayWhite
  forM_
    [0 .. (bunniesCount state - 1)]
    ( \(!i) ->
        do
          -- Advancing the array pointer to get a pointer to a bunny
          let bunny = advancePtr (bunnies state) i
          -- Advancing the bunny pointer to access the fields
          _px <- peek $ p'px bunny
          _py <- peek $ p'py bunny
          _color <- peek $ p'color bunny
          c'drawTexture (texBunny state) (floor _px) (floor _py) _color
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
          let bunny = advancePtr (bunnies state) i
          _px <- peek $ p'px bunny
          _py <- peek $ p'py bunny
          _sx <- peek $ p'sx bunny
          _sy <- peek $ p'sy bunny
          _color <- peek $ p'color bunny
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
        mx <- realToFrac <$> c'getMouseX
        my <- realToFrac <$> c'getMouseY
        forM_
          [bunniesCount state .. (bunniesCount state + newBunnies - 1)]
          ( \(!i) ->
              do
                -- Creating elements uses `poke`, just like writing
                let bunny = advancePtr (bunnies state) i
                xSpeed <- (/ 60) . fromIntegral <$> c'getRandomValue (-250) 250
                ySpeed <- (/ 60) . fromIntegral <$> c'getRandomValue (-250) 250
                r <- fromIntegral <$> c'getRandomValue 50 240
                g <- fromIntegral <$> c'getRandomValue 80 240
                b <- fromIntegral <$> c'getRandomValue 100 240
                ptr <- malloc
                poke ptr (Color r g b 255)

                poke (p'px bunny) mx
                poke (p'py bunny) my
                poke (p'sx bunny) xSpeed
                poke (p'sy bunny) ySpeed
                poke (p'color bunny) ptr
          )
        return $ state {bunniesCount = bunniesCount state + newBunnies}
      else return state

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown state = do
  -- Unload and free functions have to be manually called
  c'unloadTexture (texBunny state)
  free (texBunny state)
  forM_ [0 .. (bunniesCount state - 1)] (\(!i) -> let bunny = advancePtr (bunnies state) i in free =<< peek (p'color bunny))
  free (bunnies state)

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
