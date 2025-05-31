{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
import Raylib.Core
  ( clearBackground,
    closeWindow,
    getFrameTime,
    getMousePosition,
    initWindow,
    isKeyDown,
    isKeyPressed,
    isMouseButtonPressed,
    newAutomationEventList,
    peekAutomationEventList,
    playAutomationEvent,
    setAutomationEventBaseFrame,
    setAutomationEventList,
    setTargetFPS,
    startAutomationEventRecording,
    stopAutomationEventRecording,
    windowShouldClose,
  )
import Raylib.Core.Shapes (drawCircleV, drawRectangle, drawRectangleLines)
import Raylib.Core.Text (drawText, measureText)
import Raylib.Types
  ( AutomationEvent (AutomationEvent),
    AutomationEventList (automationEventList'events),
    AutomationEventListRef,
    Color (Color),
    KeyboardKey (KeyDown, KeyLeft, KeyP, KeyR, KeyRight, KeyUp),
    MouseButton (MouseButtonLeft),
    pattern Vector2,
  )
import Raylib.Types.Core (Vector2)
import Raylib.Util (WindowResources, drawing, managed, raylibApplication)
import Raylib.Util.Colors (black, red, white)

type AppState = (WindowResources, Int, Int, Maybe (Vector2, Float), Maybe ([AutomationEvent], Integer), Bool, Bool, Maybe AutomationEventListRef)

startup :: IO AppState
startup = do
  window <- initWindow 800 600 "raylib [core] example - basic automation events"
  setTargetFPS 60
  rWidth <- measureText "REC" 30
  pWidth <- measureText "PLAY" 30
  return (window, rWidth, pWidth, Nothing, Nothing, False, False, Nothing)

mainLoop :: AppState -> IO AppState
mainLoop (window, rWidth, pWidth, mState, playback, playing, recording, lRef) = do
  drawing
    ( do
        toggleRec <- (&& not playing) <$> isKeyPressed KeyR
        let recording' = if toggleRec then not recording else recording
        lRef' <-
          if toggleRec && recording'
            then
              ( do
                  l <- newAutomationEventList
                  Just <$> managed window (setAutomationEventList l)
              )
            else return lRef
        when toggleRec $ if recording' then setAutomationEventBaseFrame 60 >> startAutomationEventRecording else stopAutomationEventRecording
        when recording' $ drawText "REC" (790 - rWidth) 10 30 red

        lEvents <- case lRef' of
          Nothing -> return []
          Just l -> automationEventList'events <$> peekAutomationEventList l
        let playback' =
              if toggleRec && not recording'
                then Just (lEvents, 0)
                else playback
        startPlay <- isKeyPressed KeyP
        let playing' = isJust playback' && not recording' && (startPlay || playing)

        playback'' <-
          if playing'
            then
              ( do
                  let (events, f) = fromJust playback'
                  let (thisFrame, rest) = span (\(AutomationEvent frame _ _) -> frame == f) events

                  drawText "PLAY" (790 - pWidth) 10 30 red
                  mapM_ playAutomationEvent thisFrame

                  if null events then return Nothing else return $ Just (rest, f + 1)
              )
            else return playback'
        let (playback''', playing'') =
              case playback'' of
                Nothing -> if null lEvents then (Nothing, False) else (Just (lEvents, 0), False)
                v -> (v, playing')

        clearBackground white

        mousePressed <- isMouseButtonPressed MouseButtonLeft
        mousePos <- getMousePosition
        frameTime <- getFrameTime
        let mState' = if mousePressed then Just (mousePos, 0) else (\(p, t) -> (p, t + frameTime)) <$> mState
        let mState'' = mState' >>= (\m@(_, t) -> if t > 2 then Nothing else Just m)

        case mState'' of
          Nothing -> drawText "Click somewhere or press the arrow keys" 10 10 20 black >> drawText "Press 'R' to start or stop recording, and 'P' to play the recording" 10 40 20 black
          Just (p@(Vector2 x y), t) ->
            ( do
                when (t < 1.0) $ drawCircleV p (sin (realToFrac t * pi / 1.0) * 10) red
                drawText ("Mouse clicked at (" ++ show x ++ ", " ++ show y ++ ")") 10 (if y < 50 && t < 1.0 then 570 else 10) 20 black
            )

        uDown <- isKeyDown KeyUp
        dDown <- isKeyDown KeyDown
        lDown <- isKeyDown KeyLeft
        rDown <- isKeyDown KeyRight
        let lb = Color 172 204 252 255
        let shiftLeft =
              case mState'' of
                Nothing -> 0
                Just (Vector2 x y, t) -> if x > 580 && y > 490 && t < 1.0 then -580 else 0

        (if uDown then drawRectangle else drawRectangleLines) (660 + shiftLeft) 500 60 40 lb
        (if dDown then drawRectangle else drawRectangleLines) (660 + shiftLeft) 550 60 40 lb
        (if lDown then drawRectangle else drawRectangleLines) (590 + shiftLeft) 550 60 40 lb
        (if rDown then drawRectangle else drawRectangleLines) (730 + shiftLeft) 550 60 40 lb

        return (window, rWidth, pWidth, mState'', playback''', playing'', recording', lRef')
    )

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (window, _, _, _, _, _, _, _) = closeWindow (Just window)

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
