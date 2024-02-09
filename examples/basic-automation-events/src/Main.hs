{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
import Raylib.Core
  ( clearBackground,
    getFrameTime,
    getMousePosition,
    isKeyDown,
    isKeyPressed,
    isMouseButtonPressed,
    newAutomationEventList,
    peekAutomationEventList,
    playAutomationEvent,
    setAutomationEventBaseFrame,
    setAutomationEventList,
    startAutomationEventRecording,
    stopAutomationEventRecording,
  )
import Raylib.Core.Shapes (drawCircleV, drawRectangle, drawRectangleLines)
import Raylib.Core.Text (drawText, measureText)
import Raylib.Types
  ( AutomationEvent (AutomationEvent),
    AutomationEventList (automationEventList'events),
    Color (Color),
    KeyboardKey (KeyDown, KeyLeft, KeyP, KeyR, KeyRight, KeyUp),
    MouseButton (MouseButtonLeft),
    Vector2 (Vector2),
  )
import Raylib.Util (drawing, whileWindowOpen_, withWindow)
import Raylib.Util.Colors (black, red, white)

main :: IO ()
main =
  withWindow
    800
    600
    "raylib [core] example - basic automation events"
    60
    ( \window -> do
        el <- newAutomationEventList
        listRef <- setAutomationEventList el window
        rWidth <- measureText "REC" 30
        pWidth <- measureText "PLAY" 30

        whileWindowOpen_
          ( \(mState, playback, playing, recording, lRef) ->
              drawing
                ( do
                    toggleRec <- (&& not playing) <$> isKeyPressed KeyR
                    let recording' = if toggleRec then not recording else recording
                    lRef' <-
                      if toggleRec && recording'
                        then
                          ( do
                              l <- newAutomationEventList
                              setAutomationEventList l window
                          )
                        else return lRef
                    when toggleRec $ if recording' then setAutomationEventBaseFrame 60 >> startAutomationEventRecording else stopAutomationEventRecording
                    when recording' $ drawText "REC" (790 - rWidth) 10 30 red

                    list <- peekAutomationEventList lRef'
                    let playback' =
                          if toggleRec && not recording'
                            then Just (automationEventList'events list, 0)
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
                            Nothing -> let e = automationEventList'events list in if null e then (Nothing, False) else (Just (e, 0), False)
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
                            when (t < 1.0) $ drawCircleV p (sin ((realToFrac t) * pi / 1.0) * 10) red
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
                            Just ((Vector2 x y), t) -> if x > 580 && y > 490 && t < 1.0 then -580 else 0

                    (if uDown then drawRectangle else drawRectangleLines) (660 + shiftLeft) 500 60 40 lb
                    (if dDown then drawRectangle else drawRectangleLines) (660 + shiftLeft) 550 60 40 lb
                    (if lDown then drawRectangle else drawRectangleLines) (590 + shiftLeft) 550 60 40 lb
                    (if rDown then drawRectangle else drawRectangleLines) (730 + shiftLeft) 550 60 40 lb

                    return (mState'', playback''', playing'', recording', lRef')
                )
          )
          (Nothing, Nothing, False, False, listRef)
    )