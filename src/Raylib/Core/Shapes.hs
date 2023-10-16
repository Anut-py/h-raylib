{-# OPTIONS -Wall #-}

module Raylib.Core.Shapes where

import Data.List (genericLength)
import Foreign (Storable (peek), toBool)
import GHC.IO (unsafePerformIO)
import Raylib.ForeignUtil (pop, withFreeable, withFreeableArray, withFreeableArrayLen)
import Raylib.Native
  ( c'checkCollisionCircleRec,
    c'checkCollisionCircles,
    c'checkCollisionLines,
    c'checkCollisionPointCircle,
    c'checkCollisionPointLine,
    c'checkCollisionPointRec,
    c'checkCollisionPointTriangle,
    c'checkCollisionPointPoly,
    c'checkCollisionRecs,
    c'drawCircle,
    c'drawCircleGradient,
    c'drawCircleLines,
    c'drawCircleSector,
    c'drawCircleSectorLines,
    c'drawCircleV,
    c'drawEllipse,
    c'drawEllipseLines,
    c'drawLine,
    c'drawLineBezier,
    c'drawLineBezierCubic,
    c'drawLineBezierQuad,
    c'drawLineEx,
    c'drawLineStrip,
    c'drawLineV,
    c'drawPixel,
    c'drawPixelV,
    c'drawPoly,
    c'drawPolyLines,
    c'drawPolyLinesEx,
    c'drawRectangle,
    c'drawRectangleGradientEx,
    c'drawRectangleGradientH,
    c'drawRectangleGradientV,
    c'drawRectangleLines,
    c'drawRectangleLinesEx,
    c'drawRectanglePro,
    c'drawRectangleRec,
    c'drawRectangleRounded,
    c'drawRectangleRoundedLines,
    c'drawRectangleV,
    c'drawRing,
    c'drawRingLines,
    c'drawTriangle,
    c'drawTriangleFan,
    c'drawTriangleLines,
    c'drawTriangleStrip,
    c'getCollisionRec,
    c'setShapesTexture, c'drawLineBSpline, c'drawLineCatmullRom,
  )
import Raylib.Types (Color, Rectangle, Texture, Vector2 (Vector2))

setShapesTexture :: Texture -> Rectangle -> IO ()
setShapesTexture tex source = withFreeable tex (withFreeable source . c'setShapesTexture)

drawPixel :: Int -> Int -> Color -> IO ()
drawPixel x y color = withFreeable color $ c'drawPixel (fromIntegral x) (fromIntegral y)

drawPixelV :: Vector2 -> Color -> IO ()
drawPixelV position color = withFreeable position (withFreeable color . c'drawPixelV)

drawLine :: Int -> Int -> Int -> Int -> Color -> IO ()
drawLine startX startY endX endY color =
  withFreeable color $ c'drawLine (fromIntegral startX) (fromIntegral startY) (fromIntegral endX) (fromIntegral endY)

drawLineV :: Vector2 -> Vector2 -> Color -> IO ()
drawLineV start end color = withFreeable start (\s -> withFreeable end (withFreeable color . c'drawLineV s))

drawLineEx :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineEx start end thickness color =
  withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawLineEx s e (realToFrac thickness))))

drawLineBezier :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezier start end thickness color =
  withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawLineBezier s e (realToFrac thickness))))

drawLineBezierQuad :: Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezierQuad start end control thickness color =
  withFreeable start (\s -> withFreeable end (\e -> withFreeable control (\c -> withFreeable color (c'drawLineBezierQuad s e c (realToFrac thickness)))))

drawLineBezierCubic :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezierCubic start end startControl endControl thickness color =
  withFreeable
    start
    ( \s ->
        withFreeable
          end
          ( \e ->
              withFreeable
                startControl
                ( \sc ->
                    withFreeable
                      endControl
                      ( \ec ->
                          withFreeable
                            color
                            ( c'drawLineBezierCubic s e sc ec (realToFrac thickness)
                            )
                      )
                )
          )
    )

drawLineBSpline :: [Vector2] -> Float -> Color -> IO ()
drawLineBSpline points thickness color = withFreeableArrayLen points (\s p -> withFreeable color (c'drawLineBSpline p (fromIntegral s) (realToFrac thickness)))

drawLineCatmullRom :: [Vector2] -> Float -> Color -> IO ()
drawLineCatmullRom points thickness color = withFreeableArrayLen points (\s p -> withFreeable color (c'drawLineCatmullRom p (fromIntegral s) (realToFrac thickness)))

drawLineStrip :: [Vector2] -> Color -> IO ()
drawLineStrip points color = withFreeableArray points (\p -> withFreeable color $ c'drawLineStrip p (genericLength points))

drawCircle :: Int -> Int -> Float -> Color -> IO ()
drawCircle centerX centerY radius color = withFreeable color (c'drawCircle (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

drawCircleSector :: Vector2 -> Float -> Float -> Float -> Int -> Color -> IO ()
drawCircleSector center radius startAngle endAngle segments color =
  withFreeable
    center
    ( \c ->
        withFreeable
          color
          ( c'drawCircleSector c (realToFrac radius) (realToFrac startAngle) (realToFrac endAngle) (fromIntegral segments)
          )
    )

drawCircleSectorLines :: Vector2 -> Float -> Float -> Float -> Int -> Color -> IO ()
drawCircleSectorLines center radius startAngle endAngle segments color =
  withFreeable
    center
    ( \c ->
        withFreeable
          color
          ( c'drawCircleSectorLines c (realToFrac radius) (realToFrac startAngle) (realToFrac endAngle) (fromIntegral segments)
          )
    )

drawCircleGradient :: Int -> Int -> Float -> Color -> Color -> IO ()
drawCircleGradient centerX centerY radius color1 color2 =
  withFreeable color1 (withFreeable color2 . c'drawCircleGradient (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

drawCircleV :: Vector2 -> Float -> Color -> IO ()
drawCircleV center radius color =
  withFreeable center (\c -> withFreeable color (c'drawCircleV c (realToFrac radius)))

drawCircleLines :: Int -> Int -> Float -> Color -> IO ()
drawCircleLines centerX centerY radius color =
  withFreeable color (c'drawCircleLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

drawEllipse :: Int -> Int -> Float -> Float -> Color -> IO ()
drawEllipse centerX centerY radiusH radiusV color =
  withFreeable color (c'drawEllipse (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

drawEllipseLines :: Int -> Int -> Float -> Float -> Color -> IO ()
drawEllipseLines centerX centerY radiusH radiusV color =
  withFreeable color (c'drawEllipseLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

drawRing :: Vector2 -> Float -> Float -> Float -> Float -> Int -> Color -> IO ()
drawRing center innerRadius outerRadius startAngle endAngle segments color =
  withFreeable
    center
    ( \c ->
        withFreeable
          color
          ( c'drawRing
              c
              (realToFrac innerRadius)
              (realToFrac outerRadius)
              (realToFrac startAngle)
              (realToFrac endAngle)
              (fromIntegral segments)
          )
    )

drawRingLines :: Vector2 -> Float -> Float -> Float -> Float -> Int -> Color -> IO ()
drawRingLines center innerRadius outerRadius startAngle endAngle segments color =
  withFreeable
    center
    ( \c ->
        withFreeable
          color
          ( c'drawRingLines
              c
              (realToFrac innerRadius)
              (realToFrac outerRadius)
              (realToFrac startAngle)
              (realToFrac endAngle)
              (fromIntegral segments)
          )
    )

drawRectangle :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangle posX posY width height color =
  withFreeable color (c'drawRectangle (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

drawRectangleV :: Vector2 -> Vector2 -> Color -> IO ()
drawRectangleV position size color = withFreeable position (\p -> withFreeable size (withFreeable color . c'drawRectangleV p))

drawRectangleRec :: Rectangle -> Color -> IO ()
drawRectangleRec rect color = withFreeable rect (withFreeable color . c'drawRectangleRec)

drawRectanglePro :: Rectangle -> Vector2 -> Float -> Color -> IO ()
drawRectanglePro rect origin rotation color =
  withFreeable color (\c -> withFreeable rect (\r -> withFreeable origin (\o -> c'drawRectanglePro r o (realToFrac rotation) c)))

drawRectangleGradientV :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
drawRectangleGradientV posX posY width height color1 color2 =
  withFreeable
    color1
    ( withFreeable color2
        . c'drawRectangleGradientV
          (fromIntegral posX)
          (fromIntegral posY)
          (fromIntegral width)
          (fromIntegral height)
    )

drawRectangleGradientH :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
drawRectangleGradientH posX posY width height color1 color2 =
  withFreeable
    color1
    ( withFreeable color2
        . c'drawRectangleGradientH
          (fromIntegral posX)
          (fromIntegral posY)
          (fromIntegral width)
          (fromIntegral height)
    )

drawRectangleGradientEx :: Rectangle -> Color -> Color -> Color -> Color -> IO ()
drawRectangleGradientEx rect col1 col2 col3 col4 =
  withFreeable
    rect
    ( \r ->
        withFreeable
          col1
          ( \c1 ->
              withFreeable
                col2
                ( \c2 ->
                    withFreeable col3 (withFreeable col4 . c'drawRectangleGradientEx r c1 c2)
                )
          )
    )

drawRectangleLines :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangleLines posX posY width height color =
  withFreeable color (c'drawRectangleLines (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

drawRectangleLinesEx :: Rectangle -> Float -> Color -> IO ()
drawRectangleLinesEx rect thickness color =
  withFreeable color (\c -> withFreeable rect (\r -> c'drawRectangleLinesEx r (realToFrac thickness) c))

drawRectangleRounded :: Rectangle -> Float -> Int -> Color -> IO ()
drawRectangleRounded rect roundness segments color =
  withFreeable rect (\r -> withFreeable color $ c'drawRectangleRounded r (realToFrac roundness) (fromIntegral segments))

drawRectangleRoundedLines :: Rectangle -> Float -> Int -> Float -> Color -> IO ()
drawRectangleRoundedLines rect roundness segments thickness color =
  withFreeable rect (\r -> withFreeable color $ c'drawRectangleRoundedLines r (realToFrac roundness) (fromIntegral segments) (realToFrac thickness))

drawTriangle :: Vector2 -> Vector2 -> Vector2 -> Color -> IO ()
drawTriangle v1 v2 v3 color =
  withFreeable
    v1
    ( \p1 ->
        withFreeable
          v2
          ( \p2 -> withFreeable v3 (withFreeable color . c'drawTriangle p1 p2)
          )
    )

drawTriangleLines :: Vector2 -> Vector2 -> Vector2 -> Color -> IO ()
drawTriangleLines v1 v2 v3 color =
  withFreeable
    v1
    ( \p1 ->
        withFreeable
          v2
          ( \p2 -> withFreeable v3 (withFreeable color . c'drawTriangleLines p1 p2)
          )
    )

drawTriangleFan :: [Vector2] -> Color -> IO ()
drawTriangleFan points color = withFreeableArray points (\p -> withFreeable color $ c'drawTriangleFan p (genericLength points))

drawTriangleStrip :: [Vector2] -> Color -> IO ()
drawTriangleStrip points color =
  withFreeableArray points (\p -> withFreeable color $ c'drawTriangleStrip p (genericLength points))

drawPoly :: Vector2 -> Int -> Float -> Float -> Color -> IO ()
drawPoly center sides radius rotation color =
  withFreeable center (\c -> withFreeable color $ c'drawPoly c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

drawPolyLines :: Vector2 -> Int -> Float -> Float -> Color -> IO ()
drawPolyLines center sides radius rotation color =
  withFreeable center (\c -> withFreeable color $ c'drawPolyLines c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

drawPolyLinesEx :: Vector2 -> Int -> Float -> Float -> Float -> Color -> IO ()
drawPolyLinesEx center sides radius rotation thickness color =
  withFreeable
    center
    ( \c ->
        withFreeable color $
          c'drawPolyLinesEx
            c
            (fromIntegral sides)
            (realToFrac radius)
            (realToFrac rotation)
            (realToFrac thickness)
    )

checkCollisionRecs :: Rectangle -> Rectangle -> Bool
checkCollisionRecs rec1 rec2 = unsafePerformIO $ toBool <$> withFreeable rec1 (withFreeable rec2 . c'checkCollisionRecs)

checkCollisionCircles :: Vector2 -> Float -> Vector2 -> Float -> Bool
checkCollisionCircles center1 radius1 center2 radius2 =
  unsafePerformIO $ toBool <$> withFreeable center1 (\c1 -> withFreeable center2 (\c2 -> c'checkCollisionCircles c1 (realToFrac radius1) c2 (realToFrac radius2)))

checkCollisionCircleRec :: Vector2 -> Float -> Rectangle -> Bool
checkCollisionCircleRec center radius rect =
  unsafePerformIO $ toBool <$> withFreeable center (\c -> withFreeable rect $ c'checkCollisionCircleRec c (realToFrac radius))

checkCollisionPointRec :: Vector2 -> Rectangle -> Bool
checkCollisionPointRec point rect =
  unsafePerformIO $ toBool <$> withFreeable point (withFreeable rect . c'checkCollisionPointRec)

checkCollisionPointCircle :: Vector2 -> Vector2 -> Float -> Bool
checkCollisionPointCircle point center radius =
  unsafePerformIO $ toBool <$> withFreeable point (\p -> withFreeable center (\c -> c'checkCollisionPointCircle p c (realToFrac radius)))

checkCollisionPointTriangle :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Bool
checkCollisionPointTriangle point p1 p2 p3 =
  unsafePerformIO $ toBool <$> withFreeable point (\p -> withFreeable p1 (\ptr1 -> withFreeable p2 (withFreeable p3 . c'checkCollisionPointTriangle p ptr1)))

checkCollisionPointPoly :: Vector2 -> [Vector2] -> Bool
checkCollisionPointPoly point points =
  unsafePerformIO $ toBool <$> withFreeableArrayLen points (\l ps -> withFreeable point (\p -> c'checkCollisionPointPoly p ps (fromIntegral l)))

-- | If a collision is found, returns @Just collisionPoint@, otherwise returns @Nothing@
checkCollisionLines :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Maybe Vector2
checkCollisionLines start1 end1 start2 end2 =
  unsafePerformIO $
    withFreeable
      (Vector2 0 0)
      ( \res -> do
          foundCollision <- toBool <$> withFreeable start1 (\s1 -> withFreeable end1 (\e1 -> withFreeable start2 (\s2 -> withFreeable end2 (\e2 -> c'checkCollisionLines s1 e1 s2 e2 res))))
          if foundCollision then Just <$> peek res else return Nothing
      )

checkCollisionPointLine :: Vector2 -> Vector2 -> Vector2 -> Int -> Bool
checkCollisionPointLine point p1 p2 threshold =
  unsafePerformIO $ toBool <$> withFreeable point (\p -> withFreeable p1 (\ptr1 -> withFreeable p2 (\ptr2 -> c'checkCollisionPointLine p ptr1 ptr2 (fromIntegral threshold))))

getCollisionRec :: Rectangle -> Rectangle -> Rectangle
getCollisionRec rec1 rec2 =
  unsafePerformIO $ withFreeable rec1 (withFreeable rec2 . c'getCollisionRec) >>= pop
