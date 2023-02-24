{-# OPTIONS -Wall #-}

module Raylib.Core.Shapes where

import Data.List (genericLength)
import Foreign (Storable (peek), toBool, withArray)
import GHC.IO (unsafePerformIO)
import Raylib.Native
  ( c'checkCollisionCircleRec,
    c'checkCollisionCircles,
    c'checkCollisionLines,
    c'checkCollisionPointCircle,
    c'checkCollisionPointLine,
    c'checkCollisionPointRec,
    c'checkCollisionPointTriangle,
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
    c'setShapesTexture,
  )
import Raylib.Types (Color, Rectangle, Texture, Vector2 (Vector2))
import Raylib.ForeignUtil (pop, withFreeable)

setShapesTexture :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> IO ()
setShapesTexture tex source = withFreeable tex (withFreeable source . c'setShapesTexture)

drawPixel :: Int -> Int -> Raylib.Types.Color -> IO ()
drawPixel x y color = withFreeable color $ c'drawPixel (fromIntegral x) (fromIntegral y)

drawPixelV :: Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawPixelV position color = withFreeable position (withFreeable color . c'drawPixelV)

drawLine :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO ()
drawLine startX startY endX endY color =
  withFreeable color $ c'drawLine (fromIntegral startX) (fromIntegral startY) (fromIntegral endX) (fromIntegral endY)

drawLineV :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawLineV start end color = withFreeable start (\s -> withFreeable end (withFreeable color . c'drawLineV s))

drawLineEx :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawLineEx start end thickness color =
  withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawLineEx s e (realToFrac thickness))))

drawLineBezier :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawLineBezier start end thickness color =
  withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawLineBezier s e (realToFrac thickness))))

drawLineBezierQuad :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawLineBezierQuad start end control thickness color =
  withFreeable start (\s -> withFreeable end (\e -> withFreeable control (\c -> withFreeable color (c'drawLineBezierQuad s e c (realToFrac thickness)))))

drawLineBezierCubic :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
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

drawLineStrip :: [Raylib.Types.Vector2] -> Raylib.Types.Color -> IO ()
drawLineStrip points color = withArray points (\p -> withFreeable color $ c'drawLineStrip p (genericLength points))

drawCircle :: Int -> Int -> Float -> Raylib.Types.Color -> IO ()
drawCircle centerX centerY radius color = withFreeable color (c'drawCircle (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

drawCircleSector :: Raylib.Types.Vector2 -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawCircleSector center radius startAngle endAngle segments color =
  withFreeable
    center
    ( \c ->
        withFreeable
          color
          ( c'drawCircleSector c (realToFrac radius) (realToFrac startAngle) (realToFrac endAngle) (fromIntegral segments)
          )
    )

drawCircleSectorLines :: Raylib.Types.Vector2 -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawCircleSectorLines center radius startAngle endAngle segments color =
  withFreeable
    center
    ( \c ->
        withFreeable
          color
          ( c'drawCircleSectorLines c (realToFrac radius) (realToFrac startAngle) (realToFrac endAngle) (fromIntegral segments)
          )
    )

drawCircleGradient :: Int -> Int -> Float -> Raylib.Types.Color -> Raylib.Types.Color -> IO ()
drawCircleGradient centerX centerY radius color1 color2 =
  withFreeable color1 (withFreeable color2 . c'drawCircleGradient (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

drawCircleV :: Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawCircleV center radius color =
  withFreeable center (\c -> withFreeable color (c'drawCircleV c (realToFrac radius)))

drawCircleLines :: Int -> Int -> Float -> Raylib.Types.Color -> IO ()
drawCircleLines centerX centerY radius color =
  withFreeable color (c'drawCircleLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

drawEllipse :: Int -> Int -> Float -> Float -> Raylib.Types.Color -> IO ()
drawEllipse centerX centerY radiusH radiusV color =
  withFreeable color (c'drawEllipse (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

drawEllipseLines :: Int -> Int -> Float -> Float -> Raylib.Types.Color -> IO ()
drawEllipseLines centerX centerY radiusH radiusV color =
  withFreeable color (c'drawEllipseLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

drawRing :: Raylib.Types.Vector2 -> Float -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
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

drawRingLines :: Raylib.Types.Vector2 -> Float -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
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

drawRectangle :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO ()
drawRectangle posX posY width height color =
  withFreeable color (c'drawRectangle (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

drawRectangleV :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawRectangleV position size color = withFreeable position (\p -> withFreeable size (withFreeable color . c'drawRectangleV p))

drawRectangleRec :: Raylib.Types.Rectangle -> Raylib.Types.Color -> IO ()
drawRectangleRec rect color = withFreeable rect (withFreeable color . c'drawRectangleRec)

drawRectanglePro :: Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawRectanglePro rect origin rotation color =
  withFreeable color (\c -> withFreeable rect (\r -> withFreeable origin (\o -> c'drawRectanglePro r o (realToFrac rotation) c)))

drawRectangleGradientV :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO ()
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

drawRectangleGradientH :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO ()
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

drawRectangleGradientEx :: Raylib.Types.Rectangle -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> IO ()
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

drawRectangleLines :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO ()
drawRectangleLines posX posY width height color =
  withFreeable color (c'drawRectangleLines (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

drawRectangleLinesEx :: Raylib.Types.Rectangle -> Float -> Raylib.Types.Color -> IO ()
drawRectangleLinesEx rect thickness color =
  withFreeable color (\c -> withFreeable rect (\r -> c'drawRectangleLinesEx r (realToFrac thickness) c))

drawRectangleRounded :: Raylib.Types.Rectangle -> Float -> Int -> Raylib.Types.Color -> IO ()
drawRectangleRounded rect roundness segments color =
  withFreeable rect (\r -> withFreeable color $ c'drawRectangleRounded r (realToFrac roundness) (fromIntegral segments))

drawRectangleRoundedLines :: Raylib.Types.Rectangle -> Float -> Int -> Float -> Raylib.Types.Color -> IO ()
drawRectangleRoundedLines rect roundness segments thickness color =
  withFreeable rect (\r -> withFreeable color $ c'drawRectangleRoundedLines r (realToFrac roundness) (fromIntegral segments) (realToFrac thickness))

drawTriangle :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTriangle v1 v2 v3 color =
  withFreeable
    v1
    ( \p1 ->
        withFreeable
          v2
          ( \p2 -> withFreeable v3 (withFreeable color . c'drawTriangle p1 p2)
          )
    )

drawTriangleLines :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTriangleLines v1 v2 v3 color =
  withFreeable
    v1
    ( \p1 ->
        withFreeable
          v2
          ( \p2 -> withFreeable v3 (withFreeable color . c'drawTriangleLines p1 p2)
          )
    )

drawTriangleFan :: [Raylib.Types.Vector2] -> Raylib.Types.Color -> IO ()
drawTriangleFan points color = withArray points (\p -> withFreeable color $ c'drawTriangleFan p (genericLength points))

drawTriangleStrip :: [Raylib.Types.Vector2] -> Raylib.Types.Color -> IO ()
drawTriangleStrip points color =
  withArray points (\p -> withFreeable color $ c'drawTriangleStrip p (genericLength points))

drawPoly :: Raylib.Types.Vector2 -> Int -> Float -> Float -> Raylib.Types.Color -> IO ()
drawPoly center sides radius rotation color =
  withFreeable center (\c -> withFreeable color $ c'drawPoly c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

drawPolyLines :: Raylib.Types.Vector2 -> Int -> Float -> Float -> Raylib.Types.Color -> IO ()
drawPolyLines center sides radius rotation color =
  withFreeable center (\c -> withFreeable color $ c'drawPolyLines c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

drawPolyLinesEx :: Raylib.Types.Vector2 -> Int -> Float -> Float -> Float -> Raylib.Types.Color -> IO ()
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

checkCollisionRecs :: Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Bool
checkCollisionRecs rec1 rec2 = unsafePerformIO $ toBool <$> withFreeable rec1 (withFreeable rec2 . c'checkCollisionRecs)

checkCollisionCircles :: Raylib.Types.Vector2 -> Float -> Raylib.Types.Vector2 -> Float -> Bool
checkCollisionCircles center1 radius1 center2 radius2 =
  unsafePerformIO $ toBool <$> withFreeable center1 (\c1 -> withFreeable center2 (\c2 -> c'checkCollisionCircles c1 (realToFrac radius1) c2 (realToFrac radius2)))

checkCollisionCircleRec :: Raylib.Types.Vector2 -> Float -> Raylib.Types.Rectangle -> Bool
checkCollisionCircleRec center radius rect =
  unsafePerformIO $ toBool <$> withFreeable center (\c -> withFreeable rect $ c'checkCollisionCircleRec c (realToFrac radius))

checkCollisionPointRec :: Raylib.Types.Vector2 -> Raylib.Types.Rectangle -> Bool
checkCollisionPointRec point rect =
  unsafePerformIO $ toBool <$> withFreeable point (withFreeable rect . c'checkCollisionPointRec)

checkCollisionPointCircle :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Bool
checkCollisionPointCircle point center radius =
  unsafePerformIO $ toBool <$> withFreeable point (\p -> withFreeable center (\c -> c'checkCollisionPointCircle p c (realToFrac radius)))

checkCollisionPointTriangle :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Bool
checkCollisionPointTriangle point p1 p2 p3 =
  unsafePerformIO $ toBool <$> withFreeable point (\p -> withFreeable p1 (\ptr1 -> withFreeable p2 (withFreeable p3 . c'checkCollisionPointTriangle p ptr1)))

-- | If a collision is found, returns @Just collisionPoint@, otherwise returns @Nothing@
checkCollisionLines :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Maybe Raylib.Types.Vector2
checkCollisionLines start1 end1 start2 end2 =
  unsafePerformIO $
    withFreeable
      (Raylib.Types.Vector2 0 0)
      ( \res -> do
          foundCollision <- toBool <$> withFreeable start1 (\s1 -> withFreeable end1 (\e1 -> withFreeable start2 (\s2 -> withFreeable end2 (\e2 -> c'checkCollisionLines s1 e1 s2 e2 res))))
          if foundCollision then Just <$> peek res else return Nothing
      )

checkCollisionPointLine :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Int -> Bool
checkCollisionPointLine point p1 p2 threshold =
  unsafePerformIO $ toBool <$> withFreeable point (\p -> withFreeable p1 (\ptr1 -> withFreeable p2 (\ptr2 -> c'checkCollisionPointLine p ptr1 ptr2 (fromIntegral threshold))))

getCollisionRec :: Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle
getCollisionRec rec1 rec2 =
  unsafePerformIO $ withFreeable rec1 (withFreeable rec2 . c'getCollisionRec) >>= pop
