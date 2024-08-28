{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Bindings to @rshapes@
module Raylib.Core.Shapes
  ( -- * High level
    setShapesTexture,
    getShapesTexture,
    getShapesTextureRectangle,
    drawPixel,
    drawPixelV,
    drawLine,
    drawLineV,
    drawLineEx,
    drawLineStrip,
    drawLineBezier,
    drawCircle,
    drawCircleSector,
    drawCircleSectorLines,
    drawCircleGradient,
    drawCircleV,
    drawCircleLines,
    drawCircleLinesV,
    drawEllipse,
    drawEllipseLines,
    drawRing,
    drawRingLines,
    drawRectangle,
    drawRectangleV,
    drawRectangleRec,
    drawRectanglePro,
    drawRectangleGradientV,
    drawRectangleGradientH,
    drawRectangleGradientEx,
    drawRectangleLines,
    drawRectangleLinesEx,
    drawRectangleRounded,
    drawRectangleRoundedLines,
    drawRectangleRoundedLinesEx,
    drawTriangle,
    drawTriangleLines,
    drawTriangleFan,
    drawTriangleStrip,
    drawPoly,
    drawPolyLines,
    drawPolyLinesEx,
    drawSplineLinear,
    drawSplineBasis,
    drawSplineCatmullRom,
    drawSplineBezierQuadratic,
    drawSplineBezierCubic,
    drawSplineSegmentLinear,
    drawSplineSegmentBasis,
    drawSplineSegmentCatmullRom,
    drawSplineSegmentBezierQuadratic,
    drawSplineSegmentBezierCubic,
    getSplinePointLinear,
    getSplinePointBasis,
    getSplinePointCatmullRom,
    getSplinePointBezierQuad,
    getSplinePointBezierCubic,
    checkCollisionRecs,
    checkCollisionCircles,
    checkCollisionCircleRec,
    checkCollisionPointRec,
    checkCollisionPointCircle,
    checkCollisionPointTriangle,
    checkCollisionPointPoly,
    checkCollisionLines,
    checkCollisionPointLine,
    checkCollisionCircleLine,
    getCollisionRec,

    -- * Native
    c'setShapesTexture,
    c'getShapesTexture,
    c'getShapesTextureRectangle,
    c'drawPixel,
    c'drawPixelV,
    c'drawLine,
    c'drawLineV,
    c'drawLineEx,
    c'drawLineStrip,
    c'drawLineBezier,
    c'drawCircle,
    c'drawCircleSector,
    c'drawCircleSectorLines,
    c'drawCircleGradient,
    c'drawCircleV,
    c'drawCircleLines,
    c'drawCircleLinesV,
    c'drawEllipse,
    c'drawEllipseLines,
    c'drawRing,
    c'drawRingLines,
    c'drawRectangle,
    c'drawRectangleV,
    c'drawRectangleRec,
    c'drawRectanglePro,
    c'drawRectangleGradientV,
    c'drawRectangleGradientH,
    c'drawRectangleGradientEx,
    c'drawRectangleLines,
    c'drawRectangleLinesEx,
    c'drawRectangleRounded,
    c'drawRectangleRoundedLines,
    c'drawRectangleRoundedLinesEx,
    c'drawTriangle,
    c'drawTriangleLines,
    c'drawTriangleFan,
    c'drawTriangleStrip,
    c'drawPoly,
    c'drawPolyLines,
    c'drawPolyLinesEx,
    c'drawSplineLinear,
    c'drawSplineBasis,
    c'drawSplineCatmullRom,
    c'drawSplineBezierQuadratic,
    c'drawSplineBezierCubic,
    c'drawSplineSegmentLinear,
    c'drawSplineSegmentBasis,
    c'drawSplineSegmentCatmullRom,
    c'drawSplineSegmentBezierQuadratic,
    c'drawSplineSegmentBezierCubic,
    c'getSplinePointLinear,
    c'getSplinePointBasis,
    c'getSplinePointCatmullRom,
    c'getSplinePointBezierQuad,
    c'getSplinePointBezierCubic,
    c'checkCollisionRecs,
    c'checkCollisionCircles,
    c'checkCollisionCircleRec,
    c'checkCollisionPointRec,
    c'checkCollisionPointCircle,
    c'checkCollisionPointTriangle,
    c'checkCollisionPointPoly,
    c'checkCollisionLines,
    c'checkCollisionPointLine,
    c'checkCollisionCircleLine,
    c'getCollisionRec
  )
where

import Data.List (genericLength)
import Foreign (Ptr, Storable (peek), toBool)
import Foreign.C
  ( CBool (..),
    CFloat (..),
    CInt (..),
  )
import GHC.IO (unsafePerformIO)
import Raylib.Internal.Foreign (pop, withFreeable, withFreeableArray, withFreeableArrayLen)
import Raylib.Internal.TH (genNative)
import Raylib.Types (Color, Rectangle, Texture, Vector2, pattern Vector2)

$( genNative
     [ ("c'setShapesTexture", "SetShapesTexture_", "rl_bindings.h", [t|Ptr Texture -> Ptr Rectangle -> IO ()|]),
       ("c'getShapesTexture", "GetShapesTexture_", "rl_bindings.h", [t|IO (Ptr Texture)|]),
       ("c'getShapesTextureRectangle", "GetShapesTextureRectangle_", "rl_bindings.h", [t|IO (Ptr Rectangle)|]),
       ("c'drawPixel", "DrawPixel_", "rl_bindings.h", [t|CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawPixelV", "DrawPixelV_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawLine", "DrawLine_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawLineV", "DrawLineV_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawLineEx", "DrawLineEx_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawLineStrip", "DrawLineStrip_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawLineBezier", "DrawLineBezier_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawCircle", "DrawCircle_", "rl_bindings.h", [t|CInt -> CInt -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawCircleSector", "DrawCircleSector_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCircleSectorLines", "DrawCircleSectorLines_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCircleGradient", "DrawCircleGradient_", "rl_bindings.h", [t|CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO ()|]),
       ("c'drawCircleV", "DrawCircleV_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawCircleLines", "DrawCircleLines_", "rl_bindings.h", [t|CInt -> CInt -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawCircleLinesV", "DrawCircleLinesV_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawEllipse", "DrawEllipse_", "rl_bindings.h", [t|CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawEllipseLines", "DrawEllipseLines_", "rl_bindings.h", [t|CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawRing", "DrawRing_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawRingLines", "DrawRingLines_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawRectangle", "DrawRectangle_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawRectangleV", "DrawRectangleV_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawRectangleRec", "DrawRectangleRec_", "rl_bindings.h", [t|Ptr Rectangle -> Ptr Color -> IO ()|]),
       ("c'drawRectanglePro", "DrawRectanglePro_", "rl_bindings.h", [t|Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawRectangleGradientV", "DrawRectangleGradientV_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()|]),
       ("c'drawRectangleGradientH", "DrawRectangleGradientH_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()|]),
       ("c'drawRectangleGradientEx", "DrawRectangleGradientEx_", "rl_bindings.h", [t|Ptr Rectangle -> Ptr Color -> Ptr Color -> Ptr Color -> Ptr Color -> IO ()|]),
       ("c'drawRectangleLines", "DrawRectangleLines_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawRectangleLinesEx", "DrawRectangleLinesEx_", "rl_bindings.h", [t|Ptr Rectangle -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawRectangleRounded", "DrawRectangleRounded_", "rl_bindings.h", [t|Ptr Rectangle -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawRectangleRoundedLines", "DrawRectangleRoundedLines_", "rl_bindings.h", [t|Ptr Rectangle -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawRectangleRoundedLinesEx", "DrawRectangleRoundedLinesEx_", "rl_bindings.h", [t|Ptr Rectangle -> CFloat -> CInt -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawTriangle", "DrawTriangle_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawTriangleLines", "DrawTriangleLines_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawTriangleFan", "DrawTriangleFan_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawTriangleStrip", "DrawTriangleStrip_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawPoly", "DrawPoly_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawPolyLines", "DrawPolyLines_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawPolyLinesEx", "DrawPolyLinesEx_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineLinear", "DrawSplineLinear_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineBasis", "DrawSplineBasis_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineCatmullRom", "DrawSplineCatmullRom_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineBezierQuadratic", "DrawSplineBezierQuadratic_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineBezierCubic", "DrawSplineBezierCubic_", "rl_bindings.h", [t|Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineSegmentLinear", "DrawSplineSegmentLinear_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineSegmentBasis", "DrawSplineSegmentBasis_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineSegmentCatmullRom", "DrawSplineSegmentCatmullRom_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineSegmentBezierQuadratic", "DrawSplineSegmentBezierQuadratic_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSplineSegmentBezierCubic", "DrawSplineSegmentBezierCubic_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'getSplinePointLinear", "GetSplinePointLinear_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)|]),
       ("c'getSplinePointBasis", "GetSplinePointBasis_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)|]),
       ("c'getSplinePointCatmullRom", "GetSplinePointCatmullRom_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)|]),
       ("c'getSplinePointBezierQuad", "GetSplinePointBezierQuad_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)|]),
       ("c'getSplinePointBezierCubic", "GetSplinePointBezierCubic_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)|]),
       ("c'checkCollisionRecs", "CheckCollisionRecs_", "rl_bindings.h", [t|Ptr Rectangle -> Ptr Rectangle -> IO CBool|]),
       ("c'checkCollisionCircles", "CheckCollisionCircles_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> Ptr Vector2 -> CFloat -> IO CBool|]),
       ("c'checkCollisionCircleRec", "CheckCollisionCircleRec_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> Ptr Rectangle -> IO CBool|]),
       ("c'checkCollisionPointRec", "CheckCollisionPointRec_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Rectangle -> IO CBool|]),
       ("c'checkCollisionPointCircle", "CheckCollisionPointCircle_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO CBool|]),
       ("c'checkCollisionPointTriangle", "CheckCollisionPointTriangle_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CBool|]),
       ("c'checkCollisionPointPoly", "CheckCollisionPointPoly_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CBool|]),
       ("c'checkCollisionLines", "CheckCollisionLines_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CBool|]),
       ("c'checkCollisionPointLine", "CheckCollisionPointLine_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CBool|]),
       ("c'checkCollisionCircleLine", "CheckCollisionCircleLine_", "rl_bindings.h", [t|Ptr Vector2 -> CFloat -> Ptr Vector2 -> Ptr Vector2 -> IO CBool|]),
       ("c'getCollisionRec", "GetCollisionRec_", "rl_bindings.h", [t|Ptr Rectangle -> Ptr Rectangle -> IO (Ptr Rectangle)|])
     ]
 )

setShapesTexture :: Texture -> Rectangle -> IO ()
setShapesTexture tex source = withFreeable tex (withFreeable source . c'setShapesTexture)

getShapesTexture :: IO Texture
getShapesTexture = c'getShapesTexture >>= pop

getShapesTextureRectangle :: IO Rectangle
getShapesTextureRectangle = c'getShapesTextureRectangle >>= pop

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

drawLineStrip :: [Vector2] -> Color -> IO ()
drawLineStrip points color = withFreeableArray points (\p -> withFreeable color $ c'drawLineStrip p (genericLength points))

drawLineBezier :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezier start end thickness color =
  withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawLineBezier s e (realToFrac thickness))))

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
drawCircleGradient centerX centerY radius inner outer =
  withFreeable inner (withFreeable outer . c'drawCircleGradient (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

drawCircleV :: Vector2 -> Float -> Color -> IO ()
drawCircleV center radius color =
  withFreeable center (\c -> withFreeable color (c'drawCircleV c (realToFrac radius)))

drawCircleLines :: Int -> Int -> Float -> Color -> IO ()
drawCircleLines centerX centerY radius color =
  withFreeable color (c'drawCircleLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

drawCircleLinesV :: Vector2 -> Float -> Color -> IO ()
drawCircleLinesV center radius color =
  withFreeable center (\c -> withFreeable color (c'drawCircleLinesV c (realToFrac radius)))

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
drawRectangleGradientV posX posY width height top bottom =
  withFreeable
    top
    ( withFreeable bottom
        . c'drawRectangleGradientV
          (fromIntegral posX)
          (fromIntegral posY)
          (fromIntegral width)
          (fromIntegral height)
    )

drawRectangleGradientH :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
drawRectangleGradientH posX posY width height left right =
  withFreeable
    left
    ( withFreeable right
        . c'drawRectangleGradientH
          (fromIntegral posX)
          (fromIntegral posY)
          (fromIntegral width)
          (fromIntegral height)
    )

drawRectangleGradientEx :: Rectangle -> Color -> Color -> Color -> Color -> IO ()
drawRectangleGradientEx rect topLeft bottomLeft topRight bottomRight =
  withFreeable
    rect
    ( \r ->
        withFreeable
          topLeft
          ( \c1 ->
              withFreeable
                bottomLeft
                ( \c2 ->
                    withFreeable topRight (withFreeable bottomRight . c'drawRectangleGradientEx r c1 c2)
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

drawRectangleRoundedLines :: Rectangle -> Float -> Int -> Color -> IO ()
drawRectangleRoundedLines rect roundness segments color =
  withFreeable rect (\r -> withFreeable color $ c'drawRectangleRoundedLines r (realToFrac roundness) (fromIntegral segments))

drawRectangleRoundedLinesEx :: Rectangle -> Float -> Int -> Float -> Color -> IO ()
drawRectangleRoundedLinesEx rect roundness segments thickness color =
  withFreeable rect (\r -> withFreeable color $ c'drawRectangleRoundedLinesEx r (realToFrac roundness) (fromIntegral segments) (realToFrac thickness))

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

drawSplineLinear :: [Vector2] -> Float -> Color -> IO ()
drawSplineLinear points thick color = withFreeableArrayLen points (\l p -> withFreeable color (c'drawSplineLinear p (fromIntegral l) (realToFrac thick)))

drawSplineBasis :: [Vector2] -> Float -> Color -> IO ()
drawSplineBasis points thick color = withFreeableArrayLen points (\l p -> withFreeable color (c'drawSplineBasis p (fromIntegral l) (realToFrac thick)))

drawSplineCatmullRom :: [Vector2] -> Float -> Color -> IO ()
drawSplineCatmullRom points thick color = withFreeableArrayLen points (\l p -> withFreeable color (c'drawSplineCatmullRom p (fromIntegral l) (realToFrac thick)))

drawSplineBezierQuadratic :: [Vector2] -> Float -> Color -> IO ()
drawSplineBezierQuadratic points thick color = withFreeableArrayLen points (\l p -> withFreeable color (c'drawSplineBezierQuadratic p (fromIntegral l) (realToFrac thick)))

drawSplineBezierCubic :: [Vector2] -> Float -> Color -> IO ()
drawSplineBezierCubic points thick color = withFreeableArrayLen points (\l p -> withFreeable color (c'drawSplineBezierCubic p (fromIntegral l) (realToFrac thick)))

drawSplineSegmentLinear :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawSplineSegmentLinear p1 p2 thick color = withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable color (c'drawSplineSegmentLinear q1 q2 (realToFrac thick))))

drawSplineSegmentBasis :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawSplineSegmentBasis p1 p2 p3 p4 thick color = withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable p3 (\q3 -> withFreeable p4 (\q4 -> withFreeable color (c'drawSplineSegmentBasis q1 q2 q3 q4 (realToFrac thick))))))

drawSplineSegmentCatmullRom :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawSplineSegmentCatmullRom p1 p2 p3 p4 thick color = withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable p3 (\q3 -> withFreeable p4 (\q4 -> withFreeable color (c'drawSplineSegmentCatmullRom q1 q2 q3 q4 (realToFrac thick))))))

drawSplineSegmentBezierQuadratic :: Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawSplineSegmentBezierQuadratic p1 p2 p3 thick color = withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable p3 (\q3 -> withFreeable color (c'drawSplineSegmentBezierQuadratic q1 q2 q3 (realToFrac thick)))))

drawSplineSegmentBezierCubic :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawSplineSegmentBezierCubic p1 p2 p3 p4 thick color = withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable p3 (\q3 -> withFreeable p4 (\q4 -> withFreeable color (c'drawSplineSegmentBezierCubic q1 q2 q3 q4 (realToFrac thick))))))

getSplinePointLinear :: Vector2 -> Vector2 -> Float -> Vector2
getSplinePointLinear p1 p2 t = unsafePerformIO $ withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> c'getSplinePointLinear q1 q2 (realToFrac t))) >>= pop

getSplinePointBasis :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Float -> Vector2
getSplinePointBasis p1 p2 p3 p4 t = unsafePerformIO $ withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable p3 (\q3 -> withFreeable p4 (\q4 -> c'getSplinePointBasis q1 q2 q3 q4 (realToFrac t))))) >>= pop

getSplinePointCatmullRom :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Float -> Vector2
getSplinePointCatmullRom p1 p2 p3 p4 t = unsafePerformIO $ withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable p3 (\q3 -> withFreeable p4 (\q4 -> c'getSplinePointCatmullRom q1 q2 q3 q4 (realToFrac t))))) >>= pop

getSplinePointBezierQuad :: Vector2 -> Vector2 -> Vector2 -> Float -> Vector2
getSplinePointBezierQuad p1 p2 p3 t = unsafePerformIO $ withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable p3 (\q3 -> c'getSplinePointBezierQuad q1 q2 q3 (realToFrac t)))) >>= pop

getSplinePointBezierCubic :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Float -> Vector2
getSplinePointBezierCubic p1 p2 p3 p4 t = unsafePerformIO $ withFreeable p1 (\q1 -> withFreeable p2 (\q2 -> withFreeable p3 (\q3 -> withFreeable p4 (\q4 -> c'getSplinePointBezierCubic q1 q2 q3 q4 (realToFrac t))))) >>= pop

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

checkCollisionCircleLine :: Vector2 -> Float -> Vector2 -> Vector2 -> Bool
checkCollisionCircleLine center radius p1 p2 =
  unsafePerformIO $ toBool <$> withFreeable center (\c -> withFreeable p1 (\ptr1 -> withFreeable p2 (\ptr2 -> c'checkCollisionCircleLine c (realToFrac radius) ptr1 ptr2)))

getCollisionRec :: Rectangle -> Rectangle -> Rectangle
getCollisionRec rec1 rec2 =
  unsafePerformIO $ withFreeable rec1 (withFreeable rec2 . c'getCollisionRec) >>= pop
