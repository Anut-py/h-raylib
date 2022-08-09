module Raylib.Colors where

import Raylib.Types ( Color(Color) )
import Foreign (castPtr, castStablePtrToPtr, newStablePtr, Ptr)

-- Simple color palette defined in raylib.h

lightGray :: Color
lightGray = Color 200 200 200 255

gray :: Color
gray = Color 130 130 130 255

darkGray :: Color
darkGray = Color 80 80 80 255

yellow :: Color
yellow = Color 253 249 0 255

gold :: Color
gold = Color 255 203 0 255

orange :: Color
orange = Color 255 161 0 255

pink :: Color
pink = Color 255 109 194 255

red :: Color
red = Color 230 41 55 255

maroon :: Color
maroon = Color 190 33 55 255

green :: Color
green = Color 0 228 48 255

lime :: Color
lime = Color 0 158 47 255

darkGreen :: Color
darkGreen = Color 0 117 44 255

skyBlue :: Color
skyBlue = Color 102 191 255 255

blue :: Color
blue = Color 0 121 241 255

darkBlue :: Color
darkBlue = Color 0 82 172 255

purple :: Color
purple = Color 200 122 255 255

violet :: Color
violet = Color 135 60 190 255

darkPurple :: Color
darkPurple = Color 112 31 126 255

beige :: Color
beige = Color 211 176 131 255

brown :: Color
brown = Color 127 106 79 255

darkBrown :: Color
darkBrown = Color 76 63 47 255

white :: Color
white = Color 255 255 255 255

black :: Color
black = Color 0 0 0 255

blank :: Color
blank = Color 0 0 0 0

magenta :: Color
magenta = Color 255 0 255 255

rayWhite :: Color
rayWhite = Color 245 245 245   255
