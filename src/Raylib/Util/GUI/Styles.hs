{-# OPTIONS -Wall #-}
module Raylib.Util.GUI.Styles
  ( guiLoadStyleAshes,
    guiLoadStyleBluish,
    guiLoadStyleCandy,
    guiLoadStyleCherry,
    guiLoadStyleCyber,
    guiLoadStyleDark,
    guiLoadStyleEnefete,
    guiLoadStyleJungle,
    guiLoadStyleLavanda,
    guiLoadStyleSunny,
    guiLoadStyleTerminal,
  )
where

import Raylib.Internal.Native
  ( c'guiLoadStyleAshes,
    c'guiLoadStyleBluish,
    c'guiLoadStyleCandy,
    c'guiLoadStyleCherry,
    c'guiLoadStyleCyber,
    c'guiLoadStyleDark,
    c'guiLoadStyleEnefete,
    c'guiLoadStyleJungle,
    c'guiLoadStyleLavanda,
    c'guiLoadStyleSunny,
    c'guiLoadStyleTerminal,
  )

guiLoadStyleAshes :: IO ()
guiLoadStyleAshes = c'guiLoadStyleAshes

guiLoadStyleBluish :: IO ()
guiLoadStyleBluish = c'guiLoadStyleBluish

guiLoadStyleCandy :: IO ()
guiLoadStyleCandy = c'guiLoadStyleCandy

guiLoadStyleCherry :: IO ()
guiLoadStyleCherry = c'guiLoadStyleCherry

guiLoadStyleCyber :: IO ()
guiLoadStyleCyber = c'guiLoadStyleCyber

guiLoadStyleDark :: IO ()
guiLoadStyleDark = c'guiLoadStyleDark

guiLoadStyleEnefete :: IO ()
guiLoadStyleEnefete = c'guiLoadStyleEnefete

guiLoadStyleJungle :: IO ()
guiLoadStyleJungle = c'guiLoadStyleJungle

guiLoadStyleLavanda :: IO ()
guiLoadStyleLavanda = c'guiLoadStyleLavanda

guiLoadStyleSunny :: IO ()
guiLoadStyleSunny = c'guiLoadStyleSunny

guiLoadStyleTerminal :: IO ()
guiLoadStyleTerminal = c'guiLoadStyleTerminal
