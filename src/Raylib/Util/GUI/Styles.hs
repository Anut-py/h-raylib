{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Predefined styles that come with raygui. Calling one of these functions
--   will activate that style. To return to the default style, simply call
--   `Raylib.Util.GUI.guiLoadStyleDefault`.
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

import Raylib.Internal.TH (genNative)

$( genNative
     [ ("c'guiLoadStyleAshes", "GuiLoadStyleAshes_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleBluish", "GuiLoadStyleBluish_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleCandy", "GuiLoadStyleCandy_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleCherry", "GuiLoadStyleCherry_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleCyber", "GuiLoadStyleCyber_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleDark", "GuiLoadStyleDark_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleEnefete", "GuiLoadStyleEnefete_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleJungle", "GuiLoadStyleJungle_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleLavanda", "GuiLoadStyleLavanda_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleSunny", "GuiLoadStyleSunny_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleTerminal", "GuiLoadStyleTerminal_", "rgui_bindings.h", [t|IO ()|])
     ]
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
