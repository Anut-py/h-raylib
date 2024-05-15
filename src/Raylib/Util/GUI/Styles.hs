{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Predefined styles that come with raygui. Calling one of these functions
--   will activate that style. To return to the default style, simply call
--   `Raylib.Util.GUI.guiLoadStyleDefault`.
module Raylib.Util.GUI.Styles
  ( guiLoadStyleAmber,
    guiLoadStyleAshes,
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
     [ ("c'guiLoadStyleAmber", "GuiLoadStyleAmber_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleAshes", "GuiLoadStyleAshes_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleBluish", "GuiLoadStyleBluish_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleCandy", "GuiLoadStyleCandy_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleCherry", "GuiLoadStyleCherry_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleCyber", "GuiLoadStyleCyber_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleDark", "GuiLoadStyleDark_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleEnefete", "GuiLoadStyleEnefete_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleJungle", "GuiLoadStyleJungle_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleLavanda", "GuiLoadStyleLavanda_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleSunny", "GuiLoadStyleSunny_", "rgui_bindings.h", [t|IO ()|], False),
       ("c'guiLoadStyleTerminal", "GuiLoadStyleTerminal_", "rgui_bindings.h", [t|IO ()|], False)
     ]
 )

guiLoadStyleAmber :: IO ()
guiLoadStyleAmber = c'guiLoadStyleAmber

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
