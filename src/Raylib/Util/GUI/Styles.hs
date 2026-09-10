{-# LANGUAGE TemplateHaskell #-}

-- | Predefined styles that come with raygui. Calling one of these functions
--   will activate that style. To return to the default style, simply call
--   `Raylib.Util.GUI.guiLoadStyleDefault`.
module Raylib.Util.GUI.Styles
  ( guiLoadStyleAdvance,
    guiLoadStyleAmber,
    guiLoadStyleAshes,
    guiLoadStyleBluish,
    guiLoadStyleBrick,
    guiLoadStyleCandy,
    guiLoadStyleCherry,
    guiLoadStyleCyber,
    guiLoadStyleDark,
    guiLoadStyleEnefete,
    guiLoadStyleGenesis,
    guiLoadStyleJungle,
    guiLoadStyleLavanda,
    guiLoadStylePocket,
    guiLoadStyleRLTech,
    guiLoadStyleSunny,
    guiLoadStyleTerminal,
    guiLoadStyleTurbo,
    guiLoadStyleWisteria,
  )
where

import Raylib.Internal.TH (genNative)

$( genNative
     [ ("c'guiLoadStyleAdvance", "GuiLoadStyleAdvance_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleAmber", "GuiLoadStyleAmber_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleAshes", "GuiLoadStyleAshes_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleBluish", "GuiLoadStyleBluish_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleBrick", "GuiLoadStyleBrick_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleCandy", "GuiLoadStyleCandy_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleCherry", "GuiLoadStyleCherry_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleCyber", "GuiLoadStyleCyber_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleDark", "GuiLoadStyleDark_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleEnefete", "GuiLoadStyleEnefete_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleGenesis", "GuiLoadStyleGenesis_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleJungle", "GuiLoadStyleJungle_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleLavanda", "GuiLoadStyleLavanda_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStylePocket", "GuiLoadStylePocket_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleRLTech", "GuiLoadStyleRLTech_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleSunny", "GuiLoadStyleSunny_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleTerminal", "GuiLoadStyleTerminal_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleTurbo", "GuiLoadStyleTurbo_", "rgui_bindings.h", [t|IO ()|]),
       ("c'guiLoadStyleWisteria", "GuiLoadStyleWisteria_", "rgui_bindings.h", [t|IO ()|])
     ]
 )

guiLoadStyleAdvance :: IO ()
guiLoadStyleAdvance = c'guiLoadStyleAdvance

guiLoadStyleAmber :: IO ()
guiLoadStyleAmber = c'guiLoadStyleAmber

guiLoadStyleAshes :: IO ()
guiLoadStyleAshes = c'guiLoadStyleAshes

guiLoadStyleBluish :: IO ()
guiLoadStyleBluish = c'guiLoadStyleBluish

guiLoadStyleBrick :: IO ()
guiLoadStyleBrick = c'guiLoadStyleBrick

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

guiLoadStyleGenesis :: IO ()
guiLoadStyleGenesis = c'guiLoadStyleGenesis

guiLoadStyleJungle :: IO ()
guiLoadStyleJungle = c'guiLoadStyleJungle

guiLoadStyleLavanda :: IO ()
guiLoadStyleLavanda = c'guiLoadStyleLavanda

guiLoadStylePocket :: IO ()
guiLoadStylePocket = c'guiLoadStylePocket

guiLoadStyleRLTech :: IO ()
guiLoadStyleRLTech = c'guiLoadStyleRLTech

guiLoadStyleSunny :: IO ()
guiLoadStyleSunny = c'guiLoadStyleSunny

guiLoadStyleTerminal :: IO ()
guiLoadStyleTerminal = c'guiLoadStyleTerminal

guiLoadStyleTurbo :: IO ()
guiLoadStyleTurbo = c'guiLoadStyleTurbo

guiLoadStyleWisteria :: IO ()
guiLoadStyleWisteria = c'guiLoadStyleWisteria
