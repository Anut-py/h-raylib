{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (unless, when)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Raylib.Core (clearBackground, closeWindow, initWindow, isKeyPressed, setTargetFPS, windowShouldClose)
import Raylib.Types
  ( Color,
    GuiControl (Statusbar),
    GuiControlProperty (TextPadding),
    GuiState (StateDisabled),
    GuiTextAlignmentVertical (TextAlignMiddle),
    KeyboardKey (KeyE, KeyU),
    Rectangle (Rectangle),
  )
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.GUI
  ( guiButton,
    guiCheckBox,
    guiColorPicker,
    guiDisable,
    guiEnable,
    guiGetState,
    guiGetStyleBackgroundColor,
    guiGroupBox,
    guiIsLocked,
    guiLabel,
    guiLabelButton,
    guiListView,
    guiLoadStyleDefault,
    guiLock,
    guiSetStyle,
    guiSetStyleTextAlignmentVertical,
    guiSetStyleTextSize,
    guiSpinner,
    guiStatusBar,
    guiTextBox,
    guiUnlock,
    guiWindowBox,
  )
import Raylib.Util.GUI.Styles
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

data Page1 = Page1
  { showWindowBox :: Bool,
    showForm :: Bool,
    firstName :: Maybe String,
    firstNameEditable :: Bool,
    lastName :: Maybe String,
    lastNameEditable :: Bool,
    age :: Maybe Int,
    ageEditable :: Bool
  }

data Page2 = Page2
  { scroll :: Int,
    theme :: Maybe Int,
    useCustomBackground :: Bool,
    customBackground :: Maybe Color
  }

data AppState = AppState
  { window :: WindowResources,
    page :: Int,
    page1 :: Page1,
    page2 :: Page2
  }

defaultState :: WindowResources -> AppState
defaultState w =
  AppState
    { window = w,
      page = 1,
      page1 =
        Page1
          { showWindowBox = True,
            showForm = True,
            firstName = Nothing,
            firstNameEditable = False,
            lastName = Nothing,
            lastNameEditable = False,
            age = Nothing,
            ageEditable = False
          },
      page2 =
        Page2
          { theme = Nothing,
            scroll = 0,
            useCustomBackground = False,
            customBackground = Nothing
          }
    }

width :: (Num a) => a
width = 1000

height :: (Num a) => a
height = 800

pages :: Int
pages = 2

themes :: [IO ()]
themes =
  [ guiLoadStyleDefault,
    guiLoadStyleAmber,
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
    guiLoadStyleTerminal
  ]

startup :: IO AppState
startup = do
  w <- initWindow width height "raylib [raygui] test suite"
  setTargetFPS 60
  guiSetStyleTextAlignmentVertical TextAlignMiddle
  guiSetStyle Statusbar TextPadding 20
  return (defaultState w)

mainLoop :: AppState -> IO AppState
mainLoop state = do
  drawing
    ( do
        styleBackground <- guiGetStyleBackgroundColor
        clearBackground (if useCustomBackground (page2 state) then fromMaybe styleBackground (customBackground (page2 state)) else styleBackground)
        drawTitle
        drawLockAndDisableButtons
        state' <- drawPage (page state)
        newPage <- drawPaginationButtons (page state')
        return (state' {page = newPage})
    )
  where
    drawTitle =
      do
        guiState <- guiGetState

        guiSetStyleTextSize 40
        guiLabel (Rectangle 20 20 (width - 40) 40) "raygui test suite"

        guiSetStyleTextSize 20
        guiLabel (Rectangle 20 70 (width - 40) 20) ("gui state: " ++ show guiState)

        locked <- guiIsLocked
        let status
              | locked = "gui locked, press U to unlock GUI"
              | guiState == StateDisabled = "gui disabled, press E to enable GUI"
              | otherwise = ""
        guiStatusBar (Rectangle 0 (height - 40) width 40) status
    drawLockAndDisableButtons =
      do
        guiSetStyleTextSize 15
        shouldDisable <- guiButton (Rectangle 20 (height - 90) 100 40) (Just "Disable GUI")
        shouldEnable <- isKeyPressed KeyE

        shouldLock <- guiButton (Rectangle 130 (height - 90) 100 40) (Just "Lock GUI")
        shouldUnlock <- isKeyPressed KeyU

        when shouldDisable guiDisable
        when shouldEnable guiEnable

        when shouldLock guiLock
        when shouldUnlock guiUnlock
    drawPaginationButtons p =
      do
        guiSetStyleTextSize 15
        next <- guiButton (Rectangle (width - 120) (height - 90) 100 40) (Just "Next")
        previous <- guiButton (Rectangle (width - 230) (height - 90) 100 40) (Just "Previous")

        guiLabel (Rectangle (width - 330) (height - 90) 100 40) ("Page " ++ show p ++ " of " ++ show pages)

        let added = p + if next then 1 else if previous then (-1) else 0

        return (if added < 1 || added > pages then p else added)
    drawPage 1 = drawFormPage
    drawPage 2 = drawThemePage
    drawPage _ = error "invalid page"
    drawFormPage =
      do
        let ps = page1 state
            containerMargin = 20
            containerPadding = 20
            containerSpacing = 20
            containerY = 100
            containerWidth = (width / 2) - containerMargin - (containerSpacing / 2)
            containerHeight = height - 200
            formX = containerMargin
            infoX = containerWidth + containerMargin + containerSpacing
        guiGroupBox (Rectangle infoX containerY containerWidth containerHeight) (Just "user info")
        guiSetStyleTextSize 20
        if isNothing (firstName ps) || isNothing (lastName ps) || isNothing (age ps)
          then guiLabel (Rectangle (infoX + containerPadding) (containerY + 30) 200 20) "Fill out the form"
          else do
            guiLabel (Rectangle (infoX + containerPadding) (containerY + 30) 200 20) ("First name: " ++ fromJust (firstName ps))
            guiLabel (Rectangle (infoX + containerPadding) (containerY + 60) 200 20) ("Last name: " ++ fromJust (lastName ps))
            guiLabel (Rectangle (infoX + containerPadding) (containerY + 90) 200 20) ("Age: " ++ show (fromJust (age ps)))

        if showWindowBox ps
          then
            ( do
                closeBox <- guiWindowBox (Rectangle formX containerY containerWidth containerHeight) (Just "form")

                guiSetStyleTextSize 20

                if showForm ps
                  then do
                    hideForm <- guiLabelButton (Rectangle (formX + containerPadding) (containerY + 30) 100 20) (Just "Hide form")
                    clearForm <- guiLabelButton (Rectangle (formX + containerPadding + 110) (containerY + 30) 100 20) (Just "Clear form")

                    guiSetStyleTextSize 15
                    guiLabel (Rectangle (formX + containerPadding) (containerY + 65) 100 15) "First name"

                    guiSetStyleTextSize 20
                    (firstNameToggled, firstName') <-
                      guiTextBox
                        (Rectangle (formX + containerPadding) (containerY + 85) (containerWidth - (2 * containerPadding)) 40)
                        (fromMaybe "" (firstName ps))
                        Nothing
                        (firstNameEditable ps)

                    guiSetStyleTextSize 15
                    guiLabel (Rectangle (formX + containerPadding) (containerY + 135) 100 15) "Last name"

                    guiSetStyleTextSize 20
                    (lastNameToggled, lastName') <-
                      guiTextBox
                        (Rectangle (formX + containerPadding) (containerY + 155) (containerWidth - (2 * containerPadding)) 40)
                        (fromMaybe "" (lastName ps))
                        Nothing
                        (lastNameEditable ps)

                    guiSetStyleTextSize 15
                    guiLabel (Rectangle (formX + containerPadding) (containerY + 205) 100 15) "Age"

                    guiSetStyleTextSize 20
                    (ageToggled, age') <-
                      guiSpinner
                        (Rectangle (formX + containerPadding) (containerY + 225) (containerWidth - (2 * containerPadding)) 40)
                        Nothing
                        (fromMaybe 0 (age ps))
                        0
                        150
                        (ageEditable ps)

                    return $
                      state
                        { page1 =
                            ps
                              { showWindowBox = not closeBox,
                                showForm = not hideForm,
                                firstName = if clearForm || (firstName' == "") then Nothing else Just firstName',
                                firstNameEditable = not clearForm && (if firstNameToggled then not (firstNameEditable ps) else firstNameEditable ps),
                                lastName = if clearForm || (lastName' == "") then Nothing else Just lastName',
                                lastNameEditable = not clearForm && (if lastNameToggled then not (lastNameEditable ps) else lastNameEditable ps),
                                age = if clearForm then Nothing else Just age',
                                ageEditable = not clearForm && (if ageToggled then not (ageEditable ps) else ageEditable ps)
                              }
                        }
                  else do
                    displayForm <- guiLabelButton (Rectangle (formX + containerPadding) 140 100 20) (Just "Show form")
                    return $ state {page1 = ps {showWindowBox = not closeBox, showForm = displayForm}}
            )
          else
            ( do
                guiSetStyleTextSize 20
                createBox <- guiButton (Rectangle formX containerY containerWidth 40) (Just "Show window box")
                return $ state {page1 = ps {showWindowBox = createBox}}
            )
    drawThemePage = do
      let ps = page2 state

      guiSetStyleTextSize 30
      guiLabel (Rectangle 20 100 400 30) "Theme"

      guiSetStyleTextSize 20
      (scroll', theme') <- guiListView (Rectangle 20 140 400 (height - 660)) "default;amber;ashes;bluish;candy;cherry;cyber;dark;enefete;jungle;lavanda;sunny;terminal" (scroll ps) (theme ps)

      guiSetStyleTextSize 30
      guiLabel (Rectangle 20 (height - 510) 360 30) "Custom background color"
      custom' <- guiCheckBox (Rectangle 390 (height - 510) 30 30) Nothing (useCustomBackground ps)

      oldState <- guiGetState
      unless custom' guiDisable
      color' <- guiColorPicker (Rectangle 20 (height - 470) 374 360) (customBackground ps)
      unless (oldState == StateDisabled) guiEnable

      when (theme' /= theme ps) (themes !! (fromMaybe 0 theme'))
      return $ state {page2 = ps {scroll = scroll', theme = theme', useCustomBackground = custom', customBackground = Just color'}}

shouldClose :: AppState -> IO Bool
shouldClose = const windowShouldClose

teardown :: AppState -> IO ()
teardown s = closeWindow (Just (window s))

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
