{-# OPTIONS -Wall #-}

module Raylib.Util.GUI
  ( -- * Global gui state control functions
    guiEnable,
    guiDisable,
    guiLock,
    guiUnlock,
    guiIsLocked,
    guiSetAlpha,
    guiSetState,
    guiGetState,

    -- * Font set/get functions
    guiSetFont,
    guiGetFont,

    -- * Style set/get functions

    -- | In the native C code, there is just one @guiSetStyle@ function and one
    --   @guiGetStyle@ function, which take a property type and an @int@ as the
    --   property value. This @int@ can represent a plain integer, a `Color`,
    --   or an enum, depending on the property type. This is very un-Haskelly
    --   behavior and not very user friendly (as it requires the use of
    --   `colorToInt` and such), so they have been split into 3 setters and
    --   getters, one for regular `Int`s, one for `Color`s, and one for
    --   `Enum`s. There are also a bunch of specialized getters and setters for
    --   commonly used properties.

    -- ** Set style
    guiSetStyle,
    guiSetStyleC,
    guiSetStyleE,
    guiSetStyleBorderColorNormal,
    guiSetStyleBaseColorNormal,
    guiSetStyleTextColorNormal,
    guiSetStyleBorderColorFocused,
    guiSetStyleBaseColorFocused,
    guiSetStyleTextColorFocused,
    guiSetStyleBorderColorPressed,
    guiSetStyleBaseColorPressed,
    guiSetStyleTextColorPressed,
    guiSetStyleBorderColorDisabled,
    guiSetStyleBaseColorDisabled,
    guiSetStyleTextColorDisabled,
    guiSetStyleBorderWidth,
    guiSetStyleTextPadding,
    guiSetStyleTextAlignment,
    guiSetStyleTextSize,
    guiSetStyleTextSpacing,
    guiSetStyleLineColor,
    guiSetStyleBackgroundColor,
    guiSetStyleTextLineSpacing,
    guiSetStyleTextAlignmentVertical,
    guiSetStyleTextWrapMode,

    -- ** Get style
    guiGetStyle,
    guiGetStyleC,
    guiGetStyleE,
    guiGetStyleBorderColorNormal,
    guiGetStyleBaseColorNormal,
    guiGetStyleTextColorNormal,
    guiGetStyleBorderColorFocused,
    guiGetStyleBaseColorFocused,
    guiGetStyleTextColorFocused,
    guiGetStyleBorderColorPressed,
    guiGetStyleBaseColorPressed,
    guiGetStyleTextColorPressed,
    guiGetStyleBorderColorDisabled,
    guiGetStyleBaseColorDisabled,
    guiGetStyleTextColorDisabled,
    guiGetStyleBorderWidth,
    guiGetStyleTextPadding,
    guiGetStyleTextAlignment,
    guiGetStyleTextSize,
    guiGetStyleTextSpacing,
    guiGetStyleLineColor,
    guiGetStyleBackgroundColor,
    guiGetStyleTextLineSpacing,
    guiGetStyleTextAlignmentVertical,
    guiGetStyleTextWrapMode,

    -- * Styles loading functions
    guiLoadStyle,
    guiLoadStyleDefault,

    -- * Tooltips management functions
    guiEnableTooltip,
    guiDisableTooltip,
    guiSetTooltip,

    -- * Icons functionality
    guiIconText,
    guiSetIconScale,
    guiGetIcons,
    guiLoadIcons,
    guiDrawIcon,

    -- * Controls

    -- ** Container/separator controls, useful for controls organization
    guiWindowBox,
    guiGroupBox,
    guiLine,
    guiPanel,
    guiTabBar,
    guiScrollPanel,

    -- ** Basic controls set
    guiLabel,
    guiButton,
    guiLabelButton,
    guiToggle,
    guiToggleGroup,
    guiToggleSlider,
    guiCheckBox,
    guiComboBox,
    guiDropdownBox,
    guiSpinner,
    guiValueBox,
    guiTextBox,
    guiSlider,
    guiSliderBar,
    guiProgressBar,
    guiStatusBar,
    guiDummyRec,
    guiGrid,

    -- ** Advanced controls set
    guiListView,
    guiListViewEx,
    guiMessageBox,
    guiTextInputBox,
    guiColorPicker,
    guiColorPanel,
    guiColorBarAlpha,
    guiColorBarHue,
    guiColorPickerHSV,
    guiColorPanelHSV,
  )
where

import Control.Monad (void, (>=>))
import Data.Maybe (fromMaybe)
import Foreign (Ptr, Storable (peek), fromBool, nullPtr, toBool)
import Foreign.C (CBool, CUInt, newCString, withCString, peekCString)
import Raylib.Core.Textures (colorToInt, getColor)
import Raylib.ForeignUtil (pop, popCArray, popCString, withCStringBuffer, withFreeable, withFreeableArrayLen, withMaybe, withMaybeCString)
import Raylib.Native (c'guiButton, c'guiCheckBox, c'guiColorBarAlpha, c'guiColorBarHue, c'guiColorPanel, c'guiColorPanelHSV, c'guiColorPicker, c'guiColorPickerHSV, c'guiComboBox, c'guiDisable, c'guiDisableTooltip, c'guiDrawIcon, c'guiDropdownBox, c'guiDummyRec, c'guiEnable, c'guiEnableTooltip, c'guiGetFont, c'guiGetIcons, c'guiGetState, c'guiGetStyle, c'guiGrid, c'guiGroupBox, c'guiIconText, c'guiIsLocked, c'guiLabel, c'guiLabelButton, c'guiLine, c'guiListView, c'guiListViewEx, c'guiLoadIcons, c'guiLoadStyle, c'guiLoadStyleDefault, c'guiLock, c'guiMessageBox, c'guiPanel, c'guiScrollPanel, c'guiSetAlpha, c'guiSetFont, c'guiSetIconScale, c'guiSetState, c'guiSetStyle, c'guiSetTooltip, c'guiSlider, c'guiSliderBar, c'guiSpinner, c'guiStatusBar, c'guiTabBar, c'guiTextBox, c'guiTextInputBox, c'guiToggle, c'guiToggleGroup, c'guiToggleSlider, c'guiUnlock, c'guiValueBox, c'guiWindowBox)
import Raylib.Types (Color (Color), Font, GuiControl (Default), GuiControlProperty (..), GuiDefaultProperty (..), GuiIconName, GuiState, GuiTextAlignment, GuiTextAlignmentVertical, GuiTextWrapMode, Rectangle (Rectangle), Vector2 (Vector2), Vector3 (Vector3))

-- | Enable gui controls (global state)
guiEnable :: IO ()
guiEnable = c'guiEnable

-- | Disable gui controls (global state)
guiDisable :: IO ()
guiDisable = c'guiDisable

-- | Lock gui controls (global state)
guiLock :: IO ()
guiLock = c'guiLock

-- | Unlock gui controls (global state)
guiUnlock :: IO ()
guiUnlock = c'guiUnlock

-- | Check if gui is locked (global state)
guiIsLocked :: IO Bool
guiIsLocked = toBool <$> c'guiIsLocked

-- | Set gui controls alpha (global state), alpha goes from 0.0f to 1.0f
guiSetAlpha :: Float -> IO ()
guiSetAlpha = c'guiSetAlpha . realToFrac

-- | Set gui state (global state)
guiSetState :: GuiState -> IO ()
guiSetState = c'guiSetState . fromIntegral . fromEnum

-- | Get gui state (global state)
guiGetState :: IO GuiState
guiGetState = toEnum . fromIntegral <$> c'guiGetState

-- | Set gui custom font (global state)
guiSetFont :: Font -> IO ()
guiSetFont font = withFreeable font c'guiSetFont

-- | Get gui custom font (global state)
guiGetFont :: IO Font
guiGetFont = c'guiGetFont >>= pop

-- | Set style property as `Int`
guiSetStyle :: (Enum e) => GuiControl -> e -> Int -> IO ()
guiSetStyle control property value = c'guiSetStyle (fromIntegral (fromEnum control)) (fromIntegral (fromEnum property)) (fromIntegral value)

-- | Set style property as `Color`
guiSetStyleC :: (Enum e) => GuiControl -> e -> Color -> IO ()
guiSetStyleC control property color = guiSetStyle control property (colorToInt color)

-- | Set style property as `Enum`
guiSetStyleE :: (Enum e, Enum v) => GuiControl -> e -> v -> IO ()
guiSetStyleE control property value = guiSetStyle control property (fromEnum value)

-- | Set BORDER_COLOR_NORMAL style property
-- | Control border color in STATE_NORMAL
guiSetStyleBorderColorNormal :: GuiControl -> Color -> IO ()
guiSetStyleBorderColorNormal control = guiSetStyleC control BorderColorNormal

-- | Set BASE_COLOR_NORMAL style property
-- | Control base color in STATE_NORMAL
guiSetStyleBaseColorNormal :: GuiControl -> Color -> IO ()
guiSetStyleBaseColorNormal control = guiSetStyleC control BaseColorNormal

-- | Set TEXT_COLOR_NORMAL style property
-- | Control text color in STATE_NORMAL
guiSetStyleTextColorNormal :: GuiControl -> Color -> IO ()
guiSetStyleTextColorNormal control = guiSetStyleC control TextColorNormal

-- | Set BORDER_COLOR_FOCUSED style property
-- | Control border color in STATE_FOCUSED
guiSetStyleBorderColorFocused :: GuiControl -> Color -> IO ()
guiSetStyleBorderColorFocused control = guiSetStyleC control BorderColorFocused

-- | Set BASE_COLOR_FOCUSED style property
-- | Control base color in STATE_FOCUSED
guiSetStyleBaseColorFocused :: GuiControl -> Color -> IO ()
guiSetStyleBaseColorFocused control = guiSetStyleC control BaseColorFocused

-- | Set TEXT_COLOR_FOCUSED style property
-- | Control text color in STATE_FOCUSED
guiSetStyleTextColorFocused :: GuiControl -> Color -> IO ()
guiSetStyleTextColorFocused control = guiSetStyleC control TextColorFocused

-- | Set BORDER_COLOR_PRESSED style property
-- | Control border color in STATE_PRESSED
guiSetStyleBorderColorPressed :: GuiControl -> Color -> IO ()
guiSetStyleBorderColorPressed control = guiSetStyleC control BorderColorPressed

-- | Set BASE_COLOR_PRESSED style property
-- | Control base color in STATE_PRESSED
guiSetStyleBaseColorPressed :: GuiControl -> Color -> IO ()
guiSetStyleBaseColorPressed control = guiSetStyleC control BaseColorPressed

-- | Set TEXT_COLOR_PRESSED style property
-- | Control text color in STATE_PRESSED
guiSetStyleTextColorPressed :: GuiControl -> Color -> IO ()
guiSetStyleTextColorPressed control = guiSetStyleC control TextColorPressed

-- | Set BORDER_COLOR_DISABLED style property
-- | Control border color in STATE_DISABLED
guiSetStyleBorderColorDisabled :: GuiControl -> Color -> IO ()
guiSetStyleBorderColorDisabled control = guiSetStyleC control BorderColorDisabled

-- | Set BASE_COLOR_DISABLED style property
-- | Control base color in STATE_DISABLED
guiSetStyleBaseColorDisabled :: GuiControl -> Color -> IO ()
guiSetStyleBaseColorDisabled control = guiSetStyleC control BaseColorDisabled

-- | Set TEXT_COLOR_DISABLED style property
-- | Control text color in STATE_DISABLED
guiSetStyleTextColorDisabled :: GuiControl -> Color -> IO ()
guiSetStyleTextColorDisabled control = guiSetStyleC control TextColorDisabled

-- | Set BORDER_WIDTH style property
-- | Control border size, 0 for no border
guiSetStyleBorderWidth :: GuiControl -> Int -> IO ()
guiSetStyleBorderWidth control = guiSetStyle control BorderWidth

-- | Set TEXT_PADDING style property
-- | Control text padding, not considering border
guiSetStyleTextPadding :: GuiControl -> Int -> IO ()
guiSetStyleTextPadding control = guiSetStyle control TextPadding

-- | Set TEXT_ALIGNMENT style property
-- | Control text horizontal alignment inside control text bound (after border and padding)
guiSetStyleTextAlignment :: GuiControl -> GuiTextAlignment -> IO ()
guiSetStyleTextAlignment control = guiSetStyleE control TextAlignment

-- | Set TEXT_SIZE default style property
-- | Text size (glyphs max height)
guiSetStyleTextSize :: Int -> IO ()
guiSetStyleTextSize = guiSetStyle Default TextSize

-- | Set TEXT_SPACING default style property
-- | Text spacing between glyphs
guiSetStyleTextSpacing :: Int -> IO ()
guiSetStyleTextSpacing = guiSetStyle Default TextSpacing

-- | Set LINE_COLOR default style property
-- | Line control color
guiSetStyleLineColor :: Color -> IO ()
guiSetStyleLineColor = guiSetStyleC Default LineColor

-- | Set BACKGROUND_COLOR default style property
-- | Background color
guiSetStyleBackgroundColor :: Color -> IO ()
guiSetStyleBackgroundColor = guiSetStyleC Default BackgroundColor

-- | Set TEXT_LINE_SPACING default style property
-- | Text spacing between lines
guiSetStyleTextLineSpacing :: Int -> IO ()
guiSetStyleTextLineSpacing = guiSetStyle Default TextLineSpacing

-- | Set TEXT_ALIGNMENT_VERTICAL default style property
-- | Text vertical alignment inside text bounds (after border and padding)
guiSetStyleTextAlignmentVertical :: GuiTextAlignmentVertical -> IO ()
guiSetStyleTextAlignmentVertical = guiSetStyleE Default TextAlignmentVertical

-- | Set TEXT_WRAP_MODE default style property
-- | Text wrap-mode inside text bounds
guiSetStyleTextWrapMode :: GuiTextWrapMode -> IO ()
guiSetStyleTextWrapMode = guiSetStyleE Default TextWrapMode

-- | Get style property as `Int`
guiGetStyle :: (Enum e) => GuiControl -> e -> IO Int
guiGetStyle control property = fromIntegral <$> c'guiGetStyle (fromIntegral (fromEnum control)) (fromIntegral (fromEnum property))

-- | Set style property as `Color`
guiGetStyleC :: (Enum e) => GuiControl -> e -> IO Color
guiGetStyleC control property = getColor . fromIntegral <$> guiGetStyle control property

-- | Set style property as `Enum`
guiGetStyleE :: (Enum e, Enum v) => GuiControl -> e -> IO v
guiGetStyleE control property = toEnum <$> guiGetStyle control property

-- | Get BORDER_COLOR_NORMAL style property
-- | Control border color in STATE_NORMAL
guiGetStyleBorderColorNormal :: GuiControl -> IO Color
guiGetStyleBorderColorNormal control = guiGetStyleC control BorderColorNormal

-- | Get BASE_COLOR_NORMAL style property
-- | Control base color in STATE_NORMAL
guiGetStyleBaseColorNormal :: GuiControl -> IO Color
guiGetStyleBaseColorNormal control = guiGetStyleC control BaseColorNormal

-- | Get TEXT_COLOR_NORMAL style property
-- | Control text color in STATE_NORMAL
guiGetStyleTextColorNormal :: GuiControl -> IO Color
guiGetStyleTextColorNormal control = guiGetStyleC control TextColorNormal

-- | Get BORDER_COLOR_FOCUSED style property
-- | Control border color in STATE_FOCUSED
guiGetStyleBorderColorFocused :: GuiControl -> IO Color
guiGetStyleBorderColorFocused control = guiGetStyleC control BorderColorFocused

-- | Get BASE_COLOR_FOCUSED style property
-- | Control base color in STATE_FOCUSED
guiGetStyleBaseColorFocused :: GuiControl -> IO Color
guiGetStyleBaseColorFocused control = guiGetStyleC control BaseColorFocused

-- | Get TEXT_COLOR_FOCUSED style property
-- | Control text color in STATE_FOCUSED
guiGetStyleTextColorFocused :: GuiControl -> IO Color
guiGetStyleTextColorFocused control = guiGetStyleC control TextColorFocused

-- | Get BORDER_COLOR_PRESSED style property
-- | Control border color in STATE_PRESSED
guiGetStyleBorderColorPressed :: GuiControl -> IO Color
guiGetStyleBorderColorPressed control = guiGetStyleC control BorderColorPressed

-- | Get BASE_COLOR_PRESSED style property
-- | Control base color in STATE_PRESSED
guiGetStyleBaseColorPressed :: GuiControl -> IO Color
guiGetStyleBaseColorPressed control = guiGetStyleC control BaseColorPressed

-- | Get TEXT_COLOR_PRESSED style property
-- | Control text color in STATE_PRESSED
guiGetStyleTextColorPressed :: GuiControl -> IO Color
guiGetStyleTextColorPressed control = guiGetStyleC control TextColorPressed

-- | Get BORDER_COLOR_DISABLED style property
-- | Control border color in STATE_DISABLED
guiGetStyleBorderColorDisabled :: GuiControl -> IO Color
guiGetStyleBorderColorDisabled control = guiGetStyleC control BorderColorDisabled

-- | Get BASE_COLOR_DISABLED style property
-- | Control base color in STATE_DISABLED
guiGetStyleBaseColorDisabled :: GuiControl -> IO Color
guiGetStyleBaseColorDisabled control = guiGetStyleC control BaseColorDisabled

-- | Get TEXT_COLOR_DISABLED style property
-- | Control text color in STATE_DISABLED
guiGetStyleTextColorDisabled :: GuiControl -> IO Color
guiGetStyleTextColorDisabled control = guiGetStyleC control TextColorDisabled

-- | Get BORDER_WIDTH style property
-- | Control border size, 0 for no border
guiGetStyleBorderWidth :: GuiControl -> IO Int
guiGetStyleBorderWidth control = guiGetStyle control BorderWidth

-- | Get TEXT_PADDING style property
-- | Control text padding, not considering border
guiGetStyleTextPadding :: GuiControl -> IO Int
guiGetStyleTextPadding control = guiGetStyle control TextPadding

-- | Get TEXT_ALIGNMENT style property
-- | Control text horizontal alignment inside control text bound (after border and padding)
guiGetStyleTextAlignment :: GuiControl -> IO GuiTextAlignment
guiGetStyleTextAlignment control = guiGetStyleE control TextAlignment

-- | Get TEXT_SIZE default style property
-- | Text size (glyphs max height)
guiGetStyleTextSize :: IO Int
guiGetStyleTextSize = guiGetStyle Default TextSize

-- | Get TEXT_SPACING default style property
-- | Text spacing between glyphs
guiGetStyleTextSpacing :: IO Int
guiGetStyleTextSpacing = guiGetStyle Default TextSpacing

-- | Get LINE_COLOR default style property
-- | Line control color
guiGetStyleLineColor :: IO Color
guiGetStyleLineColor = guiGetStyleC Default LineColor

-- | Get BACKGROUND_COLOR default style property
-- | Background color
guiGetStyleBackgroundColor :: IO Color
guiGetStyleBackgroundColor = guiGetStyleC Default BackgroundColor

-- | Get TEXT_LINE_SPACING default style property
-- | Text spacing between lines
guiGetStyleTextLineSpacing :: IO Int
guiGetStyleTextLineSpacing = guiGetStyle Default TextLineSpacing

-- | Get TEXT_ALIGNMENT_VERTICAL default style property
-- | Text vertical alignment inside text bounds (after border and padding)
guiGetStyleTextAlignmentVertical :: IO GuiTextAlignmentVertical
guiGetStyleTextAlignmentVertical = guiGetStyleE Default TextAlignmentVertical

-- | Get TEXT_WRAP_MODE default style property
-- | Text wrap-mode inside text bounds
guiGetStyleTextWrapMode :: IO GuiTextWrapMode
guiGetStyleTextWrapMode = guiGetStyleE Default TextWrapMode

-- | Load style file over global style variable (.rgs)
guiLoadStyle :: String -> IO ()
guiLoadStyle fileName = withCString fileName c'guiLoadStyle

-- | Load style default over global style
guiLoadStyleDefault :: IO ()
guiLoadStyleDefault = c'guiLoadStyleDefault

-- | Enable gui tooltips (global state)
guiEnableTooltip :: IO ()
guiEnableTooltip = c'guiEnableTooltip

-- | Disable gui tooltips (global state)
guiDisableTooltip :: IO ()
guiDisableTooltip = c'guiDisableTooltip

-- | Set tooltip string
guiSetTooltip :: String -> IO ()
guiSetTooltip tooltip = withCString tooltip c'guiSetTooltip

-- | Get text with icon id prepended (if supported)
guiIconText :: GuiIconName -> String -> IO String
guiIconText icon text = withCString text (c'guiIconText (fromIntegral (fromEnum icon)) >=> peekCString)

-- | Set default icon drawing size
guiSetIconScale :: Int -> IO ()
guiSetIconScale = c'guiSetIconScale . fromIntegral

-- | Get raygui icons raw pointer (8192 bytes)
guiGetIcons :: IO (Ptr CUInt)
guiGetIcons = c'guiGetIcons

-- | Load raygui icons file (.rgi) into internal icons data
guiLoadIcons ::
  String ->
  Bool ->
  -- | The number of icons in the file
  Int ->
  IO [String]
guiLoadIcons fileName loadIconsName count = do
  raw <- withCString fileName (\f -> c'guiLoadIcons f (fromBool loadIconsName))
  cStrings <- popCArray count raw
  mapM popCString cStrings

-- | Draw icon using pixel size at specified position
guiDrawIcon :: GuiIconName -> Int -> Int -> Int -> Color -> IO ()
guiDrawIcon icon posX posY pixelSize color = withFreeable color (c'guiDrawIcon (fromIntegral (fromEnum icon)) (fromIntegral posX) (fromIntegral posY) (fromIntegral pixelSize))

-- | Window Box control, shows a window that can be closed
guiWindowBox ::
  Rectangle ->
  Maybe String ->
  -- | `True` if the close button is clicked
  IO Bool
guiWindowBox bounds title = toBool <$> withFreeable bounds (withMaybeCString title . c'guiWindowBox)

-- | Group Box control with text name
guiGroupBox :: Rectangle -> Maybe String -> IO ()
guiGroupBox bounds text = void (withFreeable bounds (withMaybeCString text . c'guiGroupBox))

-- | Line separator control, could contain text
guiLine :: Rectangle -> Maybe String -> IO ()
guiLine bounds text = void (withFreeable bounds (withMaybeCString text . c'guiLine))

-- | Panel control, useful to group controls
guiPanel :: Rectangle -> Maybe String -> IO ()
guiPanel bounds text = void (withFreeable bounds (withMaybeCString text . c'guiPanel))

-- | Tab Bar control
guiTabBar ::
  Rectangle ->
  [String] ->
  -- | The currently active tab's index, use `Nothing` if creating the tab bar
  --   for the first time
  Maybe Int ->
  -- | A tuple, the first element is the index of the active tab, the second
  --   element is the tab whose close button is pressed (if any)
  IO (Int, Maybe Int)
guiTabBar bounds tabNames active = do
  cStrings <- mapM newCString tabNames
  withFreeable
    bounds
    ( \b ->
        withFreeableArrayLen
          cStrings
          ( \l t ->
              withFreeable
                (fromIntegral (fromMaybe 0 active))
                ( \a -> do
                    close <- c'guiTabBar b t (fromIntegral l) a
                    active' <- peek a
                    return (fromIntegral active', if close == (-1) then Nothing else Just (fromIntegral close))
                )
          )
    )

-- | Scroll Panel control
guiScrollPanel ::
  Rectangle ->
  Maybe String ->
  Rectangle ->
  -- | The panel's scroll vector, use `Nothing` if creating the panel for the
  --   first time
  Maybe Vector2 ->
  -- | The panel's view rectangle, use `Nothing` if creating the panel for the
  --   first time
  Maybe Rectangle ->
  -- | The panel's updated scroll vector and view rectangle as a tuple
  IO (Vector2, Rectangle)
guiScrollPanel bounds text content scroll view =
  withFreeable
    bounds
    ( \b ->
        withMaybeCString
          text
          ( \t ->
              withFreeable
                content
                ( \c ->
                    withFreeable
                      (fromMaybe (Vector2 0 0) scroll)
                      ( \s ->
                          withFreeable
                            (fromMaybe (Rectangle 0 0 0 0) view)
                            ( \v -> do
                                _ <- c'guiScrollPanel b t c s v
                                scroll' <- peek s
                                view' <- peek v
                                return (scroll', view')
                            )
                      )
                )
          )
    )

-- | Label control
guiLabel :: Rectangle -> String -> IO ()
guiLabel bounds text = void (withFreeable bounds (withCString text . c'guiLabel))

-- | Button control, returns true when clicked
guiButton :: Rectangle -> Maybe String -> IO Bool
guiButton bounds text = toBool <$> withFreeable bounds (withMaybeCString text . c'guiButton)

-- | Label button control, returns true when clicked
guiLabelButton :: Rectangle -> Maybe String -> IO Bool
guiLabelButton bounds text = toBool <$> withFreeable bounds (withMaybeCString text . c'guiLabelButton)

-- | Toggle Button control
guiToggle :: Rectangle -> Maybe String -> Bool -> IO Bool
guiToggle bounds text active = toBool <$> withFreeable bounds (\b -> withMaybeCString text (\t -> withFreeable (fromBool active :: CBool) (\a -> c'guiToggle b t a >> peek a)))

-- | Toggle Group control
guiToggleGroup ::
  Rectangle ->
  -- | The names of the toggles, separated with semicolons
  String ->
  -- | The currently active toggle's index, use `Nothing` if creating the
  --   toggle group for the first time
  Maybe Int ->
  -- | The updated active toggle index
  IO Int
guiToggleGroup bounds text active = fromIntegral <$> withFreeable bounds (\b -> withCString text (\t -> withFreeable (fromIntegral (fromMaybe 0 active)) (\a -> c'guiToggleGroup b t a >> peek a)))

-- | Toggle Slider control
guiToggleSlider ::
  Rectangle ->
  -- | The names of the toggles, separated with semicolons
  String ->
  -- | The currently active toggle's index, use `Nothing` if creating the
  --   toggle slider for the first time
  Maybe Int ->
  -- | A tuple, the first element is whether the slider was clicked, the second
  --   element is the updated toggle index
  IO (Bool, Int)
guiToggleSlider bounds text active =
  withFreeable
    bounds
    ( \b ->
        withCString
          text
          ( \t ->
              withFreeable
                (fromIntegral (fromMaybe 0 active))
                ( \a -> do
                    clicked <- c'guiToggleSlider b t a
                    active' <- peek a
                    return (toBool clicked, fromIntegral active')
                )
          )
    )

-- | Check Box control
guiCheckBox ::
  Rectangle ->
  Maybe String ->
  -- | The current checkbox state (checked/unchecked)
  Bool ->
  -- | The updated checkbox state (checked/unchecked)
  IO Bool
guiCheckBox bounds text checked = toBool <$> withFreeable bounds (\b -> withMaybeCString text (\t -> withFreeable (fromBool checked :: CBool) (\c -> c'guiCheckBox b t c >> peek c)))

-- | Combo Box control
guiComboBox ::
  Rectangle ->
  -- | The names of the combobox options, separated with semicolons
  String ->
  -- | The currently active option's index, use `Nothing` if creating the
  --   combobox for the first time
  Maybe Int ->
  -- | The updated active option index
  IO Int
guiComboBox bounds text active = fromIntegral <$> withFreeable bounds (\b -> withCString text (\t -> withFreeable (fromIntegral (fromMaybe 0 active)) (\a -> c'guiComboBox b t a >> peek a)))

-- | Dropdown Box control
guiDropdownBox ::
  Rectangle ->
  -- | The names of the dropdown options, separated with semicolons
  String ->
  -- | The currently active option's index, use `Nothing` if creating the
  --   dropdown for the first time
  Maybe Int ->
  -- | `True` if the dropdown should be open (editable), false otherwise
  Bool ->
  -- | A tuple, the first element is whether the dropdown was clicked (i.e.
  --   the open/closed mode should be toggled), the second element is the
  --   updated toggle index
  IO (Bool, Int)
guiDropdownBox bounds text active editMode =
  withFreeable
    bounds
    ( \b ->
        withCString
          text
          ( \t ->
              withFreeable
                (fromIntegral (fromMaybe 0 active))
                ( \a -> do
                    toggle <- c'guiDropdownBox b t a (fromBool editMode)
                    active' <- peek a
                    return (toBool toggle, fromIntegral active')
                )
          )
    )

-- | Spinner control
guiSpinner ::
  Rectangle ->
  Maybe String ->
  -- | The current value
  Int ->
  Int ->
  Int ->
  -- | `True` if the spinner should be editable, `False` otherwise
  Bool ->
  -- | A tuple, the first element is whether the spinner was toggled (i.e.
  --   the edit mode should be toggled), the second element is the updated
  --   value
  IO (Bool, Int)
guiSpinner bounds text value minValue maxValue editMode =
  withFreeable
    bounds
    ( \b ->
        withMaybeCString
          text
          ( \t ->
              withFreeable
                (fromIntegral value)
                ( \v -> do
                    changed <- c'guiSpinner b t v (fromIntegral minValue) (fromIntegral maxValue) (fromBool editMode)
                    value' <- peek v
                    return (toBool changed, fromIntegral value')
                )
          )
    )

-- | Value Box control, updates input text with numbers
guiValueBox ::
  Rectangle ->
  Maybe String ->
  -- | The current value
  Int ->
  Int ->
  Int ->
  -- | `True` if the value box should be editable, `False` otherwise
  Bool ->
  -- | A tuple, the first element is whether the value box was toggled (i.e.
  --   the edit mode should be toggled), the second element is the updated
  --   value
  IO (Bool, Int)
guiValueBox bounds text value minValue maxValue editMode =
  withFreeable
    bounds
    ( \b ->
        withMaybeCString
          text
          ( \t ->
              withFreeable
                (fromIntegral value)
                ( \v -> do
                    changed <- c'guiValueBox b t v (fromIntegral minValue) (fromIntegral maxValue) (fromBool editMode)
                    value' <- peek v
                    return (toBool changed, fromIntegral value')
                )
          )
    )

-- | Text Box control, updates input text
guiTextBox ::
  Rectangle ->
  String ->
  -- | Text box buffer size; if `Nothing`, then it will automatically allocate
  --   a buffer large enough to fit the text
  Maybe Int ->
  -- | `True` if the text box should be editable, `False` otherwise
  Bool ->
  -- | A tuple, the first element is whether the text box was toggled (i.e.
  --   the edit mode should be toggled), the second element is the updated
  --   text box value
  IO (Bool, String)
guiTextBox bounds text bufferSize editMode =
  withFreeable
    bounds
    ( \b ->
        withCStringBuffer
          text
          bufferSize
          ( \s t -> toBool <$> c'guiTextBox b t (fromIntegral s) (fromBool editMode)
          )
    )

-- | Slider control
guiSlider ::
  Rectangle ->
  Maybe String ->
  Maybe String ->
  -- | The current value
  Float ->
  Float ->
  Float ->
  -- | A tuple, the first element is whether the slider was edited, the
  --   second element is the updated value
  IO (Bool, Float)
guiSlider bounds textLeft textRight value minValue maxValue =
  withFreeable
    bounds
    ( \b ->
        withMaybeCString
          textLeft
          ( \l ->
              withMaybeCString
                textRight
                ( \r ->
                    withFreeable
                      (realToFrac value)
                      ( \v -> do
                          edited <- c'guiSlider b l r v (realToFrac minValue) (realToFrac maxValue)
                          value' <- peek v
                          return (toBool edited, realToFrac value')
                      )
                )
          )
    )

-- | Slider Bar control
guiSliderBar ::
  Rectangle ->
  Maybe String ->
  Maybe String ->
  -- | The current value
  Float ->
  Float ->
  Float ->
  -- | A tuple, the first element is whether the slider bar was edited, the
  --   second element is the updated value
  IO (Bool, Float)
guiSliderBar bounds textLeft textRight value minValue maxValue =
  withFreeable
    bounds
    ( \b ->
        withMaybeCString
          textLeft
          ( \l ->
              withMaybeCString
                textRight
                ( \r ->
                    withFreeable
                      (realToFrac value)
                      ( \v -> do
                          edited <- c'guiSliderBar b l r v (realToFrac minValue) (realToFrac maxValue)
                          value' <- peek v
                          return (toBool edited, realToFrac value')
                      )
                )
          )
    )

-- | Progress Bar control
guiProgressBar ::
  Rectangle ->
  Maybe String ->
  Maybe String ->
  -- | The current value
  Float ->
  Float ->
  Float ->
  -- | The updated value (clamped to min/max range)
  IO Float
guiProgressBar bounds textLeft textRight value minValue maxValue =
  realToFrac
    <$> withFreeable
      bounds
      ( \b ->
          withMaybeCString
            textLeft
            ( \l ->
                withMaybeCString
                  textRight
                  ( \r ->
                      withFreeable
                        (realToFrac value)
                        ( \v -> do
                            c'guiSliderBar b l r v (realToFrac minValue) (realToFrac maxValue) >> peek v
                        )
                  )
            )
      )

-- | Status Bar control, shows info text
guiStatusBar :: Rectangle -> String -> IO ()
guiStatusBar bounds text = void (withFreeable bounds (withCString text . c'guiStatusBar))

-- | Dummy control for placeholders
guiDummyRec :: Rectangle -> String -> IO ()
guiDummyRec bounds text = void (withFreeable bounds (withCString text . c'guiDummyRec))

-- | Grid control
guiGrid ::
  Rectangle ->
  Float ->
  Int ->
  -- | The cell the mouse is currently in
  IO (Maybe Vector2)
guiGrid bounds spacing subdivs =
  withFreeable
    bounds
    ( \b ->
        withFreeable
          (Vector2 (-1) (-1))
          ( \v ->
              ( \cell -> if cell == Vector2 (-1) (-1) then Nothing else Just cell
              )
                <$> ( c'guiGrid b nullPtr (realToFrac spacing) (fromIntegral subdivs) v
                        >> peek v
                    )
          )
    )

-- | List View control
guiListView ::
  Rectangle ->
  -- | The names of the list options, separated with semicolons
  String ->
  -- | Current scroll index
  Int ->
  -- | Currently selected option index (active index)
  Maybe Int ->
  -- | A tuple, the first element is the updated scroll index, the second
  --   element is the updated active index
  IO (Int, Maybe Int)
guiListView bounds text scrollIndex active =
  withFreeable
    bounds
    ( \b ->
        withCString
          text
          ( \t ->
              withFreeable
                (fromIntegral scrollIndex)
                ( \s ->
                    withFreeable
                      (fromIntegral (fromMaybe (-1) active))
                      ( \a -> do
                          _ <- c'guiListView b t s a
                          scrollIndex' <- peek s
                          active' <- peek a
                          return (fromIntegral scrollIndex', if active' == (-1) then Nothing else Just (fromIntegral active'))
                      )
                )
          )
    )

-- | List View with extended parameters
guiListViewEx ::
  Rectangle ->
  -- | The names of the list options
  [String] ->
  -- | Current scroll index
  Int ->
  -- | Currently selected option index (active index)
  Maybe Int ->
  -- | Currently focused option index
  Maybe Int ->
  -- | A tuple, the first element is the updated scroll index, the second
  --   element is the updated active index, the third element is the updated
  --   focus index
  IO (Int, Maybe Int, Maybe Int)
guiListViewEx bounds text scrollIndex active focus = do
  cStrings <- mapM newCString text
  withFreeable
    bounds
    ( \b ->
        withFreeableArrayLen
          cStrings
          ( \c t ->
              withFreeable
                (fromIntegral scrollIndex)
                ( \s ->
                    withFreeable
                      (fromIntegral (fromMaybe (-1) active))
                      ( \a ->
                          withFreeable
                            (fromIntegral (fromMaybe (-1) focus))
                            ( \f -> do
                                _ <- c'guiListViewEx b t (fromIntegral c) s a f
                                scrollIndex' <- peek s
                                active' <- peek a
                                focus' <- peek f
                                return (fromIntegral scrollIndex', if active' == (-1) then Nothing else Just (fromIntegral active'), if focus' == (-1) then Nothing else Just (fromIntegral focus'))
                            )
                      )
                )
          )
    )

-- | Message Box control, displays a message
guiMessageBox ::
  Rectangle ->
  Maybe String ->
  String ->
  -- | Button labels separated by semicolons
  String ->
  -- | The index of the clicked button, if any (0 = close message box,
  --   1,2,... = custom button)
  IO (Maybe Int)
guiMessageBox bounds title message buttons =
  withFreeable
    bounds
    ( \b ->
        withMaybeCString
          title
          ( \t ->
              withCString
                message
                ( \m ->
                    withCString
                      buttons
                      ( \bu -> do
                          res <- c'guiMessageBox b t m bu
                          if res == (-1) then return Nothing else return (Just (fromIntegral res))
                      )
                )
          )
    )

-- | Text Input Box control, ask for text, supports secret
guiTextInputBox ::
  Rectangle ->
  Maybe String ->
  String ->
  -- | Button names, separated by semicolons
  String ->
  -- | Current text box value
  String ->
  -- | Text box buffer size; if `Nothing`, then it will automatically allocate
  --   a buffer large enough to fit the text
  Maybe Int ->
  -- | Secret (password) mode; `Just True` if the value should be censored;
  --   `Just False` if it should not be censored but there should still be a
  --   button to hide it; `Nothing` if the value should not be censored at all
  Maybe Bool ->
  -- | A tuple, the first element is the updated secret mode, the second
  --   element is the updated text box value, the third element is the index
  --   of the clicked button, if any (0 = close input box, 1,2,... = custom
  --   button)
  IO (Maybe Bool, String, Maybe Int)
guiTextInputBox bounds title message buttons value bufferSize secret = do
  ((clicked, secret'), value') <-
    withFreeable
      bounds
      ( \b ->
          withMaybeCString
            title
            ( \t ->
                withCString
                  message
                  ( \m ->
                      withCString
                        buttons
                        ( \bu ->
                            withCStringBuffer
                              value
                              bufferSize
                              ( \s te ->
                                  withMaybe
                                    (fromBool <$> secret)
                                    ( \sec -> do
                                        clicked <- c'guiTextInputBox b t m bu te (fromIntegral s) sec
                                        secret' <- if sec == nullPtr then return Nothing else Just . toBool <$> peek sec
                                        return (if clicked == (-1) then Nothing else Just (fromIntegral clicked), secret')
                                    )
                              )
                        )
                  )
            )
      )
  return (secret', value', clicked)

-- | Color Picker control (multiple color controls)
guiColorPicker ::
  Rectangle ->
  -- | Currently selected color, use `Nothing` if creating the color picker for
  --   the first time
  Maybe Color ->
  -- | Updated color
  IO Color
guiColorPicker bounds color =
  withFreeable
    bounds
    ( \b ->
        withFreeable
          (fromMaybe (Color 200 0 0 255) color)
          ( \c -> c'guiColorPicker b nullPtr c >> peek c
          )
    )

-- | Color Panel control
guiColorPanel ::
  Rectangle ->
  -- | Currently selected color, use `Nothing` if creating the color panel for
  --   the first time
  Maybe Color ->
  -- | Updated color
  IO Color
guiColorPanel bounds color =
  withFreeable
    bounds
    ( \b ->
        withFreeable
          (fromMaybe (Color 200 0 0 255) color)
          ( \c -> c'guiColorPanel b nullPtr c >> peek c
          )
    )

-- | Color Bar Alpha control
guiColorBarAlpha ::
  Rectangle ->
  -- | Currently selected alpha
  Float ->
  -- | Updated alpha
  IO Float
guiColorBarAlpha bounds alpha =
  realToFrac
    <$> withFreeable
      bounds
      ( \b ->
          withFreeable
            (realToFrac alpha)
            ( \a -> c'guiColorBarAlpha b nullPtr a >> peek a
            )
      )

-- | Color Bar Hue control
guiColorBarHue ::
  Rectangle ->
  -- | Currently selected hue
  Float ->
  -- | Updated hue
  IO Float
guiColorBarHue bounds hue =
  realToFrac
    <$> withFreeable
      bounds
      ( \b ->
          withFreeable
            (realToFrac hue)
            ( \h -> c'guiColorBarHue b nullPtr h >> peek h
            )
      )

-- | Color Picker control that avoids conversion to RGB on each call (multiple color controls)
guiColorPickerHSV ::
  Rectangle ->
  -- | Currently selected color, use `Nothing` if creating the color picker for
  --   the first time
  Maybe Vector3 ->
  -- | Updated color
  IO Vector3
guiColorPickerHSV bounds color =
  withFreeable
    bounds
    ( \b ->
        withFreeable
          (fromMaybe (Vector3 (200.0 / 255.0) 0 0) color)
          ( \c -> c'guiColorPickerHSV b nullPtr c >> peek c
          )
    )

-- | Color Panel control that updates Hue-Saturation-Value color value, used by guiColorPickerHSV
guiColorPanelHSV ::
  Rectangle ->
  -- | Currently selected color, use `Nothing` if creating the color panel for
  --   the first time
  Maybe Vector3 ->
  -- | Updated color
  IO Vector3
guiColorPanelHSV bounds color =
  withFreeable
    bounds
    ( \b ->
        withFreeable
          (fromMaybe (Vector3 (200.0 / 255.0) 0 0) color)
          ( \c -> c'guiColorPanelHSV b nullPtr c >> peek c
          )
    )
