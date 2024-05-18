{-# LANGUAGE DeriveAnyClass #-}

-- | Bindings for types used in @raygui@
module Raylib.Types.Util.GUI
  ( -- * Enumerations
    GuiState (..),
    GuiTextAlignment (..),
    GuiTextAlignmentVertical (..),
    GuiTextWrapMode (..),
    GuiControl (..),
    GuiControlProperty (..),
    GuiDefaultProperty (..),
    GuiToggleProperty (..),
    GuiSliderProperty (..),
    GuiProgressBarProperty (..),
    GuiScrollBarProperty (..),
    GuiCheckBoxProperty (..),
    GuiComboBoxProperty (..),
    GuiDropdownBoxProperty (..),
    GuiTextBoxProperty (..),
    GuiSpinnerProperty (..),
    GuiListViewProperty (..),
    GuiColorPickerProperty (..),
    GuiIconName (..),

    -- * Structures
    GuiStyleProp (..),

    -- * Pointer utilities
    p'guiStyleProp'controlId,
    p'guiStyleProp'propertyId,
    p'guiStyleProp'propertyValue,
  )
where

import Foreign
  ( Ptr,
    Storable (alignment, peek, poke, sizeOf),
    Word16,
    castPtr,
    plusPtr,
  )
import Foreign.C
  ( CInt (..),
    CUShort,
  )
import Raylib.Internal.Foreign (Freeable)

---------------------------------------
-- raygui enums -----------------------
---------------------------------------

-- | Gui control state
data GuiState
  = StateNormal
  | StateFocused
  | StatePressed
  | StateDisabled
  deriving (Eq, Show)

instance Enum GuiState where
  fromEnum x = case x of
    StateNormal -> 0
    StateFocused -> 1
    StatePressed -> 2
    StateDisabled -> 3
  toEnum x = case x of
    0 -> StateNormal
    1 -> StateFocused
    2 -> StatePressed
    3 -> StateDisabled
    n -> error $ "(GuiState.toEnum) Invalid value: " ++ show n

instance Storable GuiState where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Gui control text alignment
data GuiTextAlignment
  = TextAlignLeft
  | TextAlignCenter
  | TextAlignRight
  deriving (Eq, Show)

instance Enum GuiTextAlignment where
  fromEnum x = case x of
    TextAlignLeft -> 0
    TextAlignCenter -> 1
    TextAlignRight -> 2
  toEnum x = case x of
    0 -> TextAlignLeft
    1 -> TextAlignCenter
    2 -> TextAlignRight
    n -> error $ "(GuiTextAlignment.toEnum) Invalid value: " ++ show n

instance Storable GuiTextAlignment where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Gui control text alignment vertical
data GuiTextAlignmentVertical
  = TextAlignTop
  | TextAlignMiddle
  | TextAlignBottom
  deriving (Eq, Show)

instance Enum GuiTextAlignmentVertical where
  fromEnum x = case x of
    TextAlignTop -> 0
    TextAlignMiddle -> 1
    TextAlignBottom -> 2
  toEnum x = case x of
    0 -> TextAlignTop
    1 -> TextAlignMiddle
    2 -> TextAlignBottom
    n -> error $ "(GuiTextAlignmentVertical.toEnum) Invalid value: " ++ show n

instance Storable GuiTextAlignmentVertical where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Gui control text wrap mode
data GuiTextWrapMode
  = TextWrapNone
  | TextWrapChar
  | TextWrapWord
  deriving (Eq, Show)

instance Enum GuiTextWrapMode where
  fromEnum x = case x of
    TextWrapNone -> 0
    TextWrapChar -> 1
    TextWrapWord -> 2
  toEnum x = case x of
    0 -> TextWrapNone
    1 -> TextWrapChar
    2 -> TextWrapWord
    n -> error $ "(GuiTextWrapMode.toEnum) Invalid value: " ++ show n

instance Storable GuiTextWrapMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Gui controls
data GuiControl
  = Default
  | -- | Used also for: LABELBUTTON
    Label
  | Button
  | -- | Used also for: TOGGLEGROUP
    Toggle
  | -- | Used also for: SLIDERBAR, TOGGLESLIDER
    Slider
  | Progressbar
  | Checkbox
  | Combobox
  | Dropdownbox
  | -- | Used also for: TEXTBOXMULTI
    Textbox
  | Valuebox
  | -- | Uses: BUTTON, VALUEBOX
    Spinner
  | Listview
  | Colorpicker
  | Scrollbar
  | Statusbar
  deriving (Eq, Show)

instance Enum GuiControl where
  fromEnum x = case x of
    Default -> 0
    Label -> 1
    Button -> 2
    Toggle -> 3
    Slider -> 4
    Progressbar -> 5
    Checkbox -> 6
    Combobox -> 7
    Dropdownbox -> 8
    Textbox -> 9
    Valuebox -> 10
    Spinner -> 11
    Listview -> 12
    Colorpicker -> 13
    Scrollbar -> 14
    Statusbar -> 15
  toEnum x = case x of
    0 -> Default
    1 -> Label
    2 -> Button
    3 -> Toggle
    4 -> Slider
    5 -> Progressbar
    6 -> Checkbox
    7 -> Combobox
    8 -> Dropdownbox
    9 -> Textbox
    10 -> Valuebox
    11 -> Spinner
    12 -> Listview
    13 -> Colorpicker
    14 -> Scrollbar
    15 -> Statusbar
    n -> error $ "(GuiControl.toEnum) Invalid value: " ++ show n

instance Storable GuiControl where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Gui base properties for every control
data GuiControlProperty
  = -- | Control border color in STATE_NORMAL
    BorderColorNormal
  | -- | Control base color in STATE_NORMAL
    BaseColorNormal
  | -- | Control text color in STATE_NORMAL
    TextColorNormal
  | -- | Control border color in STATE_FOCUSED
    BorderColorFocused
  | -- | Control base color in STATE_FOCUSED
    BaseColorFocused
  | -- | Control text color in STATE_FOCUSED
    TextColorFocused
  | -- | Control border color in STATE_PRESSED
    BorderColorPressed
  | -- | Control base color in STATE_PRESSED
    BaseColorPressed
  | -- | Control text color in STATE_PRESSED
    TextColorPressed
  | -- | Control border color in STATE_DISABLED
    BorderColorDisabled
  | -- | Control base color in STATE_DISABLED
    BaseColorDisabled
  | -- | Control text color in STATE_DISABLED
    TextColorDisabled
  | -- | Control border size, 0 for no border
    BorderWidth
  | -- | Control text padding, not considering border
    TextPadding
  | -- | Control text horizontal alignment inside control text bound (after border and padding)
    TextAlignment
  deriving (Eq, Show)

instance Enum GuiControlProperty where
  fromEnum x = case x of
    BorderColorNormal -> 0
    BaseColorNormal -> 1
    TextColorNormal -> 2
    BorderColorFocused -> 3
    BaseColorFocused -> 4
    TextColorFocused -> 5
    BorderColorPressed -> 6
    BaseColorPressed -> 7
    TextColorPressed -> 8
    BorderColorDisabled -> 9
    BaseColorDisabled -> 10
    TextColorDisabled -> 11
    BorderWidth -> 12
    TextPadding -> 13
    TextAlignment -> 14
  toEnum x = case x of
    0 -> BorderColorNormal
    1 -> BaseColorNormal
    2 -> TextColorNormal
    3 -> BorderColorFocused
    4 -> BaseColorFocused
    5 -> TextColorFocused
    6 -> BorderColorPressed
    7 -> BaseColorPressed
    8 -> TextColorPressed
    9 -> BorderColorDisabled
    10 -> BaseColorDisabled
    11 -> TextColorDisabled
    12 -> BorderWidth
    13 -> TextPadding
    14 -> TextAlignment
    n -> error $ "(GuiControlProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiControlProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | DEFAULT extended properties
data GuiDefaultProperty
  = -- | Text size (glyphs max height)
    TextSize
  | -- | Text spacing between glyphs
    TextSpacing
  | -- | Line control color
    LineColor
  | -- | Background color
    BackgroundColor
  | -- | Text spacing between lines
    TextLineSpacing
  | -- | Text vertical alignment inside text bounds (after border and padding)
    TextAlignmentVertical
  | -- | Text wrap-mode inside text bounds
    TextWrapMode
  deriving (Eq, Show)

instance Enum GuiDefaultProperty where
  fromEnum x = case x of
    TextSize -> 16
    TextSpacing -> 17
    LineColor -> 18
    BackgroundColor -> 19
    TextLineSpacing -> 20
    TextAlignmentVertical -> 21
    TextWrapMode -> 22
  toEnum x = case x of
    16 -> TextSize
    17 -> TextSpacing
    18 -> LineColor
    19 -> BackgroundColor
    20 -> TextLineSpacing
    21 -> TextAlignmentVertical
    22 -> TextWrapMode
    n -> error $ "(GuiDefaultProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiDefaultProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Toggle/ToggleGroup
data GuiToggleProperty
  = -- | ToggleGroup separation between toggles
    GroupPadding
  deriving (Eq, Show)

instance Enum GuiToggleProperty where
  fromEnum x = case x of
    GroupPadding -> 16
  toEnum x = case x of
    16 -> GroupPadding
    n -> error $ "(GuiToggleProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiToggleProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Slider/SliderBar
data GuiSliderProperty
  = -- | Slider size of internal bar
    SliderWidth
  | -- | Slider/SliderBar internal bar padding
    SliderPadding
  deriving (Eq, Show)

instance Enum GuiSliderProperty where
  fromEnum x = case x of
    SliderWidth -> 16
    SliderPadding -> 17
  toEnum x = case x of
    16 -> SliderWidth
    17 -> SliderPadding
    n -> error $ "(GuiSliderProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiSliderProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | ProgressBar
data GuiProgressBarProperty
  = -- | ProgressBar internal padding
    ProgressPadding
  deriving (Eq, Show)

instance Enum GuiProgressBarProperty where
  fromEnum x = case x of
    ProgressPadding -> 16
  toEnum x = case x of
    16 -> ProgressPadding
    n -> error $ "(GuiProgressBarProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiProgressBarProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | ScrollBar
data GuiScrollBarProperty
  = -- | ScrollBar arrows size
    ArrowsSize
  | -- | ScrollBar arrows visible
    ArrowsVisible
  | -- | ScrollBar slider internal padding
    ScrollSliderPadding
  | -- | ScrollBar slider size
    ScrollSliderSize
  | -- | ScrollBar scroll padding from arrows
    ScrollPadding
  | -- | ScrollBar scrolling speed
    ScrollSpeed
  deriving (Eq, Show)

instance Enum GuiScrollBarProperty where
  fromEnum x = case x of
    ArrowsSize -> 16
    ArrowsVisible -> 17
    ScrollSliderPadding -> 18
    ScrollSliderSize -> 19
    ScrollPadding -> 20
    ScrollSpeed -> 21
  toEnum x = case x of
    16 -> ArrowsSize
    17 -> ArrowsVisible
    18 -> ScrollSliderPadding
    19 -> ScrollSliderSize
    20 -> ScrollPadding
    21 -> ScrollSpeed
    n -> error $ "(GuiScrollBarProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiScrollBarProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | CheckBox
data GuiCheckBoxProperty
  = -- | CheckBox internal check padding
    CheckPadding
  deriving (Eq, Show)

instance Enum GuiCheckBoxProperty where
  fromEnum x = case x of
    CheckPadding -> 16
  toEnum x = case x of
    16 -> CheckPadding
    n -> error $ "(GuiCheckBoxProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiCheckBoxProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | ComboBox
data GuiComboBoxProperty
  = -- | ComboBox right button width
    ComboButtonWidth
  | -- | ComboBox button separation
    ComboButtonSpacing
  deriving (Eq, Show)

instance Enum GuiComboBoxProperty where
  fromEnum x = case x of
    ComboButtonWidth -> 16
    ComboButtonSpacing -> 17
  toEnum x = case x of
    16 -> ComboButtonWidth
    17 -> ComboButtonSpacing
    n -> error $ "(GuiComboBoxProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiComboBoxProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | DropdownBox
data GuiDropdownBoxProperty
  = -- | DropdownBox arrow separation from border and items
    ArrowPadding
  | -- | DropdownBox items separation
    DropdownItemsSpacing
  deriving (Eq, Show)

instance Enum GuiDropdownBoxProperty where
  fromEnum x = case x of
    ArrowPadding -> 16
    DropdownItemsSpacing -> 17
  toEnum x = case x of
    16 -> ArrowPadding
    17 -> DropdownItemsSpacing
    n -> error $ "(GuiDropdownBoxProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiDropdownBoxProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | TextBox/TextBoxMulti/ValueBox/Spinner
data GuiTextBoxProperty
  = -- | TextBox in read-only mode: 0-text editable, 1-text no-editable
    TextReadonly
  deriving (Eq, Show)

instance Enum GuiTextBoxProperty where
  fromEnum x = case x of
    TextReadonly -> 16
  toEnum x = case x of
    16 -> TextReadonly
    n -> error $ "(GuiTextBoxProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiTextBoxProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Spinner
data GuiSpinnerProperty
  = -- | Spinner left/right buttons width
    SpinButtonWidth
  | -- | Spinner buttons separation
    SpinButtonSpacing
  deriving (Eq, Show)

instance Enum GuiSpinnerProperty where
  fromEnum x = case x of
    SpinButtonWidth -> 16
    SpinButtonSpacing -> 17
  toEnum x = case x of
    16 -> SpinButtonWidth
    17 -> SpinButtonSpacing
    n -> error $ "(GuiSpinnerProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiSpinnerProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | ListView
data GuiListViewProperty
  = -- | ListView items height
    ListItemsHeight
  | -- | ListView items separation
    ListItemsSpacing
  | -- | ListView scrollbar size (usually width)
    ScrollbarWidth
  | -- | ListView scrollbar side (0-SCROLLBAR_LEFT_SIDE, 1-SCROLLBAR_RIGHT_SIDE)
    ScrollbarSide
  deriving (Eq, Show)

instance Enum GuiListViewProperty where
  fromEnum x = case x of
    ListItemsHeight -> 16
    ListItemsSpacing -> 17
    ScrollbarWidth -> 18
    ScrollbarSide -> 19
  toEnum x = case x of
    16 -> ListItemsHeight
    17 -> ListItemsSpacing
    18 -> ScrollbarWidth
    19 -> ScrollbarSide
    n -> error $ "(GuiListViewProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiListViewProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | ColorPicker
data GuiColorPickerProperty
  = ColorSelectorSize
  | -- | ColorPicker right hue bar width
    HuebarWidth
  | -- | ColorPicker right hue bar separation from panel
    HuebarPadding
  | -- | ColorPicker right hue bar selector height
    HuebarSelectorHeight
  | -- | ColorPicker right hue bar selector overflow
    HuebarSelectorOverflow
  deriving (Eq, Show)

instance Enum GuiColorPickerProperty where
  fromEnum x = case x of
    ColorSelectorSize -> 16
    HuebarWidth -> 17
    HuebarPadding -> 18
    HuebarSelectorHeight -> 19
    HuebarSelectorOverflow -> 20
  toEnum x = case x of
    16 -> ColorSelectorSize
    17 -> HuebarWidth
    18 -> HuebarPadding
    19 -> HuebarSelectorHeight
    20 -> HuebarSelectorOverflow
    n -> error $ "(GuiColorPickerProperty.toEnum) Invalid value: " ++ show n

instance Storable GuiColorPickerProperty where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

data GuiIconName
  = IconNone
  | IconFolderFileOpen
  | IconFileSaveClassic
  | IconFolderOpen
  | IconFolderSave
  | IconFileOpen
  | IconFileSave
  | IconFileExport
  | IconFileAdd
  | IconFileDelete
  | IconFiletypeText
  | IconFiletypeAudio
  | IconFiletypeImage
  | IconFiletypePlay
  | IconFiletypeVideo
  | IconFiletypeInfo
  | IconFileCopy
  | IconFileCut
  | IconFilePaste
  | IconCursorHand
  | IconCursorPointer
  | IconCursorClassic
  | IconPencil
  | IconPencilBig
  | IconBrushClassic
  | IconBrushPainter
  | IconWaterDrop
  | IconColorPicker
  | IconRubber
  | IconColorBucket
  | IconTextT
  | IconTextA
  | IconScale
  | IconResize
  | IconFilterPoint
  | IconFilterBilinear
  | IconCrop
  | IconCropAlpha
  | IconSquareToggle
  | IconSymmetry
  | IconSymmetryHorizontal
  | IconSymmetryVertical
  | IconLens
  | IconLensBig
  | IconEyeOn
  | IconEyeOff
  | IconFilterTop
  | IconFilter
  | IconTargetPoint
  | IconTargetSmall
  | IconTargetBig
  | IconTargetMove
  | IconCursorMove
  | IconCursorScale
  | IconCursorScaleRight
  | IconCursorScaleLeft
  | IconUndo
  | IconRedo
  | IconReredo
  | IconMutate
  | IconRotate
  | IconRepeat
  | IconShuffle
  | IconEmptybox
  | IconTarget
  | IconTargetSmallFill
  | IconTargetBigFill
  | IconTargetMoveFill
  | IconCursorMoveFill
  | IconCursorScaleFill
  | IconCursorScaleRightFill
  | IconCursorScaleLeftFill
  | IconUndoFill
  | IconRedoFill
  | IconReredoFill
  | IconMutateFill
  | IconRotateFill
  | IconRepeatFill
  | IconShuffleFill
  | IconEmptyboxSmall
  | IconBox
  | IconBoxTop
  | IconBoxTopRight
  | IconBoxRight
  | IconBoxBottomRight
  | IconBoxBottom
  | IconBoxBottomLeft
  | IconBoxLeft
  | IconBoxTopLeft
  | IconBoxCenter
  | IconBoxCircleMask
  | IconPot
  | IconAlphaMultiply
  | IconAlphaClear
  | IconDithering
  | IconMipmaps
  | IconBoxGrid
  | IconGrid
  | IconBoxCornersSmall
  | IconBoxCornersBig
  | IconFourBoxes
  | IconGridFill
  | IconBoxMultisize
  | IconZoomSmall
  | IconZoomMedium
  | IconZoomBig
  | IconZoomAll
  | IconZoomCenter
  | IconBoxDotsSmall
  | IconBoxDotsBig
  | IconBoxConcentric
  | IconBoxGridBig
  | IconOkTick
  | IconCross
  | IconArrowLeft
  | IconArrowRight
  | IconArrowDown
  | IconArrowUp
  | IconArrowLeftFill
  | IconArrowRightFill
  | IconArrowDownFill
  | IconArrowUpFill
  | IconAudio
  | IconFx
  | IconWave
  | IconWaveSinus
  | IconWaveSquare
  | IconWaveTriangular
  | IconCrossSmall
  | IconPlayerPrevious
  | IconPlayerPlayBack
  | IconPlayerPlay
  | IconPlayerPause
  | IconPlayerStop
  | IconPlayerNext
  | IconPlayerRecord
  | IconMagnet
  | IconLockClose
  | IconLockOpen
  | IconClock
  | IconTools
  | IconGear
  | IconGearBig
  | IconBin
  | IconHandPointer
  | IconLaser
  | IconCoin
  | IconExplosion
  | Icon1up
  | IconPlayer
  | IconPlayerJump
  | IconKey
  | IconDemon
  | IconTextPopup
  | IconGearEx
  | IconCrack
  | IconCrackPoints
  | IconStar
  | IconDoor
  | IconExit
  | IconMode2d
  | IconMode3d
  | IconCube
  | IconCubeFaceTop
  | IconCubeFaceLeft
  | IconCubeFaceFront
  | IconCubeFaceBottom
  | IconCubeFaceRight
  | IconCubeFaceBack
  | IconCamera
  | IconSpecial
  | IconLinkNet
  | IconLinkBoxes
  | IconLinkMulti
  | IconLink
  | IconLinkBroke
  | IconTextNotes
  | IconNotebook
  | IconSuitcase
  | IconSuitcaseZip
  | IconMailbox
  | IconMonitor
  | IconPrinter
  | IconPhotoCamera
  | IconPhotoCameraFlash
  | IconHouse
  | IconHeart
  | IconCorner
  | IconVerticalBars
  | IconVerticalBarsFill
  | IconLifeBars
  | IconInfo
  | IconCrossline
  | IconHelp
  | IconFiletypeAlpha
  | IconFiletypeHome
  | IconLayersVisible
  | IconLayers
  | IconWindow
  | IconHidpi
  | IconFiletypeBinary
  | IconHex
  | IconShield
  | IconFileNew
  | IconFolderAdd
  | IconAlarm
  | IconCpu
  | IconRom
  | IconStepOver
  | IconStepInto
  | IconStepOut
  | IconRestart
  | IconBreakpointOn
  | IconBreakpointOff
  | IconBurgerMenu
  | IconCaseSensitive
  | IconRegExp
  | IconFolder
  | IconFile
  | IconSandTimer
  | IconWarning
  | IconHelpBox
  | IconInfoBox
  | Icon223
  | Icon224
  | Icon225
  | Icon226
  | Icon227
  | Icon228
  | Icon229
  | Icon230
  | Icon231
  | Icon232
  | Icon233
  | Icon234
  | Icon235
  | Icon236
  | Icon237
  | Icon238
  | Icon239
  | Icon240
  | Icon241
  | Icon242
  | Icon243
  | Icon244
  | Icon245
  | Icon246
  | Icon247
  | Icon248
  | Icon249
  | Icon250
  | Icon251
  | Icon252
  | Icon253
  | Icon254
  | Icon255
  deriving (Eq, Show)

instance Enum GuiIconName where
  fromEnum x = case x of
    IconNone -> 0
    IconFolderFileOpen -> 1
    IconFileSaveClassic -> 2
    IconFolderOpen -> 3
    IconFolderSave -> 4
    IconFileOpen -> 5
    IconFileSave -> 6
    IconFileExport -> 7
    IconFileAdd -> 8
    IconFileDelete -> 9
    IconFiletypeText -> 10
    IconFiletypeAudio -> 11
    IconFiletypeImage -> 12
    IconFiletypePlay -> 13
    IconFiletypeVideo -> 14
    IconFiletypeInfo -> 15
    IconFileCopy -> 16
    IconFileCut -> 17
    IconFilePaste -> 18
    IconCursorHand -> 19
    IconCursorPointer -> 20
    IconCursorClassic -> 21
    IconPencil -> 22
    IconPencilBig -> 23
    IconBrushClassic -> 24
    IconBrushPainter -> 25
    IconWaterDrop -> 26
    IconColorPicker -> 27
    IconRubber -> 28
    IconColorBucket -> 29
    IconTextT -> 30
    IconTextA -> 31
    IconScale -> 32
    IconResize -> 33
    IconFilterPoint -> 34
    IconFilterBilinear -> 35
    IconCrop -> 36
    IconCropAlpha -> 37
    IconSquareToggle -> 38
    IconSymmetry -> 39
    IconSymmetryHorizontal -> 40
    IconSymmetryVertical -> 41
    IconLens -> 42
    IconLensBig -> 43
    IconEyeOn -> 44
    IconEyeOff -> 45
    IconFilterTop -> 46
    IconFilter -> 47
    IconTargetPoint -> 48
    IconTargetSmall -> 49
    IconTargetBig -> 50
    IconTargetMove -> 51
    IconCursorMove -> 52
    IconCursorScale -> 53
    IconCursorScaleRight -> 54
    IconCursorScaleLeft -> 55
    IconUndo -> 56
    IconRedo -> 57
    IconReredo -> 58
    IconMutate -> 59
    IconRotate -> 60
    IconRepeat -> 61
    IconShuffle -> 62
    IconEmptybox -> 63
    IconTarget -> 64
    IconTargetSmallFill -> 65
    IconTargetBigFill -> 66
    IconTargetMoveFill -> 67
    IconCursorMoveFill -> 68
    IconCursorScaleFill -> 69
    IconCursorScaleRightFill -> 70
    IconCursorScaleLeftFill -> 71
    IconUndoFill -> 72
    IconRedoFill -> 73
    IconReredoFill -> 74
    IconMutateFill -> 75
    IconRotateFill -> 76
    IconRepeatFill -> 77
    IconShuffleFill -> 78
    IconEmptyboxSmall -> 79
    IconBox -> 80
    IconBoxTop -> 81
    IconBoxTopRight -> 82
    IconBoxRight -> 83
    IconBoxBottomRight -> 84
    IconBoxBottom -> 85
    IconBoxBottomLeft -> 86
    IconBoxLeft -> 87
    IconBoxTopLeft -> 88
    IconBoxCenter -> 89
    IconBoxCircleMask -> 90
    IconPot -> 91
    IconAlphaMultiply -> 92
    IconAlphaClear -> 93
    IconDithering -> 94
    IconMipmaps -> 95
    IconBoxGrid -> 96
    IconGrid -> 97
    IconBoxCornersSmall -> 98
    IconBoxCornersBig -> 99
    IconFourBoxes -> 100
    IconGridFill -> 101
    IconBoxMultisize -> 102
    IconZoomSmall -> 103
    IconZoomMedium -> 104
    IconZoomBig -> 105
    IconZoomAll -> 106
    IconZoomCenter -> 107
    IconBoxDotsSmall -> 108
    IconBoxDotsBig -> 109
    IconBoxConcentric -> 110
    IconBoxGridBig -> 111
    IconOkTick -> 112
    IconCross -> 113
    IconArrowLeft -> 114
    IconArrowRight -> 115
    IconArrowDown -> 116
    IconArrowUp -> 117
    IconArrowLeftFill -> 118
    IconArrowRightFill -> 119
    IconArrowDownFill -> 120
    IconArrowUpFill -> 121
    IconAudio -> 122
    IconFx -> 123
    IconWave -> 124
    IconWaveSinus -> 125
    IconWaveSquare -> 126
    IconWaveTriangular -> 127
    IconCrossSmall -> 128
    IconPlayerPrevious -> 129
    IconPlayerPlayBack -> 130
    IconPlayerPlay -> 131
    IconPlayerPause -> 132
    IconPlayerStop -> 133
    IconPlayerNext -> 134
    IconPlayerRecord -> 135
    IconMagnet -> 136
    IconLockClose -> 137
    IconLockOpen -> 138
    IconClock -> 139
    IconTools -> 140
    IconGear -> 141
    IconGearBig -> 142
    IconBin -> 143
    IconHandPointer -> 144
    IconLaser -> 145
    IconCoin -> 146
    IconExplosion -> 147
    Icon1up -> 148
    IconPlayer -> 149
    IconPlayerJump -> 150
    IconKey -> 151
    IconDemon -> 152
    IconTextPopup -> 153
    IconGearEx -> 154
    IconCrack -> 155
    IconCrackPoints -> 156
    IconStar -> 157
    IconDoor -> 158
    IconExit -> 159
    IconMode2d -> 160
    IconMode3d -> 161
    IconCube -> 162
    IconCubeFaceTop -> 163
    IconCubeFaceLeft -> 164
    IconCubeFaceFront -> 165
    IconCubeFaceBottom -> 166
    IconCubeFaceRight -> 167
    IconCubeFaceBack -> 168
    IconCamera -> 169
    IconSpecial -> 170
    IconLinkNet -> 171
    IconLinkBoxes -> 172
    IconLinkMulti -> 173
    IconLink -> 174
    IconLinkBroke -> 175
    IconTextNotes -> 176
    IconNotebook -> 177
    IconSuitcase -> 178
    IconSuitcaseZip -> 179
    IconMailbox -> 180
    IconMonitor -> 181
    IconPrinter -> 182
    IconPhotoCamera -> 183
    IconPhotoCameraFlash -> 184
    IconHouse -> 185
    IconHeart -> 186
    IconCorner -> 187
    IconVerticalBars -> 188
    IconVerticalBarsFill -> 189
    IconLifeBars -> 190
    IconInfo -> 191
    IconCrossline -> 192
    IconHelp -> 193
    IconFiletypeAlpha -> 194
    IconFiletypeHome -> 195
    IconLayersVisible -> 196
    IconLayers -> 197
    IconWindow -> 198
    IconHidpi -> 199
    IconFiletypeBinary -> 200
    IconHex -> 201
    IconShield -> 202
    IconFileNew -> 203
    IconFolderAdd -> 204
    IconAlarm -> 205
    IconCpu -> 206
    IconRom -> 207
    IconStepOver -> 208
    IconStepInto -> 209
    IconStepOut -> 210
    IconRestart -> 211
    IconBreakpointOn -> 212
    IconBreakpointOff -> 213
    IconBurgerMenu -> 214
    IconCaseSensitive -> 215
    IconRegExp -> 216
    IconFolder -> 217
    IconFile -> 218
    IconSandTimer -> 219
    IconWarning -> 220
    IconHelpBox -> 221
    IconInfoBox -> 222
    Icon223 -> 223
    Icon224 -> 224
    Icon225 -> 225
    Icon226 -> 226
    Icon227 -> 227
    Icon228 -> 228
    Icon229 -> 229
    Icon230 -> 230
    Icon231 -> 231
    Icon232 -> 232
    Icon233 -> 233
    Icon234 -> 234
    Icon235 -> 235
    Icon236 -> 236
    Icon237 -> 237
    Icon238 -> 238
    Icon239 -> 239
    Icon240 -> 240
    Icon241 -> 241
    Icon242 -> 242
    Icon243 -> 243
    Icon244 -> 244
    Icon245 -> 245
    Icon246 -> 246
    Icon247 -> 247
    Icon248 -> 248
    Icon249 -> 249
    Icon250 -> 250
    Icon251 -> 251
    Icon252 -> 252
    Icon253 -> 253
    Icon254 -> 254
    Icon255 -> 255
  toEnum x = case x of
    0 -> IconNone
    1 -> IconFolderFileOpen
    2 -> IconFileSaveClassic
    3 -> IconFolderOpen
    4 -> IconFolderSave
    5 -> IconFileOpen
    6 -> IconFileSave
    7 -> IconFileExport
    8 -> IconFileAdd
    9 -> IconFileDelete
    10 -> IconFiletypeText
    11 -> IconFiletypeAudio
    12 -> IconFiletypeImage
    13 -> IconFiletypePlay
    14 -> IconFiletypeVideo
    15 -> IconFiletypeInfo
    16 -> IconFileCopy
    17 -> IconFileCut
    18 -> IconFilePaste
    19 -> IconCursorHand
    20 -> IconCursorPointer
    21 -> IconCursorClassic
    22 -> IconPencil
    23 -> IconPencilBig
    24 -> IconBrushClassic
    25 -> IconBrushPainter
    26 -> IconWaterDrop
    27 -> IconColorPicker
    28 -> IconRubber
    29 -> IconColorBucket
    30 -> IconTextT
    31 -> IconTextA
    32 -> IconScale
    33 -> IconResize
    34 -> IconFilterPoint
    35 -> IconFilterBilinear
    36 -> IconCrop
    37 -> IconCropAlpha
    38 -> IconSquareToggle
    39 -> IconSymmetry
    40 -> IconSymmetryHorizontal
    41 -> IconSymmetryVertical
    42 -> IconLens
    43 -> IconLensBig
    44 -> IconEyeOn
    45 -> IconEyeOff
    46 -> IconFilterTop
    47 -> IconFilter
    48 -> IconTargetPoint
    49 -> IconTargetSmall
    50 -> IconTargetBig
    51 -> IconTargetMove
    52 -> IconCursorMove
    53 -> IconCursorScale
    54 -> IconCursorScaleRight
    55 -> IconCursorScaleLeft
    56 -> IconUndo
    57 -> IconRedo
    58 -> IconReredo
    59 -> IconMutate
    60 -> IconRotate
    61 -> IconRepeat
    62 -> IconShuffle
    63 -> IconEmptybox
    64 -> IconTarget
    65 -> IconTargetSmallFill
    66 -> IconTargetBigFill
    67 -> IconTargetMoveFill
    68 -> IconCursorMoveFill
    69 -> IconCursorScaleFill
    70 -> IconCursorScaleRightFill
    71 -> IconCursorScaleLeftFill
    72 -> IconUndoFill
    73 -> IconRedoFill
    74 -> IconReredoFill
    75 -> IconMutateFill
    76 -> IconRotateFill
    77 -> IconRepeatFill
    78 -> IconShuffleFill
    79 -> IconEmptyboxSmall
    80 -> IconBox
    81 -> IconBoxTop
    82 -> IconBoxTopRight
    83 -> IconBoxRight
    84 -> IconBoxBottomRight
    85 -> IconBoxBottom
    86 -> IconBoxBottomLeft
    87 -> IconBoxLeft
    88 -> IconBoxTopLeft
    89 -> IconBoxCenter
    90 -> IconBoxCircleMask
    91 -> IconPot
    92 -> IconAlphaMultiply
    93 -> IconAlphaClear
    94 -> IconDithering
    95 -> IconMipmaps
    96 -> IconBoxGrid
    97 -> IconGrid
    98 -> IconBoxCornersSmall
    99 -> IconBoxCornersBig
    100 -> IconFourBoxes
    101 -> IconGridFill
    102 -> IconBoxMultisize
    103 -> IconZoomSmall
    104 -> IconZoomMedium
    105 -> IconZoomBig
    106 -> IconZoomAll
    107 -> IconZoomCenter
    108 -> IconBoxDotsSmall
    109 -> IconBoxDotsBig
    110 -> IconBoxConcentric
    111 -> IconBoxGridBig
    112 -> IconOkTick
    113 -> IconCross
    114 -> IconArrowLeft
    115 -> IconArrowRight
    116 -> IconArrowDown
    117 -> IconArrowUp
    118 -> IconArrowLeftFill
    119 -> IconArrowRightFill
    120 -> IconArrowDownFill
    121 -> IconArrowUpFill
    122 -> IconAudio
    123 -> IconFx
    124 -> IconWave
    125 -> IconWaveSinus
    126 -> IconWaveSquare
    127 -> IconWaveTriangular
    128 -> IconCrossSmall
    129 -> IconPlayerPrevious
    130 -> IconPlayerPlayBack
    131 -> IconPlayerPlay
    132 -> IconPlayerPause
    133 -> IconPlayerStop
    134 -> IconPlayerNext
    135 -> IconPlayerRecord
    136 -> IconMagnet
    137 -> IconLockClose
    138 -> IconLockOpen
    139 -> IconClock
    140 -> IconTools
    141 -> IconGear
    142 -> IconGearBig
    143 -> IconBin
    144 -> IconHandPointer
    145 -> IconLaser
    146 -> IconCoin
    147 -> IconExplosion
    148 -> Icon1up
    149 -> IconPlayer
    150 -> IconPlayerJump
    151 -> IconKey
    152 -> IconDemon
    153 -> IconTextPopup
    154 -> IconGearEx
    155 -> IconCrack
    156 -> IconCrackPoints
    157 -> IconStar
    158 -> IconDoor
    159 -> IconExit
    160 -> IconMode2d
    161 -> IconMode3d
    162 -> IconCube
    163 -> IconCubeFaceTop
    164 -> IconCubeFaceLeft
    165 -> IconCubeFaceFront
    166 -> IconCubeFaceBottom
    167 -> IconCubeFaceRight
    168 -> IconCubeFaceBack
    169 -> IconCamera
    170 -> IconSpecial
    171 -> IconLinkNet
    172 -> IconLinkBoxes
    173 -> IconLinkMulti
    174 -> IconLink
    175 -> IconLinkBroke
    176 -> IconTextNotes
    177 -> IconNotebook
    178 -> IconSuitcase
    179 -> IconSuitcaseZip
    180 -> IconMailbox
    181 -> IconMonitor
    182 -> IconPrinter
    183 -> IconPhotoCamera
    184 -> IconPhotoCameraFlash
    185 -> IconHouse
    186 -> IconHeart
    187 -> IconCorner
    188 -> IconVerticalBars
    189 -> IconVerticalBarsFill
    190 -> IconLifeBars
    191 -> IconInfo
    192 -> IconCrossline
    193 -> IconHelp
    194 -> IconFiletypeAlpha
    195 -> IconFiletypeHome
    196 -> IconLayersVisible
    197 -> IconLayers
    198 -> IconWindow
    199 -> IconHidpi
    200 -> IconFiletypeBinary
    201 -> IconHex
    202 -> IconShield
    203 -> IconFileNew
    204 -> IconFolderAdd
    205 -> IconAlarm
    206 -> IconCpu
    207 -> IconRom
    208 -> IconStepOver
    209 -> IconStepInto
    210 -> IconStepOut
    211 -> IconRestart
    212 -> IconBreakpointOn
    213 -> IconBreakpointOff
    214 -> IconBurgerMenu
    215 -> IconCaseSensitive
    216 -> IconRegExp
    217 -> IconFolder
    218 -> IconFile
    219 -> IconSandTimer
    220 -> IconWarning
    221 -> IconHelpBox
    222 -> IconInfoBox
    223 -> Icon223
    224 -> Icon224
    225 -> Icon225
    226 -> Icon226
    227 -> Icon227
    228 -> Icon228
    229 -> Icon229
    230 -> Icon230
    231 -> Icon231
    232 -> Icon232
    233 -> Icon233
    234 -> Icon234
    235 -> Icon235
    236 -> Icon236
    237 -> Icon237
    238 -> Icon238
    239 -> Icon239
    240 -> Icon240
    241 -> Icon241
    242 -> Icon242
    243 -> Icon243
    244 -> Icon244
    245 -> Icon245
    246 -> Icon246
    247 -> Icon247
    248 -> Icon248
    249 -> Icon249
    250 -> Icon250
    251 -> Icon251
    252 -> Icon252
    253 -> Icon253
    254 -> Icon254
    255 -> Icon255
    n -> error $ "(GuiIconName.toEnum) Invalid value: " ++ show n

instance Storable GuiIconName where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

---------------------------------------
-- raygui structures ------------------
---------------------------------------

-- | Style property
--
-- NOTE: Used when exporting style as code for convenience
data GuiStyleProp = GuiStyleProp
  { -- | Control identifier
    guiStyleProp'controlId :: Word16,
    -- | Property identifier
    guiStyleProp'propertyId :: Word16,
    -- | Property value
    guiStyleProp'propertyValue :: Int
  }
  deriving (Eq, Show, Freeable)

instance Storable GuiStyleProp where
  sizeOf _ = 8
  alignment _ = 4
  peek _p = do
    controlId <- fromIntegral <$> peek (p'guiStyleProp'controlId _p)
    propertyId <- fromIntegral <$> peek (p'guiStyleProp'propertyId _p)
    propertyValue <- fromIntegral <$> peek (p'guiStyleProp'propertyValue _p)
    return $ GuiStyleProp controlId propertyId propertyValue
  poke _p (GuiStyleProp controlId propertyId propertyValue) = do
    poke (p'guiStyleProp'controlId _p) (fromIntegral controlId)
    poke (p'guiStyleProp'propertyId _p) (fromIntegral propertyId)
    poke (p'guiStyleProp'propertyValue _p) (fromIntegral propertyValue)
    return ()

p'guiStyleProp'controlId :: Ptr GuiStyleProp -> Ptr CUShort
p'guiStyleProp'controlId = (`plusPtr` 0)

p'guiStyleProp'propertyId :: Ptr GuiStyleProp -> Ptr CUShort
p'guiStyleProp'propertyId = (`plusPtr` 2)

p'guiStyleProp'propertyValue :: Ptr GuiStyleProp -> Ptr CInt
p'guiStyleProp'propertyValue = (`plusPtr` 4)
