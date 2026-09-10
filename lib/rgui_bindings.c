/**
 * See rgui_bindings.h
 */

#define RAYGUI_IMPLEMENTATION
#include "rgui_bindings.h"
#undef RAYGUI_IMPLEMENTATION

RLBIND void GuiEnable_(void)
{
  return GuiEnable();
}

RLBIND void GuiDisable_(void)
{
  return GuiDisable();
}

RLBIND void GuiLock_(void)
{
  return GuiLock();
}

RLBIND void GuiUnlock_(void)
{
  return GuiUnlock();
}

RLBIND _Bool GuiIsLocked_(void)
{
  return GuiIsLocked();
}

RLBIND void GuiSetAlpha_(float alpha)
{
  return GuiSetAlpha(alpha);
}

RLBIND void GuiSetState_(int state)
{
  return GuiSetState(state);
}

RLBIND int GuiGetState_(void)
{
  return GuiGetState();
}

RLBIND void GuiSetFont_(Font *font)
{
  return GuiSetFont(*font);
}

RLBIND Font *GuiGetFont_(void)
{
  Font *ptr = (Font *)malloc(sizeof(Font));
  *ptr = GuiGetFont();
  return ptr;
}

RLBIND void GuiSetStyle_(int control, int property, int value)
{
  return GuiSetStyle(control, property, value);
}

RLBIND int GuiGetStyle_(int control, int property)
{
  return GuiGetStyle(control, property);
}

RLBIND void GuiLoadStyle_(const char *fileName)
{
  return GuiLoadStyle(fileName);
}

RLBIND void GuiLoadStyleFromMemory_(const unsigned char *fileData, int dataSize)
{
  return GuiLoadStyleFromMemory(fileData, dataSize);
}

RLBIND void GuiLoadStyleDefault_(void)
{
  return GuiLoadStyleDefault();
}

RLBIND void GuiLoadStyleAdvance_(void)
{
  return GuiLoadStyleAdvance();
}

RLBIND void GuiLoadStyleAmber_(void)
{
  return GuiLoadStyleAmber();
}

RLBIND void GuiLoadStyleAshes_(void)
{
  return GuiLoadStyleAshes();
}

RLBIND void GuiLoadStyleBluish_(void)
{
  return GuiLoadStyleBluish();
}

RLBIND void GuiLoadStyleBrick_(void)
{
  return GuiLoadStyleBrick();
}

RLBIND void GuiLoadStyleCandy_(void)
{
  return GuiLoadStyleCandy();
}

RLBIND void GuiLoadStyleCherry_(void)
{
  return GuiLoadStyleCherry();
}

RLBIND void GuiLoadStyleCyber_(void)
{
  return GuiLoadStyleCyber();
}

RLBIND void GuiLoadStyleDark_(void)
{
  return GuiLoadStyleDark();
}

RLBIND void GuiLoadStyleEnefete_(void)
{
  return GuiLoadStyleEnefete();
}

RLBIND void GuiLoadStyleGenesis_(void)
{
  return GuiLoadStyleGenesis();
}

RLBIND void GuiLoadStyleJungle_(void)
{
  return GuiLoadStyleJungle();
}

RLBIND void GuiLoadStyleLavanda_(void)
{
  return GuiLoadStyleLavanda();
}

RLBIND void GuiLoadStylePocket_(void)
{
  return GuiLoadStylePocket();
}

RLBIND void GuiLoadStyleRLTech_(void)
{
  return GuiLoadStyleRLTech();
}

RLBIND void GuiLoadStyleSunny_(void)
{
  return GuiLoadStyleSunny();
}

RLBIND void GuiLoadStyleTerminal_(void)
{
  return GuiLoadStyleTerminal();
}

RLBIND void GuiLoadStyleTurbo_(void)
{
  return GuiLoadStyleTurbo();
}

RLBIND void GuiLoadStyleWisteria_(void)
{
  return GuiLoadStyleWisteria();
}

RLBIND void GuiEnableTooltip_(void)
{
  return GuiEnableTooltip();
}

RLBIND void GuiDisableTooltip_(void)
{
  return GuiDisableTooltip();
}

RLBIND void GuiSetTooltip_(const char *tooltip)
{
  return GuiSetTooltip(tooltip);
}

RLBIND const char *GuiIconText_(int iconId, const char *text)
{
  return GuiIconText(iconId, text);
}

RLBIND void GuiSetIconScale_(int scale)
{
  return GuiSetIconScale(scale);
}

RLBIND unsigned int *GuiGetIcons_(void)
{
  return GuiGetIcons();
}

RLBIND char **GuiLoadIcons_(const char *fileName, bool loadIconsName)
{
  return GuiLoadIcons(fileName, loadIconsName);
}

RLBIND char **GuiLoadIconsFromMemory_(const unsigned char *fileData, int dataSize, bool loadIconsName)
{
  return GuiLoadIconsFromMemory(fileData, dataSize, loadIconsName);
}

RLBIND void GuiDrawIcon_(int iconId, int posX, int posY, int pixelSize, Color *color)
{
  return GuiDrawIcon(iconId, posX, posY, pixelSize, *color);
}

RLBIND int GuiGetTextWidth_(char *a)
{
  return GuiGetTextWidth(a);
}

RLBIND int GuiWindowBox_(Rectangle *bounds, const char *title)
{
  return GuiWindowBox(*bounds, title);
}

RLBIND int GuiGroupBox_(Rectangle *bounds, const char *text)
{
  return GuiGroupBox(*bounds, text);
}

RLBIND int GuiLine_(Rectangle *bounds, const char *text)
{
  return GuiLine(*bounds, text);
}

RLBIND int GuiPanel_(Rectangle *bounds, const char *text)
{
  return GuiPanel(*bounds, text);
}

RLBIND int GuiScrollPanel_(Rectangle *bounds, const char *text, Rectangle *content, Vector2 *scroll, Rectangle *view)
{
  return GuiScrollPanel(*bounds, text, *content, scroll, view);
}

RLBIND int GuiLabel_(Rectangle *bounds, const char *text)
{
  return GuiLabel(*bounds, text);
}

RLBIND int GuiButton_(Rectangle *bounds, const char *text)
{
  return GuiButton(*bounds, text);
}

RLBIND int GuiLabelButton_(Rectangle *bounds, const char *text)
{
  return GuiLabelButton(*bounds, text);
}

RLBIND int GuiToggle_(Rectangle *bounds, const char *text, bool *active)
{
  return GuiToggle(*bounds, text, active);
}

RLBIND int GuiToggleGroup_(Rectangle *bounds, const char *text, int *active)
{
  return GuiToggleGroup(*bounds, text, active);
}

RLBIND int GuiToggleSlider_(Rectangle *bounds, const char *text, int *active)
{
  return GuiToggleSlider(*bounds, text, active);
}

RLBIND int GuiCheckBox_(Rectangle *bounds, const char *text, bool *checked)
{
  return GuiCheckBox(*bounds, text, checked);
}

RLBIND int GuiComboBox_(Rectangle *bounds, const char *text, int *active)
{
  return GuiComboBox(*bounds, text, active);
}

RLBIND int GuiDropdownBox_(Rectangle *bounds, const char *text, int *active, bool editMode)
{
  return GuiDropdownBox(*bounds, text, active, editMode);
}

RLBIND int GuiSpinner_(Rectangle *bounds, const char *text, int *value, int minValue, int maxValue, bool editMode)
{
  return GuiSpinner(*bounds, text, value, minValue, maxValue, editMode);
}

RLBIND int GuiValueBox_(Rectangle *bounds, const char *text, int *value, int minValue, int maxValue, bool editMode)
{
  return GuiValueBox(*bounds, text, value, minValue, maxValue, editMode);
}

RLBIND int GuiValueBoxFloat_(Rectangle *bounds, const char *text, char *textValue, float *value, bool editMode) {
  return GuiValueBoxFloat(*bounds, text, textValue, value, editMode);
}

RLBIND int GuiTextBox_(Rectangle *bounds, char *text, int textSize, bool editMode)
{
  return GuiTextBox(*bounds, text, textSize, editMode);
}

RLBIND int GuiSlider_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue)
{
  return GuiSlider(*bounds, textLeft, textRight, value, minValue, maxValue);
}

RLBIND int GuiSliderBar_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue)
{
  return GuiSliderBar(*bounds, textLeft, textRight, value, minValue, maxValue);
}

RLBIND int GuiProgressBar_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue)
{
  return GuiProgressBar(*bounds, textLeft, textRight, value, minValue, maxValue);
}

RLBIND int GuiStatusBar_(Rectangle *bounds, const char *text)
{
  return GuiStatusBar(*bounds, text);
}

RLBIND int GuiDummyRec_(Rectangle *bounds, const char *text)
{
  return GuiDummyRec(*bounds, text);
}

RLBIND int GuiGrid_(Rectangle *bounds, const char *text, float spacing, int subdivs, Vector2 *mouseCell)
{
  return GuiGrid(*bounds, text, spacing, subdivs, mouseCell);
}

RLBIND int GuiListView_(Rectangle *bounds, const char *text, int *scrollIndex, int *active)
{
  return GuiListView(*bounds, text, scrollIndex, active);
}

RLBIND int GuiListViewEx_(Rectangle *bounds, char **text, int count, int *scrollIndex, int *active, int *focus)
{
  return GuiListViewEx(*bounds, text, count, scrollIndex, active, focus);
}

RLBIND int GuiTabBar_(Rectangle *bounds, const char *text, int *hscroll, int *active)
{
  return GuiTabBar(*bounds, text, hscroll, active);
}

int GuiTabBarEx_(Rectangle *bounds, char **text, int count, int *hscroll, int *active, int *focus)
{
  return GuiTabBarEx(*bounds, text, count, hscroll, active, focus);
}

RLBIND int GuiMessageBox_(Rectangle *bounds, const char *title, const char *message, const char *btnText, int *btnActive)
{
  return GuiMessageBox(*bounds, title, message, btnText, btnActive);
}

RLBIND int GuiTextInputBox_(Rectangle *bounds, const char *title, const char *message, char *text, int textSize, const char *btnText, int *btnActive, bool *secretViewActive)
{
  return GuiTextInputBox(*bounds, title, message, text, textSize, btnText, btnActive, secretViewActive);
}

RLBIND int GuiColorPicker_(Rectangle *bounds, const char *text, Color *color)
{
  return GuiColorPicker(*bounds, text, color);
}

RLBIND int GuiColorPanel_(Rectangle *bounds, const char *text, Color *color)
{
  return GuiColorPanel(*bounds, text, color);
}

RLBIND int GuiColorBarAlpha_(Rectangle *bounds, const char *text, float *alpha)
{
  return GuiColorBarAlpha(*bounds, text, alpha);
}

RLBIND int GuiColorBarHue_(Rectangle *bounds, const char *text, float *value)
{
  return GuiColorBarHue(*bounds, text, value);
}

RLBIND int GuiColorPickerHSV_(Rectangle *bounds, const char *text, Vector3 *colorHsv)
{
  return GuiColorPickerHSV(*bounds, text, colorHsv);
}

RLBIND int GuiColorPanelHSV_(Rectangle *bounds, const char *text, Vector3 *colorHsv)
{
  return GuiColorPanelHSV(*bounds, text, colorHsv);
}
