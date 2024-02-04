/**
 * @file rgui_bindings.h
 * @author Anut-py
 * @brief Required methods for binding Haskell to raygui
 */

#include "rl_common.h"
#include <style_ashes.h>
#include <style_bluish.h>
#include <style_candy.h>
#include <style_cherry.h>
#include <style_cyber.h>
#include <style_dark.h>
#include <style_enefete.h>
#include <style_jungle.h>
#include <style_lavanda.h>
#include <style_sunny.h>
#include <style_terminal.h>

void GuiEnable_(void);

void GuiDisable_(void);

void GuiLock_(void);

void GuiUnlock_(void);

bool GuiIsLocked_(void);

void GuiSetAlpha_(float alpha);

void GuiSetState_(int state);

int GuiGetState_(void);

void GuiSetFont_(Font *font);

Font *GuiGetFont_(void);

void GuiSetStyle_(int control, int property, int value);

int GuiGetStyle_(int control, int property);

void GuiLoadStyle_(const char *fileName);

void GuiLoadStyleDefault_(void);

void GuiLoadStyleAshes_(void);

void GuiLoadStyleBluish_(void);

void GuiLoadStyleCandy_(void);

void GuiLoadStyleCherry_(void);

void GuiLoadStyleCyber_(void);

void GuiLoadStyleDark_(void);

void GuiLoadStyleEnefete_(void);

void GuiLoadStyleJungle_(void);

void GuiLoadStyleLavanda_(void);

void GuiLoadStyleSunny_(void);

void GuiLoadStyleTerminal_(void);

void GuiEnableTooltip_(void);

void GuiDisableTooltip_(void);

void GuiSetTooltip_(const char *tooltip);

const char *GuiIconText_(int iconId, const char *text);

void GuiSetIconScale_(int scale);

unsigned int *GuiGetIcons_(void);

char **GuiLoadIcons_(const char *fileName, bool loadIconsName);

void GuiDrawIcon_(int iconId, int posX, int posY, int pixelSize, Color *color);

int GuiWindowBox_(Rectangle *bounds, const char *title);

int GuiGroupBox_(Rectangle *bounds, const char *text);

int GuiLine_(Rectangle *bounds, const char *text);

int GuiPanel_(Rectangle *bounds, const char *text);

int GuiTabBar_(Rectangle *bounds, const char **text, int count, int *active);

int GuiScrollPanel_(Rectangle *bounds, const char *text, Rectangle *content, Vector2 *scroll, Rectangle *view);

int GuiLabel_(Rectangle *bounds, const char *text);

int GuiButton_(Rectangle *bounds, const char *text);

int GuiLabelButton_(Rectangle *bounds, const char *text);

int GuiToggle_(Rectangle *bounds, const char *text, bool *active);

int GuiToggleGroup_(Rectangle *bounds, const char *text, int *active);

int GuiToggleSlider_(Rectangle *bounds, const char *text, int *active);

int GuiCheckBox_(Rectangle *bounds, const char *text, bool *checked);

int GuiComboBox_(Rectangle *bounds, const char *text, int *active);

int GuiDropdownBox_(Rectangle *bounds, const char *text, int *active, bool editMode);

int GuiSpinner_(Rectangle *bounds, const char *text, int *value, int minValue, int maxValue, bool editMode);

int GuiValueBox_(Rectangle *bounds, const char *text, int *value, int minValue, int maxValue, bool editMode);

int GuiTextBox_(Rectangle *bounds, char *text, int textSize, bool editMode);

int GuiSlider_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue);

int GuiSliderBar_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue);

int GuiProgressBar_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue);

int GuiStatusBar_(Rectangle *bounds, const char *text);

int GuiDummyRec_(Rectangle *bounds, const char *text);

int GuiGrid_(Rectangle *bounds, const char *text, float spacing, int subdivs, Vector2 *mouseCell);

int GuiListView_(Rectangle *bounds, const char *text, int *scrollIndex, int *active);

int GuiListViewEx_(Rectangle *bounds, const char **text, int count, int *scrollIndex, int *active, int *focus);

int GuiMessageBox_(Rectangle *bounds, const char *title, const char *message, const char *buttons);

int GuiTextInputBox_(Rectangle *bounds, const char *title, const char *message, const char *buttons, char *text, int textMaxSize, bool *secretViewActive);

int GuiColorPicker_(Rectangle *bounds, const char *text, Color *color);

int GuiColorPanel_(Rectangle *bounds, const char *text, Color *color);

int GuiColorBarAlpha_(Rectangle *bounds, const char *text, float *alpha);

int GuiColorBarHue_(Rectangle *bounds, const char *text, float *value);

int GuiColorPickerHSV_(Rectangle *bounds, const char *text, Vector3 *colorHsv);

int GuiColorPanelHSV_(Rectangle *bounds, const char *text, Vector3 *colorHsv);
