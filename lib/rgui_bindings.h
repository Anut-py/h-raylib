/**
 * @file rgui_bindings.h
 * @author Anut-py
 * @brief Required methods for binding Haskell to raygui
 */

#include "rl_common.h"
#include <style_advance.h>
#include <style_amber.h>
#include <style_ashes.h>
#include <style_bluish.h>
#include <style_brick.h>
#include <style_candy.h>
#include <style_cherry.h>
#include <style_cyber.h>
#include <style_dark.h>
#include <style_enefete.h>
#include <style_genesis.h>
#include <style_jungle.h>
#include <style_lavanda.h>
#include <style_pocket.h>
#include <style_rltech.h>
#include <style_sunny.h>
#include <style_terminal.h>
#include <style_turbo.h>
#include <style_wisteria.h>

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

void GuiLoadStyleFromMemory_(const unsigned char *fileData, int dataSize);

void GuiLoadStyleDefault_(void);

void GuiLoadStyleAdvance_(void);

void GuiLoadStyleAmber_(void);

void GuiLoadStyleAshes_(void);

void GuiLoadStyleBluish_(void);

void GuiLoadStyleBrick_(void);

void GuiLoadStyleCandy_(void);

void GuiLoadStyleCherry_(void);

void GuiLoadStyleCyber_(void);

void GuiLoadStyleDark_(void);

void GuiLoadStyleEnefete_(void);

void GuiLoadStyleGenesis_(void);

void GuiLoadStyleJungle_(void);

void GuiLoadStyleLavanda_(void);

void GuiLoadStylePocket_(void);

void GuiLoadStyleRLTech_(void);

void GuiLoadStyleSunny_(void);

void GuiLoadStyleTerminal_(void);

void GuiLoadStyleTurbo_(void);

void GuiLoadStyleWisteria_(void);

void GuiEnableTooltip_(void);

void GuiDisableTooltip_(void);

void GuiSetTooltip_(const char *tooltip);

const char *GuiIconText_(int iconId, const char *text);

void GuiSetIconScale_(int scale);

unsigned int *GuiGetIcons_(void);

char **GuiLoadIcons_(const char *fileName, bool loadIconsName);

char **GuiLoadIconsFromMemory_(const unsigned char *fileData, int dataSize, bool loadIconsName);

void GuiDrawIcon_(int iconId, int posX, int posY, int pixelSize, Color *color);

int GuiGetTextWidth_(char *a);

int GuiWindowBox_(Rectangle *bounds, const char *title);

int GuiGroupBox_(Rectangle *bounds, const char *text);

int GuiLine_(Rectangle *bounds, const char *text);

int GuiPanel_(Rectangle *bounds, const char *text);

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

int GuiValueBoxFloat_(Rectangle *bounds, const char *text, char *textValue, float *value, bool editMode);

int GuiTextBox_(Rectangle *bounds, char *text, int textSize, bool editMode);

int GuiSlider_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue);

int GuiSliderBar_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue);

int GuiProgressBar_(Rectangle *bounds, const char *textLeft, const char *textRight, float *value, float minValue, float maxValue);

int GuiStatusBar_(Rectangle *bounds, const char *text);

int GuiDummyRec_(Rectangle *bounds, const char *text);

int GuiGrid_(Rectangle *bounds, const char *text, float spacing, int subdivs, Vector2 *mouseCell);

int GuiListView_(Rectangle *bounds, const char *text, int *scrollIndex, int *active);

int GuiListViewEx_(Rectangle *bounds, char **text, int count, int *scrollIndex, int *active, int *focus);

int GuiTabBar_(Rectangle *bounds, const char *text, int *hscroll, int *active);

int GuiTabBarEx_(Rectangle *bounds, char **text, int count, int *hscroll, int *active, int *focus);

int GuiMessageBox_(Rectangle *bounds, const char *title, const char *message, const char *btnText, int *btnActive);

int GuiTextInputBox_(Rectangle *bounds, const char *title, const char *message, char *text, int textSize, const char *btnText, int *btnActive, bool *secretViewActive);

int GuiColorPicker_(Rectangle *bounds, const char *text, Color *color);

int GuiColorPanel_(Rectangle *bounds, const char *text, Color *color);

int GuiColorBarAlpha_(Rectangle *bounds, const char *text, float *alpha);

int GuiColorBarHue_(Rectangle *bounds, const char *text, float *value);

int GuiColorPickerHSV_(Rectangle *bounds, const char *text, Vector3 *colorHsv);

int GuiColorPanelHSV_(Rectangle *bounds, const char *text, Vector3 *colorHsv);
