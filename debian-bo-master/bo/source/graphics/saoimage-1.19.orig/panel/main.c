
#include <stdio.h>
#include "../btnlib/tool/btnmenu.h"		/* structs */
#include "../hfiles/constant.h"

#include "main.mnu"
#include "color.mnu"
#include "cursor.mnu"
#include "scale.mnu"
#include "pan.mnu"
#include "etc.mnu"
#include "region.mnu"
#include "cmap.mnu"
#include "mono.mnu"

static PanelSpec MenuPanels[] = {
  { "MainPanel", "menumain.h", 0, 1, MainBoxes, NULL },
  { "ColorPanel", "menuclr.h", 1, 1, ColorBoxes, &ColorAttach },
  { "CursorPanel", "menucsr.h", 1, 2, CursorBoxes, &CursorAttach },
  { "ScalePanel", "menuscl.h", 1, 1, ScaleBoxes, &ScaleAttach },
  { "PanPanel", "menupan.h", 1, 1, PanBoxes, &PanAttach },
  { "EtcPanel", "menuetc.h", 1, 1, EtcBoxes, &EtcAttach },
  { "RegionPanel", "menurgn.h", 2, 1, RegionBoxes, &RegionAttach },
  { "CmapPanel", "menucmap.h", 1, 1, CmapBoxes, &CmapAttach },
  { "MonoPanel", "menumono.h", 3, 1, MonoBoxes, &MonoAttach } };

static MenuSpec Menu = {
  "makemenu.c", 9, MenuPanels };

/*
 * PanelTool main
 */
main(argc, argv)
  int argc;
  char **argv;
{
  btn_SetupMenu( &Menu );
}
