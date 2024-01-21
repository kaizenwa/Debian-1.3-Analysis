
#include <stdio.h>
#include <X11/Xlib.h>
#include "../btnlib/buttons.h"

#include "menumain.h"
#include "menuclr.h"
#include "menucsr.h"
#include "menuscl.h"
#include "menupan.h"
#include "menuetc.h"
#include "menurgn.h"
#include "menucmap.h"
#include "menumono.h"

ButtonBox MainPanel[1];
ButtonBox ColorPanel[1];
ButtonBox CursorPanel[2];
ButtonBox ScalePanel[1];
ButtonBox PanPanel[1];
ButtonBox EtcPanel[1];
ButtonBox RegionPanel[1];
ButtonBox CmapPanel[1];
ButtonBox MonoPanel[1];

ButtonBox CreateMenu ( parent, gc, visual, background )
     BoxParent *parent;		/* i: array of parent window specs */
     GC gc;			/* i: optional, else 0 */
     Visual *visual;		/* i: optional, else 0 */
     unsigned long background;	/* i: optional, else 0 */
{
  void MakeMainPanel();
  void MakeColorPanel();
  void MakeCursorPanel();
  void MakeScalePanel();
  void MakePanPanel();
  void MakeEtcPanel();
  void MakeRegionPanel();
  void MakeCmapPanel();
  void MakeMonoPanel();

  MakeMainPanel(MainPanel, parent, gc, visual, background);
  MakeColorPanel(ColorPanel, parent, gc, visual, background);
  MakeCursorPanel(CursorPanel, parent, gc, visual, background);
  MakeScalePanel(ScalePanel, parent, gc, visual, background);
  MakePanPanel(PanPanel, parent, gc, visual, background);
  MakeEtcPanel(EtcPanel, parent, gc, visual, background);
  MakeRegionPanel(RegionPanel, parent, gc, visual, background);
  MakeCmapPanel(CmapPanel, parent, gc, visual, background);
  MakeMonoPanel(MonoPanel, parent, gc, visual, background);
  AttachSubmenu(ColorPanel[0], MainPanel[0], 1, 0x4, 0x0);
  AttachSubmenu(CursorPanel[0], MainPanel[0], 2, 0x4, 0x0);
  AttachSubmenu(CursorPanel[1], MainPanel[0], 2, 0x4, 0x0);
  AttachSubmenu(ScalePanel[0], MainPanel[0], 0, 0x4, 0x0);
  AttachSubmenu(PanPanel[0], MainPanel[0], 3, 0x4, 0x0);
  AttachSubmenu(EtcPanel[0], MainPanel[0], 4, 0x4, 0x0);
  AttachSubmenu(RegionPanel[0], CursorPanel[1], 0, 0x4, 0x0);
  AttachSubmenu(CmapPanel[0], ColorPanel[0], 6, 0x4, 0x0);
  JoinMenus(CmapPanel[0], ColorPanel[0]);
  AttachSubmenu(MonoPanel[0], ColorPanel[0], 0, 0x4, 0x0);
  return( MainPanel[0] );
}

void ResizeMenu( parent, flags )
     BoxParent *parent;		/* i: array of parent window specs */
     int *flags;		/* i: array of window-changed flags */
{

  if( flags[0] ) {
    ResizeBox(MainPanel[0], &parent[0]);
  }
  if( flags[1] ) {
    ResizeBox(ColorPanel[0], &parent[1]);
    ResizeBox(CursorPanel[0], &parent[1]);
    ResizeBox(CursorPanel[1], &parent[1]);
    ResizeBox(ScalePanel[0], &parent[1]);
    ResizeBox(PanPanel[0], &parent[1]);
    ResizeBox(EtcPanel[0], &parent[1]);
    ResizeBox(CmapPanel[0], &parent[1]);
  }
  if( flags[6] ) {
    ResizeBox(RegionPanel[0], &parent[2]);
  }
  if( flags[8] ) {
    ResizeBox(MonoPanel[0], &parent[3]);
  }
}
