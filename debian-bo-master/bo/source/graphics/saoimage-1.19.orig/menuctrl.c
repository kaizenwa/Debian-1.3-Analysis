#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	menuctrl.c (Menu Control)
 * Purpose:	Manipulate the button panel under software control
 * Subroutine:	touch_submenu_button()			returns: void
 * Subroutine:	set_submenu_toggle()			returns: void
 * Subroutine:	enable_annuli_button()			returns: void
 * Subroutine:	enable_ortho_button()			returns: void
 * Subroutine:	init_buttonbox_settings()		returns: void
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	       26 April 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define stderr, NULL */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "btnlib/buttons.h"
#include "hfiles/constant.h"
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */

extern ButtonBox MainPanel[1];
extern ButtonBox ColorPanel[1];
extern ButtonBox CursorPanel[2];
extern ButtonBox ScalePanel[1];
extern ButtonBox PanPanel[1];
extern ButtonBox EtcPanel[1];
extern ButtonBox RegionPanel[1];
extern ButtonBox CmapPanel[1];
extern ButtonBox MonoPanel[1];

/*
 * Subroutine:	touch_submenu_button
 * Purpose:	Make the button panel respond (as if the given button had been
 *		clicked by the mouse) to set submenu button to agree with
 *		current internal status
 */
void touch_submenu_button ( mode, action )
     int mode;
     int action;
{
  int button;
  GC set_gc_with_background();

  /* set the forground and background for drawing buttons */
  (void)set_gc_with_background(&color.gcset.menu, color.gcset.menu.background);
  switch( mode ) {
  case COP:
    if( (button = ButtonNumber(CursorPanel[0], 1, action)) >= 0 )
      (void)TouchButton(CursorPanel[0], button, 0, 0);
    break;
  case ROP:
    if( (button = ButtonNumber(RegionPanel[0], 1, action)) >= 0 )
      (void)TouchButton(RegionPanel[0], button, 0, 0);
    break;
  case VOP:
    if( (button = ButtonNumber(ColorPanel[0], 1, action)) >= 0 )
      (void)TouchButton(ColorPanel[0], button, 0, 0);
    break;
  case MOP:
    if( (button = ButtonNumber(CmapPanel[0], 1, action)) >= 0 )
      (void)TouchButton(CmapPanel[0], button, 0, 0);
    break;
  case BOP:
    if( (button = ButtonNumber(MonoPanel[0], 1, action)) >= 0 )
      (void)TouchButton(MonoPanel[0], button, 0, 0);
    break;
  case SOP:
    if( (button = ButtonNumber(ScalePanel[0], 1, action)) >= 0 )
      (void)TouchButton(ScalePanel[0], button, 0, 0);
    break;
  case ZOP:
    if( (button = ButtonNumber(PanPanel[0], 1, action)) >= 0 )
      (void)TouchButton(PanPanel[0], button, 0, 0);
    break;
  case EOP:
    if( (button = ButtonNumber(EtcPanel[0], 1, action)) >= 0 )
      (void)TouchButton(EtcPanel[0], button, 0, 0);
    break;
  default:
    break;
  }
}

/*
 * Subroutine:	set_submenu_toggle
 * Purpose:	Set submenu toggle button to agree with current status
 */
void set_submenu_toggle ( mode, action, status )
     int mode;
     int action;
     int status;
{
  int button;
  GC set_gc_with_background();

  /* set the forground and background for drawing buttons */
  (void)set_gc_with_background(&color.gcset.menu, color.gcset.menu.background);
  switch( mode ) {
  case COP:
    if( (button = ButtonNumber(CursorPanel[0], 1, action)) >= 0 )
      (void)SetToggleButton(CursorPanel[0], button, status);
    break;
  case ROP:
    if( (button = ButtonNumber(RegionPanel[0], 1, action)) >= 0 )
      (void)SetToggleButton(RegionPanel[0], button, status);
    break;
  case VOP:
    if( (button = ButtonNumber(ColorPanel[0], 1, action)) >= 0 )
      (void)SetToggleButton(ColorPanel[0], button, status);
    break;
  case MOP:
    if( (button = ButtonNumber(CmapPanel[0], 1, action)) >= 0 )
      (void)SetToggleButton(CmapPanel[0], button, status);
    break;
  case BOP:
    if( (button = ButtonNumber(MonoPanel[0], 1, action)) >= 0 )
      (void)SetToggleButton(MonoPanel[0], button, status);
    break;
  case SOP:
    if( (button = ButtonNumber(ScalePanel[0], 1, action)) >= 0 )
      (void)SetToggleButton(ScalePanel[0], button, status);
  break;
  case ZOP:
    if( (button = ButtonNumber(PanPanel[0], 1, action)) >= 0 )
      (void)SetToggleButton(PanPanel[0], status, button);
    break;
  case EOP:
    if( (button = ButtonNumber(EtcPanel[0], 1, action)) >= 0 )
      (void)SetToggleButton(EtcPanel[0], button, status);
    break;
  default:
    break;
  }
}

/*
 * Subroutine:	enable_annuli_button
 * Purpose:	Enable and disable anuulus button depending on current cursor
 */
void enable_annuli_button ( enable )
     int enable;	/* i: enable vs. disable */
{
  int button;
  static int disabled = 0;
  GC set_gc_with_background();

  if( (button = ButtonNumber(CursorPanel[0], 1, COP_Annuli)) >= 0 ) {
    if( enable ) {
      if( disabled ) {
	(void)EnableButton(CursorPanel[0], button, BTNToggle);
	disabled = 0;
      }
    } else {
      if( disabled == 0 ) {
	(void)set_gc_with_background(&color.gcset.menu,
				     color.gcset.menu.background);
	(void)SetToggleButton(CursorPanel[0], button, 0);
	(void)DisableButton(CursorPanel[0], button);
	disabled = 1;
      }
    }
  }
}

/*
 * Subroutine:	enable_ortho_button
 * Purpose:	Enable and disable ortho button depending on current cursor
 */
void enable_ortho_button ( enable )
     int enable;	/* i: enable vs. disable */
{
  int button;
  static int disabled = 0;

  if( (button = ButtonNumber(CursorPanel[0], 1, COP_Orthogonal)) >= 0 ) {
    if( enable ) {
      if( disabled ) {
	(void)EnableButton(CursorPanel[0], button, BTNFlash);
	disabled = 0;
      }
    } else {
      if( disabled == 0 ) {
	(void)DisableButton(CursorPanel[0], button);
	disabled = 1;
      }
    }
  }
}

/*
 * Subroutine:	init_buttonbox_settings
 * Purpose:	Set buttons to states corresponding to initial defaults
 * Note:	region's buttons are set from init_region
 */
void init_buttonbox_settings ( )
{
  int button;

  /* cursor */
  touch_submenu_button (COP, cursor.type);
  if( (cursor.type == COP_Polygon) || (cursor.type == COP_Point) ) {
    enable_annuli_button(0);
    enable_ortho_button(0);
  } else if( cursor.annuli ) {
    set_submenu_toggle(COP, COP_Annuli, 1);
    enable_ortho_button(0);
  } else if( cursor.type == COP_Circle )
    enable_ortho_button(0);
  /* color */
  if( color.inverse )
    set_submenu_toggle(VOP, VOP_Invert, 1);
  if( color.halftone.inverse )
    set_submenu_toggle (BOP, BOP_Invert, 1);
  /* set the modes */
  button = ButtonNumber(ColorPanel[0], 1, VOP_Halftone);
  if( ((color.screen_depth <= 1) || (color.colormap_mode == VOP_Halftone)) &&
      (button >= 0) ) {
    /* halftone only?, set mono and then freeze (disable) it */
    (void)SetToggleButton (ColorPanel[0], button, 1);
    (void)DisableButton(ColorPanel[0], button);
  } else {
    /* color possible? set control mode, then set mono if chosen */
    touch_submenu_button(VOP, color.control_mode);
    if( color.cells.overlay )
      set_submenu_toggle(VOP, VOP_Overlay, 1);
  }
  if( color.halftone.mode == BOP_Diffuse ) {
    touch_submenu_button(BOP, BOP_Diffuse);
  } else if( color.halftone.matrixID == BOP_Matrix1 ) {
    touch_submenu_button(BOP, BOP_Matrix1);
  } else if( color.halftone.matrixID == BOP_Matrix2 ) {
    touch_submenu_button(BOP, BOP_Matrix2);
  }
  /* scale */
  touch_submenu_button (SOP, color.scale.mode);
  /* control */
  if( control.magni_track )
    set_submenu_toggle(EOP, EOP_Track, 1);
  if( control.coord_track )
    set_submenu_toggle(EOP, EOP_TextTrack, 1);
  if( control.verbose )
    set_submenu_toggle(EOP, EOP_Verbose, 1);
  /* main menu */
  if( (button = ButtonNumber(MainPanel[0], 0, control.mode)) >= 0 )
    (void)TouchButton(MainPanel[0], button, 0, 0);
}
