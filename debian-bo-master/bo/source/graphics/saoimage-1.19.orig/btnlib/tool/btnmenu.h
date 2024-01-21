#ifndef lint
static char SccsBtnMId[] = "%W%  %G%";
#endif

/*
 * Module:	btnmenu.h
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Define structures for SAO button constructor
 * Typedefs:	BtnSpec, BoxSpec, MenuSpec, PanelSpec, PanelAttach
 * Copyright:	1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		20 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>
#include "../buttons.h"		/* define OPTION_LIMIT and DATA_LIMIT */

typedef struct _BtnIcon {
  char *icon;
  char *mask;
  int width, height;
} BtnIcon;

typedef struct _BtnSpec {
  char *offo_text;		/* off unoccupied text string */
  int offo_tall;		/* 1 to move chars down, else 0 */
  char *ono_text;		/* on unoccupied text string */
  int ono_tall;			/* 1 to move chars down, else 0 */
  char *offi_text;		/* off occupied text string */
  int offi_tall;		/* 1 to move chars down, else 0 */
  char *oni_text;		/* on occupied text string */
  int oni_tall;			/* 1 to move chars down, else 0 */
  BtnIcon *offo_icon;		/* off unoccupied icon (optional) */
  BtnIcon *ono_icon;		/* on unoccupied icon (optional) */
  BtnIcon *offi_icon;		/* off occupied icon (optional) */
  BtnIcon *oni_icon;		/* on occupied icon (optional) */
  int orient;			/* 0=level, 1=down, -1=up */
  int option_cnt;		/* number of response funcs given */
  int code[OPTION_LIMIT];	/* response function codes */
  int mask[OPTION_LIMIT];	/* event recognition masks */
  int ref[OPTION_LIMIT];	/* event recognition references */
  int data[DATA_LIMIT];		/* data returned when selected */
} BtnSpec;

typedef struct _BoxSpec {
  char *title;			/* declaration identifying name */
  BoxGeometry geo;
  BtnSpec *btn;			/* list of specs to define buttons */
} BoxSpec;

typedef struct _PanelAttach {
  char *panel_title;		/* identifying title of panel */
  char *box_title;		/* identifying title of box within panel */
  int parent_index;		/* index of panel's parent window */
  int attach, join;		/* flags for AttachSubMenu and JoinMenus */
  int mask, reference;		/* button function mask and reference */
  int match_cnt;		/* number of data items to match (0+) */
  int data[DATA_LIMIT];
} PanelAttach;

typedef struct _PanelSpec {
  char *title;			/* title to identify this panel */
  char *file_name;		/* name of file for declarations */
  int parent_index;		/* index of window to hold panel */
  int box_cnt;			/* number of boxes defined */
  BoxSpec *box;			/* list of specs to define boxes */
  PanelAttach *attach;		/* attachment specs if submenu */
} PanelSpec;


typedef struct _MenuSpec {
  char *file_name;		/* name of file for main call */
  int panel_cnt;		/* number of boxes defined */
  PanelSpec *panel;		/* list of specs to define panels */
} MenuSpec;
