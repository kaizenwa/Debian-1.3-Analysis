#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	wrattach.c (Write Attach)
 * Subroutine:	btn_WriteMenuAttach()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include "btnmenu.h"

/* 
 * Subroutine:	btn_WriteMenuAttach
 * Purpose:	write code to create menu attachments
 */
void btn_WriteMenuAttach ( file, menu )
     FILE *file;
     MenuSpec *menu;
{
  int i;
  static void btn_WritePanelAttach();

  for( i=0; i<menu->panel_cnt; i++ ) {
    if( menu->panel[i].attach != NULL ) {
      btn_WritePanelAttach (file, &menu->panel[i], menu);
    }
  }
}

/*
 * Subroutine:	btn_WritePanelAttach
 */
static void btn_WritePanelAttach ( file, panel, menu )
     FILE *file;
     PanelSpec *panel;
     MenuSpec *menu;
{
  char *pnlname;
  int pnum, boxnum, btnnum;
  int i;
  PanelAttach *attach;
  static int btn_FindAttachButton();

  attach = panel->attach;
  if( btn_FindAttachButton(attach, menu, &pnum, &boxnum, &btnnum) ) {
    for( i=0; i<panel->box_cnt; i++ ) {
      if( attach->attach ) {
	(void)fprintf(file,"  AttachSubmenu(%s[%d],", panel->title, i);
	(void)fprintf(file," %s[%d], %d, 0x%x, 0x%x);\n", attach->panel_title,
		      boxnum, btnnum, attach->mask, attach->reference);
      }
      if( attach->join ) {
	(void)fprintf(file,"  JoinMenus(%s[%d], %s[%d]);\n",
		      panel->title, i, attach->panel_title, boxnum);
      }
    }
  }
}

/*
 * Subroutine:	btn_FindAttaachButton
 * Purpose:	get parameters to identify button of attachment
 */
static int btn_FindAttachButton ( attach, menu, panel_num, box_num, btn_num )
     PanelAttach *attach;
     MenuSpec *menu;
     int *panel_num, *box_num, *btn_num;
{
  int pl, bx, bn;
  int i;
  PanelSpec *panel;
  BtnSpec *btn;

  /* check for reasonableness */
  if( (attach->match_cnt <= 0) || (attach->match_cnt > DATA_LIMIT) ) {
    (void)fprintf(stderr,"bad menu attachment specification:");
    (void)fprintf(stderr,"%s %s\n",attach->panel_title, attach->box_title);
    exit(0);
  }
  /* find the attachment point */
  for( pl=0; pl<menu->panel_cnt; pl++ ) {
    /* match the panel parent for the panel */
    if( (attach->parent_index == menu->panel[pl].parent_index) &&
        (strcmp(attach->panel_title, menu->panel[pl].title) == 0) ) {
      panel = &menu->panel[pl];
      for( bx=0; bx<panel->box_cnt; bx++ ) {
	/* match the box title for the box */
	if( strcmp(panel->box[bx].title, attach->box_title) == 0 ) {
	  for( bn=0; bn<panel->box[bx].geo.btn_cnt; bn++ ) {
	    /* match the data for the button */
	    btn = &panel->box[bx].btn[bn];
	    for( i=0;
		(i<attach->match_cnt) && (attach->data[i] == btn->data[i]);
		i++ );
	    if( i >= attach->match_cnt ) {
	      *panel_num = pl;
	      *box_num = bx;
	      *btn_num = bn;
	      return( 1 );
	    }
	  }
	}
      }
    }
  }
  (void)fprintf(stderr,"WARNING: could not find attach button:");
  (void)fprintf(stderr,"%s %s\n", attach->panel_title, attach->box_title);
  return( 0 );
}
