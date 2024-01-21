#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	tool.c (Panel Tool)
 * Purpose:	Use passed array of arrays of structures to create a single
 *		subroutine call to assemble a button menu widget and a
 *		single subroutine call to resize all panels.
 * Subroutine:	btn_SetupMenu()			returns: void
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
#include <string.h>
#include "btnmenu.h"

/*
 * Subroutine:	btn_SetupMenu
 * Purpose:	Main routine to print C code to assemble and resize
 *		a complete button menu widget
 */
void btn_SetupMenu ( menu )
     MenuSpec *menu;
{
  FILE *file;
  int i, j, k;
  int max;
  void btn_SetupPanel();
  void btn_WriteMenuAttach();

  /* create the include files */
  for( i=0; i<menu->panel_cnt; i++ ) {
    btn_SetupPanel(&menu->panel[i], i);
  }
  /* open the file and declare the includes */
  file = fopen(menu->file_name, "w");
  (void)fprintf(file, "\n#include <stdio.h>\n");
  (void)fprintf(file, "#include <X11/Xlib.h>\n");
  (void)fprintf(file, "#include \"../btnlib/buttons.h\"\n\n");
  for( i=0; i<menu->panel_cnt; i++ ) {
    (void)fprintf(file, "#include \"%s\"\n", menu->panel[i].file_name);
  }
  /* declare the buttonbox arrays */
  (void)fprintf(file, "\n");
  for( i=0; i<menu->panel_cnt; i++ ) {
    (void)fprintf(file, "ButtonBox %s[%d];\n",
		  menu->panel[i].title, menu->panel[i].box_cnt);
  }
  /* create the main call to make all panels */
  (void)fprintf(file, "\nButtonBox CreateMenu");
  (void)fprintf(file, " ( parent, gc, visual, background )\n");
  (void)fprintf(file, "     BoxParent *parent;");
  (void)fprintf(file, "\t\t/* i: array of parent window specs */\n");
  (void)fprintf(file, "     GC gc;\t\t\t/* i: optional, else 0 */\n");
  (void)fprintf(file, "     Visual *visual;\t\t/* i: optional, else 0 */\n");
  (void)fprintf(file, "     unsigned long background;");
  (void)fprintf(file, "\t/* i: optional, else 0 */\n{\n");
  for( i=0; i<menu->panel_cnt; i++ ) {
    (void)fprintf(file,"  void Make%s();\n", menu->panel[i].title);
  }
  (void)fprintf(file,"\n");
  for( i=0; i<menu->panel_cnt; i++ ) {
    (void)fprintf(file, "  Make%s(%s, parent, gc, visual, background);\n",
		  menu->panel[i].title, menu->panel[i].title);
  }
  btn_WriteMenuAttach (file, menu);
  (void)fprintf(file, "  return( %s[0] );\n", menu->panel[0].title);
  (void)fprintf(file, "}\n\n");
  /* create the resize subroutine */
  (void)fprintf(file, "void ResizeMenu( parent, flags )\n");
  (void)fprintf(file, "     BoxParent *parent;");
  (void)fprintf(file, "\t\t/* i: array of parent window specs */\n");
  (void)fprintf(file, "     int *flags;");
  (void)fprintf(file, "\t\t/* i: array of window-changed flags */\n{\n");
  /* how many window panel specifiers are expected? */
  for( max=0, i=0; i<menu->panel_cnt; i++ ) {
    if( menu->panel[i].parent_index > max )
      max = menu->panel[i].parent_index;
  }
  (void)fprintf(file,"\n");
  for( j=0; j<=max; j++ ) {
    /* is this panel actually used */
    for( i=0; (i<menu->panel_cnt) && (j!=menu->panel[i].parent_index); i++ );
    if( i<menu->panel_cnt) {
      (void)fprintf(file, "  if( flags[%d] ) {\n", i);
      for( i=0; i<menu->panel_cnt; i++ ) {
	if( menu->panel[i].parent_index == j ) {
	  for( k=0; k<menu->panel[i].box_cnt; k++ ) {
	    (void)fprintf(file, "    ResizeBox(%s[%d], &parent[%d]);\n",
			  menu->panel[i].title, k,
			  menu->panel[i].parent_index);
	  }
	}
      }
      (void)fprintf(file, "  }\n");
    }
  }
  (void)fprintf(file, "}\n");
  fclose(file);
}

/*
 * Subroutine:	btn_SetupPanel
 * Purpose:	Define all box structures for a single menu panel and create
 *		a C subroutine to make it at runtime
 * Uses:	btn_SetupBox(), btn_DoJoin() below;
 */
static void btn_SetupPanel ( panel )
     PanelSpec *panel;
{
  FILE *file;
  int index, i;
  void btn_SetupBox(), btn_DoJoin();

  index = 1;
  file = fopen(panel->file_name, "w");
  (void)fprintf(file, "\n\n");
  /* print the filled record structures */
  for( i=0; i<panel->box_cnt; i++ ) {
    btn_SetupBox(file, &panel->box[i], index);
    index += panel->box[i].geo.btn_cnt;
  }
  /* create the subroutines to make this panel */
  (void)fprintf(file, "void Make%s", panel->title);
  (void)fprintf(file, " ( box, parent, gc, visual, background )\n");
  (void)fprintf(file, "     BoxParent *parent;\n");
  (void)fprintf(file, "     ButtonBox box[%d];\n", panel->box_cnt);
  (void)fprintf(file, "     GC gc;\n");
  (void)fprintf(file, "     Visual *visual;\n");
  (void)fprintf(file, "     unsigned long background;\n{\n");
  for( i=0; i<panel->box_cnt; i++ ) {
    (void)fprintf(file,"  box[%d] = MakeButtonBox", i);
    (void)fprintf
      (file,"(&parent[%d], gc, visual, background,\n", panel->parent_index);
    (void)fprintf(file,"\t\t\t &%s_geo, %s, 0);\n",
		  panel->box[i].title, panel->box[i].title);
  }
  btn_DoJoin(file, panel->box_cnt);
  (void)fprintf(file, "}\n");
  fclose(file);
}

/*
 * Subroutine:	btn_DoJoin
 * Purpose:	Print lines in C subroutine to call JoinMenus for all
 *		combinations of boxes in a menu panel
 * Method:	Combines all lower boxes with highest index, then recurses
 *		with next  highest index.
 */
static void btn_DoJoin ( file, cnt )
     FILE *file;
     int cnt;
{
  int i;

  if( cnt <= 1 )
    return;
  cnt--;
  for( i=0; i<cnt; i++ ) {
    (void)fprintf(file, "  JoinMenus (box[%d], box[%d]);\n", i, cnt);
  }
  btn_DoJoin(file, cnt);
}

/*
 * Subroutine:	btn_SetupBox
 * Purpose:	Create all data structure declarations needed for a button box
 * Uses:	btn_WriteButtonLook() in WriteLook.c
 * Uses:	btn_WriteButtonFeel() in WriteFeel.c
 * Called by:	btn_SetupPanel() above
 */
static void btn_SetupBox ( file, box, index )
     FILE *file;
     BoxSpec *box;
     int index;
{
  int i;
  void btn_SetupButton();

  /* write each button's declarations */
  for( i=0; i<box->geo.btn_cnt; i++ ) {
    btn_SetupButton(file, box->title, &box->btn[i], index + i);
  }
  /* write record declaration after declaring all the parts */
  (void)fprintf(file, "static ButtonSpec %s[] = {\n", box->title);
  for( i=0; i<box->geo.btn_cnt; ) {
    (void)fprintf(file, "  { &%s%d_look, &%s%d_feel }",
		  box->title, index + i, box->title, index + i);
    if( ++i < box->geo.btn_cnt )
      (void)fprintf(file, ",\n");
    else
      (void)fprintf(file, " };\n");
  }
  (void)fprintf(file, "static BoxGeometry %s_geo = \n  ", box->title);
  (void)fprintf(file, "{ %d, %d, %d, %d, %7f, %7f, %7f, %7f, %d, %d };\n\n",
		box->geo.panel_index, box->geo.btn_cnt,
		box->geo.box_cols, box->geo.box_rows,
		box->geo.parent_cols, box->geo.parent_rows,
		box->geo.box_col_x, box->geo.box_row_y,
		box->geo.off_inverse, box->geo.on_inverse);
}

/*
 * Subroutine:	btn_SetupButton
 * Purpose:	Create all data structure declarations needed for a button
 * Uses:	btn_WriteButtonLook() in WriteLook.c
 * Uses:	btn_WriteButtonFeel() in WriteFeel.c
 * Called by:	btn_SetupBox() above
 */
static void btn_SetupButton ( file, title, button, index )
     FILE *file;
     char *title;
     BtnSpec *button;
     int index;
{
  char btn_title[80];
  void btn_WriteButtonLook(), btn_WriteButtonFeel();

  (void)sprintf(btn_title, "%s%d", title, index);
  (void)fprintf(file,"/* %s button: %s */\n", button->offo_text, btn_title);
  btn_WriteButtonLook(file, btn_title, button);
  btn_WriteButtonFeel(file, btn_title, button);
}
