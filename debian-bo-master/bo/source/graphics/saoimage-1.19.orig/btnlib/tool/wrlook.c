#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	wrlook.c (Write Look)
 * Subroutine:	btn_WriteButtonLook()		returns: void
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
 * Subroutine:	btn_WriteButtonLook
 * Purpose:	Create and write data declarations for drawing a button
 * Called by:	btn_SetupButton() in PanelTool.c
 * Uses:	btn_WriteLabelTextPhase(), btn_WriteLabelIconPhase()
 * Method:	Use icon if given, else use text to label button.  If text is
 *		not given for offi, make same as for offo.  If text is not
 *		given for "on" make same as corresponding "off".  If
 *		corresponding "on" and "off" are the same, re-use same data
 *		declarations.
 */
void btn_WriteButtonLook ( file, title, btn )
     FILE *file;
     char *title;
     BtnSpec *btn;
{
  char *offo_ext, *ono_ext, *offi_ext, *oni_ext;
  static void btn_WriteLabelTextPhase(), btn_WriteLabelIconPhase();

  /* set up missing text pointers (and note accompanying size adjustment) */
  if( btn->ono_text == NULL ) {
    btn->ono_text = btn->offo_text;
    btn->ono_tall = btn->offo_tall;
  }
  if( btn->offi_text == NULL ) {
    btn->offi_text = btn->offo_text;
    btn->offi_tall = btn->offo_tall;
  }
  if( btn->oni_text == NULL ) {
    btn->oni_text = btn->offi_text;
    btn->oni_tall = btn->offi_tall;
  }
  /* create extensions for name of declaration and check coordination */
  offo_ext = "offo";
  ono_ext = "ono";
  if( btn->offo_icon == NULL ) {
    btn_WriteLabelTextPhase(file, title, offo_ext,
			    btn->offo_text, btn->offo_tall, btn->orient, 0);
    if( btn->ono_icon == NULL ) {
      if( strcmp(btn->offo_text, btn->ono_text) == 0 )
	ono_ext = offo_ext;
      else
	btn_WriteLabelTextPhase(file, title, ono_ext,
				btn->ono_text, btn->ono_tall, btn->orient, 0);
    } else {
      btn_WriteLabelIconPhase(file, title, ono_ext, btn->ono_icon);
    }
  } else {
    btn_WriteLabelIconPhase(file, title, offo_ext, btn->offo_icon);
    if( btn->ono_icon != NULL ) {
      if( btn->offo_icon == btn->ono_icon )
	ono_ext = offo_ext;
      else
	btn_WriteLabelIconPhase(file, title, ono_ext, btn->ono_icon);
    } else {
      btn_WriteLabelTextPhase(file, title, ono_ext,
			      btn->ono_text, btn->ono_tall, btn->orient, 0);
    }
  }

  offi_ext = "offi";
  oni_ext = "oni";
  if( btn->offi_icon == NULL ) {
    btn_WriteLabelTextPhase(file, title, offi_ext,
			    btn->offi_text, btn->offi_tall, btn->orient, 1);
    if( btn->oni_icon == NULL ) {
      if( strcmp(btn->offi_text, btn->oni_text) == 0 )
	oni_ext = offi_ext;
      else
	btn_WriteLabelTextPhase(file, title, oni_ext,
				btn->oni_text, btn->oni_tall, btn->orient, 1);
    } else {
      btn_WriteLabelIconPhase(file, title, oni_ext, btn->oni_icon);
    }
  } else {
    btn_WriteLabelIconPhase(file, title, offi_ext, btn->offi_icon);
    if( btn->oni_icon != NULL ) {
      if( btn->offi_icon == btn->oni_icon )
	oni_ext = offi_ext;
      else
	btn_WriteLabelIconPhase(file, title, oni_ext, btn->oni_icon);
    } else {
      btn_WriteLabelTextPhase(file, title, oni_ext,
			      btn->oni_text, btn->oni_tall, btn->orient, 1);
    }
  }
  (void)fprintf(file, "static ButtonLook %s_look = {\n", title);
  (void)fprintf(file, "  &%s_%s, NULL, &%s_%s, NULL,\n",
		title, offo_ext, title, ono_ext);
  (void)fprintf(file, "  &%s_%s, NULL, &%s_%s, NULL };\n",
		title, offi_ext, title, oni_ext);
}

/*
 * Subroutine:	btn_WriteLabelIconPhase
 * Purpose:	Write out array declarations for one button phase
 * Called by:	btn_WriteButtonLook() above
 * Uses:	btn_WriteBitmaps() in Bitmap.c
 * Uses:	btn_WriteButtonLabel()
 */
static void btn_WriteLabelIconPhase ( file, title, ext, icon )
     FILE *file;
     char *title, *ext;
     BtnIcon *icon;
{
  int byte_width;
  void btn_WriteBitmaps();
  static void btn_WriteButtonLabel();

  byte_width = (icon->width + 7) / 8;
  btn_WriteBitmaps(file, title, ext, icon->icon, icon->mask,
		    icon->width, icon->height, byte_width);
  btn_WriteButtonLabel(file, title, ext, byte_width, 0, 0);
}

/*
 * Subroutine:	btn_WriteLabelTextPhase
 * Purpose:	Write out array declarations for one button phase
 * Called by:	btn_WriteTextLabel() above
 * Uses:	btn_MakeTextImage(), btn_ByteToBitmap(), btn_MakeLabelMask()
 * Uses:	btn_WriteBitmaps(), btn_WriteTextBreaks()
 * Uses:	btn_WriteButtonLabel()
 * Note:	tall moves label center up a row if tall letters w/o g or y
 * Note:	tall removes lowest 2 lines, don't use with "y" or "g"
 */
static void btn_WriteLabelTextPhase ( file, title, ext, text, tall, sign, in )
     FILE *file;
     char *title, *ext;
     char *text;	/* string to turn into button label */
     int tall;		/* reduce height of icon by 2 (if tall letters) */
     int sign;		/* 0=level, 1=down, -1=up */
     int in;		/* 0=out, 1=in */
{
  int width, height;
  int byte_width;
  int xclip, yclip;
  int cnt;
  int breaklist[64];
  char *image, *shadow;
  unsigned char *icon, *mask;

  char *btn_MakeTextImage(), *btn_MakeLabelMask();
  unsigned char *btn_ByteToBitmap();
  void btn_WriteBitmaps();
  static void btn_WriteTextBreaks(), btn_WriteButtonLabel();


  image = btn_MakeTextImage(text, &width, &height, breaklist, &cnt, sign, in);
  icon = btn_ByteToBitmap(image, width, height, &byte_width);
  shadow = btn_MakeLabelMask(image, width, height);
  mask = btn_ByteToBitmap(shadow, width, height, &byte_width);
  free(image);
  free(shadow);
  if( tall && (sign == 0) )
    height -= 2;
  btn_WriteBitmaps(file, title, ext, icon, mask, width, height, byte_width);
  btn_WriteTextBreaks(file, title, ext, cnt, breaklist, &xclip, &yclip, sign);
  btn_WriteButtonLabel(file, title, ext, byte_width, xclip, yclip);
  free(icon);
  free(mask);
}

/* number of 3 character digits per row of text file */
#define FIRST_ROW 8
#define ROW_WIDTH 15
/*
 * Subroutine:	btn_WriteTextBreaks
 * Purpose:	Write and array of the pixel offsets to each break between
 *		characters in a label text string
 * Called by:	btn_WriteLabelPhase() above
 */
static void
btn_WriteTextBreaks ( file, title, ext, cnt, breaklist, xclip, yclip, sign )
     FILE *file;
     char *title;
     char *ext;
     int cnt;
     int *breaklist;
     int *xclip, *yclip;
     int sign;
{
  int i, row_end;

  if( sign == 0 ) {
    *xclip = cnt;
    *yclip = 0;
  } else if( sign >0 ) {
    *xclip = 0;
    *yclip = cnt;
  } else {
    *xclip = 0;
    *yclip = -cnt;
  }
  (void)fprintf(file, "static int %s_%s_breaks[] = {", title, ext);
  row_end = FIRST_ROW;
  i=0;
  while( i < cnt ) {
    if( row_end > cnt )
	row_end = cnt;
    while( i < row_end ) {
      (void)fprintf(file, " %d", breaklist[i]);
      if( ++i < cnt )
	(void)fprintf(file, ",");
    }
    if( row_end < cnt ) {
      row_end += ROW_WIDTH;
      if( row_end > cnt )
	row_end = cnt;
      (void)fprintf(file, "\n ");
    } else
      (void)fprintf(file, " };\n");
  }
}

/*
 * Subroutine:	btn_WriteButtonLabel
 * Purpose:	Write a ButtonLabel declaration referring to icons and lists
 *		just declared above
 * Called by:	btn_WriteLabelPhase() above
 */
static void btn_WriteButtonLabel ( file, title, ext, byte_width, xclip, yclip )
     FILE *file;
     char *title;
     char *ext;
     int byte_width;
     int xclip, yclip;
{
  (void)fprintf(file,"static ButtonLabel %s_%s = {\n", title, ext);
  (void)fprintf(file,"  (unsigned char *)%s_%s_label,", title, ext);
  (void)fprintf(file," (unsigned char *)%s_%s_mask,\n", title, ext);
  (void)fprintf(file,"  %s_%s_width, %s_%s_height,\n", title, ext, title, ext);
  (void)fprintf(file,"  0.5, 0.5, 0, 0,			/* placement */\n");
  if( (xclip == 0) && (yclip == 0) )
    (void)fprintf(file,"  %1d, 0, 0, NULL };\n", byte_width);
  else
    (void)fprintf(file,"  %1d, %1d, %1d, %s_%s_breaks };\n",
		  byte_width, xclip, yclip, title, ext);

}
