#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	wrfeel.c (Write Feel)
 * Subroutine:	btn_WriteButtonFeel()		returns: void
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
 * Subroutine:	btn_WriteButtonFeel
 * Purpose:	write structure declaraation to describe button behavior
 */
void btn_WriteButtonFeel ( file, title, button )
     FILE *file;
     char *title;
     BtnSpec *button;
{
  int cnt;
  static void btn_WriteCodeLine(), btn_WriteHexLine(), btn_WriteDataLine();

  for( cnt=0; (cnt < OPTION_LIMIT) && (button->mask[cnt] != 0); cnt++ );
  (void)fprintf(file, "static ButtonFeel %s_feel = {\n", title);
  (void)fprintf(file, "  \"%s\", %d,\n", title, cnt);
  btn_WriteCodeLine(file, title, button->code, cnt);
  btn_WriteHexLine(file, button->mask, cnt);
  btn_WriteHexLine(file, button->ref, cnt);
  btn_WriteDataLine(file, button->data, DATA_LIMIT);
}

/*
 * Subroutine:	btn_WriteCodeLine
 */
static void btn_WriteCodeLine ( file, title, code, cnt )
     FILE *file;
     char *title;
     int *code;
     int cnt;
{
  int i;
  static void btn_StampCode();

  (void)fprintf(file, "  { ");
  i=0;
  while( i < cnt ) {
    btn_StampCode(file, title, code[i]);
    if( ++i < cnt )
      (void)fprintf(file, ", ");
  }
  (void)fprintf(file, " },\n");
}

/*
 * Subroutine:	btn_WriteHexLine
 */
static void btn_WriteHexLine ( file, val, cnt )
     FILE *file;
     int *val;
     int cnt;
{
  int i;

  (void)fprintf(file, "  {");
  i=0;
  while( i < cnt ) {
    (void)fprintf(file, " 0x%04x", val[i]);
    if( ++i < cnt )
      (void)fprintf(file, ",");
  }
  (void)fprintf(file, " },\n");
}

/*
 * Subroutine:	btn_WriteDataLine
 */
static void btn_WriteDataLine ( file, data, cnt )
     FILE *file;
     int *data;
     int cnt;
{
  int i;

  (void)fprintf(file, "  {");
  i=0;
  while( i < cnt ) {
    (void)fprintf(file, " %d", data[i]);
    if( ++i < cnt )
      (void)fprintf(file, ",");
  }
  (void)fprintf(file, " } };\n");
}

/*
 * Subroutine:	btn_StampCode
 */
static void btn_StampCode ( file, title, code )
     FILE *file;
     char *title;
     int code;
{
  char *stamp;
  switch( code ) {
  case BTNNoOp:
    stamp = "NoOp";
    break;
  case BTNFlash:
    stamp = "Flash";
    break;
  case BTNOneShot:
    stamp = "OneShot";
    break;
  case BTNToggle:
    stamp = "Toggle";
    break;
  case BTNWhile:
    stamp = "While";
    break;
  case BTNCoWhile:
    stamp = "CoWhile";
    break;
  case BTNMode:
    stamp = "Mode";
    break;
  case BTNCoMode:
    stamp = "CoMode";
    break;
  default:
    (void)fprintf(stderr,"Unknown BTN response code: %d in %s\n",code,title);
    exit(0);
  }
  (void)fprintf(file, "BTN%s", stamp);    
}
