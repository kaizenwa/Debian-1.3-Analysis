
/*  @(#)news.c 1.9 89/12/21
 *
 *  These are the NeWS dependent graphics routines used by calctool.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <strings.h>
#include "calctool.h"
#include "color.h"
#include "extern.h"
#include <sys/types.h>

extern FILE *PostScript ;
extern FILE *PostScriptInput ;


clear_canvas(canvas, color)
enum can_type canvas ;
int color ;
{
  char cantype ;

       if (canvas == KEYCANVAS) cantype = 'K' ;
  else if (canvas == REGCANVAS) cantype = 'R' ;
  FPRINTF(PostScript,"%d %cC PSClearCanvas\n", color, cantype) ;
  FFLUSH(PostScript) ;
}


close_frame()
{
  FPRINTF(PostScript, "PSCloseFrame\n") ;
  FFLUSH(PostScript) ;
}


color_area(x, y, width, height, color)
int x, y, width, height, color ;
{
  FPRINTF(PostScript, "%d %d %d %d %d PSColorArea\n",
          color, x, width, height, y) ;
  FFLUSH(PostScript) ;
}


create_menu(mtype)     /* Create popup menu for right button press. */
enum menu_type mtype ;
{
  int i ;

  FPRINTF(PostScript, "/Menu%1d [\n", (int) mtype) ;
  for (i = 0; i < MAXREGS; i++)
    {
      switch (mtype)
        {
          case M_ACC    :                              /* Accuracies. */
          case M_EXCH   :                              /* Register exchange. */
          case M_LSHIFT :                              /* Left shift. */
          case M_RCL    :                              /* Register recall. */
          case M_RSHIFT :                              /* Right shift. */
          case M_STO    :                              /* Register store. */
                          FPRINTF(PostScript, "(%s)", num_names[i]) ;
                          break ;
          case M_CON    :                              /* Constants. */
                          FPRINTF(PostScript, "(%s)", con_names[i]) ;
                          break ;
          case M_FUN    :                              /* Functions. */
                          FPRINTF(PostScript, "(%s)", fun_names[i]) ;
                          break ;
        }
      FPRINTF(PostScript, " { %1d typedprint }\n", (int) mtype) ;
    }
  FPRINTF(PostScript, "] /new DefaultMenu send def\n") ;
  FFLUSH(PostScript) ;
}


destroy_frame()
{
  exit(0) ;
}


do_menu(mtype)      /* Popup appropriate menu and get value. */
enum menu_type mtype ;
{
  int mval ;

  FPRINTF(PostScript, "{ /ParentMenu Menu%1d def } ClientMenu send\n",
                      (int) mtype) ;
  FFLUSH(PostScript) ;
  pscanf(PostScriptInput, "%d", &mval) ;
  return(mval) ;
}


drawline(x1, y1, x2, y2)
int x1, y1, x2, y2 ;
{
  FPRINTF(PostScript, "%d %d %d %d PSDrawLine\n", x1, x2, y1, y2) ;
  FFLUSH(PostScript) ;
}


draw_regs()
{
  FPRINTF(PostScript, "PSDrawRegs\n") ;
  FFLUSH(PostScript) ;
}


drawtext(x, y, window, fontno, color, str)
enum can_type window ;
enum font_type fontno ;
int x, y, color ;
char *str ;
{
  int i ;
  char font, fonttype[6], line[MAXLINE] ;
 
       if (fontno == SFONT) STRCPY(fonttype, "SFont") ;
  else if (fontno == NFONT) STRCPY(fonttype, "NFont") ;
  else if (fontno == BFONT) STRCPY(fonttype, "BFont") ;
       if (window == KEYCANVAS) font = 'K' ;
  else if (window == REGCANVAS) font = 'R' ;
  line[0] = '\0' ;
  for (i = 0; i < strlen(str); i++)
    switch (str[i])
      {
        case '\\' : STRCAT(line,"\\\\") ;
                    break ;
        case '('  : STRCAT(line,"\\(") ;
                    break ;
        case ')'  : STRCAT(line,"\\)") ;
                    break ;
        default   : STRNCAT(line, &str[i], 1) ;
      }
  FPRINTF(PostScript, "(%s) %d %cCHeight %d ", line, x, font, y) ;
  FPRINTF(PostScript, "%s %d %cC PSMakeText\n", fonttype, color, font) ;
  FFLUSH(PostScript) ;
}


get_display()         /* GET function key was pressed. */
{
}


get_next_event()
{
  int type ;

  FFLUSH(PostScript) ;
  if (pscanf(PostScriptInput, "%d", &type) == EOF) destroy_frame() ;
  switch (type)
    {
      case KEYBOARD    : pscanf(PostScriptInput, "%d", &cur_ch) ;
      case LEFT_DOWN   :
      case MIDDLE_DOWN :
      case LEFT_UP     :
      case MIDDLE_UP   : pscanf(PostScriptInput, "%d%d", &curx, &cury) ;
    }
  return(type) ;
}


handle_selection()      /* Handle the GET function key being pressed. */
{
}


init_fonts()
{
  FPRINTF(PostScript, "PSInitFonts\n") ;
  FFLUSH(PostScript) ;
  nfont_width = 9 ;
}


init_ws_type()
{
  gtype = NEWS ;
  if (ps_open_PostScript() < 0) return -1 ;
  if (send_ps_file(NEWSFILE) == -1)
    {
      FCLOSE(PostScript) ;
      return(-1) ;
    }
  FFLUSH(PostScript) ;
  if (ferror(PostScript))
    {
      FCLOSE(PostScript) ;
      return(-1) ;
    }
  FPRINTF(PostScript, "PSIsColor\n") ;
  FFLUSH(PostScript) ;
  pscanf(PostScriptInput, "%d", &iscolor) ;
  FPRINTF(PostScript, "PSInitialise\n") ;
  FFLUSH(PostScript) ;
  return(0) ;
}


load_colors()      /* Create and load calctool color map. */
{
  u_char red[CALC_COLORSIZE], green[CALC_COLORSIZE], blue[CALC_COLORSIZE] ;
  int i ;

  calc_colorsetup(red, green, blue) ;
  FPRINTF(PostScript, "%d PSMakeColorTable\n", CALC_COLORSIZE) ;
  for (i = 0; i < CALC_COLORSIZE; i++)
    FPRINTF(PostScript, "%d %d %d %d PSLoadColor\n",
                         i, blue[i], green[i], red[i]) ;
  FFLUSH(PostScript) ;
}


/*ARGSUSED*/
make_frames(argc, argv)
int argc ;
char *argv[] ;
{
  FPRINTF(PostScript, "%d %d %d %d %d %d %d PSMakeFrames\n",
                     wx, wy, TWIDTH, DISPLAY+THEIGHT,
                     ix, iy, iconic) ;
  FFLUSH(PostScript) ;
}


make_icon()
{
  int depth, i, j, k, n ;
  char line[MAXLINE], name[MAXLINE], v[16][5] ;
  FILE *fin ;

  if (iscolor) STRCPY(name, "calctool.color.icon") ;
  else STRCPY(name, "calctool.icon") ;
  if ((fin = fopen(name, "r")) == NULL)
    {
      FPRINTF(stderr, "%s: can't open %s.\n", progname, name) ;
      exit(1) ;
    }
  depth = (iscolor) ? 8 : 1 ;
  FPRINTF(PostScript, "/calctoolIcon %d %d %1d { } { <\n",
                      ICONWIDTH, ICONHEIGHT, depth) ;
  FGETS(line, MAXLINE, fin) ;
  FGETS(line, MAXLINE, fin) ;
  if (iscolor)
    {
      for (i = 0; i < 64; i++)
        for (j = 0; j < 4; j++)
          {
            FGETS(line, MAXLINE, fin) ;
            for (k = 0; k < 16; k++)
              {
                if (!(k % 2)) STRNCPY(v[k], &line[(k/2)*7+3], 2) ;
                else STRNCPY(v[k], &line[(k/2)*7+5], 2) ;
                v[k][2] = '\0' ;
              }    
                 if (j == 0 || j == 1) n = 16 ;
            else if (j == 2) n = 10 ;
            if (j != 3)
              {
                for (k = 0; k < n; k++)
                  FPRINTF(PostScript, "%.2s ", v[k]) ;
                FPRINTF(PostScript, "\n") ;
              }
          }
    }
  else
    {
      for (i = 0; i < 32; i++)
        {
          FGETS(line, MAXLINE, fin) ;
          for (j = 0; j < 8; j++)
            {
              STRNCPY(v[j], &line[j*7+3], 4) ;
              v[j][4] = '\0' ;
            }
          FPRINTF(PostScript, "%.4s %.4s %.4s %.4s %.4s %.4s\n",
                              v[0], v[1], v[2], v[4], v[5], v[6]) ;
        }
    }
  FPRINTF(PostScript, "> } buildimage def\n") ;
  FFLUSH(PostScript) ;
  FCLOSE(fin) ;
}


make_items()       /* Null routine. */
{}


make_subframes()   /* Null routine. */
{}


send_ps_file(fname)
char *fname ;
{
  FILE *stream ;
  int c ;
 
  if ((stream = fopen(fname,"r")) == NULL) return -1 ;
  while ((c = getc(stream)) != EOF) PUTC(c, PostScript) ;
  FCLOSE(stream) ;
  return 0 ;
}


set_cursor(type)
int type ;
{
  FPRINTF(PostScript, "%d PSSetCursor\n", type) ;
  FFLUSH(PostScript) ;
}


start_tool()
{
  while (1)
    process_event(get_next_event()) ;
}


toggle_reg_canvas()

{
  rstate = !rstate ;
  FPRINTF(PostScript, "%d PSToggleRegCanvas\n", rstate) ;
  FFLUSH(PostScript) ;
}
