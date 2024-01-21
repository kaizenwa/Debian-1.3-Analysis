/*LINTLIBRARY*/

/*  @(#)tty.c 1.12 89/12/21
 *
 *  These are the dumb tty dependent graphics routines used by calctool.
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
#include <signal.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include "calctool.h"
#include "color.h"
#include "extern.h"

char *CE, *CL, *CM, *SE, *SO ;

char *getenv(), *tgetstr(), *tgoto() ;
int destroy_frame(), outc() ;

struct sgttyb in_new, in_old ;


SIGRET
cleanup()
{
  destroy_frame() ;
}


/*ARGSUSED*/
clear_canvas(canvas, color)
enum can_type canvas ;
int color ;
{
  int i,j ;

  if (canvas == REGCANVAS)
    for (i = 0; i < 24; i++)
      {
        tputs(tgoto(CM, i, 43), 1, outc) ;
        do_move(43, i) ;
        do_clr_eol() ;
      }
  else for (i = 0; i < 24; i++)
    {
      do_move(0, i) ;
      for (j = 0; j < 42; j++) outc(' ') ;
    }
  do_move(41, 1) ;
}


close_frame() {}     /* This option does nothing with termcap. */


/*ARGSUSED*/
color_area(x, y, width, height, color)    /* Color an area - null routine. */
int x, y, width, height, color ;
{}


/*ARGSUSED*/
create_menu(mtype)     /* Create popup menu - null routine. */
enum menu_type mtype ;
{}


destroy_frame()
{
  int i ;

  tputs(CL, 1, outc) ; 
  for (i = 0; i < 24; i++)
    {
      do_move(0, i) ;
      do_clr_eol() ;
    }
  do_move(0, 0) ;
  SIGNAL(SIGINT, SIG_IGN) ;
  IOCTL(0, TIOCSETP, &in_old) ;
  exit(0) ;
}


do_clr_eol()                  /* Clear to the end of the line. */
{
  tputs(CE, 1, outc) ;
}


/*ARGSUSED*/
do_menu(mtype)                /* Popup appropriate menu (null routine). */
enum menu_type mtype ;
{
  return 0 ;                  /* To make lint happy. */
}


do_move(x, y)                 /* Move to character position (x, y). */
int x, y ;
{
  tputs(tgoto(CM, x, y), 1, outc) ;
}


do_standend()                 /* Finish inverted area. */
{
  tputs(SE, 1, outc) ;
}


do_standout()                 /* Start inverted area. */
{
  tputs(SO, 1, outc) ;
}


drawline(x1, y1, x2, y2)
int x1, y1, x2, y2 ;
{
  double dx ;
  int i, offset, x, y ;

  if (x1)
    {
      dx = (float) (x1 - BBORDER) / (BWIDTH + BGAP) ;
      if (dx == (int) dx) offset = 1 ;
      else offset = 6 ;

      x = ((x1 - BBORDER) / (BWIDTH + BGAP) * 7) + offset ;
      y = ((y1 - BBORDER) / (BHEIGHT + BGAP) * 3) + 5 ;
      do_move(x, y) ;
      if (x1 == x2)
        {
          if ((y2 - y1) != BHEIGHT) return ;
          outc('+') ;
          for (i = 0; i < 2; i++)
            {
              do_move(x, y+i+1) ;
              outc('|') ;
            }
          do_move(x, y+3) ;
          outc('+') ;
        }
      else
        {
          if ((x2 - x1) != BWIDTH) return ;
          outc('+') ;
          for (i = 0; i < 4; i++)
            {
              do_move(x+i+1, y) ;
              outc('-') ;
            }
          do_move(x+5, y) ;
          outc('+') ;
        }
    }
  else
    {
      do_move(0, 4) ;
      for (i = 0; i < 42; i++) outc('-') ;
    }
}


draw_regs()
{
}


/*ARGSUSED*/
drawtext(tx, ty, window, fontno, color, str)
enum can_type window ;
enum font_type fontno ;
int tx, ty, color ;
char *str ;
{
  char key[5] ;
  int i, invert, sps ;

  invert = 0 ;
  if (window == REGCANVAS)                   /* Register window. */
    {
      tx = 45 ;
      ty = ((ty / 15 - 1) * 2) + 1 ;
    }
  else if (window == KEYCANVAS && tx == 5)   /* Help screen. */
    {
      tx = 1 ;
      ty = ty / 15 ;
    }
  else if (window == KEYCANVAS && ty < DISPLAY)
    {
      if (color == WHITE)
        for (i = 0; i < strlen(str); i++) str[i] = ' ' ;

      if (ty == (DISPLAY-3))              /* Base, trig and op items. */
        {
          tx = tx * 42 / (TWIDTH) + 1 ;
          ty = 3 ;
        }
      else                                /* Display item. */
        {
          tx = 41 - strlen(str) ;
          ty = 1 ;
        }
    }
  else                                    /* Button values. */
    {
      tx = ((tx - BBORDER) / (BWIDTH + BGAP) * 7) + 2 ;
      for (sps = 0, i = 0; i < 4; i++)
        if (str[i] == ' ') sps++ ;
      ty = (ty - 34) / 30 ;
      ty = (ty - (ty % 2)) * 3 / 2 + 6 + (ty % 2) ;
      switch (sps)
        {
          case 0 :
          case 1 :
          case 4 : STRCPY(key, str) ;
                   break ;
          case 2 :
          case 3 : key[0] = ' ' ;
                   STRNCPY(&key[1], str, 3) ;
        }
      key[4] = '\0' ;
      STRCPY(str, key) ;
      if (ty % 3) invert = 1 ;
    }
  do_move(tx, ty) ;
  if (window == REGCANVAS) do_clr_eol() ;
  if (invert) do_standout() ;
  outstr(str) ;
  if (invert) do_standend() ;
  do_move(41, 1) ;
}


get_display()            /* No GET key; null routine. */
{}


get_next_event()         /* Only events possible are keyboard ones. */
{
  char c ;
  int reply ;

  for (;;)
    {
      READ(0, &c, 1) ;
      cur_ch = c & 0177 ;
      return(KEYBOARD) ;
    }
}


handle_selection()       /* There is no PUT function key with termcap. */
{}


init_fonts()             /* No fonts with the termcap version. */
{
  nfont_width = 8 ;
}


init_ws_type()           /* Check if terminal capable of termcap output. */
{
  char bp[1024], termtype[MAXLINE] ;
  int i ;
  static char buf[100] ;
  char *area = buf ;

  if (getenv("TERM") != NULL) STRCPY(termtype, getenv("TERM")) ;
  if (tgetent(bp, termtype) != 1) return 1 ;
  if ((CL = tgetstr("cl", &area)) == (char *) 0) return 1 ;
  if ((CM = tgetstr("cm", &area)) == (char *) 0) return 1 ;
  if ((CE = tgetstr("ce", &area)) == (char *) 0) return 1 ;
  if ((SO = tgetstr("so", &area)) == (char *) 0) return 1 ;
  if ((SE = tgetstr("se", &area)) == (char *) 0) return 1 ;
  for (i = 0; i < 24; i++)
    {
      do_move(0, i) ;
      do_clr_eol() ;
    }
  gtype = TTY ;
  return 0 ;
}


load_colors()
{
  iscolor = 0 ;          /* No colors in the termcap implementation. */
}


/*ARGSUSED*/
make_frames(argc, argv)
int argc ;
char *argv[] ;
{
  int i ;

  SIGNAL(SIGINT, cleanup) ;

  IOCTL(0, TIOCGETP, &in_old) ;        /* Setup standard input. */
  in_new = in_old ;
  in_new.sg_flags |= RAW ;
  in_new.sg_flags &= ~(ECHO | CRMOD) ;
  IOCTL(0, TIOCSETP, &in_new) ;

  setbuf(stdout, (char *) NULL) ;

  tputs(CL, 1, outc) ;
  do_move(0, 4) ;
  for (i = 0; i < 41; i++) outc('-') ;
  for (i = 0; i < 24; i++)
    {
      do_move(42, i) ;
      if (i == 4) outc('+') ;
      else outc('|') ;
    }
}


make_icon() {}          /* There is no icon with termcap. */


make_items()
{
  do_repaint() ;        /* Redraw the calctool canvas[es]. */
}


make_subframes() {}     /* There are no subframes with termcap. */


outc(c)                 /* Output the next character to the screen. */
int c ;
{
  PUTC(c, stdout) ;
}


outstr(str)
char *str ;
{
  int i ;

  for (i = 0; i < strlen(str); i++) PUTC(str[i], stdout) ;
}


/*ARGSUSED*/
set_cursor(type)        /* There are no cursors with termcap. */
int type ;
{}


start_tool()
{
  while (1)
    process_event(get_next_event()) ;
}


toggle_reg_canvas()      /* Show or clear memory register area. */
{
  rstate = !rstate ;
  clear_canvas(REGCANVAS, WHITE) ;
  if (rstate) make_registers() ;
}
