
/*  @(#)mgr.c 1.11 89/12/21
 *
 *  These are the MGR dependent graphics routines used by calctool.
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
#include <signal.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include "dump.h"
#include "term.h"
#include "calctool.h"
#include "color.h"
#include "extern.h"

#define  S_FONT      1     /* Font descriptors. */
#define  N_FONT      2
#define  B_FONT      3

#define  ICONIC      0     /* States that the calctool display can be in. */
#define  JUST_KEYS   1
#define  SHOW_ALL    2

#define  PR_ICON     1     /* Descriptor for closed icon image. */

#define  SMALLFONT   "sail6x8r"
#define  NORMALFONT  "cour7x14b"
#define  BIGFONT     "gal12x20r"

#define  MENU_COUNT  (sizeof(menus) / (sizeof(struct menu_entry) * MAXMENUS))

struct menu_entry menus[MAXMENUS][MAXREGS] ;

char fontname[MAXLINE] ;      /* Full pathname of the small font. */

int local_mode ;         /* Used by load_icon for correct line mode. */
int mgr_infd ;           /* MGR input connection file descriptor. */
int mgr_outfd ;          /* MGR output connection file descriptor. */

short icon_image[] = {
#include "calctool.icon"
} ;

#ifdef NO_4_3SELECT
int fullmask ;               /* Full mask of file descriptors to check on. */ 
int readmask ;               /* Readmask used in select call. */ 
#else 
fd_set fullmask ;            /* Full mask of file descriptors to check on. */
fd_set readmask ;            /* Readmask used in select call. */
#endif /*NO_4_3SELECT */
 

SIGRET
clean(code)
int code ;
{
  m_bitdestroy(1) ;
  m_pop() ;
  m_ttyreset() ;
  m_clear() ;
  exit(code) ;
}


/*ARGSUSED*/
clear_canvas(canvas, color)
enum can_type canvas ;
int color ;
{
  m_func(B_CLEAR) ;
  if (canvas == KEYCANVAS)
    m_bitwrite(0, 0, TWIDTH+10, THEIGHT+DISPLAY+10) ;
  else if (canvas == REGCANVAS)
    m_bitwrite(TWIDTH+15, 0, TWIDTH+10, THEIGHT+DISPLAY+10) ;
}


close_frame()
{
  reshape(ICONIC) ;
  m_clearmode(M_ACTIVATE) ;
}


color_area(x, y, width, height, color)
int x, y, width, height, color ;
{
  if (color == BLACK)
    {
      m_func(B_COPY) ;
      m_bitwrite(x, y, width, height) ;
    }
}


create_menu(mtype)     /* Create popup menu for right button press. */
enum menu_type mtype ;
{
  int i ;
  char istr[3] ;       /* String representation for action value. */

  for (i = 0; i < MAXREGS; i++)
    {
      SPRINTF(istr, "%1d\r", i) ;
      switch (mtype)
        {
          case M_ACC    :                              /* Accuracies. */
          case M_EXCH   :                              /* Register exchange. */
          case M_LSHIFT :                              /* Left shift. */
          case M_RCL    :                              /* Register recall. */
          case M_RSHIFT :                              /* Right shift. */
          case M_STO    :                              /* Register store. */
                          menus[(int) mtype][i].value = num_names[i] ;
                          break ;
          case M_CON    :                              /* Constants. */
                          menus[(int) mtype][i].value = con_names[i] ;
                          break ;
          case M_FUN    :                              /* Functions. */
                          menus[(int) mtype][i].value = fun_names[i] ;
        }
      menus[(int) mtype][i].action = num_names[i] ;
    }
  menu_load((int) mtype, MENU_COUNT, menus[(int) mtype]) ;
}


destroy_frame()
{
  clean(0) ;
}


do_menu(mtype)      /* Popup appropriate menu and get value. */
enum menu_type mtype ;
{
  m_selectmenu2((int) mtype) ;
}


drawline(x1, y1, x2, y2)
int x1, y1, x2, y2 ;
{
  m_func(B_COPY) ;
  m_line(x1, y1, x2, y2) ;
}


draw_regs()
{
  reshape(SHOW_ALL) ;
  clear_canvas(REGCANVAS, WHITE) ;
  drawline(TWIDTH, 0, TWIDTH, THEIGHT+DISPLAY+10) ;
  make_registers() ;
}


/*ARGSUSED*/
drawtext(x, y, window, fontno, color, str)
enum font_type fontno ;
enum can_type window ;
int x, y, color ;
char *str ;
{
  int i ;

       if (fontno == SFONT) m_font(S_FONT) ;
  else if (fontno == NFONT) m_font(N_FONT) ;
  else if (fontno == BFONT) m_font(B_FONT) ;
       if (window == REGCANVAS) x += TWIDTH + 15 ;
  m_func(B_XOR) ;
  i = strlen(str)-1 ;
  while (str[i] == ' ' && i) str[i--] = '\0' ;
  m_stringto(0, x, y+4, str) ;
  m_movecursor(2500, 2500) ;
}


get_display()
{
}


get_next_event()
{
  int c ;
  static struct timeval tval = { 0, 0 } ;

  m_flush() ;
  for (;;)
    {
      readmask = fullmask ;
#ifdef NO_4_3SELECT
      SELECT(32, &readmask, 0, 0, &tval) ;
      if (readmask && (1 << mgr_infd))
#else
      SELECT(FD_SETSIZE, &readmask, (fd_set *) 0, (fd_set *) 0, &tval) ;
      if (FD_ISSET(mgr_infd, &readmask))
#endif /*NO_4_3SELECT*/
        {
          if ((c = m_getchar()) == EOF)
            {
              clearerr(m_termin) ;
              continue ;
            }
          get_mouse(&curx, &cury) ;
          switch (c)
            {
              case '\030' : return(MIDDLE_DOWN) ;
              case '\031' : return(MIDDLE_UP) ;
              case '\032' : return(LEFT_DOWN) ;
              case '\033' : return(LEFT_UP) ;
              case '\034' : return(DIED) ;           /* Window destroyed. */
              case '\035' : if (iconic) iconic = 0 ;
              case '\036' : if (rstate) reshape(SHOW_ALL) ;
                            else reshape(JUST_KEYS) ;
              case '\037' : return(CFRAME_REPAINT) ; /* Window redrawn. */
              default     : cur_ch = c ;
                            return(KEYBOARD) ;
            }
        }
    }    
}


handle_selection()
{
}


init_fonts()
{
  char path[MAXLINE] ;     /* Directory path for font files. */

#ifdef MGRHOME
  STRCPY(path, MGRHOME) ;
#else
  STRCPY(path, "/usr/mgr") ;
#endif /*MGRHOME*/

  SPRINTF(fontname, "%s/font/%s", path, SMALLFONT) ;
  m_loadfont(SFONT, fontname) ;

  SPRINTF(fontname, "%s/font/%s", path, NORMALFONT) ; 
  m_loadfont(NFONT, fontname) ;
  nfont_width = 9 ;

  SPRINTF(fontname, "%s/font/%s", path, BIGFONT) ; 
  m_loadfont(BFONT, fontname) ;

}


init_ws_type()
{
  m_setup(M_FLUSH) ;     /* Setup I/O; turn on flushing. */
  m_push(P_BITMAP | P_MENU | P_EVENT | P_FONT | P_FLAGS | P_POSITION) ;
  mgr_infd = fileno(m_termin) ;
  mgr_outfd = fileno(m_termout) ;

  SIGNAL(SIGHUP, clean) ;
  SIGNAL(SIGINT, clean) ;
  SIGNAL(SIGTERM, clean) ;
  m_ttyset() ;
  m_setraw() ;
  m_setmode(M_NOWRAP) ;
  m_setmode(M_ABS) ;
  m_setmode(ACTIVATE) ;
  m_clearmode(M_NOINPUT) ;
  m_func(B_COPY) ;

  gtype = MGR ;
  return 0 ;
}


load_colors()      /* Hardwired to a monochrome version. */
{
  iscolor = 0 ;
}


load_icon(pixrect, ibuf)
int pixrect ;
short ibuf[256] ;
{
  int size ;

  IOCTL(mgr_outfd, TIOCLGET, &local_mode) ;
  local_mode |= LLITOUT ;
  IOCTL(mgr_outfd, TIOCLSET, &local_mode) ;

  size = ICONHEIGHT * (((64 + 15) &~ 15) >> 3) ;
  m_bitldto(ICONWIDTH, ICONHEIGHT, 0, 0, pixrect, size) ;
  m_flush() ;
  WRITE(mgr_outfd, (char *) ibuf, size) ;

  local_mode &= ~LLITOUT ;
  IOCTL(mgr_outfd, TIOCLSET, &local_mode) ;
}


/*ARGSUSED*/
make_frames(argc, argv)
int argc ;
char *argv[] ;
{
#ifdef NO_4_3SELECT
  fullmask = 1 << mgr_infd ;
#else
  FD_ZERO(&fullmask) ;
  FD_SET(mgr_infd, &fullmask) ;
#endif /*NO_4_3SELECT*/

  m_setevent(BUTTON_1, "\030") ;    /* Right mouse button depressed. */
  m_setevent(BUTTON_1U, "\031") ;   /* Right mouse button released. */

  m_setevent(BUTTON_2, "\032") ;    /* Middle mouse button depressed. */
  m_setevent(BUTTON_2U, "\033") ;   /* Middle mouse button released. */

  m_setevent(ACTIVATE, "\034") ;    /* Window has been activated. */
  m_setevent(DESTROY, "\035") ;     /* Check for window being destroyed. */
  m_setevent(RESHAPE, "\036") ;     /* Check for window being reshaped. */
  m_setevent(REDRAW, "\037") ;      /* Check for window being redrawn. */
}


make_icon()
{
  load_icon(PR_ICON, icon_image) ;
}


make_items()
{
  do_repaint() ;               /* Redraw the calctool canvas[es]. */
  m_movecursor(2500, 2500) ;   /* Move character cursor offscreen. */
}


make_subframes()
{
  m_font(N_FONT) ;          /* Use the default font. */
  reshape(JUST_KEYS) ;
  m_clear() ;               /* Clear calctool window. */
}


reshape(type)
int type ;
{
  int x, y, w, h ;      /* Position and size of calctool window. */

  get_size(&x, &y, &w, &h) ;
  switch (type)
    {
      case ICONIC    : m_shapewindow(x, y, ICONWIDTH+10, ICONHEIGHT+10) ;
                       m_clear() ;
                       m_bitcopyto(0, 0, ICONWIDTH, ICONHEIGHT,
                                   0, 0, 0, PR_ICON) ;
                       break ;
      case JUST_KEYS : m_shapewindow(x, y, TWIDTH+10, THEIGHT+DISPLAY+10) ;
                       break ;
      case SHOW_ALL  : m_shapewindow(x, y, 2*TWIDTH+25, THEIGHT+DISPLAY+10) ;
    }
  m_movecursor(2500, 2500) ;
}


/*ARGSUSED*/
set_cursor(type)   /* Doesn't appear to be any way to set the cursor. */
int type ;
{
}


start_tool()
{
  while (1)
    process_event(get_next_event()) ;
}


toggle_reg_canvas()      /* Show or clear memory register area. */
{
  rstate = !rstate ;
  if (rstate) draw_regs() ;
  else reshape (JUST_KEYS) ;
}
