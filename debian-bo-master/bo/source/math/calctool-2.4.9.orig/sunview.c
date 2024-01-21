
/*  @(#)sunview.c 1.13 89/12/21
 *
 *  These are the SunView dependent graphics routines used by calctool.
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
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/selection_svc.h>
#include <suntool/selection_attributes.h>

#define  ICON_SET                       (void) icon_set
#define  MENU_SET                       (void) menu_set
#define  NOTIFY_INTERPOSE_DESTROY_FUNC  (void) notify_interpose_destroy_func
#define  PW_SETCMSNAME                  (void) pw_setcmsname
#define  PW_PUTCOLORMAP                 (void) pw_putcolormap
#define  PW_REVERSEVIDEO                (void) pw_reversevideo
#define  PW_TTEXT                       (void) pw_ttext
#define  PW_VECTOR                      (void) pw_vector
#define  PW_WRITEBACKGROUND             (void) pw_writebackground
#define  SELN_QUERY                     (void) seln_query
#define  WINDOW_DONE                    (void) window_done
#define  WINDOW_SET                     (void) window_set

#define  SMALLFONT   "/usr/lib/fonts/fixedwidthfonts/screen.r.7"
#define  NORMALFONT  "/usr/lib/fonts/fixedwidthfonts/screen.b.14"
#define  BIGFONT     "/usr/lib/fonts/fixedwidthfonts/gallant.r.19"

void func_key_proc() ;

Canvas kcanvas, rcanvas ;
Cursor help_cursor, main_cursor ;
Event *cur_event ;
Frame frame, rframe ;
Icon calctool_icon ;
Menu menus[MAXMENUS] ;
Notify_value destroy_proc() ;
Pixfont *font, *sfont, *nfont, *bfont ;
Pixwin *pw, *cpw, *rcpw ;
Seln_client sel_client ;
Seln_holder holder ;
Seln_rank rank = SELN_PRIMARY ;
Seln_result get_proc(), reply_proc() ;

short help_cursor_array[16] = {
#include "help.cursor"
} ;
mpr_static(help_cursor_pr, 16, 16, 1, help_cursor_array) ;

unsigned short icon_image[] = {
#include "calctool.icon"
} ;
mpr_static(icon_pr, 64, 64, 1, icon_image) ;

short cicon_image[] = {
#include "calctool.color.icon"
} ;
mpr_static(cicon_pr, 64, 64, 8, cicon_image) ;


/*ARGSUSED*/
void
canvas_proc(canvas, event, arg)
Canvas canvas ;
Event *event ;
caddr_t arg ;
{
  cur_event = event ;
  process_event(get_next_event(event)) ;
}


clear_canvas(window, color)
enum can_type window ;
int color ;
{
  int height, width ;
  Canvas ctype ;

  if (window == KEYCANVAS)
    {
      pw = cpw ;
      ctype = frame ;
    }
  else if (window == REGCANVAS)
    {
      pw = rcpw ;
      ctype = rframe ;
    }
  height = (int) window_get(ctype, WIN_HEIGHT) ;
  width = (int) window_get(ctype, WIN_WIDTH) ;
  PW_WRITEBACKGROUND(pw, 0, 0, width, height, PIX_SRC | PIX_COLOR(color)) ;
}


close_frame()
{
  if ((int) window_get(rframe, WIN_SHOW) == TRUE)
    WINDOW_SET(rframe, WIN_SHOW, FALSE, 0) ;
  WINDOW_SET(frame, FRAME_CLOSED, TRUE, 0) ;
  rstate = 0 ;
}


color_area(x, y, width, height, color)
int x, y, width, height, color ;
{
  PW_WRITEBACKGROUND(cpw, x, y, width, height, PIX_SRC | PIX_COLOR(color)) ;
}


create_menu(mtype)       /* Create popup menu for right button press. */
enum menu_type mtype ;
{
  int i ;

  menus[(int) mtype] = menu_create(MENU_FONT, nfont, 0) ;
  for (i = 0; i < MAXREGS; i++)
    {
      switch (mtype)
        {
          case M_ACC    :                              /* Accuracies. */
          case M_EXCH   :                              /* Register exchange. */
          case M_LSHIFT :                              /* Left shift. */
          case M_RCL    :                              /* Register recall. */
          case M_RSHIFT :                              /* Right shift. */
          case M_STO    : MENU_SET(menus[(int) mtype], /* Register store. */
                                   MENU_STRING_ITEM, num_names[i], i+1, 0) ;
                          break ;
          case M_CON    : if (strlen(con_names[i]))    /* Constants. */
                            MENU_SET(menus[(int) mtype],
                                     MENU_STRING_ITEM, con_names[i], i+1, 0) ;
                          break ;
          case M_FUN    : if (strlen(fun_names[i]))    /* Functions. */
                            MENU_SET(menus[(int) mtype],
                                     MENU_STRING_ITEM, fun_names[i], i+1, 0) ;
        }
    }
}


destroy_frame()
{
  WINDOW_DONE(frame) ;
  exit(0) ;
}


destroy_rframe(frame)
Frame frame ;
{
  rstate = 0 ;
  WINDOW_SET(frame, WIN_SHOW, FALSE, 0) ;
}


/*ARGSUSED*/
Notify_value
destroy_proc(client, status)
Notify_client client ;
Destroy_status status ;
{
  exit(0) ;
}


do_menu(mtype)          /* Popup appropriate menu and get value. */
enum menu_type mtype ;
{
  return ((int) menu_show(menus[(int) mtype], kcanvas,
                          canvas_window_event(kcanvas, cur_event), 0)) ;
}


drawline(x1, y1, x2, y2)
int x1, y1, x2, y2 ;
{
  PW_VECTOR(cpw, x1, y1, x2, y2, PIX_SET, 0) ;
}


draw_regs()
{
  make_registers() ;
  WINDOW_SET(rframe, WIN_SHOW, TRUE, 0) ;
}


drawtext(x, y, window, fontno, color, str)
enum font_type fontno ;
enum can_type window ;
int x, y, color ;
char *str ;
{
       if (fontno == SFONT) font = sfont ;
  else if (fontno == NFONT) font = nfont ;
  else if (fontno == BFONT) font = bfont ;
       if (window == KEYCANVAS) pw = cpw ;
  else if (window == REGCANVAS) pw = rcpw ;
  PW_TTEXT(pw, x, y, PIX_SRC | PIX_COLOR(color), font, str) ;
}


/*ARGSUSED*/
void
func_key_proc(client_data, args)
char *client_data ;
Seln_function_buffer *args ;
{
  get_display() ;
}


get_display()     /* The GET function key has been pressed. */
{
  if (seln_acquire(sel_client, SELN_SHELF) == SELN_SHELF)
    {
      if (shelf != NULL) free(shelf) ;
      shelf = malloc((unsigned) strlen(display)) ;
      STRCPY(shelf, display) ;         /* Safely keep copy of display. */
    }
}


Pixfont *
get_font(name)
char *name ;
{
  Pixfont *font ;

  font = pf_open(name) ;
  if (font == NULL) font = pf_default() ;
  if (font == NULL)
    {
      perror("couldn't get the default font.") ;
      exit(1) ;
    }
  return font ;
}


get_next_event(event)
Event *event ;
{
  static char eb[4] ;      /* Event buffer. */
  int i ;

#ifdef   SUN4_KEYBOARD
  char *rpad = "\000\000\000=/*789456123" ;
  char *akeys = "8264" ;
  char *sun4keys = "\015\000\000+-\000\0000\000." ;
#else
  char *rpad = "\000\000\00078945612301=" ;
  char *akeys = "5.31" ;
#endif /*SUN4_KEYBOARD*/

  nextc = event_id(event) ;
  curx = event_x(event) ;
  cury = event_y(event) ;

  if (event_is_button(event))
         if (event_is_down(event) && nextc == MS_LEFT) return(LEFT_DOWN) ;
    else if (event_is_down(event) && nextc == MS_MIDDLE) return(MIDDLE_DOWN) ;
    else if (event_is_down(event) && nextc == MS_RIGHT) return(RIGHT_DOWN) ;
    else if (event_is_up(event) && nextc == MS_LEFT) return(LEFT_UP) ;
    else if (event_is_up(event) && nextc == MS_MIDDLE) return(MIDDLE_UP) ;
    else if (event_is_up(event) && nextc == MS_RIGHT) return(RIGHT_UP) ;

/*  The following code attempts to handle a numeric keypad using the right
 *  function keys. This pad differs on the Sun3 and Sun4 keyboards. There
 *  is a compile-time define which determines which keyboard setup is
 *  looked for.
 */

  if (event_is_ascii(event))
    {

/*  If the last two events were escape and left square bracket.. */

      if (eb[0] && eb[1])
        {
          switch (nextc)
            {

/*  Interpret the arrow keys (if they are set).
 *  R8 = ^[A     R14 = ^[B     R12 = ^[C     R10 = ^[B
 */

              case 'A' :
              case 'B' :
              case 'C' :
              case 'D' : cur_ch = akeys[nextc - 'A'] ;
                         eb[0] = eb[1] = '\0' ;
                         return(KEYBOARD) ;

/*  Interpret the extra keys found on Sun4 keyboards.
 *  These have codes of the form: ^[<int>z where <int> is a number
 *  between 247 and 255.  We're only interested in 5 of these keys.
 *  These are:
 *    ^[253z = +  ^[254z = -  ^[247z = 0  ^[249z = .  ^[250z = Enter
 */
#ifdef SUN4_KEYBOARD
              case '2' : eb[2] = '2' ;
                         break ;

              case '5' : if (eb[0] && eb[1] && eb[2])
                           {
                             eb[3] = '5' ;
                             break ;
                           }
                         else 

/*  Clear event_buf and treat as normal ascii char. */

                           {
                             eb[0] = eb[1] = '\0' ;
                             cur_ch = nextc ;
                             return(KEYBOARD) ;
                           }

              case '0' :
              case '3' :
              case '4' :
              case '7' :
              case '9' : if (eb[0] && eb[1] && eb[2] && eb[3])
                           {
                             cur_ch = sun4keys[nextc - '0'] ;
                             eb[0] = eb[1] = eb[2] = eb[3] = '\0' ;
                             return(KEYBOARD) ;
                           }
                         else if (eb[0] && eb[1] && eb[2] && nextc == '4')
                           {
                             eb[3] = nextc ;
                             break ;
                           }
                         else

/*  Clear event_buf and treat as normal ascii char. */

                           {
                             eb[0] = eb[1] = '\0' ;
                             cur_ch = nextc ;
                             return(KEYBOARD) ;
                           }
#endif /*SUN4_KEYBOARD*/

               default : eb[0] = eb[1] = eb[2] = eb[3] = '\0' ;
            }
        }

/*  If previous events are ^[[ : set buffer   */

      else if (nextc == '[' && eb[0])    /* Check for left square bracket. */
        eb[1] = '[' ;
      else if (nextc == '\033')          /* Check for escape char. */
        eb[0] = '\033' ;
      else 
        {

/*  All the rest of the ASCII characters. */

          eb[0] = eb[1] = '\0' ;
          cur_ch = nextc ;
          return(KEYBOARD) ;
        }
    }

  if (event_is_key_right(event) && event_is_up(event))
    {
      for (i = 1; i < 16; i++)
        if (nextc == KEY_RIGHT(i))
          {
            cur_ch = rpad[i-1] ;
            return(KEYBOARD) ;
          }
    }
  if (nextc == KBD_DONE && down) return(EXIT_WINDOW) ;
  if (nextc == LOC_WINEXIT || nextc == LOC_RGNEXIT) return(EXIT_WINDOW) ;
  if (nextc == LOC_WINENTER || nextc == LOC_RGNENTER) return(ENTER_WINDOW) ;
  if (nextc == WIN_REPAINT) return(CFRAME_REPAINT) ;
  if ((nextc == KEY_LEFT(6)) & event_is_up(event)) return(PUT_ON_SHELF) ;
  if ((nextc == KEY_LEFT(8)) && event_is_up(event)) return(TAKE_FROM_SHELF) ;
  return(LASTEVENTPLUSONE) ;
}


Seln_result
get_proc(buffer)
Seln_request *buffer ;
{
  issel = 0 ;
  if (*buffer->requester.context == 0)
    {
      if (buffer == (Seln_request *) NULL ||
          *((Seln_attribute *) buffer->data) != SELN_REQ_CONTENTS_ASCII)
        return ;
      selection = buffer->data + sizeof(Seln_attribute) ;
      *buffer->requester.context = 1 ;
    }
  else selection = buffer->data ;
  issel = 1 ;
}


handle_selection()  /* Handle the GET function key being pressed. */
{
  char context = 0 ;

  holder = seln_inquire(rank) ;
  if (holder.state == SELN_NONE) return ;
  SELN_QUERY(&holder, get_proc, &context, SELN_REQ_CONTENTS_ASCII, 0, 0) ;
}


init_fonts()
{
  bfont = get_font(BIGFONT) ;
  nfont = get_font(NORMALFONT) ;
  nfont_width = nfont->pf_defaultsize.x ;
  sfont = get_font(SMALLFONT) ;
}


init_ws_type()
{
  if (getenv("WINDOW_PARENT") == NULL)
    {
      FPRINTF(stderr,"%s: Not a native SunView window\n", progname) ;
      return -1 ;
    }
  gtype = SVIEW ;
  return 0 ;
}


load_colors()      /* Create and load calctool color map. */
{
  Pixwin *frame_pw ;
  char colorname[CMS_NAMESIZE] ;
  u_char red[CALC_COLORSIZE], green[CALC_COLORSIZE], blue[CALC_COLORSIZE] ;

  iscolor = (cpw->pw_pixrect->pr_depth == 8) ? 1 : 0 ;
  SPRINTF(colorname, "%s%D", CALC_COLOR, getpid()) ;
  PW_SETCMSNAME(cpw, colorname) ;

  calc_colorsetup(red, green, blue) ;
  PW_PUTCOLORMAP(cpw, 0, CALC_COLORSIZE, red, green, blue) ;
  if (inv_video) PW_REVERSEVIDEO(cpw, 0, CALC_COLORSIZE) ;

  if (iscolor)
    {
      frame_pw = (Pixwin *) window_get(frame, WIN_PIXWIN, 0) ;
      PW_SETCMSNAME(frame_pw, colorname) ;
      PW_PUTCOLORMAP(frame_pw, 0, CALC_COLORSIZE, red, green, blue) ;
    }
}


make_frames(argc, argv)
int argc ;
char *argv[] ;
{
  frame = window_create((Window) 0, FRAME,
                        FRAME_ICON,       calctool_icon,
                        FRAME_SHOW_LABEL, FALSE,
                        FRAME_NO_CONFIRM, TRUE,
                        FRAME_ARGS,       argc, argv,
                        0) ;
  sel_client = seln_create(func_key_proc, reply_proc, (char *) 0) ;
  NOTIFY_INTERPOSE_DESTROY_FUNC(frame, destroy_proc) ;
  rframe = window_create(frame, FRAME,
                         FRAME_SHOW_LABEL, FALSE,
                         FRAME_NO_CONFIRM, TRUE,
                         FRAME_DONE_PROC,  destroy_rframe,
                         WIN_X,            TWIDTH+15,
                         WIN_Y,            0,
                         WIN_SHOW,         FALSE,
                         WIN_WIDTH,        TWIDTH,
                         WIN_HEIGHT,       200,
                         WIN_FONT,         nfont,
                         0) ;
 
}


make_icon()
{
  calctool_icon = icon_create(ICON_WIDTH, ICONWIDTH,
                              ICON_IMAGE, &icon_pr,
                              0) ;
}


make_items()
{
  main_cursor = window_get(kcanvas, WIN_CURSOR) ;

  if (iscolor)
    {
      calctool_icon = (Icon) window_get(frame, FRAME_ICON) ;
      ICON_SET(calctool_icon, ICON_IMAGE, &cicon_pr, 0) ;
      WINDOW_SET(frame, FRAME_ICON, calctool_icon, 0) ;
    }

  help_cursor = cursor_create(CURSOR_XHOT,  0,
                              CURSOR_YHOT,  0,
                              CURSOR_OP,    PIX_SRC | PIX_DST,
                              CURSOR_IMAGE, &help_cursor_pr, 0) ;
  window_fit(frame) ;
}


make_subframes()
{
  rcanvas = window_create(rframe, CANVAS, 0) ;
  kcanvas = window_create(frame, CANVAS,
                          CANVAS_RETAINED,     FALSE,
                          WIN_EVENT_PROC,      canvas_proc,
                          WIN_WIDTH,           TWIDTH,
                          WIN_HEIGHT,          THEIGHT + DISPLAY,
                          WIN_FONT,            nfont,
                          0) ;

  WINDOW_SET(kcanvas, WIN_CONSUME_KBD_EVENTS, WIN_ASCII_EVENTS,
                      WIN_LEFT_KEYS, WIN_TOP_KEYS, WIN_RIGHT_KEYS,
                      WIN_UP_EVENTS, 0, 0) ;
  WINDOW_SET(kcanvas, WIN_IGNORE_PICK_EVENT, LOC_MOVE, 0) ;
  cpw = canvas_pixwin(kcanvas) ;
  rcpw = canvas_pixwin(rcanvas) ;
}


/*ARGSUSED*/
Seln_result
reply_proc(item, context, length)
Seln_attribute item ;
Seln_replier_data *context ;
int length ;
{
  int size ;
  char *destp ;

  switch (item)
    {
      case SELN_REQ_CONTENTS_ASCII :

             if (context->context == NULL)
               {
                 if (shelf == NULL) return(SELN_DIDNT_HAVE) ;
                 context->context = shelf ;
               }
             size = strlen(context->context) ;
             destp = (char *) context->response_pointer ;
             STRCPY(destp, context->context) ;
             destp += size ;
             while ((int) destp % 4 != 0) *destp++ = '\0' ;
             context->response_pointer = (char **) destp ;
             *context->response_pointer++ = 0 ;
             return(SELN_SUCCESS) ;

      case SELN_REQ_YIELD :

             *context->response_pointer++ = (char *) SELN_SUCCESS ;
             return(SELN_SUCCESS) ;

      case SELN_REQ_BYTESIZE :

             if (shelf == NULL) return(SELN_DIDNT_HAVE) ;
             *context->response_pointer++ = (char *) strlen(shelf) ;
             return(SELN_SUCCESS) ;

      case SELN_REQ_END_REQUEST : return(SELN_SUCCESS) ;

      default                   : return(SELN_UNRECOGNIZED) ;
    }
}


set_cursor(type)
int type ;
{
  switch (type)
    {
      case HELPCURSOR : WINDOW_SET(kcanvas, WIN_CURSOR, help_cursor, 0) ;
                        break ;
      case MAINCURSOR : WINDOW_SET(kcanvas, WIN_CURSOR, main_cursor, 0) ;
    }
}


start_tool()
{
  window_main_loop(frame) ;
}


toggle_reg_canvas()
{
  rstate = !rstate ;
  if (rstate) draw_regs() ;
  else WINDOW_SET(rframe, WIN_SHOW, FALSE, 0) ;
}
