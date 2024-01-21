
/*  @(#)xview.c 1.11 89/12/21
 *
 *  These are the XView dependent graphics routines used by calctool.
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
#include "calctool.h"
#include "color.h"
#include "extern.h"
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cms.h>
#include <xview/font.h>
#include <xview/cursor.h>
#include <xview/sel_svc.h>
#include <xview/sel_attrs.h>

#define  MENU_SET                       (void) menu_set
#define  NOTIFY_DO_DISPATCH             (void) notify_do_dispatch
#define  NOTIFY_INTERPOSE_DESTROY_FUNC  (void) notify_interpose_destroy_func
#define  PW_SETCMSNAME                  (void) pw_setcmsname
#define  PW_PUTCOLORMAP                 (void) pw_putcolormap
#define  PW_TTEXT                       (void) pw_ttext
#define  PW_WRITEBACKGROUND             (void) pw_writebackground
#define  SELN_QUERY                     (void) seln_query
#define  XV_SET                         (void) xv_set
#define  WINDOW_DONE                    (void) window_done

void func_key_proc() ;
int menu_proc() ;

Canvas kcanvas, rcanvas ;
Event *cur_event ;
Frame frame, rframe ;
Icon calctool_icon ;
Menu menus[MAXMENUS] ;
Notify_value destroy_proc() ;
Canvas_paint_window pw, cpw, rcpw ;
Seln_client sel_client ;
Seln_holder holder ;
Seln_rank rank = SELN_PRIMARY ;
Seln_result get_proc(), reply_proc() ;
Xv_cmsdata cms_data ;
Xv_Cursor help_cursor, main_cursor ;
Xv_font bfont, font, nfont, sfont ;

char colorname[CMS_NAMESIZE] ;
u_char red[CALC_COLORSIZE], green[CALC_COLORSIZE], blue[CALC_COLORSIZE] ;

enum menu_type curmenu ;   /* Current menu (if any) being processed. */
int started ;              /* Set just before window is displayed. */

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
canvas_event_proc(canvas, event, arg)
Canvas canvas ;
Event *event ;
caddr_t arg ;
{
  if (!started) return ;
  cur_event = event ;
  process_event(get_next_event(event)) ;
}


clear_canvas(window, color)
enum can_type window ;
int color ;
{
  int height,width ;
  Canvas ctype ;

  if (window == KEYCANVAS)
    {
      pw = cpw ;
      ctype = kcanvas ;
    } 
  else if (window == REGCANVAS)
    {
      pw = rcpw ;
      ctype = rcanvas ;
    }
  height = (int) xv_get(ctype, XV_HEIGHT) ;
  width = (int) xv_get(ctype, XV_WIDTH) ;
  PW_WRITEBACKGROUND(pw, 0, 0, width, height, PIX_SRC | PIX_COLOR(color)) ;
}


close_frame()
{
  if ((int) xv_get(rframe, XV_SHOW) == TRUE)
    XV_SET(rframe, XV_SHOW, FALSE, 0) ;
  XV_SET(frame, FRAME_CLOSED, TRUE, 0) ;
  rstate = 0 ;
}


color_area(x, y, width, height, color)
int x, y, width, height, color ;
{
  PW_WRITEBACKGROUND(cpw, x, y, width, height, PIX_SRC | PIX_COLOR(color)) ;
}


create_menu(mtype)    /* Create popup menu for right button press. */
enum menu_type mtype ;
{
  int i ;

  menus[(int) mtype] = xv_create(XV_NULL, MENU_COMMAND_MENU,
                                 MENU_NOTIFY_PROC, menu_proc,
                                 0) ;
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
                                   MENU_STRING_ITEM, num_names[i], i+1,
                                   0) ;
                          break ;
          case M_CON    : if (strlen(con_names[i]))    /* Constants. */
                            MENU_SET(menus[(int) mtype],
                                     MENU_STRING_ITEM, con_names[i], i+1,
                                     0) ;
                          break ;
          case M_FUN    : if (strlen(fun_names[i]))    /* Functions. */
                            MENU_SET(menus[(int) mtype],
                                     MENU_STRING_ITEM, fun_names[i], i+1,
                                     0) ;
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
  XV_SET(frame, XV_SHOW, FALSE, 0) ;
}


/*ARGSUSED*/
Notify_value
destroy_proc(client, status)
Notify_client client ;
Destroy_status status ;
{
  exit(0) ;
}


/*  This routine works rather strangely. Because menu_show does not block
 *  under XView, do_menu cannot return a valid selection. So the menu
 *  selection handling has been moved to the notification procedure, and
 *  the appropriate code in graphics.c has been isolated into a separate
 *  routine. All in all, a bit of a kludge.
 */

do_menu(mtype)      /* Popup appropriate menu. */
enum menu_type mtype ;
{
  curmenu =  mtype ;
  menu_show(menus[(int) mtype], kcanvas, cur_event, 0) ;
  return(0) ;
}


drawline(x1, y1, x2, y2)
int x1, y1, x2, y2 ;
{
  (void) pw_vector(cpw, x1, y1, x2, y2, PIX_SET, 0) ;
}


draw_regs()
{
  make_registers() ;
  XV_SET(rframe, XV_SHOW, TRUE, 0) ;
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

  if (color == BLACK)
    (void) pw_text(pw, x, y, PIX_SRC | PIX_DST, font, str) ;
  else if (color == WHITE)
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


get_next_event(event)
Event *event ;
{
  static char eb[4] ;      /* Event buffer. */
  int i ;

#ifdef   SUN4_KEYBOARD
  char *rpad = "\000\000\000=/*789456123" ;
#else
  char *rpad = "\000\000\00078945612301=" ;
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

  if (event_is_ascii(event) && event_is_down(event))
    {

/*  With the Sun4 keyboard, the right function keypad generates key_right
 *  events except for the function keys which are not in the range R1-R15.
 *  These return ASCII values which are:
 *     INS (0) = 0,  DEL (.) = 127, Enter = 13, - = 45, + = 43.
 *
 *  These are correct except for INS and DEL. INS can be remapped to ASCII
 *  48 (the digit zero), but there is no easy way to handle DEL remapping,
 *  If you remap DEL to ASCII 46 (decimal point), this also remaps the
 *  Delete key value. For now, this value is left alone.
 */

#ifdef   SUN4_KEYBOARD
      if (nextc == 0) nextc = 48 ;            /* Remap INS (0) key. */
#endif /*SUN4_KEYBOARD*/

/*  All the rest of the ASCII characters. */
                              
      cur_ch = nextc ;    
      return(KEYBOARD) ;
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
  if (nextc == LOC_WINEXIT) return(EXIT_WINDOW) ;
  if (nextc == LOC_WINENTER) return(ENTER_WINDOW) ;

  if (nextc == WIN_RESIZE) return(CFRAME_REPAINT) ;
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
  sfont = (Xv_font) xv_find(0,          FONT,
                            FONT_STYLE, FONT_STYLE_NORMAL,
                            FONT_SIZE,  10,
                            0) ;

  nfont = (Xv_font) xv_find(0,          FONT,
                            FONT_STYLE, FONT_STYLE_NORMAL,
                            FONT_SIZE,  12,
                            0) ;
  nfont_width = 9 ;

  bfont = (Xv_font) xv_find(0,          FONT,
                            FONT_STYLE, FONT_STYLE_NORMAL,
                            FONT_SIZE,  18,
                            0) ;
}


init_ws_type()
{
  gtype = XVIEW ;
  started = 0 ;               /* Kludge to correctly handle repaints. */
  return 0 ;
}


load_colors()      /* Create and load color map - done in make_subframes. */
{
}


make_frames(argc, argv)
int argc ;
char *argv[] ;
{
  int x, y ;

  xv_init(XV_INIT_ARGS, argc, argv, 0) ;
  frame = xv_create(0, FRAME,
                    FRAME_ICON,        calctool_icon,
                    FRAME_SHOW_LABEL,  FALSE,
                    FRAME_NO_CONFIRM,  TRUE,
                    XV_WIDTH,          TWIDTH,
                    XV_HEIGHT,         THEIGHT + DISPLAY,
                    0) ;
  iscolor = ((int) xv_get(frame, WIN_DEPTH) > 1) ? 1 : 0 ;
  sel_client = seln_create(func_key_proc, reply_proc, (char *) 0) ;
  NOTIFY_INTERPOSE_DESTROY_FUNC(frame, destroy_proc) ;
  rframe = xv_create(frame, FRAME,
                     FRAME_SHOW_LABEL, FALSE,
                     FRAME_NO_CONFIRM, TRUE,
                     FRAME_DONE_PROC,  destroy_rframe,
                     XV_X,             TWIDTH + 15,
                     XV_Y,             0,
                     XV_SHOW,          FALSE,
                     XV_WIDTH,         TWIDTH,
                     XV_HEIGHT,        200,
                     XV_FONT,          nfont,
                     0) ;
 
}


make_icon()
{
  calctool_icon = xv_create(0, ICON,
                            XV_WIDTH,   ICONWIDTH,
                            ICON_IMAGE, &icon_pr,
                            0) ;
}


make_items()
{
  main_cursor = xv_get(kcanvas, WIN_CURSOR) ;

  if (iscolor)
    {
      calctool_icon = (Icon) xv_get(frame, FRAME_ICON) ;
      XV_SET(calctool_icon,
             ICON_IMAGE, &cicon_pr,
             WIN_CMS_NAME, colorname,
             0) ;
      XV_SET(frame, FRAME_ICON, calctool_icon, 0) ;
    }

  help_cursor = xv_create(NULL, CURSOR,
                          CURSOR_XHOT,  0,
                          CURSOR_YHOT,  0,
                          CURSOR_OP,    PIX_SRC | PIX_DST,
                          CURSOR_IMAGE, &help_cursor_pr,
                          0) ;
}


make_subframes()
{
  rcanvas = xv_create(rframe, CANVAS, 0) ;

  SPRINTF(colorname, "%s%D", CALC_COLOR, getpid()) ;
  calc_colorsetup(red, green, blue) ;

  cms_data.type = XV_STATIC_CMS ;
  cms_data.size = CALC_COLORSIZE ;
  cms_data.rgb_count = CALC_COLORSIZE ;
  cms_data.index = 0 ;
  cms_data.red = red ;
  cms_data.green = green ;
  cms_data.blue = blue ;

  kcanvas = xv_create(frame, CANVAS,
                      CANVAS_RETAINED,     FALSE,
                      OPENWIN_AUTO_CLEAR,  FALSE,
                      XV_WIDTH,            TWIDTH,
                      XV_HEIGHT,           THEIGHT + DISPLAY,
                      XV_FONT,             nfont,
                      CANVAS_PAINTWINDOW_ATTRS,
                          WIN_CMS_NAME, colorname,
                          WIN_CMS_DATA, &cms_data,
                          WIN_CONSUME_EVENTS,
                            MS_LEFT, MS_MIDDLE, MS_RIGHT,
                            WIN_ASCII_EVENTS, KBD_USE, KBD_DONE,
                            LOC_WINENTER, LOC_WINEXIT,
                            WIN_LEFT_KEYS, WIN_TOP_KEYS, WIN_RIGHT_KEYS,
                            0,
                          WIN_IGNORE_EVENTS,
                            LOC_MOVE, LOC_DRAG,
                            0,
                          WIN_EVENT_PROC, canvas_event_proc,
                          0,
                      0) ;

  rcpw = canvas_paint_window(rcanvas) ;
  cpw = canvas_paint_window(kcanvas) ;
}


menu_proc(menu, menu_item)
Menu menu ;
Menu_item menu_item ;
{
  int choice ;

  choice = (int) menu_get(menu_item, MENU_VALUE) ;
  if (choice) handle_menu_selection(curmenu, choice) ;
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
      case HELPCURSOR : XV_SET(kcanvas, WIN_CURSOR, help_cursor, 0) ;
                        break ;
      case MAINCURSOR : XV_SET(kcanvas, WIN_CURSOR, main_cursor, 0) ;
    }
}


start_tool()
{
  started = 1 ;
  xv_main_loop(frame) ;
}


toggle_reg_canvas()
{
  rstate = !rstate ;
  if (rstate) draw_regs() ;
  else XV_SET(rframe, XV_SHOW, FALSE, 0) ;
}
