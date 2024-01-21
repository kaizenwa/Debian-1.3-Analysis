
/*  @(#)graphics.c 1.12 89/12/21
 *
 *  These are the independent graphics routines used by calctool.
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


but_text(row, column, portion, state)
int row, column, portion ;
enum but_state state ;
{
  enum font_type butfont ;
  int i, n ;

  n = row*BCOLS*2 + column*2 + portion ;
  if (buttons[n].color == GREY) return ;
  get_label(n) ;
  for (spaces = 0, i = 0; i < strlen(pstr); i++)
    if (pstr[i] == ' ') spaces++ ;
  x = chxoff[spaces] ;
  y = (n & 1) ? 40 : 18 ;
  if (spaces == 3)  y += 4 ;
  butfont = (spaces == 3) ? BFONT : NFONT ;
  if (state == NORMAL)
    color = (!iscolor & portion) ? WHITE : BLACK ;
  if (state == INVERTED)
    color = (portion) ? BLACK : WHITE ;
  drawtext(column*(BWIDTH+BGAP)+BBORDER+x,
       DISPLAY+row*(BHEIGHT+BGAP)+BBORDER+y, KEYCANVAS, butfont, color, pstr) ;
}


do_repaint()     /* Redraw the calctool canvas[es]. */
{
  make_canvas(0) ;
}


drawbox(x, y, width, height)
int x, y, width, height ;
{
  drawline(x, y, x+width, y) ;
  drawline(x, y, x, y+height) ;
  drawline(x, y+height, x+width, y+height) ;
  drawline(x+width, y, x+width, y+height) ;
}


draw_button(row, column, portion, state)
int row, column, portion ;
enum but_state state ;
{
  int n ;

  n = row*BCOLS*2 + column*2 + portion ;
  if (!portion)
    {
      color = (iscolor) ? buttons[n].color : WHITE ;
      drawbox(column*(BWIDTH+BGAP)+BBORDER,
              DISPLAY+row*(BHEIGHT+BGAP)+BBORDER, BWIDTH, BHEIGHT) ;
      fillbox(column*(BWIDTH+BGAP)+BBORDER+1,
              DISPLAY+row*(BHEIGHT+BGAP)+BBORDER+1, 42, 50, 1, color) ;
    }
  else
    { 
      drawbox(column*(BWIDTH+BGAP)+BBORDER+5,
              DISPLAY+row*(BHEIGHT+BGAP)+BBORDER+26, 34, 21) ;
      color = (iscolor) ? buttons[n].color : BLACK ;
      fillbox(column*(BWIDTH+BGAP)+BBORDER+6,
              DISPLAY+row*(BHEIGHT+BGAP)+BBORDER+27, 32, 19, 1, color) ;
    }
  but_text(row, column, portion, state) ;
}


fillbox(x, y, width, height, boundry, color)
int x, y, width, height, boundry, color ;
{
  if (boundry)
    {
      color_area(x, y, width, height, WHITE) ;
      color_area(x+1, y+1, width-2, height-2, color) ;
    } 
  else color_area(x, y, width, height, color) ;
}


get_menu_value()     /* Get menu value if valid right mouse press. */
{
  int i, n, val ;

  n = row*BCOLS*2 + column*2 + portion ;
  for (i = 0; i < MAXMENUS; i++)
    if (buttons[n].value == validmenu[i])
      {
        val = do_menu((enum menu_type) i) ;
        if (val) handle_menu_selection(i, val) ;
        break ;
      }
}


grey_buttons(base)     /* Grey out numeric buttons depending upon base. */
enum base_type base ;
{
  char val ;
  int column, i, n, portion, row ;

  if (gtype == TTY) return ;
  for (i = 0; i < 16; i++)
    {
      val = digits[i] ;
      for (n = 0; n < TITEMS; n++)
        if (val == buttons[n].value) break ;           
      if (i < basevals[(int) base])          
        {                 
          if (i < 10) buttons[n].color = LBLUE ;
          else buttons[n].color = PINK ;
        }  
      else buttons[n].color = GREY ;
      row = n / (BCOLS*2) ;
      column = (n - (row*BCOLS*2)) / 2 ;
      portion = n & 1 ;
      draw_button(row, column, portion, NORMAL) ;
    }                    
}


handle_down_event(type)
int type ;
{
  x = curx ;
  y = cury ;
  if (!down)
    {
      if (pending_op == '?')
        {
          down = type ;
          return ;
        }
      for (row = 0; row < BROWS; row++)
        for (column = 0; column < BCOLS; column++)
          if ((x > (column*(BWIDTH+BGAP)+BBORDER)) &&
              (x < (column*(BWIDTH+BGAP)+BBORDER+BWIDTH)) &&
              ((y - DISPLAY) > (row*(BHEIGHT+BGAP)+BBORDER)) &&
              ((y - DISPLAY) < (row*(BHEIGHT+BGAP)+BBORDER+BHEIGHT)))
            {
              portion = (y - DISPLAY - BBORDER -
                         (row*(BHEIGHT+BGAP))) / (BHEIGHT/2) ;
              inv_but(row, column, portion, INVERTED) ;
              down = type ;
              return ;
            }
    } 
}


handle_menu_selection(menu, item)   /* Process right button menu selection. */
int menu, item ;
{
  pending = validmenu[menu] ;
  current = num_names[item-1][0] ;
  do_pending() ;
  down = 0 ;
  inv_but(row, column, portion, NORMAL) ;
}


inv_but(row, column, portion, state)
int row, column, portion ;
enum but_state state ;
{
  int n ;
 
  n = row*BCOLS*2 + column*2 + portion ;
  if (pending_op != '?')
    {
      if (state == NORMAL)
        if (iscolor) color = buttons[n].color ;
        else color = (portion) ? BLACK : WHITE ;
      if (state == INVERTED)
        color = (portion) ? WHITE : BLACK ;
      fillbox(column*(BWIDTH+BGAP)+BBORDER+6,
              DISPLAY+row*(BHEIGHT+BGAP)+BBORDER+5+(portion*22),
              32, 19, portion, color) ;
      but_text(row, column, portion, state) ;
    }
}


make_canvas(toggle)
int toggle ;
{
  if (toggle) tstate = !tstate ;
  color = (iscolor) ? GREY : WHITE ;
  clear_canvas(KEYCANVAS, color) ;
  if (iscolor) color_area(0, 0, TWIDTH, DISPLAY, WHITE) ;
  drawline(0, DISPLAY, TWIDTH, DISPLAY) ;
  for (row = 0; row < BROWS; row++)
    for (column = 0; column < BCOLS; column++)
      for (portion = 0; portion < 2; portion++)
        draw_button(row, column, portion, NORMAL) ;

  set_item(BASEITEM,base_str[(int) base]) ;
  set_item(DISPLAYITEM, display) ;
  set_item(NUMITEM, dtype_str[(int) dtype]) ;
  set_item(OPITEM, items[(int) OPITEM].text) ;
  set_item(TTYPEITEM,ttype_str[(int) ttype]) ;
  set_item(HYPITEM, (hyperbolic) ? "HYP " : "    ") ;
  set_item(INVITEM, (inverse) ? "INV " : "    ") ;
  make_registers() ;
}


make_menus()      /* Create the popup menus used by the graphics versions. */
{

/*  There are nine popup menus. These are associated with the following keys:
 *
 *  ACC  - range of possible accuracies (0 - 9).
 *
 *  CON  - constant values plus associated comments, if present.
 *
 *  EXCH - list of register numbers (0 - 9).
 *
 *  FUN  - function definitions plus associated comments, if present.
 *
 *  HELP - contains all the keys in the calculator.
 *
 *   <   - range of possible left shift values (0 - 9).
 *
 *   >   - range of possible right shift values (0 - 9).
 *
 *  RCL  - list of register numbers (0 - 9).
 *
 *  STO  - list of register numbers (0 - 9).
 */

  create_menu(M_ACC) ;     /* Accuracies. */
  create_menu(M_CON) ;     /* Constant definitions. */
  create_menu(M_EXCH) ;    /* Register exchange. */
  create_menu(M_FUN) ;     /* Function definitions. */
  create_menu(M_LSHIFT) ;  /* Left shift. */
  create_menu(M_RSHIFT) ;  /* Right shift. */
  create_menu(M_RCL) ;     /* Register recall. */
  create_menu(M_STO) ;     /* Register store. */
}


make_registers()           /* Calculate memory register frame values. */
{
  char line[MAXLINE] ;     /* Current memory register line. */
  int n ;

  if (!rstate) return ;
  clear_canvas(REGCANVAS, WHITE) ;
  drawtext(15, 20, REGCANVAS, NFONT, BLACK, "Memory Registers") ;
  for (n = 0; n < MAXREGS; n++)
    {
      SPRINTF(line, "%1d   %s", n, make_number(mem_vals[n])) ;
      drawtext(15, 40+15*n, REGCANVAS, NFONT, BLACK, line) ;
    }
}


process_event(type)       /* Process this event. */
int type ;
{
  int i, n ;

  n = row*BCOLS*2 + column*2 + portion ;
  switch (type)
    {
      case CFRAME_REPAINT  : make_canvas(0) ;
                             set_item(BASEITEM, base_str[(int) base]) ;
                             set_item(TTYPEITEM, ttype_str[(int) ttype]) ;
                             break ;
      case EXIT_WINDOW     : if (pending_op != '?')
                               if (n >= 0 && n <= (NOBUTTONS*2))
                                 {
                                   draw_button(row, column, portion, NORMAL) ;
                                   if (!portion)
                                     draw_button(row, column, 1, NORMAL) ;
                                 }
                             down = 0 ;
                             break ;
      case KEYBOARD        : nextc = cur_ch ;
                             for (n = 0; n < TITEMS; n++)
                               if (nextc == buttons[n].value) break ;
                             if (n == TITEMS) return ;
                             row = n / (BCOLS * 2) ;
                             column = (n - (row * BCOLS * 2)) / 2 ;
                             portion = n & 1 ;
                             process_item(n) ;
                             break ;
      case LEFT_DOWN       :
      case MIDDLE_DOWN     :
      case RIGHT_DOWN      : handle_down_event(type) ;
                             if (type == RIGHT_DOWN) get_menu_value() ;
                             break ;
      case LEFT_UP         :
      case MIDDLE_UP       :
      case RIGHT_UP        : x = curx ;
                             y = cury ;
                             if ((type == LEFT_UP && down == LEFT_DOWN) ||
                                 (type == MIDDLE_UP && down == MIDDLE_DOWN) ||
                                 (type == RIGHT_UP && down == RIGHT_DOWN))
                               {
                                 if (pending_op != '?' && n <= (NOBUTTONS*2))
                                   inv_but(row, column, portion, NORMAL) ;
                                 down = 0 ;
                                 if (n >= 0 && n <= (NOBUTTONS*2))
                                   process_item(n) ;
                               }
                             break ;
      case RFRAME_REPAINT  : make_registers() ;
                             break ;
      case TAKE_FROM_SHELF : handle_selection() ;
                             if (issel)
                               for (i = 0 ; i < strlen(selection); i++)
                                 for (n = 0; n < TITEMS; n++)
                                   if (selection[i] == buttons[n].value)
                                     {
                                       process_item(n) ;
                                       break ;
                                     }
                             break ;
      case PUT_ON_SHELF    : get_display() ;
                             break ;
      case DIED            : exit(0) ;
    }                           
}


set_item(itemno, str)
enum item_type itemno ;
char *str ;
{
  enum font_type font ;
  char *old_text ;
  int x, y ;
 
  old_text = items[(int) itemno].text ;
  if (itemno == DISPLAYITEM)
    x = 5+(MAX_DIGITS - strlen(old_text))*nfont_width ;
  else x = items[(int) itemno].x ;
  y = items[(int) itemno].y ;
  font = items[(int) itemno].font ;
  old_text = items[(int) itemno].text ;
  drawtext(x, y, KEYCANVAS, font, WHITE, old_text) ;

  if (itemno == DISPLAYITEM) x = 5+(MAX_DIGITS - strlen(str))*nfont_width ;

  drawtext(x, y, KEYCANVAS, font, BLACK, str) ;
  STRCPY(items[(int) itemno].text, str) ;
}
