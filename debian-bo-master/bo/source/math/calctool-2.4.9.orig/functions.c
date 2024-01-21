
/*  @(#)functions.c 1.7 89/11/01
 *
 *  This file contains the seperate functions used by calctool,
 *  whenever a calculator button is pressed.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Basic algorithms, copyright (c) Ed Falk.
 *                Sun Microsystems, Mountain View.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <errno.h>
#include <strings.h>
#include <math.h>
#include "calctool.h"
#include "color.h"
#include "extern.h"

extern double acos(), acosh(), asin(), asinh(), atan(), atanh() ;
extern double cos(), cosh(), exp(), fabs(), log(), log10(), pow() ;
extern double sin(), sinh(), sqrt(), tan(), tanh() ;
extern double addition(), subtraction(), multiply(), division() ;

BOOLEAN ibool() ;
double setbool() ;


do_accuracy()     /* Set display accuracy. */
{
  if (current >= '0' && current <= '9')
    {
      accuracy = char_val(current) ;
      make_registers() ;
    }
}


do_base()    /* Change the current base setting. */
{
  switch (current)
    {
      case 'B' : base = BIN ;
                 break ;
      case 'O' : base = OCT ;
                 break ;
      case 'D' : base = DEC ;
                 break ;
      case 'H' : base = HEX ;
    }
  grey_buttons(base) ;
  set_item(BASEITEM, base_str[(int) base]) ;
  show_display(disp_val) ;
  if (rstate) make_registers() ;
}


do_calculation()      /* Perform arithmetic calculation and display result. */
{
  if (current == '=' && old_cal_value == '=')
    if (new_input) result = last_input ;
    else disp_val = last_input ;

  if (current != '=' && old_cal_value == '=') cur_op = '?' ;
  switch (cur_op)
    {
      case CCTRL('c') :                              /* cos. */
      case CCTRL('s') :                              /* sin. */
      case CCTRL('t') :                              /* tan. */
      case '?'        : result = disp_val ;          /* Undefined. */
                        break ;
      case '+'        :                              /* Addition. */
                        result = addition(result, disp_val) ;
                        break ;
      case '-'        :                              /* Subtraction. */
                        result = subtraction(result, disp_val) ;
                        break ;
      case 'x'        :                              /* Multiplication. */
                        result = multiply(result, disp_val) ;
                        break ;
      case '/'        :                              /* Division. */
                        result = division(result, disp_val) ;
                        break ;
      case '%'        : result *= disp_val * 0.01 ;              /* % */
                        break ;
      case 'Y'        : result = pow(result, disp_val) ;         /* y^x */
                        break ;
      case '&'        : result = setbool(ibool(result) &         /* AND */
                                         ibool(disp_val)) ;
                        break ;
      case '|'        : result = setbool(ibool(result) |         /* OR */
                                         ibool(disp_val)) ;
                        break ;
      case '^'        : result = setbool(ibool(result) ^         /* XOR */
                                         ibool(disp_val)) ;
                        break ;
      case 'n'        : result = setbool(~(ibool(result) ^       /* XNOR */
                                           ibool(disp_val))) ;
                        break ;
      case '='        : break ;                                  /* Equals. */
    }
  show_display(result) ;
  if (!(current == '=' && old_cal_value == '=')) last_input = disp_val ;

  disp_val = result ;
  if (current != '=') cur_op = current ;
  old_cal_value = current ;
  new_input = key_exp = 0 ;
}


do_clear()       /* Clear the calculator display and re-initialise. */
{
  clear_display() ;
  if (error) set_item(DISPLAYITEM, "") ;
  initialise() ;
}


do_constant()
{
  if (current >= '0' && current <= '9')
    {
      disp_val = con_vals[char_val(current)] ;
      show_display(disp_val) ;
    }
}


do_delete()     /* Remove the last numeric character typed. */
{
  if (strlen(display)) display[strlen(display)-1] = '\0' ;

/*  If we were entering a scientific number, and we have backspaced over
 *  the exponent sign, then this reverts to entering a fixed point number.
 */

  if (key_exp && !(index(display, '+')))
    {
      key_exp = 0 ;
      display[strlen(display)-1] = '\0' ;
      set_item(OPITEM, "") ;
    }

  set_item(DISPLAYITEM, display) ;
  disp_val = convert_display() ;    /* Convert input to a number. */
}


do_exchange()         /* Exchange display with memory register. */
{
  double temp ;

  if (current >= '0' && current <= '9')
    {
      temp = disp_val ;
      disp_val = mem_vals[char_val(current)] ;
      mem_vals[char_val(current)] = temp ;
      make_registers() ;
    }
}


do_expno()           /* Get exponential number. */
{
  if (!new_input)
    {
      STRCPY(display, "1.0 +") ;
      new_input = pointed = 1 ;
      toclear = 0 ;
    }
  else if (!pointed)
    {
      STRNCAT(display, ". +", 3) ;
      pointed = 1 ;
    }
  else if (!key_exp) STRNCAT(display, " +", 2) ;
  key_exp = 1 ;
  exp_posn = index(display, '+') ;
  set_item(DISPLAYITEM, display) ;
  disp_val = convert_display() ;       /* Convert input to a number. */
}


double
do_factorial(val)     /* Calculate the factorial of val. */
double val ;
{
  double a ;
  int i ;

  a = val ;
  if (val == (int) val && val > 0.0)   /* Only for positive integers. */
    {
      i = val ;
      a = 1.0 ;
      while ((i > 0) && (a != HUGE))
        {
          a = multiply(a, (float) i) ;
          i-- ;
        }
    }
  return (a) ;
}


do_function()      /* Perform a user defined function. */
{
  int fno, i, n ;

  pending = 0 ;
  if (current >= '0' && current <= '9')
    {
      fno = char_val(current) ;
      for (i = 0 ; i < strlen(fun_vals[fno]); i++)
        for (n = 0; n < TITEMS; n++)
          if (fun_vals[fno][i] == buttons[n].value)
            {
              process_item(n) ;
              break ;
            }
    }
}


do_help()         /* Show online help facility. */
{
  char help_str[MAXLINE], nextline[MAXLINE], *p ;
  int n, y ;

  if (pending_op == '?')                        /* HELP. */
    {     
      if (ishelp) ishelp++ ;
      pending_op = '=' ;
      make_canvas(0) ;
      set_cursor(MAINCURSOR) ;
    }     
  else    
    {
      clear_canvas(KEYCANVAS, WHITE) ;
      y = 20 ;
      if (!ishelp)
        drawtext(5, y, KEYCANVAS, NFONT, BLACK, "No help file found.") ;
      else
        {
          for (n = 0; n < TITEMS; n++)
            if (current == buttons[n].value) break ;
          color = (iscolor) ? buttons[n].color : WHITE ;
          clear_canvas(KEYCANVAS, color) ;
          SPRINTF(help_str, "_%s_\n", buttons[n].str) ;
          rewind(hfd) ;
          y = 15 ;
          p = fgets(nextline, BUFSIZ, hfd) ;
          if (EQUAL(p, "_calctool.help_\n"))
            {
              while (p = fgets(nextline, BUFSIZ, hfd))
                if (*p == '_' && EQUAL(p, help_str)) break ;
              if (!p) drawtext(5, y, KEYCANVAS, NFONT, BLACK,
                               "No help for this item.") ;
              for (;;)
                {
                  FGETS(nextline, BUFSIZ, hfd) ;
                  if (nextline[0] == '_') break ;
                  nextline[strlen(nextline)-1] = '\0' ;
                  drawtext(5, y, KEYCANVAS, NFONT, BLACK, nextline) ;
                  y += 15 ;
                }
            }    
          else drawtext(5, y, KEYCANVAS, NFONT, BLACK,
                        "Invalid help file given.") ;
        }
      drawtext(5, y+25, KEYCANVAS, NFONT, BLACK,
               "Click LEFT or press any valid key.") ;
      pending_op = '?' ;
      return ;
    }
}


do_immediate()
{
  switch (current)
    {
      case '[' : disp_val = setbool(ibool(disp_val)) ;          /* &32 */
                 break ;
      case ']' : disp_val = setbool(ibool(disp_val) & 0xffff) ; /* &16 */
                 break ;
      case '{' : disp_val = exp(disp_val) ;                     /* e^x */
                 break ;
      case '}' : disp_val = exp(LN10*disp_val) ;                /* 10^x */
                 break ;
      case 'N' : disp_val = log(disp_val) ;                     /* ln */
                 break ;
      case 'G' : disp_val = log10(disp_val) ;                   /* log */
                 break ;
      case 'S' : disp_val = sqrt(disp_val) ;                    /* SQRT */
                 break ;
      case '~' : disp_val = setbool(~ibool(disp_val)) ;         /* NOT */
                 break ;
      case 'R' : disp_val = division(1.0, disp_val) ;           /* 1/x */
                 break ;
      case '!' : disp_val = do_factorial(disp_val) ;            /* x! */
                 break ;
      case '@' : disp_val = multiply(disp_val, disp_val) ;      /* x^2 */
                 break ;
      case 'C' : if (key_exp)                                   /* CHS */
                   {
                     if (*exp_posn == '+') *exp_posn = '-' ;
                     else                  *exp_posn = '+' ;
                     set_item(DISPLAYITEM, display) ;
                     disp_val = convert_display() ;
                     key_exp = 0 ;
                   }
                 else disp_val = -disp_val ;
    }
  show_display(disp_val) ;
}


do_keys()      /* Display/undisplay the calctool key values. */
{
  make_canvas(1) ;
}


do_number()
{
  int n ;
  static int maxvals[4] = {1, 7, 9, 15} ;

  n = current - '0' ;
  if (base == HEX && current >= 'a' && current <= 'f')
    n = current - 'a' + 10 ;
  if (n > maxvals[(int) base]) return ;

  if (toclear)
    {
      SPRINTF(display, "%c", current) ;
      toclear = 0 ;
    }
  else if (strlen(display) < disp_length[(int) base])
    STRNCAT(display, &current, 1) ;
  set_item(DISPLAYITEM, display) ;
  disp_val = convert_display() ;    /* Convert input to a number. */
  new_input = 1 ;
}


do_numtype()         /* Set number type (fixed or scientific). */
{
  int n ;

  if (dtype == FIX) dtype = SCI ;
  else dtype = FIX ;
  n = row*BCOLS*2 + column*2 + portion ;
  STRCPY(buttons[n].str, (dtype == FIX) ? "SCI " : "FIX ") ;
  set_item(NUMITEM, dtype_str[(int) dtype]) ;
  draw_button(row, column, 0, NORMAL) ;
  draw_button(row, column, 1, NORMAL) ;
  show_display(disp_val) ;
}


do_pending()
{
  grey_buttons(HEX) ;    /* Reshow all the keys. */
  switch (pending)
    {
      case '#'        : do_constant() ;                            /* CON */
                        break ;
      case CCTRL('e') : do_exchange() ;                            /* EXCH */
                        break ;
      case CCTRL('f') : do_function() ;                            /* FUN */
                        break ;
      case 's'        :                                            /* STO */
      case 'r'        : do_sto_rcl() ;                             /* RCL */
                        if (pending_op == '+' || pending_op == '-' ||
                            pending_op == 'x' || pending_op == '/') return ;
                        break ;
      case '<'        :                                            /* < */
      case '>'        : do_shift() ;                               /* > */
                        break ;
      case 'A'        : do_accuracy() ;                            /* ACC */
                        break ;
      case '?'        : do_help() ;                                /* ? */
                        if (pending_op == '?') return ;
                        break ;
      default         : if (!pending)
                          {
                            pending = current ;
                            pending_op = '=' ;
                            if (pending == '?') set_cursor(HELPCURSOR) ;
                            if (pending == '?' && (ishelp <= 1)) do_pending() ;
                            return ;
                          }
    }
  show_display(disp_val) ;
  if (error) set_item(OPITEM, "CLR") ;
  else set_item(OPITEM, "") ;
  pending = 0 ;
  grey_buttons(base) ;      /* Just show numeric keys for current base. */
}


do_point()                  /* Handle numeric point. */
{
  if (!pointed)
    {
      if (toclear)
        {
          STRCPY(display, ".") ;
          toclear = 0 ;
        }
      else if (strlen(display) < disp_length[(int) base])
        STRNCAT(display, ".", 1) ;
      pointed = 1 ;
    }
  set_item(DISPLAYITEM, display) ;
  disp_val = convert_display() ;    /* Convert input to a number. */
}


do_portion()
{
  switch (current)
    {
      case 'U' : disp_val = fabs(disp_val) ;       /* ABS. */
                 break ;
      case 'F' : disp_val -= (int) disp_val ;      /* FRAC. */
                 break ;
      case 'I' : disp_val = (int) disp_val ;       /* INT. */
    }
  show_display(disp_val) ;
}


do_set_mode()           /* Set or unset various calculator modes. */
{
  switch (current)
    {
      case CCTRL('d') :                                    /* DEG */
      case CCTRL('g') :                                    /* GRAD */
      case CCTRL('r') : do_trigtype() ;                    /* RAD */
                        break ;
      case 'h'        : hyperbolic = !hyperbolic ;         /* HYP */
                        set_item(HYPITEM, (hyperbolic) ? "HYP " : "    ") ;
                        break ;
      case 'i'        : inverse = !inverse ;               /* INV */
                        set_item(INVITEM, (inverse) ? "INV " : "    ") ;
                        break ;
      case CCTRL('n') : do_numtype() ;                     /* FIX/SCI */
    }
  if (rstate) make_registers() ;
}


do_shift()     /* Perform bitwise shift on display value. */
{
  int n, shift ;
  BOOLEAN temp ;

  if (current >= '0' && current <= '9')
    {
      for (n = 0; n < TITEMS; n++)
        if (current == buttons[n].value) break ;
      shift = char_val(buttons[n].value) ;
      temp = ibool(convert_display()) ;
      switch (pending)
        {
          case '<' : temp = temp << shift ;
                     break ;
          case '>' : temp = temp >> shift ;
        }
      STRCPY(display, make_number(setbool(temp))) ;
      disp_val = last_input = convert_display() ;
    }
}


do_sto_rcl()     /* Save/restore value to/from memory register. */
{
  if (current >= '0' && current <= '9')
    switch (pending)
      {
        case 'r' : disp_val = mem_vals[char_val(current)] ;
                   break ;
        case 's' : switch (pending_op)
                     {
                       case '+' : mem_vals[char_val(current)] += disp_val ;
                                  break ;
                       case '-' : mem_vals[char_val(current)] -= disp_val ;
                                  break ;
                       case 'x' : mem_vals[char_val(current)] *= disp_val ;
                                  break ;
                       case '/' : mem_vals[char_val(current)] /= disp_val ;
                                  break ;
                       case '=' : mem_vals[char_val(current)] = disp_val ;
                     }  
                   pending_op = 0 ;
                   make_registers() ;
      }                 
  else if (current == '+' || current == '-' ||
           current == 'x' || current == '/') pending_op = current ;
}


do_trig()         /* Perform all trigonometric functions. */
{
  double temp ;

  if (!inverse)
    {
           if (ttype == DEG)  temp = disp_val * PI / 180.0 ;
      else if (ttype == GRAD) temp = disp_val * PI / 200.0 ;
      else                    temp = disp_val ;

      if (!hyperbolic)
        switch (current)
          {
            case CCTRL('c') : tresults[(int) RAD] = cos(temp) ;    /* cos */
                              break ;
            case CCTRL('s') : tresults[(int) RAD] = sin(temp) ;    /* sin */
                              break ;
            case CCTRL('t') : tresults[(int) RAD] = tan(temp) ;    /* tan */
          }
      else
        switch (current)
          {
            case CCTRL('c') : tresults[(int) RAD] = cosh(temp) ;   /* cosh */
                              break ;
            case CCTRL('s') : tresults[(int) RAD] = sinh(temp) ;   /* sinh */
                              break ;
            case CCTRL('t') : tresults[(int) RAD] = tanh(temp) ;   /* tanh */
          }

      tresults[(int) DEG]  = tresults[(int) RAD] ;
      tresults[(int) GRAD] = tresults[(int) RAD] ;
    }
  else
    {
      if (!hyperbolic)
        switch (current)
          {
            case CCTRL('c') : disp_val = acos(disp_val) ;     /* acos */
                              break ;
            case CCTRL('s') : disp_val = asin(disp_val) ;     /* asin */
                              break ;
            case CCTRL('t') : disp_val = atan(disp_val) ;     /* atan */
          }
      else
        switch (current)
          {
            case CCTRL('c') : disp_val = acosh(disp_val) ;    /* acosh */
                              break ;
            case CCTRL('s') : disp_val = asinh(disp_val) ;    /* asinh */
                              break ;
            case CCTRL('t') : disp_val = atanh(disp_val) ;    /* atanh */
          }

      tresults[(int) DEG]  = disp_val * 180.0 / PI ;
      tresults[(int) GRAD] = disp_val * 200.0 / PI ;
      tresults[(int) RAD]  = disp_val ;
    }

  cur_op = current ;
  show_display(tresults[(int) ttype]) ;
  disp_val = tresults[(int) ttype] ;
}


do_trigtype()          /* Change the current trigonometric type. */
{
  switch (current)
    {
      case CCTRL('d') : ttype = DEG ;
                        break ;
      case CCTRL('g') : ttype = GRAD ;
                        break ;
      case CCTRL('r') : ttype = RAD ;
    }
  if (cur_op == CCTRL('c') || cur_op == CCTRL('s') || cur_op == CCTRL('t'))
    {
      disp_val = tresults[(int) ttype] ;
      show_display(tresults[(int) ttype]) ;
    }
  set_item(TTYPEITEM, ttype_str[(int) ttype]) ;
}


BOOLEAN
ibool(x)
double x ;
{
  BOOLEAN p ;
 
  if (x > 68719476736.00) return(0) ;
  else if (x < -68719476736.00) return(0) ;
       else
         {
           while(x < 0.0) x += 4294967296.00 ;
           while(x > 4294967296.00) x -= 4294967296.00 ;
           p = x ;
           return (p) ;
         }
}


double
setbool(p)
BOOLEAN p ;
{
  BOOLEAN q ;
  double val ;

  q = p & 0x80000000 ;
  p &= 0x7fffffff ;
  val = p ;
  if (q) val += 2147483648.0 ;
  return(val) ;
}
