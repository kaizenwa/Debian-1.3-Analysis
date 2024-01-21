
/*  @(#)display.c 1.8 89/11/01
 *
 *  Display manipulation routines used by calctool.
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
#include <strings.h>
#include <math.h>
#include "calctool.h"
#include "color.h"
#include "extern.h"


char_val(chr)
char chr ;
{
       if (chr >= '0' && chr <= '9') return(chr - '0') ;
  else if (chr >= 'a' && chr <= 'f') return(chr - 'a' + 10) ;
  else return(-1) ;
}


clear_display()
{
  int i ;

  pointed = 0 ;
  toclear = 1 ;
  STRCPY(display, "0.") ;
  for (i = 0; i < accuracy; i++) STRNCAT(display, "0", 1) ;
  set_item(DISPLAYITEM, display) ;
  hyperbolic = inverse = 0 ;
  set_item(HYPITEM, "    ") ;
  set_item(INVITEM, "    ") ;
  disp_val = 0.0 ;
}


double
convert_display()    /* Convert input string into a double. */
{
  int exp, exp_sign, i, inum ;
  double val ;
  char *optr ;

  val = 0.0 ;
  exp = 0 ;
  optr = display ;
  while ((inum = char_val(*optr)) >= 0)
    {
      val = val * basevals[(int) base] + inum ;
      *optr++ ;
    }
      
  if (*optr == '.')
    for (i = 1; (inum = char_val(*++optr)) >= 0; i++)
      val += inum / powers[i][(int) base] ;

  while (*optr == ' ') optr++ ;

  if (*optr != '\0')
    {
      if (*optr == '-') exp_sign = -1 ;
      else exp_sign = 1 ;

      while ((inum = char_val(*++optr)) >= 0)
        exp = exp * basevals[(int) base] + inum ;
    }
  exp *= exp_sign ;

  if (key_exp)
    val *= pow((double) basevals[(int) base], (double) exp) ;
  return(val) ;
}
 
 
get_label(n)
int n ;
{
  if (tstate)
    switch (buttons[n].value)
        {
          case CCTRL('c') :
          case CCTRL('d') :
          case CCTRL('e') :
          case CCTRL('f') :
          case CCTRL('g') :
          case CCTRL('n') :
          case CCTRL('r') :
          case CCTRL('s') :
          case CCTRL('t') : SPRINTF(pstr, "^%c  ", buttons[n].value + 96) ;
                            break ;
          case CCTRL('h') : STRCPY(pstr, "bsp ") ;
                            break ;
          case '\177'     : STRCPY(pstr, "del ") ;
                            break ;
          default         : SPRINTF(pstr, "%c   ", buttons[n].value) ;
        }
  else STRCPY(pstr, buttons[n].str) ;
}


initialise()
{
  error = 0 ;              /* Currently no display error. */
  cur_op = '?' ;           /* No arithmetic operator defined yet. */
  old_cal_value = '?' ;
  result = 0.0 ;           /* No previous result yet. */
  last_input = 0.0 ;
}


char *
make_fixed(number, cmax)    /* Convert fixed number. */
double number ;             /* Value to convert. */
int cmax ;                  /* Maximum characters to generate. */
{
  char *optr ;
  double val ;
  int ndig ;                   /* Total number of digits to generate. */
  int ddig ;                   /* Number of digits to left of . */
  int dval ;

  optr = fnum ;
  val = fabs(number) ;
  if (number < 0.0) *optr++ = '-' ;
  val += .5 / powers[accuracy][(int) base] ;

  if (val < 1.0)
    {
      ddig = 0 ;
      *optr++ = '0' ;
      cmax-- ;
    }
  else
    {
      for (ddig = 0; val >= 1.0; ddig++)
        val /= powers[1][(int) base] ;
    }

  ndig = MIN(ddig + accuracy, --cmax) ;

  while (ndig-- > 0)
    {
      if (ddig-- == 0) *optr++ = '.' ;
      val *= powers[1][(int) base] ;
      dval = val ;
      *optr++ = digits[dval] ;
      val -= (int) val ;
    }
  *optr++ = '\0' ;
  toclear = 1 ;
  pointed = 0 ;
  return(fnum) ;
}


char *
make_number(number)     /* Convert display value to current base. */
double number ;         /* Value to convert. */
{
  double val ;

/*  Try to apply sanity checks to try to detect if the number to be
 *  converted is not a valid number. These are based on the isnan and
 *  isinf routines, which are only available on certain machines.
 */

  if (number != number || (number*0 != 0))    /* Same as isnan & isinf. */
    {
      STRCPY(display, "Error") ;
      error = 1 ;
      set_item(OPITEM, "CLR") ;
      return(display) ;
    }

  val = fabs(number) ;
  if (dtype == SCI ||
      dtype == FIX && val != 0.0 && (val > max_fix[(int) base] ||
                      val < exp_p1[accuracy][(int) base]))
    return(make_scientific(number)) ;
  else return(make_fixed(number, MAX_DIGITS)) ;
}


char *
make_scientific(number)     /* Convert scientific number. */
double number ;             /* Value to convert. */
{
  char fixed[MAX_DIGITS+1] ;
  char *optr ;
  double mant ;                /* Mantissa */
  double val ;
  int exp = 0 ;                /* Exponent */
  int i ;
  int eng = 0 ;                /* Scientific not engineering value. */
  double atmp ;

  optr = snum ;
  val = fabs(number) ;
  if (number < 0.0) *optr++ = '-' ;
  mant = val ;
  atmp = 1.0 / powers[10][(int) base] ;

  if (mant != 0.0)
    {
      while (mant >= powers[10][(int) base])
        {
          exp += 10 ;
          mant *= atmp ;
        }
        
      while ((!eng && mant >= powers[1][(int) base]) ||
             (eng && (mant >= powers[3][(int) base] || exp % 3 != 0)))
        {
          exp += 1 ;
          mant /= powers[1][(int) base] ;
        }
 
      while (mant < atmp)
        {
          exp -= 10 ;
          mant *= powers[10][(int) base] ;
        }

      while (mant < 1.0 || (eng && exp % 3 != 0))
        {
          exp -= 1 ;
          mant *= powers[1][(int) base] ;
        }
    }    

  STRCPY(fixed, make_fixed(mant, MAX_DIGITS-6)) ;
  for (i = 0; i < strlen(fixed); i++) *optr++ = fixed[i] ;

  *optr++ = 'e' ;

  if (exp < 0)
    {
      exp = -exp ;
      *optr++ = '-' ;
    }
  else *optr++ = '+' ;

  if ((*optr = digits[exp / ((int) powers[2][(int) base])]) != '0')
    optr++  ;
  exp %= (int) powers[2][(int) base] ;
  *optr++ = digits[exp / ((int) powers[1][(int) base])] ;
  exp %= (int) powers[1][(int) base] ;
  *optr++ = digits[exp] ;
  *optr++ = '\0' ;
  toclear = 1 ;
  pointed = 0 ;
  return(snum) ;
}


process_item(n)
int n ;
{
  int i,isvalid ;

  if (n < 0 || n > TITEMS) return ;

  current = buttons[n].value ;
  if (current == 'X') current = 'x' ;         /* Reassign "extra" values. */
  if (current == '*') current = 'x' ;
  if (current == '\015') current = '=' ;
  if (current == 'Q') current = 'q' ;

  if (error)
    {
      isvalid = 0 ;                    /* Must press a valid key first. */
      for (i = 0; i < MAXVKEYS; i++)
        if (current == validkeys[i]) isvalid = 1 ;
      if (pending == '?') isvalid = 1 ;
      if (!isvalid) return ;
      error = 0 ;
    }

  if (pending)
    {
      for (n = 0; n < TITEMS; n++)
        if (pending == buttons[n].value) break ;
    }
  switch (buttons[n].opdisp)
    {
      case OP_SET   : set_item(OPITEM, buttons[n].str) ;
                      break ;
      case OP_CLEAR : if (error) set_item(OPITEM, "CLR") ;
                            else set_item(OPITEM, "") ;
    }
  (*buttons[n].func)() ;
}


show_display(val)
double val ;
{
  if (!error)
    {
      STRCPY(display, make_number(val)) ;
      set_item(DISPLAYITEM, display) ;
    }
}
