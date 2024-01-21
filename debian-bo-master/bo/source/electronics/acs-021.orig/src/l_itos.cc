/*$Id: l_itos.cc,v 11.22 96/02/18 11:44:03 al Exp $ -*- C++ -*-
 * Integer to string (signed).
 * num = number to convert.
 * str = string to put it in.  Must be big enough, or else!!
 *	    Must have length at least len+1.
 * len = number of significant digits.
 *	 If minus, left justify, else right.
 * fmt = format : 0	  = normal : sign if minus.
 *		  FMTSIGN = always sign.
 */
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
	char	*itos(int,char*,int,int);
/*--------------------------------------------------------------------------*/
char *itos(int num, char *str, int len, int fmt)
{
  int ii;
  char sign;
  
  if (num < 0){
    sign = '-';
    num = -num;
  }else if (fmt & ftos_SIGN){
    sign = '+';
  }else{
    sign = ' ';
  }
  
  utos( (unsigned)num, &str[1], len );
  for ( ii=1; str[ii]==' '; ++ii )
    ;
  str[ii-1] = sign;
  return str;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
