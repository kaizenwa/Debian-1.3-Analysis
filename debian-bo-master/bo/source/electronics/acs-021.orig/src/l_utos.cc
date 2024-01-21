/*$Id: l_utos.cc,v 11.22 96/02/18 11:44:09 al Exp $ -*- C++ -*-
 * unsigned to string
 * num = number to convert.
 * str = string to put it in.  Must be big enough, or else!!
 *	    Must have length at least len.
 * len = number of significant digits.
 *	 If minus, left justify, else right.
 */
#include <stdlib.h>
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
	char	  *utos(unsigned,char*,int);
/*--------------------------------------------------------------------------*/
char *utos(unsigned num, char *str, int len)
{
  int ii, jj;
  
  if ( len==0 ) return str;			    /*	reject zero length  */
  ii = abs(len);
  
  do{					/*   build string starting at tail  */
    str[--ii] = (char)(num % 10 + '0');
  }while ( (num/=10)>0 && ii>0 );
  
  if (len > 0){			    /*	 if right justify, fill with blank  */
    while (ii > 0)
      str[--ii] = ' ';
  }else{				/* else if left justify, move left  */
    for (jj=0;  ii<-len;  )		/*    then fill with blanks	    */
      str[jj++] = str[ii++];
    while (jj < -len)
      str[jj++] = ' ';
  }
  return str;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
