/*$Id: l_ftos.cc,v 11.22 96/02/18 11:44:01 al Exp $ -*- C++ -*-
 * float to string
 * builds string representing floating number
 * num = number to convert.
 * str = string to put it in.  Must be big enough, or else!!
 *	    Must have length at least len+6.
 *	    sig digits + dec.pt.(1) + sign(1) + exp(4)
 * len = max number of displayed digits. (left + right of dp)
 *	    (includes d.p. if 'e' notation and 2 digit exp)
 * fmt = format : 0	  = alpha for exp notation
 *		  FMTEXP  = exponential notation
 *		  FMTSIGN = always inlcude sign
 *		  FMTFILL = fill in zeros
 * BUG:
 * returns a pointer to static space where the string is.
 * there is a finite pool, so repeated calls work, to a point.
 * after that, the space is overwritten, every POOLSIZE calls
 */
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
	char*	ftos(double,const char*,int,int);
	void	ftos_set_floor(double);
	int	ftos_set_format(int);
	int	ftos_set_precision(int);
	int	ftos_set_width(int);
/*--------------------------------------------------------------------------*/
const int POOLSIZE = 10;
const int MAXLENGTH = 40;
static double ftos_floor = 1e-99;
static int    ftos_format = 0;
static int    ftos_prec = 7;
static int    ftos_width = 0;
void ftos_set_floor(double f) {ftos_floor = f;};
int  ftos_set_format(int f)   {int old=ftos_format;ftos_format=f;return old;}
int  ftos_set_precision(int f){int old=ftos_prec;  ftos_prec=f;  return old;}
int  ftos_set_width(int f)    {int old=ftos_width; ftos_width=f; return old;}
/*--------------------------------------------------------------------------*/
char *ftos(double num, const char *dummy, int len, int fmt)
	/* num = number to convert			*/
	/* dummy = place holder, was where to put it	*/
	/* len = max length of new string		*/
	/* fmt = how to format it			*/
{
  char sign;	/* sign storage ( +,-,bl )		*/
  int expo;	/* exponent				*/
  int flg;	/* flag used to supress leading zeros	*/
  int dig;	/* the next digit to print		*/
  int iii;	/* loop counter				*/
  int nnn;	/* char counter -- pos in string	*/
  double rnd;	/* rounding adder			*/
  static char strpool[POOLSIZE][MAXLENGTH];	/* destination string pool */
  static int poolindex = 0;
  char *str;
  
  if (!fmt)
    fmt = ftos_format;

  poolindex++;
  if (poolindex >= POOLSIZE)
    poolindex = 0;
  str = strpool[poolindex];
  for (iii=0;  iii<(int)strlen(dummy);  ++iii)
    str[iii] = ' ';
  for (     ;  iii<MAXLENGTH;      ++iii)
    str[iii] = '\0';
  
  nnn = 0;
  if (len <= 3)
    return str;				/*  error   */
  
  if (fabs(num) < ftos_floor)
    num = 0.;
  
  for ( iii=len+5; iii>=0; --iii )
    str[iii] = ' ';			/* fill with blanks		    */
  
  if (num == 0.){
    strcpy( str, " 0." );
    nnn = strlen( str );		/* num==0 .. build string 0.000...  */
    while ( --len )
      str[nnn++] = '0';
    expo = 0;
    sign = ' ';
  }else{					/* num != 0 */
    if (num < 0.){
      sign = '-';			/* sign */
      num = -num;
    }else if (fmt & ftos_SIGN){
      sign = '+';
    }else{
      sign = ' ';
    }
    
    expo = -3;
    while (num < .001){			/* scale to .001 - 1.0 */
      num *= 1000.;
      expo -= 3;
    }
    while (num >= 1.){
      num *= .001;
      expo += 3;
    }
    if ((fmt&ftos_EXP && expo<-9) || expo>10 || expo<-13)
      --len;				/* one less digit if 'e' notation   */
    if (len <= 3){			/*	    and exp is 2 digits	    */
       return str;
    }
    
    rnd = .5 / pow(10., len);		/* find amt to add to round	    */
    if (num < .01)
       rnd /= 100.;
    else if (num < .1)
       rnd /= 10.;
    num += rnd;				/* add it			    */
    if (num >= 1.){
       num *= .001;			/* created an extra digit: rescale  */
       expo += 3;
    }
    
    flg = 0;
    nnn = 1;
    if (expo == -3){			/* exp is -3.			    */
       expo = 0;			/* print in fixed point, no exponent*/
       str[nnn++] = '0';
       str[nnn++] = '.';
       while (len > 0){
	 num *= 10.;
	 dig = (int)floor(num);
	 num -= (double)dig;
	 str[nnn++] = (char)(dig + '0');
	 if ((flg += dig))
	   --len;
       }
     }else{
       for (iii=2; len>0; --iii){	/* mantissa			    */
	 num *= 10.;			/* get next digit		    */
	 dig = (int)floor(num);
	 num -= (double)dig;		/* subtract off last digit	    */
	 if ((flg += dig)){		/* if int part !=0		    */
	   str[nnn++]=(char)dig+'0';	/*  (not all zeros so far)	    */
	   --len;			/* stuff the digit into the string */
	 }
	 if (!iii && len>0){		/* if we found the dec.pt. and	    */
	   str[nnn++] = '.';		/*   haven't used up all the space  */
	 }				/* put a dec.pt. in the string	    */
       }
     }
  }
  
  for (iii=1; str[iii]==' '; ++iii)	/* insert sign at beginning	    */
    ;
  str[iii-1] = sign;
  
  if (!(fmt&ftos_FILL))			/* supress trailing zeros	    */
    while ( str[nnn-1]=='0' )
      str[--nnn] = (char)((nnn<(int)strlen(dummy)) ? ' ' : '\0');  
  
  if (expo == 0)
    return str;
  
  if (fmt&ftos_EXP || expo>10 || expo<-16){   /* if exponential format	    */
    str[nnn++] = 'E';			    /* put the letter 'E' and	    */
    if (expo < 100) 			    /* convert the exponent	    */
      itos( expo, &str[nnn], -2, ftos_SIGN );
    else
      utos( (unsigned)expo, &str[nnn], -3 );
  }else{				   /* if letter-scale format	    */
    str[nnn++] = "fpnum KMGT"[(expo+15)/3];/* put the appropriate letter    */
  }				 /* note that letter-scale is not valid	    */
				 /* for exp==-3 or exp not in -15..+12	    */
				 /* this is trapped but letter-scale is also*/
				 /* not valid if exp not divisible by 3.    */
				 /* This is not trapped, since it supposedly*/
				 /* cant happen.			    */
  if (str[nnn-1] == 'M'){
    str[nnn++] = 'e';
    str[nnn++] = 'g';
  }
  if (strlen(dummy)==0)		/* BUG: this cleans up stray trailing	*/
    trim(str);			/* blanks.  I don't know why they exist	*/
  
  return str;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
