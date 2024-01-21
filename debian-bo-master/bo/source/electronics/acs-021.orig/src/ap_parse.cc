/*$Id: ap_parse.cc,v 11.22 96/02/18 11:41:48 al Exp $ -*- C++ -*-
 * parse name followed by numeric argument from command line
 * to call...  list the arguments:
 *   each is:
 *	string to match (upper case part must match, lower may match
 *		user string is not case sensitive
 *	variable type (see argparse.h)
 *	pointer to variable to fill
 *	if enumerated type: value
 *	if "2double": second argument
 *   end with either null pointer or null string
 *
 * cnt is incrmented to the next argument in the input string
 * returns the number of things it did
 * where something like "foo=4.33" or "bar = 8.5k 55" are each one thing
 *
 * will try to handle as many arguments as possible.
 * ***BUG***  ENUM is really an int.  The size must be correct.
 */
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include "md_bool.h"
#include "ap.h"
/*--------------------------------------------------------------------------*/
//	int	CS::argparse(AP_MODE,...);
/*--------------------------------------------------------------------------*/
int CS::argparse(AP_MODE mode, ...)
{
  char *key;
  va_list marker;
  AP_TYPE type;
  int did = 0;

  do {
    va_start(marker,mode);
    while (key = va_arg(marker,char*),  key && *key){
      type = va_arg(marker,AP_TYPE);
      if (type == aDOUBLE || type == aUDOUBLE || type == aODOUBLE
			  || type == a2DOUBLE || type == aSDOUBLE
       || type ==aIDOUBLE || type ==aOIDOUBLE || type ==aSIDOUBLE){
	double* arg1 = va_arg(marker,double*);
	double* arg2 = (type==a2DOUBLE) 
			? va_arg(marker,double*) : (double*)NULL;
	double offset = (type==aODOUBLE  ||  type==aOIDOUBLE)
			? va_arg(marker,double)  : 0.;
	double scale  = (type==aSDOUBLE  ||  type==aSIDOUBLE)
			? va_arg(marker,double)  : 1.;
	if (pmatch(key)){
	  *arg1 = (type==aUDOUBLE) ? fabs(ctof()) : ctof();
	  *arg1 += offset;
	  *arg1 *= scale;
	  if (type==aIDOUBLE || type==aOIDOUBLE || type==aSIDOUBLE)
	    if (*arg1 != 0.)
	      *arg1 = 1. / *arg1;
	  if (arg2)
	    *arg2 = ctof();
	  did++;
	  break;
	}
      }else if (type == aINT || type == aUINT  ||  type == aFINT
			     || type == aOCTAL ||  type == aHEX){
	int* arg1 = va_arg(marker,int*);
	if (pmatch(key)){
	  switch (type){
	    case aUINT:	 *arg1 = abs(ctoi());	break;
	    case aINT:	 *arg1 = ctoi();	break;
	    case aOCTAL: *arg1 = ctoo();	break;
	    case aHEX:	 *arg1 = ctox();	break;
	    case aFINT:	 *arg1 = (int)ctof();	break;
	    default:	 assert(0);		break;
	  }
	  did++;
	  break;
	}
      }else if (type == aENUM || type == aORENUM || type == aANDENUM){
	int* arg1  = va_arg(marker,int*);
	int value = va_arg(marker,int);
	if (pmatch(key)){
	  switch (type){
	    case aENUM:	   *arg1 = value;	break;
	    case aORENUM:  *arg1 |= value;	break;
	    case aANDENUM: *arg1 &= value;	break;
	    default:	   assert(0);		break;
	  }
	  did++;
	  break;
	}
      }else if (type == aTRUE || type == aFALSE){
	bool* arg1  = va_arg(marker,bool*);
	if (pmatch(key)){
	  switch (type){
	    case aTRUE:  *arg1 = true; 	break;
	    case aFALSE: *arg1 = false; 	break;
	    default:	 assert(0);	break;
	  }
	  did++;
	  break;
	}
      }else if (type == aIGNORE){
	if (pmatch(key)){
	  did++;
	  break;
	}
      }else if (type == aFUNCTION || type == a2FUNCTION){
	void* arg1 = va_arg(marker,void*);
	void* arg2 = (type==a2FUNCTION) ? va_arg(marker,void*) : NULL;
	if (pmatch(key)){
	  void (*argf)(const char*,int*);
	  argf = (void(*)(const char*,int*))arg1;
	  (*argf)(cmd,&cnt);
	  if (arg2){
	    argf = (void(*)(const char*,int*))arg2;
	    (*argf)(cmd,&cnt);
	  }
	  did++;
	  break;
	}
      }else if (type == aFFUNCTION || type == a2FFUNCTION){
	void* arg1 = va_arg(marker,void*);
	void* arg2 = (type==a2FFUNCTION) ? va_arg(marker,void*) : NULL;
	if (pmatch(key)){
	  void (*argf)(CS*);
	  argf = (void(*)(CS*))arg1;
	  (*argf)(this);
	  if (arg2){
	    argf = (void(*)(CS*))arg2;
	    (*argf)(this);
	  }
	  did++;
	  break;
	}
      }else{
	assert(0);
      }
    }
    va_end(marker);
  } while (key && *key && mode==REPEAT);
  return did;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
