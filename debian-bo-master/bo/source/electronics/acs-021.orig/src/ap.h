/*$Id: ap.h,v 11.38 96/03/24 18:00:47 al Exp $  -*- C++ -*-
 * stuff for the "ap" family of parsing functions
 */
#ifndef AP_H
#define AP_H
#include <ctype.h>
#include <math.h>
#include <string.h>
#include "md_bool.h"
/*--------------------------------------------------------------------------*/
enum AP_MODE{
  ONEPASS,	/* NO */
  REPEAT	/* YES */
};
enum AP_TYPE{
  aDOUBLE,	/* double */
  aUDOUBLE,	/* double, force positive */
  aINT,		/* int */
  aUINT,	/* int, force positive */
  aENUM,	/* int, set to next param, do not read */
  aODOUBLE,	/* double with offset */
  a2DOUBLE,	/* 2 doubles */
  aORENUM,	/* int, or to next param, do not read */
  aANDENUM,	/* int, and to next param, do not read */
  aFUNCTION,	/* call function with params cmd,cnt */
  a2FUNCTION,	/* 2 functions, in sequence */
  aSDOUBLE,	/* double with scale */
  aFINT,	/* int, but take in double format */
  aIDOUBLE,	/* inverted double */
  aSIDOUBLE,	/* scaled, inverted double (scaled first) */
  aOIDOUBLE,	/* offset, inverted double (offset first) */
  aOCTAL,	/* int in octal */
  aHEX,		/* int in hex */
  aFFUNCTION,	/* call function with CS object */
  a2FFUNCTION,	/* 2 functions, in sequence */
  aTRUE,	/* type bool */
  aFALSE,	/* another type bool */
  aIGNORE	/* ignore this keyword */
};
enum AP_MOD{
  mNONE,	/* nothing special */
  mSCALE,	/* scale it after reading */
  mOFFSET,	/* add an offset */
  mINVERT,	/* save 1 / the number */
  mPOSITIVE,	/* store absolute value */
  mOCTAL,	/* read the number in octal */
  mHEX		/* read the number in hex */
};
  
class CS {
private:
   const char *cmd;
   int  cnt;
   int  last;
   bool ok;
   int  length;
public:
   CS(const char *c,int i=0){cmd=c;cnt=i;last=-1;length=strlen(c);ok=true;}
   int	    cursor()const{return cnt;}
   CS &	    reset(int c=0){cnt=c; ok=true; return *this;}
   const char* fullstring()const{return cmd;}
   const char* tail()const{return &cmd[cnt];}
   int	    ctoc(){return cmd[cnt++];}
   CS &	    skip(int c=1){cnt+=c; ok=cnt<=length; return *this;}
   int	    peek()const{return cmd[cnt];}
   bool	    match(char c)const{return (cmd[cnt]==c);}
   bool	    match1(const char *c)const{return cmd[cnt]&&strchr(c,cmd[cnt]);}
   bool     stuck(){ok=last<cnt; last=cnt; return !ok;}
   bool	    more(){skipbl(); return bool(cmd[cnt]);}
   bool	    end(){skipbl(); return !cmd[cnt];}
   bool	    is_xdigit()const{return (match1("0123456789abcdefABCDEF"));}
   bool	    is_digit()const{return (match1("0123456789"));}
   bool	    is_pfloat()const{return (match1(".0123456789"));}
   bool	    is_float()const{return (match1("+-.0123456789"));}
   bool	    is_alpha()const{return isalpha(toascii(cmd[cnt]));}
   bool	    is_term(const char *t)
   	    {char c=peek();return (c=='\0' || isspace(c) || strchr(t,c));}
   CS &	    skipcom(){return skip1b(",=");}
   CS &	    skiplparen(){return skip1b("([");}
   CS &	    skiprparen(){return skip1b(")]");}
   CS &	    skipequal(){return skip1b("=");}
   double   ctopf(){return fabs(ctof());}
   CS &     warn(int i){return warn(i,cursor());}
   	    operator bool(){return ok;}
   double   ctof();				// ap_ctof.cc
   int	    ctoi();				// ap_ctoi.cc
   unsigned ctou();
   int	    ctoo();
   int	    ctox();
   char*    ctostr(char*,int,const char*);	// ap_ctos.cc
   CS &	    check(int);				// ap_error.cc
   CS &	    warn(int,int);
#if !defined(BAD_BOOL)
   CS &     set(const char*, bool*, bool);	// ap_get.cc
#endif
   CS &     set(const char*, int*, int);
   CS &     get(const char*, int*, AP_MOD=mNONE, int=0);
   CS &     get(const char*, double*, AP_MOD=mNONE, double=0);
   CS &     get(const char*, double*, double*);
   CS &	    pmatch(const char*);		// ap_match.cc
   int	    argparse(AP_MODE,...);		// ap_parse.cc
   CS &	    skipbl();				// ap_skip.cc
   CS &	    skip1b(const char*);
   CS &	    skip1(const char*);
   CS &	    skiparg();
};	
/*--------------------------------------------------------------------------*/
template <class T>
inline CS & set(CS & cmd, const char* key, T* val, T newval)
{
  if (cmd.pmatch(key)){
    *val = newval;
  }
  return cmd;
}    
/*--------------------------------------------------------------------------*/
#if defined(PEDANTIC_TEMPLATES) || defined(PedTemP)
template <class T>
inline CS & set(CS & cmd, char* key, T* val, T newval)
{
  if (cmd.pmatch(key)){
    *val = newval;
  }
  return cmd;
}
#endif
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
