/*$Id: ap_ctos.cc,v 11.22 96/02/18 11:41:40 al Exp $ -*- C++ -*-
 * get string from string
 */
#include "ap.h"
/*--------------------------------------------------------------------------*/
//	char*	CS::ctostr(char *d,int l,const char*t);
/*--------------------------------------------------------------------------*/
/* ctostr: character input to string
 * scan (and eat) an input string (cmd) using index (cnt).
 * result in (des)  (null terminated).
 * max length (actual char count) is (len).
 * (des) must be at least (len)+1 characters long.
 * (cmd) unchanged.  (*cnt) updated to point to next argument.
 * skips leading whitespace.  skips trailing whitespace and comma
 * skips parts of input word too big for destination
 */
char* CS::ctostr(char *des, int len, const char *term)
{
  skipbl();
  int ii;
  for (ii = 0;  ii < len && !is_term(term);  ii++){
    des[ii] = ctoc();
  }
  des[ii] = '\0';

  while (!is_term(term))
    skip();
  skipcom();
  return des;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
