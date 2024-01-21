/*$Id: ap_match.cc,v 11.38 96/03/24 17:59:04 al Exp $ -*- C++ -*-
 * string compare
 * compares characters until end of first string
 * any non-alpha character may be a terminator
 * Characters in reference string in UPPER case must match.
 * Always requires at least one character to match.
 */
#include "ap.h"
/*--------------------------------------------------------------------------*/
//	CS &	CS::pmatch(const char*);
/*--------------------------------------------------------------------------*/
static inline char to_lower(char c){return ((isupper(c))?tolower(c):c);}
/*--------------------------------------------------------------------------*/
CS & CS::pmatch(const char *str2)
{
  int start = cursor();
  skipbl();
  int tokenstart = cursor();
  while (*str2 && to_lower(peek()) == to_lower(*str2)){
    skip();
    ++str2;
  }
  if (strcmp(str2,"$$")==0){
    skipcom();
    ok = true;
  }else if (cursor() == tokenstart || is_alpha() 
	    || isupper(*str2) || isdigit(*str2)){
    reset(start);
    ok = false;    
  }else{
    skipcom();
    ok = true;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
