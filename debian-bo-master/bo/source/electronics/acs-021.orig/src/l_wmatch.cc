/*$Id: l_wmatch.cc,v 11.24 96/02/25 14:10:29 al Exp $ -*- C++ -*-
 * wmatch: string match with wild cards
 * s1 may have wild cards: ? any character matches; * any repeated 0 or more
 * returns true or false
 * normally not case sensitive,
 *	but \ before any letter in s1 forces exact match
 * recursive
 */
#include "l_lib.h"
#include <assert.h>
/*--------------------------------------------------------------------------*/
	bool	wmatch(const char*,const char*);
/*--------------------------------------------------------------------------*/
static inline char to_lower(char c){return ((isupper(c))?tolower(c):c);}
/*--------------------------------------------------------------------------*/
bool wmatch(const char *s2, const char *s1)
{
  if (!*s2 && !*s1){
    return true;
  }else if (!*s2 || !*s1){
    return false;
  }else if (to_lower(*s2) == to_lower(*s1)){
    return wmatch(s2+1, s1+1);
  }else if (*s1 == '?'){
    return wmatch(s2+1, s1+1);
  }else if (*s1 == '*'){
    if (wmatch(s2+1, s1)){
      return true;
    }else if (wmatch(s2, s1+1)){
      return true;
    }else{
      return wmatch(s2+1, s1+1);
    }
  }else{
    return false;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
