/*$Id: ap_skip.cc,v 11.38 96/03/24 17:59:06 al Exp $ -*- C++ -*-
 * collection of functions to skip input
 * all except skip1 skip leading whitespace, skip whatever is being skipped,
 * then skip trailing whitespace.
 */
#include <assert.h>
#include "ap.h"
/*--------------------------------------------------------------------------*/
//	CS &	CS::skipbl();
//	CS &	CS::skip1b(const char*);
//	CS &	CS::skip1(const char*);
//	CS &	CS::skiparg();
/*--------------------------------------------------------------------------*/
/* skipbl: skip whitespace.  (any non-graphic character is ws)
 * =,(,) are also ws
 * update string pointer
 * pointer points to first non-space
 * does NOT update ok flag
 */
CS & CS::skipbl()
{
  while (peek()  &&  (!isgraph(peek())))
    skip();
  return *this;
}
/*--------------------------------------------------------------------------*/
/* skip1b: skip 1 matching character and surrounding blanks 
 * ok = did it
 */
CS & CS::skip1b(const char *t)
{
  skipbl();
  skip1(t);
  skipbl();
  return *this;
}
/*--------------------------------------------------------------------------*/
/* skip1: skip 1 character matching any in t
 * ok = did it
 */
CS & CS::skip1(const char *t)
{
  if (match1(t)){
    skip();
    assert(ok);
  }else{
    ok = false;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
/* skiparg: skip an argument (maybe just a comma)
 * ok = skipped something
 */
CS & CS::skiparg()
{
  int here = cursor();
  if (!skipcom()){
    if (peek())
      skip();
    while (is_alpha() || is_pfloat())
      skip();
    skipcom();
  }
  ok = cursor() > here;
  return *this;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
