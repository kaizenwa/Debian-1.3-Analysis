/*$Id: ap_error.cc,v 11.38 96/03/24 17:59:02 al Exp $ -*- C++ -*-
 * Error handler.
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
/*--------------------------------------------------------------------------*/
//	CS &	CS::check(int i);
//	CS &	CS::warn(int i,int c);
/*--------------------------------------------------------------------------*/
/* syntax_check: handle syntax errors
 * called on parsing an input string when nothing else matches.
 * if the rest of the line is nothing, just return
 * if comment, increment *cnt, so what is left is a valid comment string
 * otherwise, it is an error (the arrow pointing at the offending word)
 */
CS & CS::check(int badness)
{
  skipbl();
  switch (peek()){
    case '\'':	ok = true;  skip();	   break;
    case '\0':	ok = true;		   break;
    default:	ok = false; warn(badness); break;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
/* syntax_msg: print syntax error message
 * echo a portion of the input with an arrow pointing to the current place
 */
CS & CS::warn(int badness, int spot)
{
  if (badness >= OPT::picky){
    if (spot < 20){
      mprintf(IO::mstderr, "%.40s\n", cmd);
      mtab(spot, IO::mstderr);
      error(badness,"^ ?\n");
    }else{
      mprintf(IO::mstderr, "... %.36s\n", &cmd[spot-16]);
      mtab(20, IO::mstderr);
      error(badness,"^ ?\n");
    }
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
