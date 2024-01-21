/*$Id: l_pmatch.cc,v 11.38 96/03/24 18:00:12 al Exp $ -*- C++ -*-
 * string compare
 * compares characters until end of first string
 * any non-alpha character is a terminator
 * returns offset into string to legal start of next token
 * (skips trailing spaces and comma, if any)
 *     if they match so far, and enough matched,   else NO (0)
 * Characters in reference string in UPPER case must match.
 * Always requires at least one character to match.
 */
#include "l_lib.h"
#include "ap.h"
/*--------------------------------------------------------------------------*/
	int	pmatch(const char*,const char*);
/*--------------------------------------------------------------------------*/
static inline char to_lower(char c){return ((isupper(c))?tolower(c):c);}
static inline int is_alpha(int c){return isalpha(toascii(c));}
/*--------------------------------------------------------------------------*/
/* pmatch: match str1 (under test) against str2 (reference)
 * if no match, returns 0
 * if match, returns the number of characters that match,
 *	---including whitespace---
 * return value is usually interpreted as a truth value:
 *	true if match exists.
 * str1 (being tested) is considered to be case-insensitive
 *	is terminated by a non-alpha character
 * str2 (reference) has special considerations:
 *	upper case characters must match, and must be present
 *	lower case characters must match if present, but may be omitted
 * strings are alpha only.
 */
int pmatch (const char *str1, const char *str2)
{
  CS cmd(str1);

  cmd.skipbl();
  while (to_lower(cmd.peek()) == to_lower(*str2)){
    str2++;
    if (!is_alpha(cmd.ctoc()) || (!is_alpha(cmd.peek()) && !isupper(*str2))){
      cmd.skipcom();
      return cmd.cursor();
    }
  }
  return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
