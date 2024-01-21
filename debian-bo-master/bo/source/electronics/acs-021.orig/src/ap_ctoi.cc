/*$Id: ap_ctoi.cc,v 11.38 96/03/24 17:58:59 al Exp $ -*- C++ -*-
 * get integer from string, in a variety of formats
 * update string pointer
 * return integer if got, else 0
 * pointer points to char following number just got
 * or first non-space
 */
#include "ap.h"
/*--------------------------------------------------------------------------*/
//	int	 CS::ctoi();
//	unsigned CS::ctou();
//	int	 CS::ctoo();
//	int	 CS::ctox();
/*--------------------------------------------------------------------------*/
static inline char to_lower(char c){return ((isupper(c))?tolower(c):c);}
/*--------------------------------------------------------------------------*/
/* ctoi: character input to integer
 * Returns signed integer, or 0 if the string is not a number.
 * Input must be integer: no multipliers, no decimal point.
 * Dot or letter belongs to the next token.
 */
int CS::ctoi()
{
  int val = 0;
  int sign = 1;

  skipbl();
  int here = cursor();
  if (skip1("-")){
    sign = -1;
  }else{
    skip1("+");
  }

  while (is_digit())
    val = 10 * val + (ctoc()-'0');

  skipcom();
  ok = cursor() > here;
  return val * sign;
}
/*--------------------------------------------------------------------------*/
/* ctou: character input to unsigned integer
 * Returns unsigned integer, or 0 if the string is not a number.
 * Input must be integer: no multipliers, no decimal point.
 * Dot or letter belongs to the next token.
 */
unsigned CS::ctou()
{
  int val = 0;

  skipbl();
  int here = cursor();
  while (is_digit())
    val = 10 * val + (ctoc()-'0');
  skipcom();
  ok = cursor() > here;
  return val;
}
/*--------------------------------------------------------------------------*/
/* ctoo: character octal input to integer
 * Returns integer, or 0 if the string is not a number.
 * Input must be integer: no multipliers, no decimal point.
 * Dot or letter belongs to the next token.
 * There is no check against '8' and '9'.
 */
int CS::ctoo()
{
  int val = 0;

  skipbl();
  int here = cursor();
  while (is_digit())
    val = 8 * val + (ctoc()-'0');
  skipcom();
  ok = cursor() > here;
  return val;
}
/*--------------------------------------------------------------------------*/
/* ctox: character hex input to unsigned integer
 * Returns integer, or 0 if the string is not a number.
 * Input must be hex integer: no multipliers, no decimal point.
 * Dot or letter belongs to the next token.
 */
int CS::ctox()
{
  int val = 0;

  skipbl();
  int here = cursor();
  while (is_xdigit()){
    if (is_digit()){
      val = 16 * val + (ctoc()-'0');
    }else{
      val = 16 * val + (to_lower(ctoc())-'a'+10);
    }
  }
  skipcom();
  ok = cursor() > here;
  return val;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
