/*$Id: ap_ctof.cc,v 11.22 96/02/18 11:41:37 al Exp $ -*- C++ -*-
 * get double from string
 * update string pointer
 * return double number if got, else 0
 * supports letter multipliers (spice style)
 * skips trailing letters (10uhenries == 10u)
 * skips trailing spaces and one comma
 * pointer points to char following comma
 * or first non-space following number just got
 * or first non-space (if non-number)
 */
#include "ap.h"
/*--------------------------------------------------------------------------*/
//	double	CS::ctof();
/*--------------------------------------------------------------------------*/
double CS::ctof()
{
  double val = 0.0;
  double power = 1.0;
  int    sign = 1;

  skipbl();
  if (!is_float()){
    skipcom();
    ok = false;
    return 0.;
  }

  if (skip1("-")){			// sign
    sign = -1;
  }else{
    skip1("+");
  }

  while (is_digit())			// up to dec pt
    val = 10.0 * val + (ctoc()-'0');

  skip1(".");				// dec pt

  while (is_digit()){			// after dec pt
    val = 10.0 * val + (ctoc()-'0');
    power *= .1;
  }

  if (skip1("eE")){			// exponent: E form
    int expo = 0;
    int es = 1;
    if (skip1("-")){
      es = -1;
    }else{
      skip1("+");
    }
    while (is_digit())
      expo = 10 * expo + (ctoc()-'0');
    expo *= es;
    power *= pow(10., expo);
  }else if (skip1("mM")){		// M is special
    if (skip1("eE")){			// meg
      power *= 1e6;
    }else if (skip1("iI")){		// mil
      power *= 25.4e-6;
    }else{				// plain m (milli)
      power *= 1e-3;
    }
  }else if (skip1("uU")){		// other letters
    power *= 1e-6;
  }else if (skip1("nN")){
    power *= 1e-9;
  }else if (skip1("pP")){
    power *= 1e-12;
  }else if (skip1("fF")){
    power *= 1e-15;
  }else if (skip1("kK")){
    power *= 1e3;
  }else if (skip1("gG")){
    power *= 1e9;
  }else if (skip1("tT")){
    power *= 1e12;
  }
  while (is_alpha())			// skip letters
    skip();

  skipcom();
  ok = true;
  return (sign*val*power);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
