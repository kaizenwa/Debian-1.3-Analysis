/*$Id: l_lib.h,v 11.23 96/02/21 12:46:18 al Exp $ -*- C++ -*-
 */
#ifndef L_LIB_H
#define L_LIB_H
#include <ctype.h>
#include <math.h>
#include <string.h>
#include "md_bool.h"
/*--------------------------------------------------------------------------*/
	char*	trim(char*);
	int	pmatch(const char*,const char*);
	bool	wmatch(const char*,const char*);
/*--------------------------------------------------------------------------*/
//ftos stuff
	char*	ftos(double,const char*,int,int);
	void	ftos_set_floor(double);
	int	ftos_set_format(int);
	int	ftos_set_precision(int);
	int	ftos_set_width(int);
enum {			/* formatting bit-fields */
  ftos_DEFAULT = 0,	/* default formatting, with letters */
  ftos_EXP = 1,		/* use 'e' notation, almost like printf */
  ftos_SIGN = 2,	/* always include sign */
  ftos_FILL = 4		/* fill in trailing zeros */
};
	char*	itos(int,char*,int,int);
	char*	utos(unsigned,char*,int);
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
