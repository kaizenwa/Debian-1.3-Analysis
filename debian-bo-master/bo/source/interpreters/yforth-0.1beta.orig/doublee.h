/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     doublee.h
 * Abstract:        include file for "double-number extension" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __DOUBLEE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __DOUBLEE_H__
#define __DOUBLEE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(two_rote,						"2rot",					0)
code(d_u_less,						"du<",					0)

#ifdef PROTOTYPES

#endif

#endif

