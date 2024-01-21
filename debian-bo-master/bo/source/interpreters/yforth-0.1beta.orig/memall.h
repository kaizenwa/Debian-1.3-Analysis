/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: memall.h
 * Abstract:    Include file for "Memory Allocation" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __MEMALL_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __MEMALL_H__
#define __MEMALL_H__

#include "yforth.h"
#include "macro.h"


/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(allocate,                      "allocate",             0)
code(free,                          "free",                 0)
code(resize,                        "resize",               0)

#ifdef PROTOTYPES

#endif

#endif

