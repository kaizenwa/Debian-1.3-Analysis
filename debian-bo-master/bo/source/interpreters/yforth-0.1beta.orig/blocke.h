/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: blocke.h
 * Abstract:    Block extension include file
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __BLOCKE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __BLOCKE_H__
#define __BLOCKE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

variable(UCell, s_c_r,				"scr")

/**************************************************************************/
/* PORTOTYPES *************************************************************/
/**************************************************************************/

code(empty_buffers,					"empty-buffers",		0)
code(list,							"list",					0)
code(thru,							"thru",					0)

#ifdef PROTOTYPES

#endif

#endif





