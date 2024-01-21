/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     facility.h
 * Abstract:        include file for "facility" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __FACILITY_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __FACILITY_H__
#define __FACILITY_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(at_x_y,						"at-xy",				0)
code(key_question,					"key?",					0)
code(page,							"page",					0)

#ifdef PROTOTYPES

#endif

#endif

