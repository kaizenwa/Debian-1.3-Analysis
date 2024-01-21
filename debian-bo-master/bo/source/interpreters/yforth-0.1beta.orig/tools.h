/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: tools.h
 * Abstract: 	Include file for "Programming Tools" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __TOOLS_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __TOOLS_H__
#define __TOOLS_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(dot_s,							".s",					0)
code(question,						"?",					0)
code(dump,							"dump",					0)
code(see,							"see",					0)
code(words,							"words",				0)

#ifdef PROTOTYPES

#endif

#endif
