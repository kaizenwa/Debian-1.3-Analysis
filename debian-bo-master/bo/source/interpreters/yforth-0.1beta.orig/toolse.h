/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: toolse.h
 * Abstract:	Include file for "Programming Tools extension" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __TOOLSE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __TOOLSE_H__
#define __TOOLSE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(ahead,							"ahead",				COMP_ONLY | IMMEDIATE)
code(bye,							"bye",					0)
code(pick,							"cs-pick",				COMP_ONLY)
code(roll,							"cs-roll",				COMP_ONLY)
code(bracket_else,					"[else]",				IMMEDIATE)
code(bracket_if,					"[if]",					IMMEDIATE)
code(bracket_then,					"[then]",				IMMEDIATE)

#ifdef PROTOTYPES

#endif

#endif
