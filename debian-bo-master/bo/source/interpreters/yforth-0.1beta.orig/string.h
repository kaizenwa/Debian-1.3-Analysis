/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:
 * Abstract:
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __STRING_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __STRING_H__
#define __STRING_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(dash_trailing,					"-trailing",			0)
code(slash_string,					"/string",				0)
code(blank,							"blank",				0)
code(c_move,						"cmove",				0)
code(c_move_up,						"cmove>",				0)
code(compare,						"compare",				0)
code(search,						"search",				0)
code(s_literal,						"sliteral",				COMP_ONLY | IMMEDIATE)

#ifdef PROTOTYPES

#endif

#endif
