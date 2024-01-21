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
#	undef __SEARCH_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __SEARCH_H__
#define __SEARCH_H__

#include "yforth.h"
#include "macro.h"


/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(definitions,					"definitions",			0)
code(forth_wordlist,				"forth-wordlist",		0)
code(get_current,					"get-current",			0)
code(get_order,						"get-order",			0)
code(search_wordlist,				"search-wordlist",		0)
code(set_current,					"set-current",			0)
code(set_order,						"set-order",			0)
code(wordlist,						"wordlist",				0)

#ifdef PROTOTYPES

/**************************************************************************/
/* AUXILIARY FUNCSIONS PROTOTYPES *****************************************/
/**************************************************************************/

void save_vocabulary(struct voc_marker *vm);
void load_vocabulary(struct voc_marker *vm);

#endif

#endif
