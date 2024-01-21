/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     locals.h
 * Abstract:        include file for "locals" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __LOCALS_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __LOCALS_H__
#define __LOCALS_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(paren_local_paren,				"(local)",				COMP_ONLY)

code(paren_bp_restore_paren,		"(bp!)",				0)
code(paren_bp_save_paren,			"(bp@)",				0)
code(paren_read_local_paren,		"(rLocal)",				0)
code(paren_write_local_paren,		"(wLocal)",				0)

#ifdef PROTOTYPES

/**************************************************************************/
/* AUXILIARY FUNCTIONS PROTOTYPES *****************************************/
/**************************************************************************/

void clear_locals(void);
void free_locals(void);
void init_locals(void);
void declare_local(Char *s, UCell u);
struct word_def *get_first_local(void);
int locals_defined(void);

#endif

#endif

