/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     exceptio.h
 * Abstract:        include file for "exception" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __EXCEPTION_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __EXCEPTION_H__
#define __EXCEPTION_H__

#include "yforth.h"
#include "macro.h"

#ifdef PROTOTYPES
struct exception_frame {
	jmp_buf catch_buf;
	Cell *sp, *rp, *bp;
	Real *fp;
	struct exception_frame *last;
};
#endif

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(catch,							"catch",				0)
code(throw,							"throw",				0)

#ifdef PROTOTYPES

#endif

#endif

