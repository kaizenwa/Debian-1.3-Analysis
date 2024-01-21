/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: ycore.h 
 * Abstract:	YCore word set (non-standard words specific to yForth?. Don't
 *		expect to find these words in other envionments).
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __YCORE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __YCORE_H__
#define __YCORE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(yforth_version,				"ver",					0)
code(save_image,					"save-image",			0)
code(system,						"system",				0)

#ifdef PROTOTYPES

#endif

#endif
