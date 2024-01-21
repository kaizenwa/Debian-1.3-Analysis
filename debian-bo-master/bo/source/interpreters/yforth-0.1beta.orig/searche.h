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
#	undef __SEARCHE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __SEARCHE_H__
#define __SEARCHE_H__

#include "yforth.h"
#include "macro.h"


/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(also,							"also",					0)
code(forth,							"forth",				0)
code(only,							"only",					0)
code(order,							"order",				0)
code(previous,						"previous",				0)

#ifdef PROTOTYPES

#endif

#endif
