/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: filee.h
 * Abstract:    Include file for "File extension" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __FILEE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __FILEE_H__
#define __FILEE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(file_status,					"file-status",			0)
code(flush_file,					"flush-file",			0)
code(rename_file,					"rename-file",			0)


#ifdef PROTOTYPES

#endif

#endif
