/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: file.h
 * Abstract:    File word-set include file
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __FILE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __FILE_H__
#define __FILE_H__

#include <stdio.h>
#include "yforth.h"
#include "macro.h"

#define FILE_R_O		0
#define FILE_W_O		2
#define FILE_R_W		4
#define FILE_BIN		1

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(bin,							"bin",					0)
code(close_file,					"close-file",			0)
code(create_file,					"create-file",			0)
code(delete_file,					"delete-file",			0)
code(file_position,					"file-position",		0)
code(file_size,						"file-size",			0)
code(include_file,					"include-file",			0)
code(included,						"included",				0)
code(open_file,						"open-file",			0)
code(r_o,							"r/o",					0)
code(r_w,							"r/w",					0)
code(read_file,						"read-file",			0)
code(read_line,						"read-line",			0)
code(reposition_file,				"reposition-file",		0)
code(resize_file,					"resize-file",			0)
code(w_o,							"w/o",					0)
code(write_file,					"write-file",			0)
code(write_line,					"write-line",			0)

#ifdef PROTOTYPES

/**************************************************************************/
/* AUXILIARY FUNCTIONS PROTOTYPES *****************************************/
/**************************************************************************/

Cell truncate_file(FILE *f, UDCell cur, UDCell ud);
Cell expand_file(FILE *f, UDCell cur, UDCell ud);
Char *get_file_name(void);
void load_file(Char *name);

#endif

#endif
