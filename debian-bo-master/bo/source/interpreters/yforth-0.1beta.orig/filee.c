/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: filee.c
 * Abstract:    File extension word set
 */

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <errno.h>
#include "yforth.h"
#include "file.h"
#include "filee.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

extern Char file_name[];

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _file_status() {
	register FILE *f;
	get_file_name();
	f = fopen(file_name, "rb");
	*--sp = 0;
	if (f) {
		*--sp = 0;
		fclose(f);
	} else *--sp = errno;
}

void _flush_file() {
	register FILE *f = (FILE *) *sp;
	if (fflush(f)) sp[0] = errno;
	else sp[0] = 0;
}

void _rename_file() {
	register Char *file_name2;
	get_file_name();
	file_name2 = (Char *) malloc(strlen(file_name) + 1);
	if (file_name2) {
		strcpy(file_name2, file_name);
		get_file_name();
		if (rename(file_name, file_name2)) *--sp = errno;
		else *--sp = 0;
		free(file_name2);
	} else *--sp = errno;
}


