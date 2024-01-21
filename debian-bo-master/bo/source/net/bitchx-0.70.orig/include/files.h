/*
 * files.h -- header file for files.c
 *
 * Direct file manipulation for irc?  Unheard of!
 * (C) 1995 Jeremy Nelson
 * See the COPYRIGHT file for copyright information
 *
 */

#ifndef FILES_H
#define FILES_H

#include "irc_std.h"

extern	int	open_file_for_read _((char *));
extern	int	open_file_for_write _((char *));
extern	int	file_write _((int, char *));
extern	int	file_writeb _((int, char *));
extern	char *	file_read _((int));
extern	char *	file_readb _((int, int));
extern	int	file_eof _((int));
extern	int	file_close _((int));
extern 	int	open_file_for_bwrite _((char *));
extern	int	file_copy _((int, int));
extern        int     file_valid _((int));

#endif
