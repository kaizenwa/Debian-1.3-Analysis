/*
 * ircaux.h: header file for ircaux.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: ircaux.h,v 1.15 1995/11/09 16:02:57 mrg Exp $
 */

#ifndef __ircaux_h_
#define __ircaux_h_

#include <stdio.h>

	char	*check_nickname _((char *));
	char	*next_arg _((char *, char **));
	char	*new_next_arg _((char *, char **));
	char	*expand_twiddle _((char *));
	char	*upper _((char *));
	char	*lower _((char *));
	char	*sindex _((char *, char *));
	char	*rfgets _((char *, int, FILE *));
	char	*path_search _((char *, char *));
	char	*double_quote _((char *, char *));
	char	*new_malloc _((int));
#ifdef ALLOC_DEBUG
	void	alloc_cmd _((char *, char *, char *));
#endif
	char	*new_realloc _((char *, int));
	void	malloc_strcpy _((char **, char *));
	void	malloc_strcat _((char **, char *));
	void	new_free _((void *));
	void	wait_new_free _((char **));
	FILE	*zcat _((char *));
	int	is_number _((char *));
	int	connect_by_number _((int, char *, int));
	int	my_stricmp _((char *, char *));
	int	my_strnicmp _((char *, char *, int));
	int	set_non_blocking _((int));
	int	set_blocking _((int));
	int	scanstr _((char *, char *));
	void	really_free _((int));
	void	strmcpy _((char *, char *, int));
	void	strmcat _((char *, char *, int));
	void	strmcat_ue _((char *, char *, int));

#endif /* __ircaux_h_ */
