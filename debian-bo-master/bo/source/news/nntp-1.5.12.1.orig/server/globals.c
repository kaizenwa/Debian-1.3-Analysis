#ifndef lint
static char	sccsid[] = "@(#)$Id: globals.c,v 1.14 1994/12/03 21:54:30 sob Exp sob $";
#endif

/*
 * Common variables.
 */

#include "common.h"

/*
 * Variables initialized from conf.h
 */

char	spooldir[] = SPOOLDIR;
char	activefile[] = ACTIVE_FILE;
char	accessfile[] = ACCESS_FILE;
char	distributionsfile[] = DISTRIBUTIONS_FILE;
char	subscriptionsfile[] = SUBSCRIPTIONS_FILE;
char	newsgroupsfile[] = NEWSGROUPS_FILE;
char	historyfile[] = HISTORY_FILE;
char	overviewfmtfile[] = OVER_FMT_FILE;
char	activetimesfile[] = ACTIVE_TIMES_FILE;
char	inews[] = INEWS;
char	rnews[] = RNEWS;

#ifdef	XTHREAD
char	threaddir[] = THREAD_DIR;
char	*threadfile = NULL;
#endif

/*
 * Other random externals.
 */

char	**group_array;
char 	*actbuf;
int	num_groups;
int	ingroup = 0;
char	*group_name = NULL;
long	group_artnum = 0;
int	art_ptr;
int	num_arts;
#ifdef DYNAMIC_ART_ARRAY
int	*art_array = 0;		/* dynamic array */
unsigned int size_art_array = 0;	/* current size of art_array */
#else
int	art_array[MAX_ARTICLES];
#endif
FILE	*art_fp;
int	uid_poster, gid_poster;
char	*home_poster;
int	canpost, canread, canxfer;
char	**ngpermlist;
int	ngpermcount;
char	hostname[256];
int	debug
#ifdef DEBUG
	= DEBUG
#endif
	;

#ifdef AUTH
int	Needauth;	/* 1 if we need to do authorization */
char	User[100];	/* username for authentication */
char	Host[100];	/* host name for authentication */
#endif

#ifdef LOG
int	arts_acsd;
int	grps_acsd;
#endif
