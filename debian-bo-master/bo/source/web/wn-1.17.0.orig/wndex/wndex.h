/*
    Wn: A Server for the HTTP
    File: wndex/wndex.h
    Version 1.16.0
    
    Copyright (C) 1995, 1996  <by John Franks>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include "../config.h"
#include "../wn/common.h"
#include "err.h"

#define INDEX_TMPFILE		"indxcach.tmp"
#define NUM_TITLE_LINES		(30)

#define streq( a, b)	( strcmp( (a), (b)) == 0 )

#define	SMALLBUF	(256)

#ifndef TRUE
#define TRUE	(1)
#endif

#ifndef FALSE
#define FALSE	(0)
#endif

/* bits of flag */
#define WN_NOINDEX	(1)
#define WN_HASCONTENT	(2)
#define WN_HASENCODING	(4)
#define WN_ISLINK	(8)
#define WN_ISURL	(16)

#define	hasencoding(x)	(x->flag & WN_HASENCODING)
#define hascontent(x)	(x->flag & WN_HASCONTENT)


extern void	init(),
		loadmime(),
		chop(),
		getcontent(),
		getkeytitle(),
		exit(),
		writeitem(),
		clear_slist(),
		add_to_slist(),
		do_serveall(),
		addpair();

extern int	recurse,
		stdioflg,
		verboseflg,
		quiet;


extern char	*get_next_line(),
		*strlower(),
		*mystrncpy(),
		cntlfname[],
		cachefname[];


typedef struct Entry {
	char	file[SMALLLEN],
		url[MIDLEN],
		title[MIDLEN],
		content[SMALLLEN],
		defaultcontent[SMALLLEN],
		defwrapper[CACHELINE_LEN],
		defincludes[CACHELINE_LEN],
		owner[MIDLEN],
		cacheline[CACHELINE_LEN],
		cntlfpath[MIDLEN],
		cachefpath[MIDLEN];

	int	foundtitle,
		foundexp,
		foundkey,
		inlist,
		firsttime,
		isindexfile,
		serveall,
		doindex;


	unsigned	flag,
			attributes,
			defattributes;

} Entry;

extern Entry	top;


