/*
    Wn: A Server for the HTTP
    File: wn/wn.h
    Version 1.17.0
    
    Copyright (C) 1996  <by John Franks>

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

#include <sys/types.h>
#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>
#ifdef NEED_TIME_H
#include <time.h>
#endif

#include "common.h"
#include "err.h"
#include "extra.h"

#define HTTPVERSION "HTTP/1.0"
#define WN_HTML_MARK	"WN_mark"
#define	ACCEPTLEN (2048)
#define MAXDIRLEN (256)
#define NUMFIELDS (20)

#define FREE	(0)
#define ROOTCHK	(1)
#define DIRCHK	(2)

#define FRP_FILE	(0)
#define FRP_PIPE	(1)

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN	(256)
#endif

#define DENYMSG	"Access denied, or file does not exist"
#define DENYSTATUS	"404"
#define CLIENT_ERR	"400"
#define SERV_ERR	"500"

extern FILE	*safer_popen();

extern char	rootdir[],
		wnlogfile[],
		errlogfile[],
		pid_file[],
		vhostfile[],
		cfname[],
		afname[],
		hostname[],
		remotehost[],
		remaddr[],
		*mystrncpy(),
		*mystrncat(),
		*mymemcpy(),
		*strlower();

extern unsigned	alarm(),
		cache_id,
		acache_id,
		interface_num;

extern int	errno,
		port,
		admin_mode,
		debug_log,
		chop(),
		modified(),
		getfpath(),
		getfpath2(),
		chkaccess(),
		amperline(),
		http_prolog(),
		set_show(),
		get_parse_token(),
		eval_if(),
		read_cache_file();

extern void	wn_init(),
		wn_exit(),
		flush_outbuf(),
		send_out_mem(),
		send_out_fd(),
		load_virtual(),
		rfc931(),
		get_stat(),
		chk_cntrl(),
		check_perm(),
		exec_ok(),
		exit(),
		clear_req(),
		do_standalone(),
		do_connection(),
		dolocation(),
		file_open(),
		process_url(),
		writelog(),
		read_cache_dirinfo(),
		list_search(),
		cache_search(),
		send_nomatch(),
		send_isearch(),
		sendtext(),
		sendbin(),
		image(),
		do_wrap(),
		do_nomatchsub(),
		sendgrep(),
		sendinfo(),
		sendcgi(),
		send_markline_doc(),
		send_text_line(),
		search_prolog(),
		search_epilog(),
		get_remote_ip(),
		get_remote_info(),
		get_local_info(),
		startlog(),
		wn_cleanup(),
		www_unescape(),
		sendredirect(),
		send204(),
		cgi_env(),
		check_query(),
		do_swrap(),
		update_mod_time(),
		open_wnlog(),
		www_err(),
		set_interface_root(),
		reset_parse_err(),
		parse_html_err(),
		daemon_logerr(),
		logerr(),
		senderr();


typedef enum { 
	RTYPE_DENIED,
	RTYPE_UNCHECKED,
	RTYPE_FILE,
	RTYPE_CGI,
	RTYPE_NPH_CGI,
	RTYPE_GSEARCH,
	RTYPE_CONTEXTSEARCH,
	RTYPE_LINESSEARCH,
	RTYPE_ISEARCH,
	RTYPE_MARKLINE,
	RTYPE_TSEARCH,
	RTYPE_KSEARCH,
	RTYPE_TKSEARCH,
	RTYPE_FIELDSEARCH,
	RTYPE_LISTSEARCH,
	RTYPE_INFO,
	RTYPE_HEADER,
	RTYPE_REDIRECT,
	RTYPE_NOT_MODIFIED,
	RTYPE_NO_AUTH,
	RTYPE_FINISHED,
	RTYPE_IMAGEMAP,
	RTYPE_NOACCESS
} Reqtype;

typedef struct Request {
	char	request[BIGLEN],	/* The original request */
		cacheline[CACHELINE_LEN],	/* filled in by chkcache */
			/* These are pointers into cacheline */
		*title,			/* Item title */
		*content_type,		/* MIME content type */
		*encoding,		/* MIME content-transfer-encoding */
		*keywords,		/* string of keywords */
		*field[NUMFIELDS],	/* user defined fields */
		*includes,		/* comma separated insert files */
		*wrappers,		/* comma separated wrapper files */
		*swrapper,		/* search wrapper files */
		*nomatchsub,		/* Substitute for empty search result*/
		*filter,		/* Name of filter  */
		*maxage,                /* Maxage in ascii seconds */
		*expires,		/* Expiration date */
			/* These two are pointers into filepath */
		*relpath,		/* Path rel to rootdir */
		*basename,		/* Base name of file  */
		*inclptr,		/* Ptr to current wrap or include */

		rootdir[SMALLLEN],	/* Complete pathname of root dir  */
		filepath[MIDLEN],	/* Complete pathname of file  */
 		cachepath[MIDLEN],	/* Complete pathname of cache file */
		query[MIDLEN],		/* Stuff after '?' in URL */
		param_field[SMALLLEN],	/* Stuff after ';' before '=' in URL */
		*param_value,		/* Stuff after '=' before '?' in URL */
		pathinfo[MIDLEN],	/* PATH_INFO for CGI */
		mod_date[SMALLLEN],	/* Last-Modified HTTP header line */
		range[RANGELEN],	/* range from range header */
		length[TINYLEN];	/* File length in ASCII */
		
	FILE	*fp;
	int	fptype;			/* FP_PIPE or FP_FILE */

	long	datalen;		/* Length of file as a long */

	time_t	mod_time;		/* File modification time */

	Reqtype	type;			/* RTYPE_FILE, RTYPE_CGI, etc. */

	unsigned	attributes,
			status,
			filetype,

			cache_uid,
			cache_gid;

	int		do_wrap_1st_time;

} Request;

extern Request		*this_rp;


/* Bits in the Request attributes  are in common.h */

/* Bits in the request status */
#define	WN_CANT_STAT		(1)
#define	WN_NOT_WORLD_READ	(2)
#define WN_PROLOGSENT		(4)
#define WN_CGI_SET		(8)

/* Bits in the Request filetype */
#define	WN_TEXT			(1)
#define	WN_DIR			(2)
#define WN_ISHTML		(4)
#define	WN_DEFAULT_DOC		(8)
#define	WN_IMAGEMAP		(16)
#define	WN_BYTERANGE		(32)
#define	WN_LINERANGE		(64)
#define	WN_RFC_BYTERANGE	(128)

typedef struct Dir_info {
	char	dirline[BIGLEN],
		*accessfile,
		*swrapper,
		*defincludes,
		*defwrapper,
		*nomatchsub,
		*subdirs,
		*dir_owner,
		*cachemod,		/* Cache data base module */
		*filemod,		/* File data base  module */
		*indexmod,		/* Index search module  module */
		*authtype,		/* Type of authorization */
		*authrealm,		/* Realm for authorization */
		*authmod,		/* module to do authorization */
		*defdoc,		/* default document for this dir */
		*default_content,	/* default content type */
		*default_maxage,	/* default value of maxage  */
		authmodule[MIDLEN],
		filemodule[MIDLEN],
		cachemodule[MIDLEN],
		indexmodule[MIDLEN],
		cantstat_url[MIDLEN/2],
		authdenied_file[MIDLEN/2],
		noaccess_url[MIDLEN/2];

	unsigned	attributes,
			defattributes;

} Dir_info;

extern Dir_info	*dir_p;

/* Bits in the Dir attributes */
#define	WN_DIRNOSEARCH	(1)
#define WN_DIRWRAPPED	(2)
#define WN_SERVEALL	(4)


typedef struct Cache_entry {
	char	*line,
		headerlines[BIGLEN],
		*basename,
		*title,
		*keywords,
		*field[NUMFIELDS],	/* user defined fields */
		*content,
		*encoding,	/* MIME content-transfer-encoding */
		*includes,
		*wrappers,	/* comma separated wrapper files */
		*swrapper,	/* comma separated search wrapper files */
		*nomatchsub,
		*filter,
		*maxage,                /* Maxage in ascii seconds */
		*expires,
		*dynamic,	/* doc should not be cached if "true" */
		*parse,		/* parse for includes, "true" or "false" */
		*cgi,		/* Create CGI environment, "true" or "false" */
		*url,		/* URL link to remote object */
		*redirect,	/* URL link to redirected object */
		*end;

	unsigned	attributes,
			filetype;
} Cache_entry;

typedef enum { 
	GET,
	CONDITIONAL_GET,
	UNLESS_GET,
	POST,
	PUT,
	HEAD,
	UNKNOWN
} Methodtype;

typedef enum { 
	HTTP0_9,
	HTTP1_0
} Prottype;

typedef struct Inheader {
	char	accept[ACCEPTLEN],
		cookie[ACCEPTLEN],
		charset[ACCEPTLEN/4],
		lang[ACCEPTLEN/4],
		url_path[BIGLEN],
		content[SMALLLEN],
		encoding[SMALLLEN],	/* MIME content-transfer-encoding */
		length[SMALLLEN],
		referrer[MIDLEN],
		ua[SMALLLEN],
		from[SMALLLEN],
		host_head[SMALLLEN],
		authorization[MIDLEN],
		inmod_date[SMALLLEN],
		range[RANGELEN],
		tmpfile[MIDLEN];

	Methodtype	method;
	Prottype	protocol;
} Inheader;

extern Inheader	*inheadp;

typedef struct Outheader {
	char	
		list[BIGLEN],
		redirect[MIDLEN],
		expires[SMALLLEN],
		range[SMALLLEN],
		status[SMALLLEN];
} Outheader;

extern Outheader	*outheadp;


typedef struct Inbuffer {
	char	buffer[INBUFFSIZE],
		*bcp;
	int	cur_sz;
} Inbuffer;


#define LOGBUFLEN	(4096)

typedef struct Connection {
	int		pid,
			keepalive,	/* boolean */
			trans_cnt;

	char		logbuf[LOGBUFLEN],
			outbuf[OUT_BUFFSIZE],
			authuser[SMALLLEN],
			rfc931name[SMALLLEN],
			*out_ptr,
			*log_ptr,
			*scheme;	/* "http" or something else */

	long		bytecount;

	Inbuffer	*bufp;
} Connection;

extern Connection	*this_conp;



extern unsigned	serv_perm;

/* Bits in serv_perm */
#define	WN_TRUSTED_UID	(1)
#define	WN_TRUSTED_GID	(2)
#define	WN_FORBID_EXEC	(4)
#define	WN_RESTRICT_EXEC	(8)
#define	WN_COMP_UID	(16)
#define	WN_COMP_GID	(32)
#define	WN_ATRUSTED_UID	(64)
#define	WN_ATRUSTED_GID	(128)






#define streq( a, b)	( strcmp( (a), (b)) == 0 )
#define iswndir( x)	( x->filetype & WN_DIR  )
#define isdirwrapped( x)	( x->attributes & WN_DIRWRAPPED  )


