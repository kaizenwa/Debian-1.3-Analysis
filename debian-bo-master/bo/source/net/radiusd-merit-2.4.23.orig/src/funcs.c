/*
 * RADIUS -- Remote Authentication Dial In User Service
 * 
 * COPYRIGHT  (c)  1992, 1993, 1994, 1995, 1996
 * THE REGENTS OF THE UNIVERSITY OF MICHIGAN AND MERIT NETWORK, INCORPORATED
 * ALL RIGHTS RESERVED
 * 
 * PERMISSION IS GRANTED TO USE, COPY, CREATE DERIVATIVE WORKS AND REDISTRIBUTE
 * THIS SOFTWARE AND SUCH DERIVATIVE WORKS IN BINARY FORM ONLY FOR ANY PURPOSE,
 * SO LONG AS NO FEE IS CHARGED, AND SO LONG AS THE COPYRIGHT NOTICE ABOVE, THIS
 * GRANT OF PERMISSION, AND THE DISCLAIMER BELOW APPEAR IN ALL COPIES MADE; AND
 * SO LONG AS THE NAME OF THE UNIVERSITY OF MICHIGAN IS NOT USED IN ANY
 * ADVERTISING OR PUBLICITY PERTAINING TO THE USE OR DISTRIBUTION OF THIS
 * SOFTWARE WITHOUT SPECIFIC, WRITTEN PRIOR AUTHORIZATION.
 * 
 * THIS SOFTWARE IS PROVIDED AS IS, WITHOUT REPRESENTATION FROM THE UNIVERSITY
 * OF MICHIGAN AS TO ITS FITNESS FOR ANY PURPOSE, AND WITHOUT WARRANTY BY THE
 * UNIVERSITY OF MICHIGAN OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
 * WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE.  THE REGENTS OF THE UNIVERSITY OF MICHIGAN SHALL NOT BE
 * LIABLE FOR ANY DAMAGES, INCLUDING SPECIAL, INDIRECT, INCIDENTAL, OR
 * CONSEQUENTIAL DAMAGES, WITH RESPECT TO ANY CLAIM ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OF THE SOFTWARE, EVEN IF IT HAS BEEN OR IS HEREAFTER
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 * 
 * For a License to distribute source code or to charge a fee for the program
 * or a product containing the program, contact MERIT at the University of
 * Michigan:
 * 
 * aaa-license@merit.edu
 * 
 * [This version puts NO LIMITS on the use.  It grants the right to create
 * DERIVATIVE WORKS.  The user may copy and distribute the code in the form
 * received AND DERIVATIVE WORKS, so long as no fee is charged.  If copies are
 * made, our copyright notice and the disclaimer must be included on them.  USE
 * THIS VERSION WITH CARE.  THIS VERSION VERY LIKELY WILL KILL ANY POTENTIAL
 * FOR LATER COMMERCIALIZATION OF THE SOFTWARE.]
 *
 *
 * Public entry points in this file:
 * 
 * add_string
 * authtype_toa
 * avpair_add
 * avpair_assign
 * avpair_copy
 * avpair_get
 * avpair_new
 * avpair_vtoa
 * compress_file
 * debug_list
 * debug_pair
 * dumpit
 * fprint_attr_val
 * gen_valpairs
 * get_errmsg
 * get_passwd
 * get_vp
 * get_last_vp
 * insert_vp
 * loghead
 * logit
 * missing_attribute
 * parse_realm
 * prune_pairs
 * _reply_message
 * reply_sprintf
 * setupsock
 * trunc_logfile
 * type_string
 * 
 */

static char     rcsid[] = "$Id: funcs.c,v 1.99 1996/06/14 16:14:47 web Exp $";

#include	<sys/types.h>
#include	<sys/param.h>

#if defined(sys5)
#include	<sys/sysmacros.h>
#endif	/* sys5 */

#include	<sys/socket.h>
#include	<sys/time.h>

#if defined(aix) || defined(ultrix)
#include	<fcntl.h>
#else	/* aix */
#include	<sys/fcntl.h>
#endif	/* aix */

#include	<sys/wait.h>
#include	<net/if.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>

#include	<stdio.h>
#include	<netdb.h>
#include	<time.h>
#include	<ctype.h>
#include	<errno.h>
#include	<dirent.h>
#include	<syslog.h>
#include	<unistd.h>
#include	<varargs.h>

#include	"radius.h"

extern int       debug_flag;
extern int       dumpcore;
extern int       file_logging;
extern int       spawn_flag;
extern int       zap_logfile;

#if !defined(__FreeBSD__) && !defined(_BSDI_VERSION) && !defined(__NetBSD__)
extern int       sys_nerr;
extern char     *sys_errlist[];
#endif	/* __FreeBSD__ */

extern FILE     *ddt;
extern FILE     *msgfd;
extern char     *radius_dir;
extern AATVPTR   rad_authen_aatv;
extern time_t    birthdate;
extern char      recv_buffer[4096];
extern char      send_buffer[4096];
extern char      ourhostname[MAXHOSTNAMELEN];

/*************************************************************************
 *
 *	Function: add_string (string, convert)
 *
 *	Purpose: Store string in storage block, returning pointer.  
 *		 Optimizes string storage allocation and allows only
 *		 one copy of string to be stored.  A NULL string pointer
 *		 returns a pointer to a null string.
 *
 *		 If convert is non-zero, the string is stored in lower-case.
 *		 Thus a case insensitive match is performed to attempt to
 *		 find an already existing copy of the string in the table.
 *
 *		 Use this only to store non-dynamic (i.e., configuration)
 *		 strings as there is no way to delete strings and lookups
 *		 to see if a string is already is present are quite 
 *		 inefficient.
 *
 *************************************************************************/

char *
add_string (string, convert)

char          *string;
int            convert;

{
	typedef struct char_blk
	{
		struct char_blk        *next;
		char		       *next_ptr;
		int			rem_len;
		char			strings[2048];
	} CHAR_BLK;

	static CHAR_BLK *first_blk = (CHAR_BLK *) NULL;
	CHAR_BLK        *last_blk;
	CHAR_BLK        *blk;
	CHAR_BLK        *space_blk; /* 1st block with sufficient space */
	register char   *ptr;
	register char	*fptr;
	register char	*str;
	register UINT4	 len;
	register UINT4	 slen;
	char		 lcstr[AUTH_ID_LEN]; /* Case converted string */
	static char     *func = "add_string";
	
	if (!string)
	{
		string = "";	/* Convert NULL pointer to null string */
	}

	/* Copy input to all lowercase */
	if (convert & ASLC)
	{
		for (ptr = lcstr, fptr = string ; *fptr ; )
		{
			*ptr++ = tolower (*fptr++);
		}
		*ptr++ = '\0';
		len = ptr - lcstr;
		string = lcstr;
	}
	else
	{
		len = strlen (string) + 1;
	}

	space_blk = (CHAR_BLK *) NULL;

	/* See if we're already storing this string */
	for (blk = last_blk = first_blk ; blk ; last_blk = blk, blk = blk->next)
	{
		for (str = blk->strings ; str < blk->next_ptr ; str += slen)
		{
			if (len > (slen = (UINT4) *str++))
			{
				continue;
			}
		
			fptr = slen - len + str; 
			ptr = string;
			while (*ptr++ == *fptr)
			{
				if (*fptr++ == '\0') /* Found it! */
				{	
					return fptr - len;
				}
			}
		}
		if (space_blk == (CHAR_BLK *) NULL && blk->rem_len > len)
		{
			space_blk = blk;
		}
	}

	if (convert & FINDONLY)
	{
		return NULL;
	}

	/* Need to store new string.  See if we found space in previous block */
	if (space_blk == (CHAR_BLK *) NULL)
	{
		if ((blk = (CHAR_BLK *) malloc (sizeof(CHAR_BLK))) 
							== (CHAR_BLK *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT,
				"%s: FATAL couldn't allocate CHAR_BLK storage",
				func);
			abort ();
		}
		blk->next_ptr = blk->strings;
		blk->rem_len = sizeof (blk->strings);
		blk->next = (CHAR_BLK *) NULL;
		if (last_blk == (CHAR_BLK *) NULL)
		{
			first_blk = blk;
		}
		else
		{
			last_blk->next = blk;
		}
		space_blk = blk;
	}
	fptr = space_blk->next_ptr;
	*fptr++ = len;
	strcpy (fptr, string);
	space_blk->next_ptr = fptr + len;
	space_blk->rem_len -= len + 1;
	return fptr;
} /* end of add_string () */

/******************************************************************************
 *
 *	Function: authtype_toa
 *
 *	Purpose: Return a string representation of the authreq code.
 *
 *	Returns: pointer to static string.
 *
 *****************************************************************************/

char *
authtype_toa (code)

int             code;		/* code indicating authen, acct, etc. */

{
	static char *typelist[] =
	{
		"invalid(0)",		/* invalid */
		"access",		/* PW_ACCESS_REQUEST */
		"accept",		/* PW_ACCESS_ACCEPT */
		"reject",		/* PW_ACCESS_REJECT */
		"acct-req",		/* PW_ACCOUNTING_REQUEST */
		"acct-resp",		/* PW_ACCOUNTING_RESPONSE */
		"acct-status",		/* PW_ACCOUNTING_STATUS */
		"pw-request",		/* PW_PASSWORD_REQUEST */
		"pw-ack",		/* PW_PASSWORD_ACK */
		"pw-reject",		/* PW_PASSWORD_REJECT */
		"acct-msg",		/* PW_ACCOUNTING_MESSAGE */
		"access-challenge",	/* PW_ACCESS_CHALLENGE */
		"status-server",	/* PW_STATUS_SERVER */
		"status-client"		/* PW_STATUS_CLIENT */
	};
	static char unknown[20];

	if ((code <= 0) || ((sizeof (typelist) / sizeof (typelist[0])) <= code))
	{
		if (code == PW_FORWARDING)
		{
			return "forwarding";
		}

		sprintf (unknown, "invalid(%d)", code);
		return unknown;
	}
 
	return typelist[code];
} /* end of authtype_toa () */

/******************************************************************************
 *
 *	Function: avpair_add
 *
 *	Purpose: Add an attribute-value pair to the given list.
 *
 *	Returns: pointer to added a/v pair upon success,
 *		 NULL pointer upon failure.
 *
 *	Remarks: Always appends the new pair to the end of the list.
 *
 *****************************************************************************/

VALUE_PAIR *
avpair_add (list, attrid, pval, len)

VALUE_PAIR    **list;		/* a list of attribute-value pairs. */
int             attrid;		/* Attribute id number. */
void           *pval;		/* Pointer to value. */
int             len;		/* length of value, or 0 */

{
	VALUE_PAIR     *vp;

	vp = avpair_new (attrid, pval, len);

	if (vp != (VALUE_PAIR *) NULL)
	{
		insert_vp (list, (VALUE_PAIR *) NULL, vp);
	}

	return vp;

} /* end of avpair_add */

/******************************************************************************
 *
 *	Function: avpair_assign
 *
 *	Purpose: Assign the given raw value to an attribute-value pair.
 *
 *	Returns:  0 on success,
 *		 -1 on failure.
 *
 *****************************************************************************/

int
avpair_assign (vp, pval, len)

VALUE_PAIR     *vp;	/* pointer to the attribute-value pair */
void           *pval;	/* pointer to value to be assigned */
int             len;	/* for strings: */
			/*  len == 0 ==> null-terminated ASCII string */
			/*  len > 0  ==> len is length of raw binary data */
			/* for non-strings: */
			/*  len == 0 ==> just plain data, just gets copied */
			/*  len > 0  ==> convert data from network ... */
			/*		... representation before copying */

{
	int             result = -1;
	static char    *func = "avpair_assign";

	if (len < 0 || len > AUTH_STRING_LEN)
	{
		logit (LOG_DAEMON, LOG_ERR, "%s: bad attribute length %d",
			func, len);
	}
	else
	{
		switch (vp->type)
		{
		    case PW_TYPE_STRING: /* Use lvalue field to store length. */
		    case PW_TYPE_OCTETS:
		    case PW_TYPE_VENDOR:
			if (len > 0) /* use length to control the raw copy */
			{
				memcpy (vp->strvalue, (char *) pval, len);
				vp->strvalue[len] = 0; /* In case of string! */
				vp->lvalue = len;
			}
			else /* len == 0 means the string is NULL terminated */
			{
				strncpy (vp->strvalue, (char *) pval,
					AUTH_STRING_LEN);
				vp->lvalue = strlen ((char *) pval);
			}
			result = 0;
			break;

		    case PW_TYPE_DATE:
		    case PW_TYPE_INTEGER:
		    case PW_TYPE_IPADDR:
			if (len > 0) /* need to convert the raw network value */
			{
				vp->lvalue = ntohl(* (UINT4 *) pval);
			}
			else /* len == 0 indicates just plain data */
			{
				memcpy (&vp->lvalue, pval, sizeof (vp->lvalue));
			}
			*vp->strvalue = '\0'; /* Insure null for type IPADDR */
			result = 0;
			break;

		    default:
			dprintf(1, (LOG_DAEMON, LOG_DEBUG,
				"%s: Unknown attribute %d", func, vp->type));
		}
	}
	return result;
} /* end of avpair_assign () */

/******************************************************************************
 *
 *	Function: avpair_copy
 *
 *	Purpose: Copy a specified attribute-value pair from one given
 *		 list to another.
 *
 *	Returns: 0 if the attribute-value pair doesn't exist in the source list,
 *		 1 (non-zero) if the attribute-value pair is copied.
 *
 ******************************************************************************
 */

int
avpair_copy (pdst, psrc, attr)

VALUE_PAIR    **pdst;
VALUE_PAIR     *psrc;
int             attr;

{
	VALUE_PAIR     *pvp;
	VALUE_PAIR     *pnew;
	static char    *func = "avpair_copy";

	if ((pvp = get_vp (psrc, attr)) == (VALUE_PAIR *) NULL)
	{
		return 0;
	}

	if ((pnew = (VALUE_PAIR *) malloc (sizeof (VALUE_PAIR)))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory", func);
		abort ();
	}

	memcpy ((char *) pnew, (char *) pvp, sizeof (VALUE_PAIR));
	pnew->next = (VALUE_PAIR *) NULL;
	insert_vp (pdst, (VALUE_PAIR *) NULL, pnew);

	return 1;
} /* end of avpair_copy () */

/*******************************************************************************
 *
 *	Function: avpair_get
 *
 *	Purpose: Find specified a/v pair from a list and return its value.
 *
 *	Returns: Type of the attribute on success (look at PW_TYPE_*),
 *		 or -1 on failure.
 *
 ******************************************************************************/

int
avpair_get (pval, vplist, attrid)

void           *pval;		/* OUT: buffer for returning value */
VALUE_PAIR     *vplist;		/* pointer to list of attribute-value pairs */
int             attrid;		/* attribute to find */

{
	VALUE_PAIR        *vp;
	static char       *func = "avpair_get";

	vp = get_vp (vplist, attrid);
	if (vp == (VALUE_PAIR *) NULL)
	{
		return (-1);
	}

	switch (vp->type)
	{
	    case PW_TYPE_STRING:
		strcpy ((char *) pval, vp->strvalue);
		break;

	    case PW_TYPE_OCTETS:
	    case PW_TYPE_VENDOR:
		memcpy ((char *) pval, vp->strvalue, vp->lvalue);
		break;

	    case PW_TYPE_DATE:
	    case PW_TYPE_INTEGER:
	    case PW_TYPE_IPADDR:
		*((UINT4 *) pval) = vp->lvalue;
		break;

	    default:
		dprintf(1, (LOG_DAEMON, LOG_DEBUG,
			"%s: Unknown attribute %d", func, vp->type));
	}
	return vp->type;
} /* end of avpair_get () */

/******************************************************************************
 *
 *	Function: avpair_new
 *
 *	Purpose: Make an new attribute-value pair with given parameters.
 *
 *	Returns: Pointer to generated a/v pair when successful,
 *		 NULL when failure.
 *
 *****************************************************************************/

VALUE_PAIR *
avpair_new (attrid, pval, len)

int             attrid;		/* integer id number of the attribute */
void           *pval;		/* pointer to value */
int             len;		/* length of value, or 0 */

{
	VALUE_PAIR     *vp = (VALUE_PAIR *) NULL;
	DICT_ATTR      *pda;
	static char    *func = "avpair_new";

	if ((pda = dict_attrget (attrid)) == (DICT_ATTR *) NULL)
	{
		dprintf(1, (LOG_DAEMON, LOG_DEBUG,
				"%s: Unknown attribute %d", func, attrid));
	}
	else
	{
		if ((vp = (VALUE_PAIR *) malloc (sizeof (VALUE_PAIR)))
							!= (VALUE_PAIR *) NULL)
		{
			strncpy (vp->name, pda->name, sizeof (vp->name));
			vp->attribute = attrid;
			vp->next = (VALUE_PAIR *) NULL;
			vp->type = pda->type;
			if (avpair_assign (vp, pval, len) == 0)
			{
				return vp;
			}
			free (vp);
			vp = (VALUE_PAIR *) NULL;
		}
		else
		{
			logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory",
				func);
			abort ();
		}
	}
	return vp;
} /* end of avpair_new () */

#define	MAX_AVPAIR_VTOA 20

/*************************************************************************
 *
 *	Function: avpair_vtoa
 *
 *	Purpose: Produce a string representation of the value of a pair.
 *
 *************************************************************************/

char *
avpair_vtoa (vp, sws)

VALUE_PAIR	*vp;
int		 sws;

{
  int             max;
  int             pos;
  UINT4           num;
  char           *c;
  char           *p;
  DICT_VALUE     *dval;
  struct in_addr  inad;
  static char     buffers[MAX_AVPAIR_VTOA][1024];
  static int      ndx = 0;
  char           *buff = buffers[ndx];
  static char    *func = "avpair_vtoa";

  if (vp == (VALUE_PAIR *) NULL)
  {
	if ((sws & AVPAIR_VTOA_NULL) != 0)
	{
		return "";
	}
	return NULL;
  }

  ndx++;
  if (ndx > MAX_AVPAIR_VTOA)
  {
	ndx = 0;
  }

  switch (vp->type)
  {
    case PW_TYPE_STRING:
	p = (char *) buff;
	c = vp->strvalue;

	if ((sws & AVPAIR_VTOA_QUOTE) != 0)
	{
		*p++ = '\'';
	}

	max = vp->lvalue;
	if (max == 0)
	{
	  dprintf(2, (LOG_DAEMON, LOG_DEBUG,
		       "%s: Using strlen() for vp 0x%p {type=%d, attr=%d}",
		       func, vp, vp->type, vp->attribute));
	  max = strlen (vp->strvalue);
	}

	pos = 0;
	for (pos = 0; pos < max; pos++)
	{
		if (c[pos] == '\0')
		{
			break;
		}

		switch (c[pos])
		{
		    case '\b':		/* BACKSPACE */
			*p++ = '\\';
			*p++ = 'b';
			break;

		    case '\r':		/* Carriage Return */
			*p++ = '\\';
			*p++ = 'r';
			break;

		    case '\n':		/* Newline / Line feed */
			*p++ = '\\';
			*p++ = 'n';
			break;

		    case '\t':		/* Horizontal tab */
			*p++ = '\\';
			*p++ = 't';
			break;

		    case '\\':		/* Self-escape */
			*p++ = '\\';
			*p++ = '\\';
			break;

		    case '\'':		/* Quote-escape */
			*p++ = '\\';
			*p++ = '\'';
			break;

		    default:
			if ((' ' <= c[pos]) && (c[pos] < 0x7f))
			{
				*p++ = c[pos];
			}
			else
			{
				char tmp[10];

				if (c[pos] == 0)
				{
					strcpy (p, "\\0");
				}
				else
				{
					sprintf (tmp, "\\x%2.2x",
						(unsigned char) c[pos]);
					strcpy (p, tmp);
				}
				p += strlen (p);
			}
		}
	}  /* for each character... */

	if ((sws & AVPAIR_VTOA_QUOTE) != 0)
	{
		*p++ = '\'';
	}

	*p = '\0';
 
	break;

    case PW_TYPE_INTEGER:
	dval = dict_valget (vp->lvalue, vp->name);
	if (dval != (DICT_VALUE *) NULL)
	{
		strcpy (buff, dval->name);
	}
	else
	{
#if defined(__alpha)
	sprintf (buff, "%d", vp->lvalue);
#else	/* defined(alpha) */
	sprintf (buff, "%ld", vp->lvalue);
#endif	/* defined(alpha) */
	}
	break;
 
    case PW_TYPE_IPADDR:
	inad.s_addr = htonl(vp->lvalue);
	strcpy (buff, inet_ntoa (inad));
	break;
 
    case PW_TYPE_DATE:
	strftime (buff, sizeof (buff), "%b %e %Y",
	gmtime ((time_t *) & vp->lvalue));
	break;
 

    case PW_TYPE_OCTETS:
	strcpy (buff, "0x");
	p = (char *) buff;
	p += strlen (buff);
	c = vp->strvalue;

	for (pos = 0; pos < vp->lvalue; pos++)
	{
		char tmp[3];
 
		sprintf (tmp, "%2.2x", c[pos]);
		strcpy (p, tmp);
		if (pos % 4 == 3)
		{
			strcat (p, " ");
		}
		p += strlen (p);
	}
	*p = '\0';
	break;

    case PW_TYPE_VENDOR:
	c = vp->strvalue;
	if (*c != 0)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: Invalid vendor-specific code, 0x%x", func, *c);
		return NULL;
	}
	memcpy ((char *) &num, c, 4);
	pos = ntohl(num);

	sprintf (buff, "v%d-", pos);
	for (pos = 4; pos < vp->lvalue; pos++)
	{
		char tmp[3];
 
		sprintf (tmp, "%2.2x", c[pos]);
		strcpy (p, tmp);
		p += strlen (p);
	}
	*p = '\0';
	break;

    default:
	if (sws & AVPAIR_VTOA_NULL)
	{
		return "";
	}
	return NULL;
  } /* switch (vp) */

return buff;

} /* end of avpair_vtoa () */

/*************************************************************************
 *
 *	Function: compress_file
 *
 *	Purpose: Compress and reopen a file.
 *
 *************************************************************************/

void
compress_file (fd, fname)

FILE          **fd;
char           *fname;

{
	int             pid;
	struct tm      *clock;
	time_t          now;
	char            today[80];
	char            name[128];
	char            newname[128];
	static char    *func = "compress_file";

	now = time (0);
	now -= 86400; /* the archived log is for yesterday! */
	sprintf (name, "%s/%s", radius_dir, fname);
	clock = localtime (&now);
	strftime (today, 15, "%y%m%d", clock);
	sprintf (newname, "%s/%s.%s", radius_dir, fname, today);
	if (link (name, newname) < 0) /* get another handle */
	{
		return;
	}

	if (unlink (name) < 0)
	{
		return;
	}

	sync ();		/* update disk metadata */

	fclose (*fd);

	if ((*fd = fopen (name, "w")) == (FILE *) NULL)
	{
		return;
	}

	now += 86400; /* correct the time now */
	strcpy (today, ctime (&now));
	fprintf (*fd,
		"%-24.24s: created by PID %u Version %s up since %-24.24s\n",
		today, getpid (), RADIUS_VERSION, ctime (&birthdate));
#ifndef SCO
	fchmod (fileno (*fd), 0644);
#endif	/* SCO */
	
	if (spawn_flag > 0) /* only compress if we won't block */
	{
		pid = (int) fork ();
		if (pid < 0) /* error */
		{
	      		logit (LOG_DAEMON, LOG_ALERT,
				"%s: fork: %s", func, get_errmsg ());
			exit (-5);
		}
		if (pid > 0) /* parent */
		{
			return;
		}
		else /* in child */
		{
			execl (RADIUS_COMPRESS, RADIUS_COMPRESS,
				newname, (char *) NULL);
			exit (0);
		}
	}

	return;
} /* end of compress_file () */

/*************************************************************************
 *
 *	Function: debug_list
 *
 *	Purpose: Print the value pair list to the desired File.
 *
 *************************************************************************/

void
debug_list (fd, pair)

FILE           *fd;
VALUE_PAIR     *pair;

{
	VALUE_PAIR     *vp;

	if (debug_flag)
	{
		if (ddt)
		{
			fd = ddt;
		}

		vp = pair;

		while (vp != (VALUE_PAIR *) NULL)
		{
			debug_pair (fd, vp);
			vp = vp->next;
		}
	}
	return;
} /* end of debug_list () */

/*************************************************************************
 *
 *	Function: debug_pair
 *
 *	Purpose: Print the Attribute-value pair to the desired File.
 *
 *************************************************************************/

void
debug_pair (fd, pair)

FILE           *fd;
VALUE_PAIR     *pair;

{
	if (debug_flag)
	{
		if (ddt)
		{
			fd = ddt;
		}

		fputs ("    ", fd);
		fprint_attr_val (fd, pair);
		fputs ("\n", fd);
	}
	return;
} /* end of debug_pair () */

/*************************************************************************
 *
 *	Function: dumpit
 *
 *	Purpose: Dump the memory area using logit()
 *
 *	Usage: dumpit (facility, level, ptr, ptrlen, offset, format, args, ...);
 *
 *		Where facility and level are found in syslog.h and the
 *		format is just a printf-style format string using the args.
 *
 *************************************************************************/

int
dumpit (facility, level, ptr, ptrlen, offset, format, va_alist)

int           facility;		/* Logging facility, see dprintf() */
int           level;		/* Logging level, see dprintf() */
char         *ptr;		/* Pointer to start of dumping region. */
unsigned int  ptrlen;		/* Length of the region to dump. */
unsigned int  offset;		/* Offset from "ptr" to where to start dump. */
char         *format;		/* A sprintf() like format string. */
va_dcl				/* Arguments for sprintf() string. */

{
	va_list         pvar;
	int             done;

	static char     buffer[MAXPATHLEN]; /* Work area. */

	va_start (pvar);
	vsprintf (buffer, format, pvar);
	va_end (pvar);

	if (strlen (buffer) > 0)
	{
		logit (facility, level, "%s", buffer);
	}

	logit (facility, level, "Hex dump at 0x%p/%x for %d bytes", ptr, offset,
	       ptrlen);

	while (ptrlen > 0)
	{
	    done = hex_dump (buffer, ptr, ptrlen, offset);
	    ptrlen -= done;
	    offset += done;
	    ptr += done;
	    logit (facility, level, "%s", buffer);
	}

	return 0;

} /* end of dumpit () */

void
fprint_attr_val (fd, pair)

FILE           *fd;
VALUE_PAIR     *pair;

{
	char           *val;

	if (pair->name[0] == '\0')
	{
		return;
	}

	if ((val = avpair_vtoa (pair, 0)) != (char *) NULL)
	{
	    fprintf (fd, "%s = %s", pair->name, val);
	}
	else
	{
	    fprintf (fd, "%s is unknown", pair->name);
	}

	return;

} /* end of fprint_attr_val () */

/*************************************************************************
 *
 *	Function: gen_valpairs
 *
 *	Purpose: Takes attribute/value pairs from buffer and builds a
 *		 value_pair list using allocated memory.
 *
 *************************************************************************/

VALUE_PAIR *
gen_valpairs (auth)

AUTH_HDR       *auth;

{
	int             attribute;
	int             attrlen;
	int             bad_packet = 0; /* Set to one if bad packet rec'v'd. */
	int             len;
	UINT4           lvalue;
	u_char         *ptr;
	DICT_ATTR      *attr;
	VALUE_PAIR     *vp;
	VALUE_PAIR     *pair;
	static char    *func = "gen_valpairs";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	/*
	 *	Extract attribute-value pairs
	 */
	ptr = auth->data;
	len = ntohs(auth->length) - AUTH_HDR_LEN; /* length of attributes */
	vp = (VALUE_PAIR *) NULL;

	while (len > 0)
	{
		attribute = *ptr++;
		attrlen = *ptr++;
		attrlen -= 2;
		if (attrlen < 0 || attrlen > AUTH_STRING_LEN || attrlen > len)
		{
			dumpit (LOG_DAEMON, LOG_ERR, ptr, attrlen, 0,
				"Received attribute %d with invalid length %d",
				attribute, attrlen);
			bad_packet = 1;
			break;
		}

		if ((attr = dict_attrget (attribute)) == (DICT_ATTR *) NULL)
		{
			ddumpx(1, (LOG_DAEMON, LOG_DEBUG, ptr, attrlen, 0,
				"Received unknown attribute %d of length %d",
				attribute, attrlen));
			bad_packet = 1;
		}
		else
		{
			if ((pair =
				(VALUE_PAIR *) malloc (sizeof (VALUE_PAIR))) ==
					(VALUE_PAIR *) NULL)
			{
				logit (LOG_DAEMON, LOG_ALERT,
					"%s: FATAL out of memory", func);
				abort ();
			}
			strcpy (pair->name, attr->name);
			pair->attribute = attr->value;
			pair->type = attr->type;
			pair->next = (VALUE_PAIR *) NULL;

			switch (attr->type)
			{

			    case PW_TYPE_STRING:
			    case PW_TYPE_OCTETS:
			    case PW_TYPE_VENDOR:
				memcpy (pair->strvalue, (char *) ptr, attrlen);
				pair->strvalue[attrlen] = '\0';
				pair->lvalue = attrlen;
				debug_pair (stdout, pair);
				insert_vp (&vp, (VALUE_PAIR *) NULL, pair);
				break;

			    case PW_TYPE_INTEGER:
			    case PW_TYPE_IPADDR:
				memcpy ((char *) &lvalue, (char *) ptr,
					sizeof (UINT4));
				pair->lvalue = ntohl(lvalue);
				*pair->strvalue = '\0'; /* zap DNS for IPADDR */
				debug_pair (stdout, pair);
				insert_vp (&vp, (VALUE_PAIR *) NULL, pair);
				break;

			    default:
				dprintf(1, (LOG_DAEMON, LOG_DEBUG,
					"    %s (Unknown Type)", attr->name));
				free (pair);
				break;
			}

		}
		ptr += attrlen;
		len -= attrlen + 2;
	}

	if (bad_packet)
	{
		ddumpx(1, (LOG_DAEMON, LOG_DEBUG, auth->data,
			ntohs(auth->length) - AUTH_HDR_LEN, 0,
			"Dump of entire bad packet"));
	}

	return (vp);
} /* end of gen_valpairs () */

/*************************************************************************
 *
 *	Function: get_errmsg
 *
 *	Purpose: Return pointer to static string which gives complete
 *		 filesystem error message.
 *
 *************************************************************************/

char *
get_errmsg ()

{
	static char     errmsg[80];

	if (errno < sys_nerr)
	{
		return (char *) sys_errlist[errno];
	}
	else
	{
		sprintf (errmsg, "Error %d", errno);
		return errmsg;
	}
} /* end of get_errmsg () */

/******************************************************************************
 *
 *	Function: get_passwd
 *
 *	Purpose: Decrypts password contained in the PW_USER_PASSWORD or
 *		 PW_ENCRYPTED_PASSWORD value-pair in the AUTH_REQ queue
 *		 entry.  Tries to match one of these passwords against
 *		 the "user_secret", if the "user_secret" is not a NULL
 *		 pointer.  Will also perform a CHAP authentication check,
 *		 if the PW_CHAP_PASSWORD value-pair is found in the request
 *		 and "user_secret" is provided.
 *
 *	Returns: 2 to indicate CHAP failure,
 *		 1 to indicate PASSWORD match failure,
 *		 0 to indicate success,
 *		-1 if no PASSWORD or CHAP value-pair is present.
 *
 ******************************************************************************/

int
get_passwd (authreq, pw, user_secret, salt)

AUTH_REQ       *authreq;	/* Pointer to authentication request.  */
char           *pw;		/* Receives decrypted password.        */
				/* (If NULL, only CHAP is attempted.)  */
char           *user_secret;	/* Secret we share with this user or   */
				/* NULL.  If NULL, CHAP is not allowed */
				/* and the password match check is not */
				/* performed */
char           *salt;		/* Encryption salt.  If this and the   */
				/* user_secret are both non-NULL, then */
				/* crypt() is called to encrypt the    */
				/* password before attempting to match */
				/* the user_secret or the check CHAP   */
				/* reply.*/

{
	VALUE_PAIR     *item;
	u_char          buffer[AUTH_PASS_LEN + AUTH_VECTOR_LEN + 1];
	int             i;
	int             secretlen;
	int             result;
	u_char          digest[CHAP_VALUE_LENGTH];
	u_char         *ptr;
	char           *crypt ();
	static char    *func = "get_passwd";

	dprintf(3, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (pw != NULL &&
		(item = get_vp (authreq->request, PW_USER_PASSWORD))
							!= (VALUE_PAIR *) NULL)
	{
		/* Use the secret to setup the decryption digest */
		secretlen = strlen ((char *) authreq->secret);
		strcpy ((char *) buffer, (char *) authreq->secret);
		memcpy ((char *) buffer + secretlen, (char *) authreq->vector,
			AUTH_VECTOR_LEN);
		md5_calc (digest, buffer, secretlen + AUTH_VECTOR_LEN);
		memcpy (pw, item->strvalue, AUTH_PASS_LEN);
		for (i = 0; i < AUTH_PASS_LEN; i++)
		{
			pw[i] ^= digest[i];
		}
		pw[AUTH_PASS_LEN] = '\0'; /* thanks to billw@cisco.com */
		result = 0;
		if (user_secret != (char *) NULL)
		{
			if (salt != (char *) NULL)
			{
				pw = crypt (pw, salt);
			}

			if ((result = strcmp (pw, user_secret)) != 0)
			{
				result = 1;	/* Indicate pw match failure */
			}
		}
	}
	else if (user_secret != (char *) NULL &&
			(item = get_vp (authreq->request, PW_CHAP_PASSWORD))
							!= (VALUE_PAIR *) NULL)
	{
		/* Use MD5 to verify CHAP */
		ptr = buffer;
		*ptr++ = *item->strvalue;
		strcpy ((char *) ptr, user_secret);

		/*
		 * Allow for CHAP using an encrypted password. (The client
		 * encrypts the password before generating the CHAP challenge
		 * reply.)  This lets CHAP be used even when passwords are
		 * stored in an encrypted form.  No clients provide this
		 * capability yet, but they could start doing so now!
		 */

		if (salt != (char *) NULL)
		{
			ptr = (u_char *) crypt ((char *) ptr, salt);
		}

		secretlen = strlen ((char *) ptr);
		ptr += secretlen;
		memcpy ((char *) ptr, (char *) authreq->vector,
			AUTH_VECTOR_LEN);
		md5_calc (digest, buffer,
			  1 + CHAP_VALUE_LENGTH + secretlen);
		/* Compare them */
		if ((result = memcmp ((char *) digest, item->strvalue + 1,
				      CHAP_VALUE_LENGTH)) != 0)
		{
			result = 2;	/* Indicate CHAP failure */
		}

	}
	else
	{
		if (pw)
			*pw = '\0';	/* Return null as pw */
		result = -1;	/* -1 indicates no PW or CHAP vp in request */
	}
	return (result);
} /* end of get_passwd () */

/*************************************************************************
*
*	Function: get_last_vp
*
*	Purpose: Find the last attribute value-pair (which matches the specified
*		 attribute) from the specified value-pair list.
*
*************************************************************************/

VALUE_PAIR *
get_last_vp (vp, attr)

VALUE_PAIR     *vp;
UINT4           attr;

{
	VALUE_PAIR     *last_vp;

	/*
	 *	Find the first matching value-pair.
	 */	

	for (; vp != (VALUE_PAIR *) NULL && vp->attribute != attr; vp = vp->next)
	{
		continue;
	}

	/*
	 *	If we run off the end of the list, return a NULL.
	 */
	if (vp == (VALUE_PAIR *) NULL)
	{
		return (VALUE_PAIR *) NULL;
	}

	/* 
	 *	Scan the remainder of the list for a match.
	 *	If we found a match, return it.
	 */
	last_vp = get_last_vp (vp->next, attr);

	if (last_vp != (VALUE_PAIR *) NULL)
	{
		return (last_vp);
	}

	/* If didn't find a match, return the one we found above. */
	return (vp);	

} /* end of get_last_vp () */

/*************************************************************************
*
*	Function: get_vp
*
*	Purpose: Find the first attribute value-pair (which matches the given
*		 attribute) from the specified value-pair list.
*
*************************************************************************/

VALUE_PAIR *
get_vp (vp, attr)

VALUE_PAIR     *vp;
UINT4           attr;

{
	for (; vp != (VALUE_PAIR *) NULL && vp->attribute != attr; vp = vp->next)
	{
		continue;
	}

	return (vp);
} /* end of get_vp () */

/*************************************************************************
 *
 *	Function: hex_dump
 *
 *	Purpose: Format a region of octets into a formatted string
 *		 suitable for displaying that region of memory.
 *
 *************************************************************************/

int
hex_dump (buffer, data, len, offset)

char         *buffer;
char         *data;
int           len;
int           offset;

{
    int             i = 0;
    int             j = 0;
    int             size = 0;
    int             wlen = 0;
    char            hex[3];  /* a hex string placeholder. */
    unsigned char  *start = (unsigned char *) data;

    if ((!buffer) || (!data) || (len <= 0))
    {
	return -1;
    }

    sprintf (buffer, "0x%p:", data);
    
    buffer += strlen (buffer);
    if (offset >= 0)
    {
	sprintf (buffer, " 0x%4.4x|", offset);
	buffer += strlen (buffer);
    }
    
    if (len > 16)
    {
	len = 16;
    }
    
    /* Dump the data in hexadecimal. */

    for (i = 0; i < len; i += 4)
    {
	wlen = i + 4;
	if (wlen > len)
	{
		wlen = len;
	}

	strcat (buffer, " ");
	buffer++;
	for ( j = i; j < wlen; j++)
	{
	    sprintf (hex, "%2.2X", *start++);
	    strcat (buffer, hex);
	    buffer += 2;
	    size++;
	}
	
	for ( ; j < (i + 4) ; j++)
	{
	    strcat (buffer, "..");
	    buffer += 2;
	}
    }
    
    for ( ; i < 16 ; i += 4)
    {
	strcat (buffer, " ........");
	buffer += 9;
    }

    /* Dump the data in ASCII. */

    strcat (buffer, "| |");
    buffer += strlen (buffer);
    for (i = 0; i < len; i++)
    {
	if ((' ' <= *data) && (*data < 0x7f))
	{
	    *buffer++ = *data;
	}
	else
	{
	    *buffer++ = '.';
	}
	data++;
    }

    for ( ; i < 16 ; i++)
    {
	*buffer++ = '|';
    }

    *buffer++ = '|';
    *buffer = '\0';  /* Terminate the string. */

    return size;
 
} /* end of hex_dump () */

/*************************************************************************
 *
 *	Function: insert_vp
 *
 *	Purpose: Given the address of an existing list "a" and a pointer
 *		 to an entry "p" in that list, add the value pair "b" to
 *		 the "a" list after the "p" entry.  If "p" is NULL, add
 *		 the value pair "b" to the end of "a".
 *
 *************************************************************************/

void
insert_vp (a, p, b)

VALUE_PAIR     **a;
VALUE_PAIR     *p;
VALUE_PAIR     *b;

{
	VALUE_PAIR     *this_node;
	VALUE_PAIR     *vp;
	static char    *func = "insert_vp";

	if (b->next != (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT,
			"%s: FATAL value pair (0x%p) next ptr. (0x%p) not NULL",
			func, b, b->next);
		dumpcore = 1;
		abort ();
	}

	if (*a == (VALUE_PAIR *) NULL)
	{
		*a = b;
		return;
	}

	vp = *a;

	if ( p == (VALUE_PAIR *) NULL) /* run to end of "a" list */
	{
		while (vp != (VALUE_PAIR *) NULL)
		{
			this_node = vp;
			vp = vp->next;
		}
	}
	else /* look for the "p" entry in the "a" list */
	{
		this_node = *a;
		while (this_node != (VALUE_PAIR *) NULL)
		{
			if (this_node == p)
			{
				break;
			}
			this_node = this_node->next;
		}
	}

	b->next = this_node->next;
	this_node->next = b;

	return;
} /* end of insert_vp () */

/*************************************************************************
 *
 *	Function: logit
 *
 *	Purpose: Log the provided error message to the error logging facility.
 *
 *	Usage: logit (facility, level, format, args, ...);
 *
 *		Where facility and level are found in syslog.h and the
 *		format is just a printf-style format string using the args.
 *
 *************************************************************************/

int
logit (va_alist)

va_dcl

{
	va_list         pvar;
	int             priority;
	int             facility;
	int             level;
	char           *format;
	time_t          timeval;
	char            filename[MAXPATHLEN];
	static char     buffer[MAXPATHLEN];
	static char    *func = "logit";

	va_start (pvar);
	facility = va_arg (pvar, int);
	level = va_arg (pvar, int);
	format = va_arg (pvar, char *);
	vsprintf (buffer, format, pvar);
	va_end (pvar);

	if (file_logging == 1) /* log it to the logfile */
	{
		if (msgfd == stderr)
		{
			msgfd = (FILE *) NULL;
		}

		if (msgfd == (FILE *) NULL)
		{
			sprintf (filename, "%s/%s", radius_dir, RADIUS_LOG);
			msgfd = fopen (filename, "a");

			if (msgfd == (FILE *) NULL)
			{
				fprintf (stderr,
					"%s: Couldn't open %s for logging\n",
					func, filename);
				msgfd = stderr;
			}
		}

		if (level != LOG_DEBUG) /* don't log debugging messages */
		{
			timeval = time (0);
			fprintf (msgfd, "%-24.24s: %s\n",
				ctime (&timeval), buffer);

			if (level == LOG_ALERT)
			{
				fflush (msgfd);
			}
		}
	}
	else /* was not logging to a file */
	{
		if (file_logging == 2) /* log it to stderr */
		{
			fprintf (msgfd, "%s\n", buffer);
		}
		else /* log it to syslog(3) */
		{
			zap_logfile = 0;
			priority = facility | level;
			syslog (priority, "%s", buffer);
		}
	}

	if (level == LOG_DEBUG && ddt != (FILE *) NULL)
	{
		fprintf (ddt, "%s\n", buffer); /* log it to pre-opened device */
	}

	return 0;
} /* end of logit () */

/******************************************************************************
 *
 *	Function: loghead
 *
 *	Purpose: Write a header in a logfile if it is empty.  This function
 *		 guarantees the header is only written once at the very
 *		 beginning of the file.
 *
 *	Usage: loghead (file, format, args, ...);
 *
 *		Where file is a FILE pointer to an open file and the
 *		format is just a printf style format string using the args.
 *
 *****************************************************************************/

int
loghead (va_alist)

va_dcl

{
	va_list          pvar;
	int              fd;
	char            *format;
	FILE            *fp;
	struct flock     lck;

	va_start (pvar);
	fp = va_arg (pvar, FILE *);
	fd = fileno (fp);
	lck.l_type = F_WRLCK;
	lck.l_whence = 0;
	lck.l_start = SEEK_SET;
	lck.l_len = 0L;
	lck.l_pid = (pid_t) 0;
	fcntl (fd, F_SETLKW, lck);
	if (lseek (fd, 0L, SEEK_CUR) == (off_t) 0L)
	{
		format = va_arg (pvar, char *);
		vfprintf (fp, format, pvar);
		va_end (pvar);
	}
	lck.l_type = F_UNLCK;
	fcntl (fd, F_SETLK, lck);
	return 0;
} /* end of loghead () */

/*************************************************************************
 *
 *	Function: missing_attribute
 *
 *	Purpose: Generate standard log message for missing attributes.
 *
 *************************************************************************/

void
missing_attribute (authreq, func, attribute, etc)

AUTH_REQ       *authreq;
char	       *func;		/* Function which called us. */
int		attribute;	/* Missing attribute. */
char	       *etc;		/* Optional additional information. */

{
	VALUE_PAIR    *vp;
	DICT_ATTR     *attr = dict_attrget (attribute);
	char          *attr_name;
	char           unknown_attr[20];

	if (attr != (DICT_ATTR *) NULL)
	{
		attr_name = attr->name;  /* Mark name. */
	}
	else
	{
		sprintf (unknown_attr, "unknown(%d)", attribute);
		attr_name = unknown_attr;
	}

	/* Get NAS-Identifier or IP address. */
	if ((vp = get_vp (authreq->cur_request, PW_NAS_IDENTIFIER))
							== (VALUE_PAIR *) NULL)
	{
		vp = get_vp (authreq->cur_request, PW_NAS_IP_ADDRESS);
	}

	logit (LOG_DAEMON, LOG_ERR,
       "%s:* MISSING %s (%d) in %s (type %d) request %d from %s via. %s[%d] %s",
		func, attr_name, attribute, authtype_toa (authreq->code),
		authreq->code, authreq->id,
		(vp == (VALUE_PAIR *) NULL) ? "?" : avpair_vtoa (vp, 0),
		ip_hostname (authreq->ipaddr), authreq->udp_port,
		(etc == (char *) NULL)  ? "" : etc);
	return;
} /* end of missing_attribute () */

/*************************************************************************
 *
 *	Function: parse_realm
 *
 *	Purpose: Split user entered string found in PW_USER_NAME into
 *		 the PW_USER_ID and PW_USER_REALM a/v pairs and hang
 *		 them both on both the authreq->request and the
 *		 authreq->cur_request lists for later use.
 *
 *	Returns: pointer to the realm a/v pair,
 *		 NULL otherwise.
 *
 *************************************************************************/

VALUE_PAIR *
parse_realm (authreq)

AUTH_REQ       *authreq;

{
	int             count;
	int             type;
	VALUE_PAIR     *vp;
	VALUE_PAIR    **vp_prev;
	DICT_ATTR      *attr;
	char           *agent;
	char           *filter;
	char           *pos;
	char           *realm;
	char           *u_realm;
	char           *userid;
	char            name[AUTH_ID_LEN + 1];
	static char    *func = "parse_realm";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if ((vp = get_vp (authreq->cur_request, PW_USER_REALM))
							!= (VALUE_PAIR *) NULL)
	{
		return vp;	/* It's already there, so just return it! */
	}

	if ((vp = get_vp (authreq->cur_request, PW_USER_ID))
							== (VALUE_PAIR *) NULL)
	{
		return vp;	/* The PW_USER_ID must be there to parse! */
	}

	/* Make a local copy of the name. */
	strncpy (name, vp->strvalue, AUTH_ID_LEN);
	name[AUTH_ID_LEN] = '\0'; /* Guarantee a null terminated string. */

	if ((pos = strchr (name, '@')) != (char *) NULL) /* Is the '@' style. */
	{
		if ((userid = strtok (name, "@")) == (char *) NULL)
		{
			return (VALUE_PAIR *) NULL; /* Can't use a null name. */
		}
		if ((u_realm = strtok (NULL, "")) == (char *) NULL)
		{
			u_realm = "";
		}
	}
	else /* There was no '@' in the User-Id. */
	{
		if ((pos = strchr (name, '/')) != (char *) NULL) /* '/' style */
		{
			if ((u_realm = strtok (name, "/")) == (char *) NULL)
			{
				u_realm = "";
			}
			if ((userid = strtok (NULL, "")) == (char *) NULL)
			{
				return (VALUE_PAIR *) NULL; /* Name was null. */
			}
		}
		else /* There was neither '@' nor '/' in the User-Id. */
		{
			userid = name;
			u_realm = "";
		}
	}

	/* Check for null user id. */
	if ((userid == (char *) NULL) || (*userid == '\0'))
	{
		return (VALUE_PAIR *) NULL; /* Name was null. */
	}

	strcpy (vp->strvalue, userid);	/* Modify the existing value... */
	vp->lvalue = strlen (vp->strvalue);	/* and set length correctly. */

	/* Next, build the realm a/v pair. */
	if ((vp = (VALUE_PAIR *) malloc (sizeof (VALUE_PAIR)))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT,
			"%s: FATAL out of memory", func);
		abort ();
	}

	attr = dict_attrget (PW_USER_REALM);
	strcpy (vp->name, attr->name);
	vp->attribute = attr->value;
	vp->type = attr->type;
	vp->next = (VALUE_PAIR *) NULL;

	if ((u_realm == (char *) NULL) || (u_realm[0] == '\0'))
	{
		vp->strvalue[0] = '\0';
		vp->lvalue = 0;
	}
	else /* There is a realm string. */
	{
		if (find_auth_type (u_realm, PW_PROTTYPE_DFLT,
					(char *) authreq->file_pfx, &type,
					&agent, &realm, &filter) != 0)
		{
			strcpy (vp->strvalue, u_realm);
		}
		else /* Use primary (canonical) realm name. */
		{
			strcpy (vp->strvalue, realm);
		}
		vp->lvalue = strlen (vp->strvalue);	/* Set length */
	}
	
	/*
	 *	Now stick it at end of original request items on the
	 *	cur_request list so it can be included in cur_count.
	 */
	count = authreq->cur_count;
	vp_prev = &authreq->cur_request;
	while (count-- > 0)
	{
		if ((vp_prev = &(*vp_prev)->next) == (VALUE_PAIR **) NULL)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: cur_count is bad!", func);
			abort ();
		}
	}
	vp->next = *vp_prev;
	*vp_prev = vp;

	authreq->cur_count++; /* indicate and record the additional a/v pair */

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: name = '%s', realm = '%s'",
		func, userid, vp->strvalue));

	return vp;
} /* end of parse_realm () */

/*************************************************************************
 *
 *	Function: prune_pairs    XXX: turn this into an AATV later
 *
 *	Purpose: Remove all extraneous a/v pairs before replying to NAS.
 *
 *	Returns: 0 == normal return,
 *		-1 == some error occurred.
 *
 *	Remark: This code may change the order of the a/v pairs returned.
 *
 *************************************************************************/

int
prune_pairs (authreq, vendor, result)

AUTH_REQ       *authreq;	/* modify this request's cur_request list */
PRUN_LIST      *vendor;		/* for this vendor's NAS */
int             result;		/* the packet type Access-Accept or -Reject */

{
	int             i;
	int             j;
	int             msg_flag;
	int             success;
	VALUE_PAIR    **prev_ptr;
	VALUE_PAIR     *vp;
	short           cnt[256];	/* holds the count of each attribute */
	static char    *func = "prune_pairs";

	switch (result)
	{
	    case EV_ACK:
		msg_flag = PRUN_FLG1;
		break;

	    case EV_NAK:
		msg_flag = PRUN_FLG2;
		break;

	    case EV_ACC_CHAL: /* Not handled according to RADIUS DRAFT RFC */
		return 0;
		break;

	    default:
		return (-1);
		break;
	}

	if (result == EV_ACK && authreq->code == PW_ACCOUNTING_REQUEST) /* no */
	{
		list_free (authreq->cur_request);
		authreq->cur_request = (VALUE_PAIR *) NULL;
		return 0;
	}

	/* Initialize the array cnt[]. */

	memset (cnt, '\0', sizeof (cnt));

	/* Record the frequency of each reply a/v pair. */

	prev_ptr = &authreq->cur_request;
	for ( vp = *prev_ptr;
		vp != (VALUE_PAIR *) NULL ;
		vp = *prev_ptr)
	{
		if (vp->attribute > 255) /* then it must be a check-item */
		{
			*prev_ptr = vp->next;
			free (vp);	/* this one is not allowed */
		}
		else /* it is a reply-item, so count it */
		{
			++cnt[vp->attribute];
			prev_ptr = &vp->next;
		}
	}

	/* Prune the cur_request list. */

	prev_ptr = &authreq->cur_request;
	for (vp = *prev_ptr;
		vp != (VALUE_PAIR *) NULL;
		vp = *prev_ptr)
	{
		j = 0;
		success = 0;
		while (vendor->rules[j].value > 0) /* last element is == zero */
		{
			if (vendor->rules[j].value == vp->attribute)
			{
				success = 1;
				break;
			}
			j++;
		}

		if (success) /* then j is the index into the rules array */
		{
			/* See if this attribute type is allowed in this msg? */
			if (vendor->rules[j].flags & msg_flag)
			{
				i = vendor->rules[j].count;
				if (i < 0)
				{
					i = 9999; /* pick a suitable infinity */
				}

				if (cnt[vp->attribute] > i) /* reduce counter */
				{
					if (cnt[vp->attribute] > 0) /* if pos */
					{
						--cnt[vp->attribute];
					}
					*prev_ptr = vp->next;
					free (vp); /* this one is not allowed */
					vp = (VALUE_PAIR *) NULL;
				}
				else /* it's a keeper! */
				{
					prev_ptr = &vp->next;
				}
			}
			else /* no, unconditionally remove this a/v pair */
			{
				*prev_ptr = vp->next;
				free (vp);	/* this one is not allowed */
				vp = (VALUE_PAIR *) NULL;
			}
		}
		else /* this a/v pair is not in the rules, logit and leave it */
		{
			logit (LOG_AUTH, LOG_ERR,
				"%s: odd attribute number %d in reply",
				func, vp->attribute);
			prev_ptr = &vp->next;
		}
	}

	return 0;
} /* end of prune_pairs () */

/*************************************************************************
 *
 *	Function: _reply_message
 *
 *	Purpose: Generate a reply message and stick it into the list
 *		 of reply items to send back to the client.
 *
 *	Returns: 0 == normal return
 *		-1 == msgno is out of range
 *
 *	Remarks: The message is also logged.
 *
 *************************************************************************/

int
_reply_message (authreq, msgno, func, filename, line)

AUTH_REQ       *authreq;	/* this authentication request */
ERRORCODE       msgno;		/* message number */
char           *func;		/* calling function name */
char           *filename;	/* Filename were error occured. */
int             line;		/* Line number where error occured. */

{
	static char    *msg[] =
	{
		"Internal error",
		"Configuration error",
		"Out of memory",
		"Error creating file",
		"No token available",
		"No ports available for guests",
		"Too many simultaneous sessions",
		"ABS failure",
		"Error querying balance",
		"Your account balance is too low"
	};

	if ((u_int)(--msgno) < numbof(msg))
	{
		logit (LOG_AUTH, LOG_INFO, "%s: %s at %s line %d", func,
			msg[msgno], filename, line);
		reply_sprintf (0, authreq, "%s [%s()]", msg[msgno], func);
		return 0;
	}
	reply_sprintf (0, authreq, "Software error [%s()]", func);
	logit (LOG_AUTH, LOG_ERR, "%s: message %d out of range at %s line %d",
		func, msgno, filename, line);
	return (-1);
} /* end of _reply_message () */

/*************************************************************************
 *
 *	Function: reply_sprintf
 *
 *	Purpose: Generate a reply message and stick it into the list
 *		 of reply items to send back to the client.
 *
 *	Returns: 0 == normal return
 *		-1 == authreq is bad somehow.
 *
 *	Remarks: The message is also logged if logsw is 1.
 *               It ALWAYS adds a <CR><LF> ("\r\n") to the message.
 *
 *************************************************************************/

int
reply_sprintf ( va_alist /* int logsw, AUTH_REQ *authreq, char *format, ... */)

va_dcl

{
	va_list         pvar;

	int             logsw;  /* First argument. */
	AUTH_REQ       *authreq;
	VALUE_PAIR     *msg;
	char           *format;

	char            buf[MAXPATHLEN];

	/* Gather parameters. */

	va_start (pvar);
	logsw = va_arg (pvar, int);
	authreq = va_arg (pvar, AUTH_REQ *);
	format = va_arg (pvar, char *);

	/* Format message, add it to reply, log it if necessary. */

	vsprintf (buf, format, pvar);

	if ( logsw != 0 )
	{
	      logit (LOG_AUTH, LOG_INFO, "%s", buf);
	}

	strcat (buf, "\r\n");     /* Force newline. */
	if ((msg = get_last_vp (authreq->cur_request, PW_REPLY_MESSAGE))
							!= (VALUE_PAIR *) NULL)
	{
		/* Check to see if there's any space remaining. */
		if ((strlen (buf) + strlen (msg->strvalue)) > AUTH_STRING_LEN)
		{
			msg = (VALUE_PAIR *) NULL;
		}
	}

	/* Concatenate the new message onto the last message. */
	if (msg != (VALUE_PAIR *) NULL)
	{
		strcat (msg->strvalue, buf);
	}
	else
	{
		avpair_add (&authreq->cur_request, PW_REPLY_MESSAGE, buf, 0);
	}

	return 0;
} /* end of reply_sprintf () */

/*************************************************************************
 *
 *	Function: setupsock
 *
 *	Purpose: Gets and binds a socket.
 *
 *************************************************************************/

int
setupsock (sin, portnum)

struct sockaddr_in *sin;
int             portnum;

{
	int             sock;
	int             sinlen;
	static char    *func = "setupsock";

	/*
	 * Get a socket.
	 */
	if ((sock = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
	{
		fprintf (stderr, "%s: socket() UDP socket %d failed, %s\n",
			func, portnum, sys_errlist[errno]);
		exit (-11);
	}
	sinlen = sizeof (struct sockaddr_in);
	memset ((char *) sin, '\0', sinlen);

	/*
	 * Get server's listening port number
	 */
	sin->sin_port = htons(portnum);

	/*
	 * Bind socket to port.  bind finds free port if portnum == 0
	 */
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = INADDR_ANY;
	if (bind (sock, (struct sockaddr *) sin,
		sizeof (struct sockaddr_in)) < 0)
	{
		fprintf (stderr, "%s: bind() UDP socket %d failed, %s\n",
			func, portnum, sys_errlist[errno]);
		exit (-11);
	}
	/* Retrieve complete socket info */
	if (getsockname (sock, (struct sockaddr *) sin, &sinlen) < 0)
	{
		fprintf (stderr, "%s: getsockname() UDP socket %d failed, %s\n",
			func, portnum, sys_errlist[errno]);
		exit (-11);
	}
	return sock;
} /* end of setupsock () */

/*************************************************************************
 *
 *	Function: trunc_logfile
 *
 *	Purpose: Truncate, compress and rename the logfile near midnight.
 *
 *************************************************************************/

void
trunc_logfile (fd, fname)

FILE          **fd;
char           *fname;

{
	static int      archived = 0;
	struct tm      *clock;
	time_t          now;

	now = time (0);
	clock = localtime (&now);
	if (((clock->tm_wday == TRUNCATION_DAY) || (TRUNCATION_DAY == 7)) &&
		(clock->tm_hour == 0 && archived == 0))
	{
		compress_file (fd, fname);

		archived = 1;
	}
	else
	{
		if (clock->tm_hour != 0)
		{
			archived = 0;
		}
	}

	return;
} /* end of trunc_logfile () */

/*************************************************************************
 *
 *	Function: type_string
 *
 *	Purpose: Returns protocol type string for logging of authentication
 *		 requests.
 *
 *************************************************************************/

char *
type_string (authreq, protpair)

AUTH_REQ       *authreq;
VALUE_PAIR     *protpair;

{
	char           *ptr;
	VALUE_PAIR     *user_type;
	static char     string[10];

	user_type = get_vp (authreq->request, PW_SERVICE_TYPE);
	if (user_type != (VALUE_PAIR *) NULL &&
		user_type->lvalue == PW_AUTHENTICATE_ONLY)
	{
		ptr = "auth";
	}
	else
	{
		ptr = (protpair == (VALUE_PAIR *) NULL)
			? "dumb" : avpair_vtoa (protpair, AVPAIR_VTOA_NULL);
	}
	strncpy (string, ptr, sizeof (string));
	return (string);
} /* end of type_string () */
