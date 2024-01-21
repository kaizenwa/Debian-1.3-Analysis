/*  VER 038  TAB P   $Id: logmsg.c,v 1.12 1996/11/22 12:31:52 src Exp $
 *
 *  handle error messages and such...
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"

#include <stdarg.h>
#include <errno.h>
#if HAVE_SYSLOG_H
  #include <syslog.h>
#endif

/*
 *  open log facility
 */	
void log_open()
{
    /* open syslog if required */
#if HAVE_SYSLOG_H
#ifdef LOG_NEWS
    openlog(pname, LOG_PID, LOG_NEWS);
#else
    openlog(pname, LOG_PID);
#endif
#endif
}

#if !HAVE_VPRINTF
  /* poor souls: give them a surrogate */
  #define vsprintf(buf, fmt, ap) strcpy(buf, fmt)
#endif

/*
 *  log a message   
 *
 *  the type is:
 *    L_ERR	syslog LOG_ERR
 *    L_ERRno	syslog LOG_ERR, consult errno
 *    L_INFO	syslog LOG_INFO
 *    L_DEBUG	 debug
 *    L_GET	 debug, w/crlf
 *    L_PUT	 debug, w/crlf
 */	
void log_msg(int type, const char *fmt, ...)
{
    va_list ap;
    int e;
    char buf[BUFSIZ]; /* BUG: can we overwrite it? */

    va_start(ap, fmt);

    switch (type) {
    default:
    case L_ERRno:
    case L_ERR:
	e = errno;
	vsprintf(buf, fmt, ap);
#if HAVE_STRERROR
	if (type == L_ERRno) {
	    sprintf(buf + strlen (buf), ": %s", strerror(e));
	}
#endif
	strcat(buf, "\n");
#if HAVE_SYSLOG_H
	if (!debug_flag)
	    syslog(LOG_ERR, buf);
	else
#endif
	    fprintf(stderr, "%s: %s", pname, buf);
	break;

    case L_INFO:
	vsprintf(buf, fmt, ap);
	strcat(buf, "\n");
#if HAVE_SYSLOG_H
	if (!debug_flag)
	    syslog(LOG_INFO, buf);
	else
#endif
	    fprintf(stderr, "%s", buf);
	break;

    case L_GET:
	if (debug_flag >= 2) {
	    vsprintf(buf, fmt, ap);
	    fprintf(stderr, "<- %s", buf);
	}
	break;

    case L_PUT:
	if (debug_flag >= 2) {
	    vsprintf(buf, fmt, ap);
	    fprintf(stderr, "-> %s", buf);
	}
	break;

    case L_DEBUG3:
	if (debug_flag < 3) break;
    case L_DEBUG: 
	if (debug_flag >= 1) {
	    vsprintf(buf, fmt, ap);
	    fprintf(stderr, "%s\n", buf);
	}
	break;
    }
    va_end (ap);
}
