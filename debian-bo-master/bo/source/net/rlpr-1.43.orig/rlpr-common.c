/* filename: rlpr-common.c
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr-common.c,v 1.13 1997/01/14 04:58:06 meem Exp $
 * contents: general-purpose functions for rlpr
 *
 * Time-stamp: <1997/01/13 19:01 -- meem@sherilyn.wustl.edu>
 */

/* copyright (c) 1996, 1997 meem, meem@gnu.ai.mit.edu
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include "config.h"

#ifdef HAVE_SYSLOG_H
#include <syslog.h>		      /* for syslog(), LOG_* */
#endif /* HAVE_SYSLOG_H */

#include <fcntl.h>                    /* for fcntl()         */
#include <sys/types.h>                /* for off_t           */
#include <stdio.h>
#include <string.h>                   /* for strerror()      */
#include <ctype.h>                    /* for tolower()       */
#include <sys/stat.h>                 /* for fstat()         */
#include <sys/utsname.h>              /* for uname()         */
#include <unistd.h>                   /* for fstat()         */
#include <stdarg.h>                   /* for varargs         */
#include <stdlib.h>                   /* for malloc()        */
#include <netdb.h>                    /* for gethostbyname() */
#include "rlpr-common.h"

/* modified slightly from Stevens, "UNIX Network Programming", p. 279 */

int writen(int fd, const char *ptr, int nbytes) {
  int nleft, nwritten;

  nleft = nbytes;
  while (nleft > 0) {
    nwritten = write(fd, ptr, nleft);
    if (nwritten <= 0) return nwritten;
    nleft -= nwritten;
    ptr   += nwritten;
  }

  return nbytes;
}

/* not used right now but it will be handy when i implement non-blocking
 * reads a writes
 */

int set_fl(int fd, int *flags) {
  int val;
  
  if ((val = fcntl(fd, F_GETFL, 0)) < 0)
    return -1;
  
  if (fcntl(fd, F_SETFL, val | *flags) < 0)
    return -1;
    
  *flags = val;
  return 1;
}

int read_fd_to_fd(int rfd, int wfd) {
  static char buf[BUFSIZ * 4];
  int count;
  
  while ((count = read(rfd, buf, sizeof buf)) > 0)
    safe_writen(wfd, buf, count);
  return count;
}

int get_local_hostname(char * dest, size_t dest_sz) {
  struct utsname buf;
  if (uname(&buf) < 0) return -1;

  strncpy(dest, buf.nodename, dest_sz);
  return 0;
}

void init_sockaddr(struct sockaddr_in *sin,
                   const char *hostname, u_short port_hbo)
{
  struct hostent *hp;
  
  memset(sin, 0, sizeof(*sin));
  sin->sin_family = AF_INET;

  if (hostname) 
    if ((hp = gethostbyname(hostname)) == NULL)
      rlpr_msg(FATAL, NO_ERRNO,	      /* FIXME this is a policy decision */
	       "hostname \"%s\" does not seem to exist!", hostname);
    else memcpy(&sin->sin_addr, hp->h_addr, hp->h_length);
    
  if (port_hbo)
    sin->sin_port = htons(port_hbo);
}

void toggle_euid(void) {
  static int times_called = 0;
  static uid_t setuid_uid, euid;

  if (!times_called)
    setuid_uid = geteuid();
  euid = (times_called++ % 2) ? setuid_uid : getuid();
  
#ifdef HAVE_SETEUID             /* use POSIX first */
  seteuid(euid);
#elif  HAVE_SETREUID            /* fallback on BSD */
  setreuid(-1, euid);
#elif  HAVE_SETRESUID           /* HP/UX saturday night special */
  setresuid(-1, euid, -1);
#else
#error you do not appear to have seteuid(), setreuid() or setresuid().
       you need one for secure operation of the rlpr package.
#endif
}

char * strlower(char *str) {
  char *tmp = str;
  while ((*str = tolower(*str))) str++;
  return tmp;
}  

/* this works with an fd because it is used with temporary
 * files that have already been unlink()'ed.
 */

off_t filesz(int fd) {
  static struct stat st;
  return (fstat(fd, &st) < 0) ? (off_t)-1 : st.st_size;
}

int bind_try_range(struct sockaddr_in * sin, int lo, int hi, int sock) {
  int i;

  for (i = lo; i <= hi; i++) {
    sin->sin_port = htons(i);
    if (bind(sock, (struct sockaddr *) sin, sizeof(*sin)) == 0)
      return 0;
  }
  return -1;
}

const char * h_strerror(void) {

  extern int h_errno;		/* some systems don't declare this in netdb.h */
  
  static struct { int error; const char * msg; }
  errlist[] =
    { { HOST_NOT_FOUND, "specified host is unknown" },
      { NO_ADDRESS    , "the request is valid but has no IP address" },
      { NO_RECOVERY   , "a non-recoverable name server error occurred" },
      { TRY_AGAIN     , "a temporary error occurred. try again" },
      { 0             ,  NULL } };
  int i;

  for (i = 0; errlist[i].msg; i++)
    if (h_errno == errlist[i].error)
      return errlist[i].msg;

  return "unknown error";
}

void rlpr_msg(enum level l, enum errno e, char *fmt, ...) {
  va_list ap;
  FILE * stream = (l == INFO) ? stdout : stderr;

  if (l == DEBUG && !props_.debug) return;
  if (l <  FATAL &&  props_.quiet) return;
  va_start(ap, fmt);


  if (props_.syslog) {
    static char buf[BUFSIZ];
    vsprintf(buf, fmt, ap);
    if (e == ERRNO) strcat(buf, ": %m");
#ifdef HAVE_SYSLOG_H
    syslog(l == DEBUG ? LOG_DEBUG :
	   l == INFO  ? LOG_INFO  :
	   l == WARNING ? LOG_WARNING : LOG_ERR, buf);
#endif /* HAVE_SYSLOG_H */
  } else {
    fprintf(stream, "%s: %s", name, (l == DEBUG)   ? "debug: "   :
                       	            (l == INFO)    ? ""          : 
                             	    (l == WARNING) ? "warning: " : "fatal: ");
    vfprintf(stream, fmt, ap);
    if (e == ERRNO) fprintf(stream, ": %s", strerror(errno));
    fputc('\n', stream);
  }
  va_end(ap);
  if (l == FATAL) exit(EXIT_FAILURE);
}

inline void * rlpr_malloc(size_t sz) {
  void *ptr = malloc(sz);
  check_ptr(ptr);
  return ptr;
}

inline void * rlpr_strdup(char * orig) {
  void *ptr = strdup(orig);
  check_ptr(ptr);
  return ptr;
}

/* these are only used if the host platform doesn't have them */

#ifndef HAVE_STRCSPN
size_t strcspn(const char *s, const char *reject) {
  const char *sp;

  for (sp = s; *sp; sp++)
    if (strchr(reject, *sp) != NULL) break;
  return sp - s;
}
#endif /* not HAVE_STRCSPN */

#ifndef HAVE_STRDUP
char * strdup(const char *s) {
  char * ptr = malloc(strlen(s) + 1);
  return ptr ? strcpy(ptr, s) : NULL;
}
#endif /* not HAVE_STRDUP */

#ifndef HAVE_STRSTR
char * strstr(const char *haystack, const char *needle) {
  if (*needle == '\0') return (char *) haystack;

  for (;(haystack = strchr(haystack, *needle)) != NULL; haystack++) {
    /* see if the rest matches */
    const char *hayp = haystack, *needp = needle;
    for (;;)
      if (*++needp == '\0') return (char *) haystack;
      else if (*++hayp != *needp) break;
  }

  return NULL;
}
#endif /* not HAVE_STRSTR */
