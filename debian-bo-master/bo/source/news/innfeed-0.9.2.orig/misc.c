/* -*- c -*-
 *
 * Author:      James Brister <brister@vix.com> -- berkeley-unix --
 * Start Date:  Wed Dec 27 17:10:18 1995
 * Project:     INN (innfeed)
 * File:        misc.c
 * RCSId:       $Id: misc.c,v 1.13 1996/11/28 19:14:30 brister Exp $
 *
 * Copyright:   Copyright (c) 1996 by Internet Software Consortium
 *
 *              Permission to use, copy, modify, and distribute this
 *              software for any purpose with or without fee is hereby
 *              granted, provided that the above copyright notice and this
 *              permission notice appear in all copies.
 *
 *              THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE
 *              CONSORTIUM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *              SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *              MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET
 *              SOFTWARE CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT,
 *              INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *              WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *              WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 *              TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 *              USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Description: 
 * 
 */

#if ! defined (lint)
static const char *rcsid = "$Id: misc.c,v 1.13 1996/11/28 19:14:30 brister Exp $" ;
static void use_rcsid (const char *rid) {   /* Never called */
  use_rcsid (rcsid) ; use_rcsid (rid) ;
}
#endif

#include "config.h"


#include <stdlib.h>
#include <sys/types.h>
#include <stdarg.h>
#include <stdio.h>
#include <netdb.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include <fcntl.h>
#include <errno.h>
#include <syslog.h>
#include <sys/param.h>

#if defined (DO_HAVE_UNISTD)
#include <unistd.h>
#endif

#include <signal.h>

#if USE_INNLIB
#include "libinn.h"
#else
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include "misc.h"
#include "msgs.h"
#include "endpoint.h"


u_int openfds ;
char *program ;                 /* this should be set to argv[0] */
int debuggingOutput ;
u_int loggingLevel ;
char **PointersFreedOnExit ;





int maxFds (void)
{
  static int size = 0 ;
    
#if USE_INNLIB

  size = getfdcount () ;

#elif defined (FDCOUNT_GETDTAB)

    if (size <= 0)
      {
        if ((size = getdtablesize()) < 0)
          return -1;
      }

#elif defined (FDCOUNT_GETRLIMIT)

  {
    struct rlimit rl ;
      
    if (size <= 0)
      {
        if (getrlimit(RLIMIT_NOFILE, &rl) < 0)
          return -1 ;
        size = rl.rlim_cur;
      }
  }

#elif defined (FDCOUNT_SYSCONF)

  if (size <= 0)
    {
      if ((size = sysconf(_SC_OPEN_MAX)) < 0)
        return -1;
    }

#elif defined (FDCOUNT_ULIMIT)

  if (size <= 0)
    {
      if ((size = ulimit(4, 0L)) < 0)
        return -1;
    }

#elif defined (FDCOUNT_CONSTANT)

#if defined (NOFILE)
  size = NOFILE ;
#else
  size = 20 ;
#endif

#endif

  return size;
}


void dprintf (u_int level, const char *fmt, ...) 
{
  static pid_t myPid ;
  char timeString [30] ;
  time_t now ;
  va_list ap ;
    
  if (myPid == 0)
    myPid = getpid ()  ;
  
  if (loggingLevel < level)
    return ;
  
  now = theTime() ;
  strcpy (timeString, ctime (&now) + 4) ; /* strip off leading day name */
  timeString [15] = '\0' ;      /* strip off trailing year and newline */

  va_start (ap, fmt) ;
  fprintf (stderr, "%s %s[%ld]: ",timeString,
           (program ? program : "UNKNOWN PROGRAM NAME"), (long) myPid) ;
  vfprintf (stderr, fmt, ap) ;
  va_end (ap) ;
}

bool debuggingDump = true ;
extern void (*gPrintInfo) (void) ;
void (*gCleanUp) (void) = 0 ;

void die (const char *fmt, ...)
{
  char timeString [30] ;
  time_t now = time (NULL) ;
  va_list ap ;

  strcpy (timeString,ctime (&now)) ;
  timeString [24] = '\0' ;
  
  va_start (ap, fmt) ;
  fprintf (stderr, "%s %s: ",
           timeString, (program ? program : "UNKNOWN PROGRAM NAME")) ;
  vfprintf (stderr, fmt, ap) ;
  fprintf (stderr,"\n") ;
  va_end (ap) ;

#if SNAPSHOT_ON_DIE
  if (debuggingDump && gPrintInfo != NULL)
    gPrintInfo () ;
#endif

  if (gCleanUp != NULL)
    gCleanUp () ;
  
  if (CORE_DIRECTORY != NULL)
    (void) chdir (CORE_DIRECTORY) ;
  
  sleep (5) ;
  abort () ;
}

void warn (const char *fmt, ...)
{
  char timeString [30] ;
  time_t now = time (NULL) ;
  va_list ap ;

  strcpy (timeString,ctime (&now)) ;
  timeString [24] = '\0' ;

  va_start (ap, fmt) ;
  fprintf (stderr, "%s %s: ",
 	   timeString, (program ? program : "UNKNOWN PROGRAM NAME")) ;
  vfprintf (stderr, fmt, ap) ;
  fprintf (stderr,"\n") ;
  va_end (ap) ;
}



static const char * const pvt_h_errlist[] = {
  "Resolver Error 0 (no error)",
  "Unknown host",                         /* 1 HOST_NOT_FOUND */
  "Host name lookup failure",             /* 2 TRY_AGAIN */
  "Unknown server error",                 /* 3 NO_RECOVERY */
  "No address associated with name",      /* 4 NO_ADDRESS */
};

static int pvt_h_nerr = (sizeof pvt_h_errlist / sizeof pvt_h_errlist[0]);

/* return a friendly string for the current value of h_errno. Pinched from
   Stevens */
const char *host_err_str (void)
{
  static char msgstr [200] ;

  if (h_errno != 0)
    {
      if (h_errno > 0 && h_errno < pvt_h_nerr)
        sprintf (msgstr,"(%s)", pvt_h_errlist[h_errno]) ;
      else
        sprintf (msgstr,"(herrno = %d)", h_errno) ;
    }
  else
    msgstr [0] = '\0' ;

  return msgstr ;
}


/* return true if the file exists and is a regular file. */
bool fileExistsP (const char *filename)
{
  struct stat buf ;

  if (stat (filename,&buf) < 0)
    return false ;

  return (S_ISREG (buf.st_mode) ? true : false) ;
}


bool isDirectory (const char *filename)
{
  struct stat buf ;

  if (stat (filename,&buf) < 0)
    return false ;

  return (S_ISDIR (buf.st_mode) ? true : false) ;
}



bool getNntpResponse (char *p, int *code, char **rest)
{
  bool rval = true ;
  int cd = 0 ;
  int digits = 0 ;

  if (rest)
    *rest = 0 ;
  *code = 0 ;

  if (p == NULL)
    return false ;
  
  while (*p && isspace (*p))
    p++ ;

  while (*p && isdigit (*p))
    {
      digits++ ;
      cd = (cd * 10) + (*p - '0') ;
      p++ ;
    }

  if (digits != 3)
    return false ;
  
  if (*p == '-')
    p++ ;
  
  while (*p && isspace (*p))
    p++ ;
      
  if (rest)
    *rest = p ;

  *code = cd ;
  
  return rval ;
}


#if 0
/* pinched from INN main source */
int
writev(fd, vp, vpcount)
    int                 fd;
    struct iovec        *vp;
    int                 vpcount;
{
    int                 count;

    for (count = 0; --vpcount >= 0; count += vp->iov_len, vp++)
        if (xwrite(fd, vp->iov_base, vp->iov_len) < 0)
            return -1;
    return count;
}

#endif



#if defined (DO_NEED_STRDUP)

char *strdup (const char *string)
{
  char *p = NULL ;
  
  if (string != NULL)
    {
      p = MALLOC (strlen (string) + 1) ;
      assert (p != NULL) ;

      strcpy (p,string) ;
    }


  return p ;
}

#endif


/* Pull out a message id from a response on to a streaming command */
char *getMsgId (const char *p)
{
  const char *q ;
  char *rval ;
  
  while (*p && isspace (*p)) p++ ;
  while (*p && !isspace (*p)) p++ ; /* skip response code */
  while (*p && isspace (*p)) p++ ;

  if ( *p == '\0' )
    return NULL ;

  q = p ;
  while ( *q && !isspace (*q) )
    q++ ;
  
  rval = MALLOC ((size_t) (q - p + 1)) ;
  assert (rval != NULL) ;

  strncpy (rval,p,(size_t) (q - p)) ;
  rval [q - p] = '\0' ;
  
  return rval ;
}




char *findNonBlankString (char *ptr, char **tail)
{
  char *p, *q ;

  for (p = ptr ; *p && isspace (*p) ; p++)
    /* nada */ ;
  if ( ! *p )
    return NULL ;

  for (q = p ; *q && !isspace (*q) ; q++)
    /* nada */ ;

  *tail = q ;

  return p ;
}


/* strtok can't handle zero length tokens. */
char *mystrtok (char *line, const char *sep)
{
  static char *newPoint ;
  char *oldline ;
  
  if (line == NULL && newPoint == NULL)
    return NULL ;

  if (line != NULL)
    {
      oldline = line ;
      while (*line != '\0' && strchr (sep,*line) == NULL)
        line++ ;

      if (*line == '\0')
        newPoint = NULL ;
      else
        {
          newPoint = line + 1 ;
          *line = '\0' ;
        }
    }
  else
    {
      if (newPoint == NULL)
        return NULL ;
      
      oldline = newPoint ;
      line = oldline ;
      
      while (*line != '\0' && strchr (sep,*line) == NULL)
        line++ ;

      if (*line == '\0')
        newPoint = NULL ;
      else
        {
          newPoint = line + 1 ;
          *line = '\0' ;
        }
    }

  return oldline ;
}



void trim_ws (char *string)
{
  char *p ;
  u_int len ;

  assert (string != NULL) ;

  len = strlen (string) ;
  if (len == 0)
    return ;
  
  for (p = string + len - 1 ; p >= string && isspace (*p) ; p--)
    /* nada */ ;
  *++p = '\0' ;
}


#if 0
/* Scribble on top of memory we're about to free. */
void deadBeef (void *base, size_t byteCount)
{
  unsigned char *b = (unsigned char *) base ;
  int i ;

#if 0

  memset (base, 0, byteCount) ;

#else

  assert (b != NULL) ;

  for (i = 0 ; i < ((int) byteCount) - 4 ; i += 4)
    {
#if 0
      *((int *) (b + i)) = 0xdeadbeef ;
#else
      b [i + 0] = (unsigned char) 0xde ;
      b [i + 1] = (unsigned char) 0xad ;
      b [i + 2] = (unsigned char) 0xbe ;
      b [i + 3] = (unsigned char) 0xef ;
#endif
    }
  
  switch (byteCount % 4)
    {
      case 0:
        *(b + i + 3) = (unsigned char) 0xef ;
        
      case 3:
        *(b + i + 2) = (unsigned char) 0xbe ;

      case 2:
        *(b + i + 1) = (unsigned char) 0xad ;

      case 1:
        *b = (unsigned char) 0xde ;
    }

#endif
}
#endif 

/* Not using plain flock or lockf 'cause I don't want to waste file
   descriptors. This routine is based on the file shlock.c from INN. */
bool lockFile (const char *fileName)
{
  char buff [20] ;
  char tmpName [MAXPATHLEN], realName [MAXPATHLEN] ;
  char *p ;
  int fd, i ;
  pid_t pid = getpid () ;

  strcpy (realName,fileName) ;
  if ((p = strrchr (realName, '/')) != NULL)
    {
      *p = '\0' ;
      sprintf (tmpName, "%s/lockf%ld", realName, (long) pid) ;
      *p = '/' ;
    }
  else
    sprintf (tmpName, "lockf%ld", (long) pid) ;
  
  /* Create the temporary name for the lock file. */
  while ((fd = open (tmpName, O_RDWR | O_CREAT | O_EXCL, 0644)) < 0)
    {
      switch (errno)
        {
          default:
            unlink (tmpName) ;
            syslog (LOG_ERR,NO_OPEN_LOCK,tmpName) ;
            return false ;

          case EEXIST:
            if (unlink (tmpName) < 0)
              {
                syslog (LOG_ERR,NO_UNLINK_LOCK,tmpName) ;
                return false ;
              }
            break;
        }
    }

  /* stick our pid in the temp file. */
  sprintf (buff,"%ld\n",(long) pid) ;
  if (write (fd,buff,(size_t) strlen (buff)) != (int) strlen (buff))
    {
      syslog (LOG_ERR,NO_WRITE_LOCK_PID) ;
      close (fd) ;
      unlink (tmpName) ;
      return false ;
    }
  close (fd) ;

  /* now link the real name to the temp file. */
  while (link (tmpName,realName) < 0)
    {
      switch (errno) 
        {
          default:              /* opps. bailing out. */
            syslog (LOG_ERR,NO_LINK_LOCK,realName) ;
            unlink (tmpName) ;
            return false ;

          case EEXIST:          
            /* the real lock file exists. So pull out the pid in there and
               see if that process is still alive. */
            if ((fd = open (realName,O_RDONLY)) < 0)
              {
                syslog (LOG_ERR,NO_OPEN_LOCK,realName) ;
                unlink (tmpName) ;
                return false ;
              }

            if ((i = read (fd,buff,sizeof (buff) - 1)) <= 0)
              {
                close (fd) ;
                unlink (tmpName) ;
                return false ;
              }
            close (fd) ;
            
            buff [i] = '\0' ;
            pid = (pid_t) atol (buff) ;
            if (pid <= 0)
              {
                syslog (LOG_ERR,BAD_PID,realName,buff) ;
                unlink (tmpName) ;
                return false ;
              }

            /* now send a null signal to the process named inside to see if
               it's still alive. */
            if (kill (pid,0) == 0)
              {
                syslog (LOG_ERR,LOCK_EXISTS,realName,(int) pid) ;
                unlink (tmpName) ;
                return false ;    /* process is still alive */
              }

            /* process that took out the lock is gone */
            if (unlink (realName) < 0)
              {
                syslog (LOG_ERR,NO_UNLINK_LOCK,realName) ;
                unlink (tmpName) ;
                return false ;
              }
        }
    }

  unlink (tmpName) ;

  return true ;
}


void unlockFile (const char *lockfile)
{
  (void) unlink (lockfile) ;
}



#if defined (DO_NEED_STRERROR)

extern int sys_nerr;
extern char *sys_errlist[];

char *strerror (int errnum)
{
    static char buff[30];

    if (errnum >= 0 && errnum < sys_nerr)
      return sys_errlist[errnum];

    (void)sprintf(buff, "Error code %d\n", errnum);
    return buff;
}

#endif /* defined (NEED_STRERROR) */



bool endsIn (const char *string, const char *tail)
{
  size_t len = strlen (tail) ;
  size_t slen = strlen (string) ;

  if (slen < len)
    return false ;
  else if (strcmp (string + slen - len, tail) == 0)
    return true ;
  else
    return false ;
}

      
/* append the contents of src to dest. src is removed if append if
   successful */
bool appendFile (const char *dest, const char *src)
{
  FILE *inTmp, *outTmp ;
  char buff [BUFSIZ] ;
  size_t rval ;

      /* append the outputFilename file to the inputFilename file */
  if ((outTmp = fopen (dest, "a")) == NULL)
    die ("fopen (%s): %s",dest, strerror (errno)) ;
  if ((inTmp = fopen (src, "r")) == NULL)
    die ("fopen (%s): %s",src, strerror (errno)) ;

  while ((rval = fread (buff,sizeof (char),BUFSIZ,inTmp)) > 0)
    {
      if (fwrite (buff,sizeof (char), rval, outTmp) != rval)
        die ("fwrite: %s", strerror (errno)) ;
    }

  if (ferror (inTmp))
    die ("Error on inTmp in newTape") ;
  if (ferror (outTmp))
    die ("Error on outTmp in newTape") ;

  if (fclose (inTmp) != 0)
    die ("fclose (inTmp): appendFile (%s,%s): %s",dest,src,strerror (errno)) ;
      
  if (fclose (outTmp) != 0)
    die ("fclose (outTmp): appendFile (%s,%s): %s",dest,src,strerror (errno)) ;

  if (unlink (src) != 0)
    die ("unlink (%s): %s", src, strerror (errno)) ;

  return true ;
}


/* return true if file1 is older than file2 */
bool isOlder (const char *file1, const char *file2)
{
  struct stat buf1 ;
  struct stat buf2 ;

  if (stat (file1,&buf1) < 0)
    return false ;

  if (stat (file2,&buf2) < 0)
    return false ;

  return ((buf1.st_mtime < buf2.st_mtime) ? true : false) ;
}


void freeCharP (char *charp)
{
  FREE (charp) ;
}


/* return the length of the file reference by the given file descriptor */
long fileLength (int fd)
{
  struct stat buf ;

  if (fstat (fd,&buf) < 0)
    return false ;

  return ((long) buf.st_size) ;
}



const char *boolToString (bool val)
{
  return val ? "true" : "false" ;
}

void addPointerFreedOnExit (char *pointerToFree)
{
  static int totalPointers = 0 ;
  static int nextPointer = 0 ;

  if (nextPointer == 0 || nextPointer == totalPointers - 1)
    {
      register int i;

      totalPointers += 16 ;
      if (PointersFreedOnExit == NULL)
	PointersFreedOnExit = ALLOC (char *, totalPointers) ;
      else
	PointersFreedOnExit =
	  REALLOC (PointersFreedOnExit, char *, totalPointers) ;

      ASSERT (PointersFreedOnExit != NULL) ;

      for (i = nextPointer; i < totalPointers; i++)
	PointersFreedOnExit [i] = NULL;
    }
  PointersFreedOnExit [nextPointer++] = pointerToFree ;
}
