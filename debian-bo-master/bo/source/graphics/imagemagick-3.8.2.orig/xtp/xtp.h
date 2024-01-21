/*
  Include declarations
*/
#if defined(__hpux)
#define _HPUX_SOURCE  1
#endif
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#if !defined(macintosh)
#include <sys/types.h>
#include <sys/stat.h>
#else
#include <SIOUX.h>
#include <console.h>
#include <unix.h>
#include <types.h>
#include <stat.h>
#endif
#include <pwd.h>
#undef index
#undef assert

/*
  Define declarations for the xtp program.
*/
#if defined(__cplusplus) || defined(c_plusplus)
#define class  c_class
#endif
#define False  0
#define IsGlob(text) \
  ((strchr(text,'*') != (char *) NULL) || \
   (strchr(text,'?') != (char *) NULL) || \
   (strchr(text,'{') != (char *) NULL) || \
   (strchr(text,'}') != (char *) NULL))
#define Max(x,y)  (((x) > (y)) ? (x) : (y))
#define MaxTextLength  2048
#define True  1
#define Warning(message,qualifier)  \
{  \
  (void) fprintf(stderr,"%s: %s",client_name,message);  \
  if (qualifier != (char *) NULL)  \
    (void) fprintf(stderr," (%s)",qualifier);  \
  (void) fprintf(stderr,".\n");  \
}

#ifndef lint
static char
  Version[]="@(#)ImageMagick 3.8.0 97/01/15 cristy@dupont.com";
#endif
