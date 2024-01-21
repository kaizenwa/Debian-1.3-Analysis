/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1996 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"

/*{{{ Include Files */

#include <stdio.h>
#include <slang.h>

#include "jdmacros.h"


#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <string.h>
#include <limits.h>

#include <errno.h>

#if defined(__DECC) && defined(VMS)
# include <unixio.h>
#endif

#ifdef VMS
#include <file.h>
#endif

#ifdef __unix__
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/file.h>
# ifdef HAVE_SYS_FCNTL_H
#  include <sys/fcntl.h>
# endif
#endif

#ifdef msdos
#include <fcntl.h>
#include <io.h>
#include <sys/stat.h>
#endif

#ifdef __os2__
#include <fcntl.h>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif
 
#ifndef O_RDONLY
#ifdef VMS
#include <file.h>
#else
#include <fcntl.h>
#endif
#endif

#include "vfile.h"
#ifdef JED
#include "misc.h"
#endif


/*}}}*/

unsigned int VFile_Mode = VFILE_TEXT;

VFILE *vopen(char *file, unsigned int size, unsigned int fmode) /*{{{*/
{
   int fd;
   unsigned int mode;

#ifdef O_BINARY
   mode = O_BINARY;
#else 
   mode = 0;
#endif
      
   if ((fd = open(file, mode | O_RDONLY, 0)) < 0) return(NULL);
   return vstream(fd, size, fmode);
}

/*}}}*/

void vclose(VFILE *v) /*{{{*/
{
   (void) close(v->fd);
   if (v->buf != NULL) SLFREE(v->buf);
   SLFREE(v);
}

/*}}}*/

VFILE *vstream(int fd, unsigned int size, unsigned int mode) /*{{{*/
{
   VFILE *v;
   
   if (NULL == (v = (VFILE *) SLMALLOC(sizeof(VFILE)))) return(NULL);
   v->bmax = v->bp = v->buf = NULL;
   v->fd = fd;
   v->eof = NULL;
   v->size = size;
   if (mode == 0) mode = VFile_Mode;
   v->mode = mode;
   v->cr_flag = 0;
   return v;
}

/*}}}*/

/* I malloc one extra so that I can always add a null character to last line */
char *vgets(VFILE *vp, unsigned int *num) /*{{{*/
{
   register char *bp, *bp1;
   register char *bmax, *bpmax;
   char *neew;
   int fd = vp->fd;
   unsigned int n, max, fmode = vp->mode;
   int doread = 0;
   n = vp->size;
   
   *num = 0;
   if (NULL == vp->buf)
     {
#if defined (msdos) || defined (__os2_16__)
	if (!n) n = 512;
#else
	if (!n) n = 64 * 1024;
#endif
	
	if (NULL == (neew = (char *) SLMALLOC(n + 1)))
	  {
#ifdef JED
	     msg_error("Unable to malloc space.");
#endif
	     return(NULL);
	  }
	
	vp->bp = vp->buf = neew;
	vp->bmax = neew + n;
	doread = 1;
     }

   bp = vp->bp;
   if ((vp->eof != NULL) && (bp >= vp->eof)) return (NULL);
   bp1 = vp->buf;
   bmax = vp->bmax;
   
   while (1)
     {	
	if (doread)
	  {
	     int nread;
	     
	     max = (int) (vp->bmax - bp);
	     while ((nread = read(fd, bp, max)) != 0)
	       {
		  if (nread == -1)
		    {
#ifndef pc_system
# ifdef EINTR
		       if (errno == EINTR)
			 continue;
# endif
# ifdef EAGAIN
		       if (errno == EAGAIN)
			 {
			    sleep (1);
			    continue;
			 }
# endif
#endif
		       return NULL;
		    }
		  
		  max -= nread;
		  bp += nread;
	       }
	     if (max) vp->eof = bp;
	     if (bp == bp1)
	       {
		  return(NULL);
	       }
	     bp = bp1;
	  }
	else bp1 = bp;
	   
	/* extract a line */
	if (vp->eof != NULL) bmax = vp->eof;
	
	n = (unsigned int) (bmax - bp);
#if defined(msdos) && !defined(__WATCOMC__)
	if (n)
	  {
	     bpmax = bp;
#ifndef __WIN32__
	     asm  {
		mov bx, di
		mov al, 10
		mov cx, n
		les di, bpmax
		cld
		repne scasb
		inc cx
		sub n, cx
		mov di, bx
	     }
	     bp += n;
#else
 	     if (NULL == (bpmax = SLMEMCHR(bp, '\n', n)))
 	       bp += n;
 	     else
 	       bp = bpmax;
#endif /* __WIN32__ */
	     if (*bp != '\n') bp++;
	  }
	
	if (bp < bmax)
	  {
	     vp->bp = ++bp;
	     *num = (unsigned int) (bp - bp1);
	     
	     /* if it is text, replace the carriage return by a newline
	        and adjust the number read by 1 */
	     bp -= 2;
	     if ((fmode == VFILE_TEXT) && (*num > 1) && (*bp == '\r'))
	       {
		  *bp = '\n';
		  *num -= 1;
		  vp->cr_flag = 1;
	       }
	     return bp1;
	  }
#else
	if (NULL != (bpmax = SLMEMCHR(bp, '\n', n)))
	  {
	     bpmax++;
	     vp->bp = bpmax;
	     *num = (unsigned int) (bpmax - bp1);
	     
	     if ((fmode == VFILE_TEXT) && (*num > 1))
	       {
		  bpmax -= 2;
		  if (*bpmax == '\r') 
		    {
		       vp->cr_flag = 1;
		       *bpmax = '\n'; (*num)--;
		    }
	       }
	     return bp1;
	  }
	bp = bp + n;
#endif	/* msdos */
	if (vp->eof != NULL)
	  {
	     *num = (unsigned int) (bp - bp1);
	     vp->bp = bp;
	     
	     
#if defined(pc_system) || defined(__os2__)
	     /* kill ^Z at EOF if present */
	     if ((fmode == VFILE_TEXT) && (*num) && (26 == *(bp - 1))) 
	       {
		  *num -= 1;
		  if (!*num) bp1 = NULL;
	       }
#endif
	     return(bp1);
	  }
	
	doread = 1;

	bp = bp1;
	bp1 = vp->buf;
	if (bp != bp1)
	  {
	     /* shift to beginning */
	     while (bp < bmax) *bp1++ = *bp++;
	     bp = bp1;
	     bp1 = vp->buf;
	  }
	else 
	  {
	     bp = bmax;
	     vp->bmax += 2 * (int) (vp->bmax - vp->buf);
	     neew = (char *) SLREALLOC(vp->buf, vp->bmax - vp->buf + 1);
	     if (neew == NULL) 
	       {
#ifdef JED
		  msg_error("Realloc Error.");
#endif

		  return(NULL);
	       }
	     
	     bp = neew + (int) (bmax - vp->buf);
	     bmax = vp->bmax = neew + (int) (vp->bmax - vp->buf);
	     bp1 = vp->buf = neew;
	  }
     }
}

/*}}}*/

