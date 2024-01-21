/* afio file compresssion code */

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include <stdlib.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "patchlevel.h"


#ifndef	major
#include <sys/sysmacros.h>
#endif /* major */

#include "afio.h"

int matchcompext(char *);

/* default value vor gzip 1.2.3 */
int gzipfactor=6;

/* compress all files? */
int forceZflag = 0;

/* this can be set to use something else but gzip: */
char *compressprog = NULL;
int compressargs = 0;


/* max. (virtual) memory usage for -Z option */
off_t maxmem=2*1024*1024;

/* files whose length is below this length won't be compressed */
long compthreshold=0;


/* stuff to build argument list for compression/decompression : */

#define	MAX_ARGS  101

char 	* compress_arg_list[MAX_ARGS+1];
/* MAX_ARGS+1 because we need to leave room for a NULL to mark the end */
int	compress_arg_no = 1;

void	add_arg(char *arg)
{
    if((compress_arg_no == 1)&&(*arg=='\0'))
    {
	/* special case: -Q "" means no arguments. */
	return;
    }
    
    if(compress_arg_no < MAX_ARGS )
	compress_arg_list[compress_arg_no++] = arg;
    else
    {
	fprintf (stderr, "afio: Fatal: maximium number of -Q arguments exceeded.\n");
	exit(1);
    }
    
}

/*
 * meminit, memwrite, memreset, memread, memfree.
 */

int memerror;
char *membank;
size_t memsize,membytesread,membytesleft;

void meminit()
{
 memerror=0;
 memsize=0;
 membank=(char *)malloc(0);
}

void memwrite(char *buf, int count)
{
 char *oldbank;

 if(memerror) return;

 oldbank=membank;
 membank=(char *)realloc(membank,memsize+count);
 if(membank==NULL) 
  {
    warn("Memory","Low on virtual memory, zipping twice instead.");
    free(oldbank);
    memerror=1;
    return;
  }
 memcpy(membank+memsize,buf,count);
 memsize+=count;
}

void memreset()
{
 membytesread=0;
 membytesleft=memsize;
} 

int memread(char *buf,int count)
{
 if(membytesleft==0) return 0;

 if (count > membytesleft) count=membytesleft;
 memcpy(buf,membank+membytesread,count);
 membytesread+=count;
 membytesleft-=count;
 return count; 
}

void memfree()
{
 if(memerror==0) free(membank);
}


/*******/

/*
 * Fork a gzip zipping the file name. The zipped version is output through a 
 * pipe, the pipe handle is returned.
 * Returns -1 on failure.
 */
int comppid;

int setupgzip(char *name)
{
 int pipedes[2];
 char farg[3];

 farg[0]='-';
 farg[1]=gzipfactor+'0';
 farg[2]='\0';

 if(pipe(pipedes)==-1) { perror("pipe"); return -1; }

 if ((comppid = xfork ("out(), compressing", NODIE)) == 0) 
    {		
      if (arfd != STDIN && arfd != STDOUT) VOID close (arfd);

      dup2(pipedes[1],fileno(stdout));
      close(pipedes[1]);

      VOID close (fileno (stdin));

      if (open (name, O_RDONLY) >= 0)
      { 
	  if(! compressargs)
	      execlp (compressprog, compressprog, "-c", farg, 0);
	  else
	      execvp (compressprog, compress_arg_list);
      }
      exit (1);
   }

 close(pipedes[1]);

 /* fprintf(stderr,"pid %d init, pipedes=%d.\n",comppid,pipedes[0]);  */

 return pipedes[0];
}

#include <sys/wait.h> 
#include <sys/types.h>

void waitforgzip()
{
 /* fprintf(stderr,"wait for gzip %d.\n",comppid); */

 xwait (comppid, "out(), wait for gzip child", FALSE);
}

/*
 * compress "name" if we can
 * If we compress we change the statbuf size, the file name, close
 * the old pointer to the file and return the pointer to the compressed
 * version;
 */

#include <sys/dir.h>
void
compressfile (fdp, name, asb, cratio)
     int *fdp;
     char *name;
     reg Stat *asb;
     int *cratio;
{
  int compout;
  char *tmpcomp;
  Stat asb2;
  int zipfd,len;
  char buf[4096];
  long ziplen;
  int namelen,usemem;

  if (cratio)
    *cratio = 100;

  /* We need to mark the file as being suitable or not for decompression on
     reading the archive. This is done by adding a .z extension. 

     There is a problem with this: a file that is not suitable for
     compression may already have such an extension. 
     Since this is a regular file, we can use the
     device number to indicate something weird going on here. 

     We look at the bit (asb->sb_rdev)&RDEV_NOTCOMPR:
     If it is 1, it means that the file wasn't suitable for compression,
     and thus that any .z present was there on the original name.
     If it is 0, this does NOT imply that the file was compressed by afio.

     Note that it being 0 doesn't mean `decompress me', it merely
     indicates that a .z extender, _if_ _present_, was added by afio.
     (However, as of version 2.3.6, false does mean that the file was
     compressed by afio and a .z extender added.  In older archives one may
     see filenames with the bit 0 and not ending in .z. Such files 
     have a compressed version larger than the original.)
  */

  /* indicate unsuitable for compression, may change it later */
   asb->sb_rdev |= RDEV_NOTCOMPR;

  namelen = strlen (name);
  /* compress only if no links and not already compressed
     and length is at least threshold.  */
  if (!lflag &&
      (asb->sb_nlink == 1) &&
      ((!matchcompext(name) &&
      (asb->sb_size >= compthreshold)) || forceZflag)
     )
  {  
    /* make sure compress could put on the .Z */
    if ((tmpcomp = strrchr (name, '/')) != NULL)
      tmpcomp++;
    else
      tmpcomp = name;
#ifdef MAXNAMLEN	   /* BSD otherwise should be sysV (FFS on sysV?) */
    if (strlen (tmpcomp) + 2 > MAXNAMLEN)
#else
    if (strlen (tmpcomp) + 2 > DIRSIZ)
#endif
      {
#ifndef LONGZFILE
	VOID warn (name, " is too long to tack on .z");
	return;
#endif
      }

  /*  fprintf(stderr,"---nam: %s, len: %d ziplen: ",name,asb->sb_size); */

    usemem=1;
    
    if((zipfd=setupgzip(name))!=-1)
      {
        ziplen=0;
        if(usemem) meminit();
        while((len=read(zipfd,buf,4096))!=0)
          {
           if(len<0) {  fprintf(stderr,
                          "Trouble zippping file, storing uncompressed\n"); 
                        /* read error on pipe, do not use gzip on this file */
                        ziplen=0; break;
                     }    
           if(usemem) memwrite(buf,len);
           ziplen+=len;
           /* too much memory used? */
           if(usemem && ziplen>maxmem)
	     { /* prepare to zip twice and free memory */
               usemem=0;
               if (!memerror) memfree();
               /* fprintf(stderr,"zipping twice."); */
	     }
	  }
        close(zipfd);

        /* wait for child to exit */ 
        if(xwait (comppid, "out(), wait for gzip child", FALSE)!= 0)
            {  fprintf(stderr,
                    "Trouble zippping file, storing uncompressed\n"); 
               ziplen=0;
            }  
  
        if(memerror) usemem=0;

    /*   fprintf(stderr,"%ld\n",ziplen); */
        asb2.sb_size=ziplen;

	if (ziplen>0)
	  {
	    /* if compressed file is smaller than original, of if forceZflag
	       is set, use compressed data */
	    if (asb2.sb_size < asb->sb_size || forceZflag )
	      {
                if(usemem)
                  { 
                    close (*fdp);
                    strcat (name, ".z");
                    /* was suitable for compression */
                    asb->sb_rdev &= ~RDEV_NOTCOMPR;

		    if (cratio)
		      *cratio = (asb2.sb_size * 100L) / asb->sb_size;
		    asb->sb_size = asb2.sb_size;
		    *fdp = MEMFD; 
		  }
                else
		  if ((compout = setupgzip(name)) >= 0)
                    {
                      zipfdfd=compout;
                      compout=ZIPFD;

                      close (*fdp);
                      strcat (name, ".z");
                      /* was suitable for compression */
		      asb->sb_rdev &= ~RDEV_NOTCOMPR;
                       
		      if (cratio)
		        *cratio = (asb2.sb_size * 100L) / asb->sb_size;
		      asb->sb_size = asb2.sb_size;
		      *fdp = compout;
		    }
	      }
            else
              {
                if(usemem) memfree();
	      }
	  }
      }
    
  }
}


