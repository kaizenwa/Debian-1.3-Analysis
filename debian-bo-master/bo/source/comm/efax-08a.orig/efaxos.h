#ifndef _EFAXOS_H
#define _EFAXOS_H

#ifndef PATH_MAX
#ifdef FILENAME_MAX
#define PATH_MAX FILENAME_MAX
#else
#define PATH_MAX 255
#endif
#endif

/* signals to be caught */

#define ANSISIGS  SIGABRT, SIGFPE, SIGILL, SIGINT, SIGSEGV, SIGTERM
#define UNIXSIGS  SIGHUP, SIGQUIT, SIGIOT, SIGALRM
#define CATCHSIGS ANSISIGS, UNIXSIGS

/* Bit order reversal table. */

extern unsigned char normalbits [ ] ;

typedef enum ttymodes		/* serial port modes:  */
{
    COMMAND,			/*   19200 8N1, no f/c, DTR high */
    SEND,			/*   19200 send-only XON/XOFF f/c */
    VOICECOMMAND,		/*   38400 8N1, no f/c, DTR high */
    VOICESEND,			/*   38400 send-only XON/XOFF f/c*/
    DROPDTR,			/*   ", DTR low */
    ORIGINAL			/*   restore original settings */
} ttymodes ;

/* OS-specific i/o & delay functions */

/* We define new stream i/o macros because it's not possible to
   do non-blocking reads/writes with C stream i/o [UNIX select()
   gives the status of the file, not the stream buffer].*/

#define IBUFSIZE 1024	    /* read up to this many bytes at a time from fax */
#define OBUFSIZE 1024	    /* maximum bytes to write at a time to fax */

typedef struct tfilestruct {
  int fd ;
  unsigned char *ip, *iq, *op, *oq ;
  unsigned char ibuf [ IBUFSIZE ], obuf[ OBUFSIZE ] ;
  unsigned char *ibitorder, *obitorder ;
  int bytes, pad, lines ;
  time_t start ;
  long mstart ;
} TFILE ;

/* tgetc() is a macro like getc().  It evaluates to the next
   character from the fax device or EOF after idle time t. */

#define tgetc(f,t) ( (f)->ip >= (f)->iq && tundrflw(f,t) == EOF ? EOF : \
		    *(unsigned char*)(f)->ip++ )

/* tgetd() also removes DLE escapes, detects DLE-ETX terminators
   and fixes bit order. Evaluates to the next character, EOF on
   error/timeout, or -2 on DLE-ETX.  */

#define IBUFC(f) ( ((unsigned char*)(f)->ip)[-1] )

#define tgetd(f,t) ( tgetc(f,t) < 0 ? EOF : \
       IBUFC(f) != DLE ? (f)->ibitorder[ IBUFC(f) ] : \
       tgetc(f,t) == ETX ? -2 : \
       ( IBUFC(f) == DLE || IBUFC(f) == SUB ) ? (f)->ibitorder [ DLE ] : \
       msg ( "W0invalid escape sequence (DLE-%s) in data", \
	    cname ( IBUFC(f) ) ) )

/* tputc() is a macro like putc().  It evaluates to the character
   written or EOF on error. */

#define tputc(c,f,t) ( (f)->op >= (f)->oq && tflush(f,t) == EOF ? EOF : \
		    (unsigned char) ( *(f)->op++ = (c) ) )

/* tobytes() evaluates to the number of bytes in the output
   buffer */

#define tobytes(f) ( (f)->op - (f)->obuf ) 

int tundrflw ( TFILE *f, int t ) ;
int tflush   ( TFILE *f, int t ) ;
int toclear ( TFILE *f ) ;

int tdata    ( TFILE *f, int t ) ;
void tinit ( TFILE *f, int fd, int reverse ) ;
int ttyopen  ( TFILE *f, char *fname, int reverse ) ;
int ttymode  ( TFILE *f, ttymodes mode ) ;
void msleep  ( int t ) ;
long proc_ms ( void ) ;
int time_ms ( void ) ;

/* POSIX execl */

extern int execl ( const char *path, const char *arg , ... ) ;

/* UUCP-style device locks */

#define HDBLKFLAG '#'		/* prefix to force HDB (text) lock files */

				/* [un]lock serial port using named files */
int lockall ( char **lkfiles, int log ) ;
int unlockall ( char **lkfiles ) ;

/* extract program name to be used in messages from argv0 */  

char *efaxbasename ( char *p ) ;

/* default fax modem device */

#define FAXFILE "/dev/fax"



#endif
