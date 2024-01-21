#include <ctype.h>		/* ANSI C */
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h> 
#include <time.h>

#include "efaxmsg.h"

#define MAXTSTAMP 80		/* maximum length of a time stamp */
#define MAXMSGBUF 4096		/* maximum status/error message bytes held */

#define NLOG 2

char *verb[NLOG] = { "ewin", "" } ;
char *argv0 = "" ;

int nxtoptind = 1 ;		/* for communication with nextopt() */
char *nxtoptarg ;

/* Print time stamp. */

time_t tstamp ( time_t last, FILE *f )
{
  time_t now ;
  char tbuf [ MAXTSTAMP ] ;

  now = time ( 0 ) ;

  strftime ( tbuf, MAXTSTAMP,  ( now - last > 600 ) ? "%c" : "%M:%S",
	    localtime( &now ) ) ;
  fputs ( tbuf, f ) ;

  return now ;
}


#ifdef NO_STRERROR
/* For systems without strerror(3) */

extern char *strerror( int i )       
{
  return ( i >= 0 && i < sys_nerr ) ? sys_errlist[i] : "Unknown Error" ;
}
#endif


/* Return string corresponding to character c. */

char *cname ( u_char c ) 
{
#define CNAMEFMT "<0x%02x>"
#define CNAMELEN 6+1
  static char *cnametab [ 256 ] = { /* character names */
  "<NUL>","<SOH>","<STX>","<ETX>", "<EOT>","<ENQ>","<ACK>","<BEL>",
  "<BS>", "<HT>", "<LF>", "<VT>",  "<FF>", "<CR>", "<SO>", "<SI>", 
  "<DLE>","<XON>","<DC2>","<XOFF>","<DC4>","<NAK>","<SYN>","<ETB>",
  "<CAN>","<EM>", "<SUB>","<ESC>", "<FS>", "<GS>", "<RS>", "<US>" } ;
  static char names[ (127-32)*2 + 129*(CNAMELEN) ], *p=names ;
  static int i=0 ;
    
  if ( ! i ) 
    for ( i=32 ; i<256 ; i++ ) {
      sprintf ( (char*) ( cnametab [ i ] ) = p, i<127 ? "%c" : CNAMEFMT, i ) ;
      p += strlen ( p ) + 1 ;
    }

  return cnametab [ c ] ;
} 

/* Print a message with a variable number of printf()-type
   arguments if the first character appears in the global
   verb[ose] string.  Other leading characters and digits do
   additional actions: + allows the message to be continued on
   the same line, '-' buffers the message instead of printing it,
   E, and W expand into strings, S prints the error message for
   the most recent system error, a digit sets the return value, a
   space ends prefix but isn't printed.  Returns 0 if no prefix
   digit. */

int msg ( char *fmt, ... ) 
{ 
  static int init=0 ;
  static FILE *logfile [ NLOG ] = { stderr, stdout } ;
  static char msgbuf [ NLOG ] [ MAXMSGBUF ] ;
  static time_t logtime [ NLOG ] = { 0, 0 } ;
  static int atcol1 [ NLOG ] = { 1, 1 } ;
  
  int err=0, i ;
  int dolf=1, flush=1 ;
  char *pe="", *ps=0 ;
  char *p ;
  
  va_list ap ;
  va_start ( ap, fmt ) ;

  if ( ! init ) {
    for ( i=0 ; i<NLOG ; i++ )
      setvbuf ( logfile[i], msgbuf[i], _IOFBF, MAXMSGBUF ) ;
    cname ( 0 ) ;
    init = 1 ;
  }
  
  for ( i=0 ; i<NLOG ; i++ ) {

    if ( strchr ( verb[i], tolower ( *fmt ) ) ) {
      
      for ( p=fmt ; *p ; p++ ) {
	switch ( *p ) {
	case ' ': p++ ; goto print ;
	case 'E': pe = "Error: " ; break ;
	case 'W': pe = "Warning: " ; break ;
	case 'S': ps = strerror ( errno ) ; break ;
	case '+': dolf = 0 ; break ;
	case '-': flush = 0 ; break ;
	default: 
	  if ( isdigit ( *p ) ) {
	    err = *p - '0' ; 
	  } else if ( ! isupper ( *p ) ) {
	    goto print ;
	  }
	}
      }
      
    print:
      
      if ( atcol1[i] ) {
	fprintf ( logfile[i], "%s: ", argv0 ) ;
	logtime[i] = tstamp ( logtime[i], logfile[i] ) ; 
	fprintf ( logfile[i],  " %s", pe ) ;
      }
      vfprintf( logfile[i], p, ap ) ;
      if ( ps ) fprintf ( logfile[i], " %s", ps ) ;
      if ( dolf ) fputs ( "\n", logfile[i] ) ;
      atcol1[i] = dolf ? 1 : 0 ;
      if ( flush ) fflush ( logfile[i] ) ;
      
    }
    
  }
  
  va_end ( ap ) ;
  
  return err ;
}


/* Simple (one option per argument) version of getopt(3). */

int nextopt( int argc, char **argv, char *args )
{
  char *a, *p ;

  if ( nxtoptind >= argc || *(a = argv[nxtoptind]) != '-' ) return -1 ;
  nxtoptind++ ;

  if ( ! *(a+1) || ( ( p = strchr ( args, *(a+1) ) ) == 0 ) )
    return msg ( "Eunknown option (%s)", a ), '?' ; 

  if ( *(p+1) != ':' ) nxtoptarg = 0 ;
  else
    if ( *(a+2) ) nxtoptarg = a+2 ;
    else
      if ( nxtoptind >= argc ) return msg ( "Eno argument for (%s)", a ), '?' ;
      else nxtoptarg = argv [ nxtoptind++ ] ;
  return *(a+1) ;
}

