#include <ctype.h>		/* ANSI C */
#include <signal.h>    
#include <stdio.h>
#include <string.h>

#include "efaxio.h"		/* EFAX */
#include "efaxmsg.h"
#include "efaxos.h"

#define MAXRESPB 1024	    /* maximum bytes of modem responses saved */

char *prompts[] = {		/* modem responses that are prompts */
  "OOK", "-CONNECT FAX", "CCONNECT", "NNO CARRIER", "EERROR",
  "NNO DIALTONE", "BBUSY", "NNO ANSWER", "M+FCERROR", "VVCON", 
  "DDATA", "FFAX", /*?*/ 0 } ;

int lockpolldelay = 8000 ;	/* milliseconds between checks of lock files */

			    /* signals to be caught so can hang up phone */
int catch [] = { CATCHSIGS, 0 } ;


/* Modem features */

int c1=0, c20=0 ;		/* use class 1/class 2.0 */
int cmdpause = T_CMD ;		/* delay before each init command */
int vfc = 0 ;			/* virtual flow control */
u_char startchar = DC2 ;	/* character to start reception */

/* Get a modem response into buffer s, storing up to n bytes.
   The response includes characters from the most recent control
   character until the first LF following some text.  Returns s
   or null if times-out in t deciseconds or on i/o error. Trace
   messages are buffered to reduce possible timing problems. */

char *tgets( TFILE *f, char *s, int len, int t )
{
  int i, n, c ;

  for ( i=n=0 ; 1 ; i++ ) {
    if ( ( c = tgetc ( f, t ) ) == EOF ) break ;
    if ( i == 0 ) msg ( "M-+ .%03d [", time_ms ( ) ) ;
    msg ( "M-+ %s", cname ( c ) ) ;
    if ( n > 0 && c == LF ) break ;
    if ( ! iscntrl ( c ) && n < len ) s[n++] = c ;
  }

  if ( n >= len ) msg ( "W- modem response overflow" ) ;
  s[ n < len ? n : len-1 ] = '\0' ;
  if ( i > 0 ) 
    if ( c == EOF ) msg ( "M- <...%.1f s>]", (float)t/10 ) ;
    else msg ( "M- ]" ) ;

  return c == EOF ? 0 : s ;
}


/* Send character or string to modem immediately (for commands).  Return
   like putc() and puts(). */

int tputcnow ( TFILE *f, char c, int t ) 
{ 
  return tputc ( c, f, t ) < 0 ? EOF : ( tflush ( f, t ) ? EOF : c ) ; 
}

int tputs ( TFILE *f, char *s, int t )
{
  int n=0 ;
  while ( s && *s && ( n = tputc ( *s++, f, t ) ) != EOF ) ;
  tflush ( f, t ) ;
  return n ;
}


/* Scan responses since giving previous command (by cmd()) for a
   match to string 's' at start of response.  If a matching
   response is found it finds the start of the data field which
   is defined as the next non-space character in the current or
   any subsequent responses. If ip is not null, reads one integer
   (decimal format) into ip. [problem: Class 2.0 status responses
   are in hex.] Returns pointer to start of data field of
   response string or NULL if not found. */

char responses [ MAXRESPB ], *lresponse = responses ;

char *sresponse ( char *s, int *ip )
{
  char *p, *r = 0 ;
  int lens, lenr ;
  
  lens = strlen ( s ) ;
  for ( p=responses ; p<lresponse ; p += strlen(p) + 1 ) {

    if ( ! strncmp ( p, s, lens ) ) {
      r = p + lens ;

      lenr = strlen ( r ) ;
      if ( strspn ( r, " \r\n" ) == lenr && r+lenr < lresponse ) r += lenr ;

      if ( ip && sscanf ( r, "%d", ip ) > 0 )
	msg ( "R read value %d from \"%s\"", *ip, r ) ;
    }
    
  }

  return r ;
}

/* Search for a match to the string s in a null-terminated array of
   possible prefix strings pointed to by p.  The first character of each
   prefix string is skipped.  Returns pointer to the table entry or NULL if
   not found. */

char *strtabmatch ( char **p, char *s )
{
  while ( *p && strncmp ( *p+1, s, strlen ( *p+1 ) ) ) p++ ;
  return ( ! *p || **p == '-' ) ? NULL : *p ;
}


/* Send command to modem and check responses.  All modem commands
   go through this function. Collects pending (unexpected)
   responses and then pauses for inter-command delay (cmdpause)
   if t is negative.  Writes command s to modem if s is not null.
   Reads responses and terminates when a response is one of the
   prompts in prompts[] or if times out in t deciseconds.
   Repeats command if detects a RING response (probable
   collision). Returns the first character of the matching prefix
   string (e.g. 'O' for OK, 'C' for CONNECT, etc.)  or EOF if no
   such response was received within timeout t. */

int cmd ( TFILE *f, char *s, int t )
{
  char buf [ CMDBUFSIZE ], *p = "" ;
  int resplen=0 ;

  lresponse = responses ;

  retry:

  msg ( s ? "C- command  \"%s\"" : "C- waiting", s) ;

  while ( s && tgets ( f, buf, CMDBUFSIZE, t<0 ? cmdpause : 0 ) )
    msg ( "W- unexpected response \"%s\"", buf ) ;

  if ( s ) { 
    if ( strlen(s) >= CMDBUFSIZE-4 ) {
      msg ( "E modem command \"%s\" too long", s ) ;
    } else {
      sprintf ( buf, "AT%s\r", s ) ;
      tputs ( f, buf, t ) ;
    }
  }

  while ( t && ( p = tgets ( f, buf, CMDBUFSIZE, t<0 ? -t : t ) ) ) {

    if ( ( resplen += strlen ( buf ) + 1 ) <= MAXRESPB ) {
      strcpy ( lresponse, buf ) ;
      lresponse += strlen ( buf ) + 1 ;
    }

    if ( ( p = strtabmatch ( (char**) prompts, buf ) ) ) {
      msg ( "C- response \"%s\"", buf ) ;
      break ;
    }

    if ( ! strcmp ( buf, "RING" ) ) { msleep ( 100 ) ; goto retry ; }
  }

  return p ? *p : EOF ;
}


/* Send command to modem and wait for reply after testing (and
   possibly setting) current error status via err
   pointer. Returns command response or EOF on timeout. */

int ckcmd ( TFILE *f, int *err, char *s, int t, int r )
{
  int c ;
  if ( ( ! err || ! *err ) && ( c = cmd ( f, s, t ) ) != r && r ) {
    msg ( err ? "E %s %s %s" : "W %s %s %s",
	 c == EOF ? "timed out" : "wrong response",
	 s ? "after command: " :  "after waiting",
	 s ? s : "" ) ;
    if ( err ) *err = 3 ;
  }
  return c ;
}


/* Resynchronize modem from an unknown state.  If no immediate
   response, try pulsing DTR low (needs &D{2,3,4}), and then
   cancelling data or fax data modes.  In each case, discards any
   responses for about 2 seconds and then tries command ATQ0V1 to
   enable text responses.  Returns 0 if OK or 4 if no response.
   */

int modemsync ( TFILE *f )
{
  int err=0, method=0 ;

  for ( method=0 ; ! err ; method++ ) {
    switch ( method ) {
    case 0 : 
      break ;
    case 1 :
      break ;
      ttymode ( f, VOICECOMMAND ) ;
    case 2 : 
      msg ("Isync: dropping DTR") ;
      ttymode ( f, COMMAND ) ; msleep ( 200 ) ;
      ttymode ( f, DROPDTR ) ; msleep ( 200 ) ;
      ttymode ( f, COMMAND ) ; 
      break ;
    case 3 :
      msg ("Isync: sending escapes") ;
      ttymode ( f, VOICECOMMAND ) ;
      tputcnow ( f, CAN, -1 ) ;
      tputs ( f, DLE_ETX, -1 ) ; 
      msleep ( 100 ) ;
      ttymode ( f, COMMAND ) ;
      tputcnow ( f, CAN, -1 ) ;
      tputs ( f, DLE_ETX, -1 ) ; 
      msleep ( 1500 ) ;
      tputs ( f, "+++", -1 ) ; 
      break ;
    case 4 :
      err = msg ("E4sync: modem not responding") ;
      continue ;
    }
    while ( method && cmd ( f, 0, 20 ) != EOF ) ;
    if ( cmd ( f, "Q0V1", -20 ) == OK ) break ;
  }
  return err ;
} 


/* Set up modem by sending initialization/reset commands.
   Accepts either OK or CONNECT responses. Optionally changes
   baud rate if a command begins with a number. Returns 0 if OK,
   3 on errors. */

int setup ( TFILE *f, char **cmds, int ignerr )
{
  int err=0 ;
  char c ;

  for ( ; ! err && *cmds ; cmds++ ) {
#if 0
    if ( *cmds && isdigit( **cmds ) ) {
      
    }
#endif
    if ( ( c = cmd ( f, *cmds, -TO_RESET ) ) != OK && c !=  VCONNECT && 
	! ignerr ) {
      err = msg ( "E3modem command (%s) failed", *cmds ? *cmds : "none" ) ;
    }
  }

  return err ;
}


/* Terminate session.  Makes sure modem is responding, sends
   modem reset commands or hang-up command if none, removes lock
   files. Returns 0 if OK, 3 on error.*/

int end_session ( TFILE *f, char **zcmd,  char **lkfile )
{
  int err = 0 ;

  if ( f ) 
    err = modemsync ( f ) ;
  if ( f && zcmd && ! err ) 
    err = setup ( f, zcmd, 0 ) ;
  if ( f )
    ttymode ( f, ORIGINAL ) ;
  if ( lkfile )
    unlockall ( lkfile ) ;
  return err ;
} 
    

/* Initialize session.  Try locking and opening fax device until opened or
   get error. Then set tty modes, register signal handler, set up
   modem. Returns 0 if OK, 2 on errors, 3 if initialization failed, 4 if no
   modem response. */

int begin_session ( TFILE *f, char *fname, int reverse, char **lkfile, 
		   ttymodes mode, void (*onsig) (int) )
{
  int i, err=0, busy=0, minbusy=0 ;

  do {
    err = lockall ( lkfile, busy >= minbusy ) ;
    if ( ! err ) err = ttyopen ( f, fname, reverse ) ;
    if ( err == 1 ) { 
      if ( busy++ >= minbusy ) {
	msg ( "W %s locked or busy. waiting.", fname ) ;
	minbusy = minbusy ? minbusy*2 : 1 ;
      }
      msleep ( lockpolldelay ) ;
    }
  } while ( err == 1 ) ;
  
  if ( ! err ) msg ( "Iopened %s", fname ) ;

  if ( ! err ) err = ttymode ( f, mode ) ;
  
  for ( i=0 ; ! err && catch [ i ] ; i++ ) 
    if ( signal ( catch [ i ], onsig ) == SIG_ERR ) 
      err = msg ( "ES2can't set signal %d handler:", catch [ i ] ) ;
  
  if ( !err ) err = modemsync ( f ) ;

  return err ;
}
