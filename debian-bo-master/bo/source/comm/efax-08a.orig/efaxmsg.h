#ifndef _EFAXMSG_H
#define _EFAXMSG_H

#ifndef u_char
#define u_char unsigned char
#endif

#include <time.h>

		    /* Messages & Program Arguments */

enum  cchar {				/* control characters */
  NUL, SOH, STX, ETX, EOT, ENQ, ACK, BEL, BS,  HT,  LF,
  VT,  FF,  CR,  SO,  SI,  DLE, XON, DC2, XOFF,DC4, NAK,
  SYN, ETB, CAN, EM,  SUB, ESC, FS,  GS,  RS,  US } ;

extern char *verb[] ;		/* types of messages to print */
extern char *argv0 ;		/* program name */

char *cname ( unsigned char c ) ;
time_t tstamp ( time_t last, FILE *f ) ;
int msg ( char *fmt, ... ) ;

extern int nxtoptind ;
extern char *nxtoptarg ;

extern int sys_nerr;
extern char *sys_errlist[];

int nextopt( int argc, char **argv, char *args ) ;

#endif


