#ifndef _EFAXIO_H
#define _EFAXIO_H

#include "efaxos.h"		/* TFILE definition */

#ifndef u_char
#define u_char unsigned char
#endif

#define CMDBUFSIZE 256		/* longest possible command or response */
#define DLE_ETX "\020\003"	/* DLE-ETX (end of data) string */
#define TO_RESET  50		/* t/o for modem reset commands (>>2.6s) */
#define T_CMD     1		/* pause before each modem command */

enum promptcodes {		/* codes for modem prompts */
   BUSY = 'B', CONNECT = 'C', DATA='D', ERROR = 'E', 
   MODULATION='M', NO = 'N', OK = 'O', RING = 'R', VCONNECT = 'V' } ;

		      /* Modem features */

extern int c1, c20 ;		/* use class 1/class 2.0 */
extern int cmdpause ;		/* delay before each init command */
extern vfc ;			/* virtual flow control */
extern u_char startchar ;	/* character to start reception */
extern int lockpolldelay ;	/* milliseconds between checks of lock files */

		 /* Modem interface routines */

int tputcnow ( TFILE *f, char c, int t ) ;
int tputs ( TFILE *f, char *s, int t ) ;

int cmd ( TFILE *f, char *s , int t ) ;
int ckcmd ( TFILE *f, int *err, char *s, int t, int r ) ;
int modemsync ( TFILE *f ) ;
char *sresponse ( char *s, int *ip ) ;

int setup ( TFILE *f, char **cmds, int ignerr ) ;


int begin_session ( TFILE *f, char *fname, int reverse, char **lkfile, 
		   ttymodes mode, void (*onsig) (int) ) ;

int end_session ( TFILE *f, char **zcmd,  char **lkfile ) ;

#endif
