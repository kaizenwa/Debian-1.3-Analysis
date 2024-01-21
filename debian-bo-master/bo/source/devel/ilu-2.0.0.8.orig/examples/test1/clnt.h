/* $Id: clnt.h,v 1.2 1996/04/05 23:52:35 mdavidso Exp $ */
/* Last edited by Mike Spreitzer April 3, 1996 2:42 pm PST */

extern int doit();

#if defined (_WINIO)
#define OUTPUT	WIN_PRINTF
extern void WIN_PRINTF(char *format, ...);
#else
#define OUTPUT	printf
#endif

