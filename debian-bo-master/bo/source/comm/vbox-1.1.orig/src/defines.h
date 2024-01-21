/* $Id: defines.h,v 1.1 1996/06/27 15:58:07 root Exp $ */

#ifndef _VBOX_DEFINES
#define _VBOX_DEFINES 1

/** Defines **************************************************************/

#define CloseStream(S)		{ fclose(S); S = NULL; }
#define CloseFD(F)			{ close(F); F = -1; }
#define SleepSecs(S)			{ sleep(S); }

#define False()				return(0)
#define True()					return(1)
#define Cancel()				return(-1)
#define Delete					unlink

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (0)
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

#ifndef __NCURSES_H
typedef char bool;
#endif /* __NCURSES_H */

typedef char boolean;

#endif /* _VBOX_DEFINES_H */
