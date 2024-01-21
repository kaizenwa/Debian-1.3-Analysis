/*
 *    This version is for HP-UX 3.0 (on HP9000 Series 800)
 */

#include "s-hpux2-1.h"

#define    SIGNAL_HANDLERS_ARE_VOID

/*
 *    Define DETATCH_TERMINAL to be a command sequence which
 *    will detatch a process from the control terminal
 */

#define	DETATCH_TERMINAL	setpgrp();
