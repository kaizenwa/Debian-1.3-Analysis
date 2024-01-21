/*
 *      This version is for ultrix systems
 *	From: dietrich@cernvax.cern.ch (dietrich wiegandt)
 */

#include "s-bsd4-2.h"

/*
 *      Define HAVE_VARARGS, Ultrix has vprintf, vsprintf as well
 */

#define HAVE_VARARGS

/*
 *      Ultrix signal handlers have type void (see signal.h)
 */

#define	SIGNAL_HANDLERS_ARE_VOID	/* */
