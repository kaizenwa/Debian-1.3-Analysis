/*	rewrite.h,v 1.1 1995/03/07 13:04:20 nm4 Exp
 *
 * rewrite.h:
 *	interface file for rewrite driver.
 */

#include "pathalias.h" /* much is shared with pathalias driver */

/* macros local to the reroute driver */
/* (none, except those shared with the pathalias driver) */

/* private information stored per router file entry */
struct rewrite_private {
    /* `pap' must be the first element here. */
    struct pathalias_private pap;	/* attributes shared with pathalias */
};
