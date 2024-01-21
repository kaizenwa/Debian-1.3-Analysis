/* includes.h
 *   #include stuff used by the tacacs server.
 *
 * Last modified: 25-Dec-1990/al
 *
 * 25-Dec-1990  al      Include even more stuff
 * 24-Dec-1990  al      Yanked out of big tacacs-server.c
 *
 */

#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]time.h"
#define __TYPES_LOADED 1	/* kludge to avoid CC-W-CONFLICTDECL error */
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include]errno.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include]netdb.h"
#include <ctype.h>
#include <descrip.h>
#include <errno.h>
#include <stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unixlib.h>
#include libdtdef
#include lnmdef
#include opcdef
#include psldef
#include rmsdef
#include ssdef
#include uaidef

/* Handy UofA macros & defines, in C$LIBRARY == UA_LIB:UALIB.TLB */
#include "UA_VMS"

#include "tacacs-server.h"

typedef struct
{				/* exit handler block */
	LONG            dummy;
	void            (*routine) ();
	LONG            arg_count,
	               *condition;
}               EXH_BLK;

#define	LOG_ERR		1
#define	LOG_INFO	2
#define	LOG_PID		3
#define	LOG_DEBUG	4

/* Flags for checking users in our database. */
#define	F_PASSWORD	0x1	/* Password verification only */
#define	F_DIALBACK	0x2	/* Dialback when this ID is entered */
#define	F_STUDENT	0x10	/* User is student */
#define	F_RESEARCH	0x20	/* User is from the research community */
