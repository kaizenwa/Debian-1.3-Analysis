/* $Id: srvr.h,v 1.3 1996/06/16 23:17:01 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 16, 1996 3:13 pm PDT */

#include <iluxport.h>

extern int      doit(char *pinfo, ilu_TransportInfo tinfo,
		     ilu_boolean threadly,
		     ilu_boolean init_credentials);
/*
 * Both pointer arguments may be defaulted to NULL; defaulting tinfo
 * means to initialize credentials and use security if available.
 */

extern ilu_TransportInfo DefaultTInfo(ilu_boolean secure);
/*
 * Return the default ilu_TransportInfo.  Returns NIL if /secure/
 * but security support isn't configured into ILU.
 */


#if defined (_WINIO)
#define OUTPUT	WIN_PRINTF
extern void WIN_PRINTF(char *format, ...);
#else
#define OUTPUT	printf
#endif
