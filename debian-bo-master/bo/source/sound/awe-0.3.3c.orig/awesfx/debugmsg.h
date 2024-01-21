/*================================================================
 * debug message handler
 *================================================================*/

#ifndef DEBUGMSG_H_DEF
#define DEBUGMSG_H_DEF

extern int verbose;
#define DEBUG(LVL,XXX)	{if (verbose > LVL) { XXX; }}

#endif
