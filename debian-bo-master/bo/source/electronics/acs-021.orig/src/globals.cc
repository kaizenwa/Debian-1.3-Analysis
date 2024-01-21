/*$Id: globals.cc,v 11.28 96/03/03 23:07:48 al Exp $ -*- C++ -*-
 */
#include "e_node.h"
#include "l_jmpbuf.h"
/*--------------------------------------------------------------------------*/
/* nodes, hold between commands */
NODE* nstat;			/* node status flags			*/
/*--------------------------------------------------------------------------*/
/* other (the ultimate in disorder) */
char head[BUFLEN+3];
/*--------------------------------------------------------------------------*/
/* command interpreter, control stuff */
JMP_BUF env;			/* environment for setjmp, longjmp	*/
bool crtplot = false;		/* flag: is plotting on the crt		*/
run_mode_t run_mode;		/* variations on handling of dot cmds   */

/* sweep command */
int swp_count[RECURSE];		/* sweep counter			*/
int swp_steps[RECURSE];		/* sweep number of steps		*/
int swp_type[RECURSE];		/* type of sweep (log or linear)	*/
int swp_nest;			/* sweep nesting (recursion) level	*/

char e_int[]	= "internal error: %s\n";
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
