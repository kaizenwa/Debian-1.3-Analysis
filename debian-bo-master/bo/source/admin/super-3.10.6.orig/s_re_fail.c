#ifdef vms
#include stdio
#else
#include <stdio.h>
#endif

/* 
 * s_re_fail:
 *	internal error handler for s_re_exec.
 *
 *	should probably do something like a
 *	longjump to recover gracefully.
 */ 
void	
s_re_fail(s, c)
char *s;
char c;
{
	(void) fprintf(stderr, "%s [opcode %o]\n", s, c);
	exit(1);
}
