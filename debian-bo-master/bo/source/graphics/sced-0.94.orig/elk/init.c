#include "elk_private.h"
#include <unistd.h>

extern int init_eval();
extern char *Elk_Eval(char *expr);
extern int init_callbacks();

int
init_elk(int ac, char **av)
{
	int fakeac = 1;
	char *fakeav[1];
	Boolean	had_init = FALSE;

	fakeav[0] = av[0];

	if ( ! access("init.scm", R_OK) )
	{
		Elk_Init(fakeac, fakeav, 0, "init.scm");
		had_init = TRUE;
	}
	else
		Elk_Init(fakeac, fakeav, 0, NULL);
	init_eval();
	/*
	 * Now we define various functional interfaces for calling
	 * from scheme to C
	 */
	init_callbacks();
	if ( had_init )
		Elk_Eval("(startup)\n");
	return 0;
}
