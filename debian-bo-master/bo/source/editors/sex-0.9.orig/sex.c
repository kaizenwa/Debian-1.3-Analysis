/*
 * File:	sex.c
 * Purpose:	A simple editor for X, main program.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: sex.c,v 1.4 1996/10/16 04:11:35 liw Exp $"
 */

#include <stdio.h>
#include <locale.h>
#include <publib.h>

#include "x.h"


int main(int argc, char **argv) {
	set_progname(argv[0], "SeX");
	setlocale(LC_ALL, "");
	if (log_add(stderr, log_level_chat) == -1)
		errormsg(1, 0, "can't add stderr to logged files!");

	x_do_it(argc, argv);

	/*
	 * If we get here, something is wrong.
	 */
 	errormsg(0, 0, "internal error -- x_do_it returned!");
	abort();
}
