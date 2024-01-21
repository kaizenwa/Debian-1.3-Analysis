/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:
 *
 *	The options scanning module
 */

#include <stdio.h>
#include "xtrojka.h"
#include "debug.h"

extern flag	is_debug_info;


/*
 *	implementation
 */
void get_options(argc, argv)
int argc;
char **argv;
/*
 * parse the arguments
 */
{
	DEBUG("options.c","get_options");

	while(--argc) {
		if((!strcmp(argv[argc], "-debug")) 
		|| (!strcmp(argv[argc], "-d"))) {
#ifdef DEBUG_INFO
			is_debug_info = True;
#else
			show_no_debug();
#endif
		}
		if((!strcmp(argv[argc], "-help")) 
		|| (!strcmp(argv[argc], "-h"))
		|| (!strcmp(argv[argc], "-?"))) {
			show_help();
			exit(0);
		}
		if((!strcmp(argv[argc], "-scores")) 
		|| (!strcmp(argv[argc], "-s"))) {
			show_scores_offline();
			exit(0);
		}
	}
}


