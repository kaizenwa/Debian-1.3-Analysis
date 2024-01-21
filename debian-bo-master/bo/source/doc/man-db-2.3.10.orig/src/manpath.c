/*
 * manpath.c: display either the manpath or catpath
 *  
 * Copyright (C), 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * Thu Nov 17 08:29:39 GMT 1994  Wilf. (G.Wilford@ee.surrey.ac.uk) 
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>

#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#else /* !HAVE_GETOPT_H */
#  include "lib/getopt.h"
#endif /* HAVE_GETOPT_H */

#define NLS_SET	manpathSet
#include "nls/nls.h"

#ifdef NLS
nl_catd catfd;
#endif /* NLS */

#include "manconfig.h"
#include "lib/error.h"
#include "manp.h"

extern char *optarg;
extern int optind, opterr, optopt;

#ifndef debug
int debug;
#endif
char *program_name;
int quiet = 0;

static const struct option long_options[] =
{
    {"catpath", no_argument, 		0, 'c'},
    {"global",  no_argument,		0, 'g'},
    {"debug",   no_argument, 		0, 'd'},
    {"help",    no_argument, 		0, 'h'},
    {"quiet",   no_argument, 		0, 'q'},
    {"version", no_argument, 		0, 'V'},
    {"systems",  required_argument, 	0, 'm'},
    {0, 0, 0, 0}
};

static const char args[] = "cgdhqVm:";

static void usage(int status)
{
	printf (CATGETS(manpath_USAGE, 
		"usage: %s [[-gcdq] [-m system]] | [-V] | [-h]\n"), program_name);
	printf (CATGETS(manpath_OPT,
		"-c --catpath                show relative catpaths.\n"
		"-g --global                 show the entire global manpath.\n"
	        "-d --debug                  produce debugging info.\n"
	        "-q --quiet                  produce fewer warnings.\n"
	        "-m --systems system         express which `systems' to use.\n"
	        "-V --version                show version.\n"
	        "-h --help                   show this usage message.\n"));

	exit (status);
}

/*
 * Examine user's PATH and print a reasonable MANPATH.
 */
int main (int argc, char *argv[])
{
	int c, global = 0, cat = 0;
	char *alt_system = NULL;
	char *path_string;
	int option_index; /* not used, but required by getopt_long() */

	program_name = xstrdup(basename((argv[0])));
	NLS_INIT;

	while ((c = getopt_long (argc, argv, args,
				 long_options, &option_index)) != EOF) {
		switch (c) {

			case 'c':
				cat = 1;
				break;
			case 'd':
#ifndef debug
			    	debug = 1;
#endif
			    	break;
		    	case 'q':
			    	quiet = 1;
			    	break;
			case 'm':
				alt_system = optarg;
				break;
			case 'g':
				global = 1;
				break;
			case 'V':
				ver();
				break;
		    	case 'h':
		    		usage(OK);
		    		break;
		    	default:
		    		usage(FAIL);
		    		break;
		}
	}
	
	path_string = manpath (alt_system);

	if (global) {
		path_string = get_mandb_manpath();
		if (!path_string)
			error (FAIL, 0,
			       CATGETS(manpath_NO_GLOBAL,
				       "warning: no global manpaths set in config file %s"),
			       CONFIG_FILE);
	}
	if (cat)
		path_string = cat_manpath(path_string);

	printf ("%s\n", path_string);
	exit (OK);
}
