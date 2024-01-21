/*
    dbview.c - View dBase III files
    Copyright (c) 1995  Martin Schulze <Martin.Schulze@Infodrom.North.DE>

    This file is part of the dbview package, a viewer for dBase II files.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
 * Thu Sep 26 21:38:33 1996:  Martin Schulze <joey@infodrom.north.de>
 *	Added trimming option (-t) to also get untrimmed browse output.
 *	Thanks to Magnus Lassus <mlassus@walli.uwasa.fi> for the idea and
 *	providing me with a patch.
 */

#include "version.h"
#include "db_dump.h"

#include <stdio.h>
#include <getopt.h>

void help_short()
{
    printf ("%s %s - %s, (c) 1996 by Martin Schulze\n", progname, version, longname);
    printf ("\n");
    printf ("%s [-b [-t]] [-d delim] [-e] [-h] [-i] [-o] [-o] [-v] dbfile\n", progname);
}

void help_long()
{
    printf ("%s %s - %s, (c) 1996 by Martin Schulze\n", progname, version, longname);
    printf ("\n");
    printf ("  --browse, -b           browse the database\n");
    printf ("  --delimiter, -d        set the delimiter for browse output\n");
    printf ("  --description, -e      display field description\n");
    printf ("  --help, -h             display help\n");
    printf ("  --info, -i             display db information\n");
    printf ("  --omit, -o             omit db records\n");
    printf ("  --reserve, -r          reserve fieldnames from beeing translated\n");
    printf ("  --trim, -t             trim browse fields\n");
    printf ("  --version, -v          display version\n");
}

void main (int argc, char **argv)
{
    int opt_index;
    char c;
    static const struct option long_options[] =
      {
	{"browse", no_argument, 0, 'b'},
	{"delimiter", required_argument, 0, 'd'},
	{"description", no_argument, 0, 'e'},
	{"help", no_argument, 0, 'H'},
	{"info", no_argument, 0, 'i'},
	{"omit", no_argument, 0, 'o'},
	{"reserve", no_argument, 0, 'r'},
	{"trim", no_argument, 0, 't'},
	{"version", no_argument, 0, 'v'},
	{0, 0, 0, 0}
	};
    char dbfile[256];
    int flags = 0;
    char delim = ':';

    optind = 0;
    while ( (c = getopt_long(argc, argv, "Hbd:ehiortv", long_options, &opt_index)) != -1 ) {
	switch (c) {
	case 'H':	/* --help */
	    help_long (); exit (0);
	case 'b':
	    flags |= DB_FL_BROWSE;
	    break;
	case 'd':
            if (!argv[optind]) {
                printf ("Too few arguments.\n");
                break;
            }
	    delim = optarg[0];
	    break;
	case 'e':
	    flags |= DB_FL_DESCR;
	    break;
	case 'h':
	    help_short (); exit (0);
	case 'i':
	    flags |= DB_FL_INFO;
	    break;
	case 'o':
	    flags |= DB_FL_OMIT;
	    break;
	case 'r':
	    flags |= DB_FL_RESERVE;
	    break;
	case 't':
	    flags |= DB_FL_TRIM;
	    break;
	case 'v':	/* --version */
	    printf ("%s %s - %s\n", progname, version, longname); exit (0);
	}   /* switch */
    }   /* while */

/*    printf ("argc=%d, optind=%d\n", argc, optind); */
    if ( argc - optind < 1) {
	help_short();
	exit (0);
    }

    strcpy(dbfile, argv[optind]);
    /* if ! -f dbfile thnen -f dbfile.dbf */

    db3_process (dbfile, flags, delim);

}
