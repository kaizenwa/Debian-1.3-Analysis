/*
 * CDTOOL is a program to play and catalog audio compact discs on
 * a cdrom drive
 *
 * Copyright 1994 Thomas Insel.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * with this program; if not, write to the Free Software Foundation,
 * Inc., 675 Mass Ave., Cambridge, MA 02139, USA.
 *
 * For information on CDTOOL, alternate licensing, or whatever, 
 * contact Thomas Insel via email (tinsel@uiuc.edu) or write to
 * Thomas Insel, 210 Parkview Drive, Bloomington, IL 61701-2038.
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>

#include "config.h"

static char rcsid[] = "$Id: main.c,v 1.2 1994/06/07 23:20:53 tinsel Exp $";

main(int argc, char *argv[]) {
    int i, cdfile;
    char *progname, *cd_device;

    if ( (progname=rindex(argv[0],'/')) != 0)
        progname++;
    else
        progname = argv[0];

    if (argc >= 2)
        for (i=1; i<argc; i++)
            if ( !strcmp(argv[i],"--help") || !strcmp(argv[i],"-?") ) {
                do_usage(progname);
                exit(0);
            }

    if ( (cd_device=getenv("CDTOOLDEV")) == NULL) 
        cd_device = strdup(CD_DEVICE);

    if ( (cdfile=open(cd_device, O_RDONLY)) == -1 ) {
        fprintf(stderr, "%s: can't open cdrom (%s)\n", argv[0], cd_device);
        exit(1);
    }

    if ( strcmp(progname, "cdplay") == 0 )
        do_play(argc, argv, cdfile);
    else if ( strcmp(progname, "cdstop") == 0 )
        do_stop(argc, argv, cdfile);
    else if ( strcmp(progname, "cdeject") == 0 )
        do_eject(argc, argv, cdfile);
    else if ( strcmp(progname, "cdpause") == 0 )
        do_pause(argc, argv, cdfile);
    else if ( strcmp(progname, "cdir") == 0 )
        do_info(argc, argv, cdfile);
    else if ( strcmp(progname, "cdvolume") == 0 )
        do_volume(argc, argv, cdfile);
    else do_usage(progname);

    close(cdfile);

}

do_usage(char *progname) {

	fprintf(stderr,VERSION_STRING); fprintf(stderr,"\n");

    if ( strcmp(progname, "cdplay") == 0 )
        fprintf(stderr, "usage: cdplay [start] [stop]\n");
    else if ( strcmp(progname, "cdstop") == 0 )
        fprintf(stderr, "usage: cdstop\n");
    else if ( strcmp(progname, "cdeject") == 0 )
        fprintf(stderr, "usage: cdeject\n");
    else if ( strcmp(progname, "cdpause") == 0 ) {
        fprintf(stderr, "usage: cdpause\n");
        fprintf(stderr, "  resume play with cdplay\n");
    } else if ( strcmp(progname, "cdir") == 0 ) {
        fprintf(stderr, "usage: cdir -n [-s | -t]\n");
        fprintf(stderr, " -n            listing without titles\n");
        fprintf(stderr, " -s            short listing (title/artist/track)\n");
        fprintf(stderr, " -t            print template for database entry\n");
    } else 
        fprintf(stderr, "call as cdplay, cdstop, cdeject, cdpause, or cdir\n");

    fprintf(stderr, " -? --help     print this message\n");
}
