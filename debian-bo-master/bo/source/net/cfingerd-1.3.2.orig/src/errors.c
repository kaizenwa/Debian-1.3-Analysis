/*
 * CFINGERD
 * Verbose error handler
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include "cfingerd.h"
#include "proto.h"

/* Error messages */
ECRUFT errors[] = {
    { NULL, 0 },
    {"This daemon cannot be run in stand-alone mode.  Please read the\n"
     "documentation pertaining to the -s option\n", 0},
    {"This daemon must be run as root!\n", 0},
    {"This daemon must be run inside INETD with this option!\n", 0},
    {"Please edit your cfingerd.conf file.  An error was detected.\n", 0},
    {"This program just died on a segmentation violation.  Please report\n"
     "this to the programmer, khollis@bitgate.com if you can reproduce it.\n", 0},
    {"This program should not be run as mode 1755 or the like with a\n"
     "1000 chmod (suid).  Please change it to 0755 (recommended.)\n", 0},
    {"Sorry, you indicated the wrong number of arguments for this\n"
     "command line option.\n", 0},
    {"Sorry, the owner of this system has temporarily shut off all\n"
     "finger requests.  Please try querying this system at a later date.\n", 0},
    {"Settings in cfingerd.conf contradict themselves.\n"
     "You cannot have sites in trusted/rejected listings containing wildcards\n"
     "if you have ALLOW_HOSTNAME_WILDCARDS disabled, as you do in cfingerd.conf.\n"
     "If you want to have wildcards allowed, please enable this option.\n", 0},
    {"Sorry, the owner of this site has turned off all searchable finger\n"
     "queries to this system.\n", 0},
    {"Sorry, no DNS entry was found matching your IP address, and this site\n"
     "does not allow fingers from sites with non-matched DNS entries.\n", 0}
};
