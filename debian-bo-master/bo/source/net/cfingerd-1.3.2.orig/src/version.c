/*
 * CFINGERD
 * Version information
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

#include "version.h"

/*
 * SHOW_VERSION_INFO
 *
 * Show program version information.
 */
void show_version_info(void)
{
    syslog(LOG_NOTICE, "Version info");

    printf("\nThis site is running the Configurable Finger-Query Daemon version\n");
    printf("%s, available from Ken Hollis (khollis@bitgate.com) on either\n", VERSION);
    printf("sunsite.unc.edu, ftp.cdrom.com, or bitgate.com.\n\n");
    printf("For more information, or if you have any questions about the software\n");
    printf("daemon, please write to Ken Hollis at the above address or write to your\n");
    printf("system administrator.\n\n");

#ifdef	IN_TESTING
    printf("This version of CFINGERD is currently undergoing either extensive\n");
    printf("testing, or it is being worked on.  If you see this message, it probably\n");
    printf("means this program is being tested.  If you see any bug reports in\n");
    printf("this phase of development and it still appears after a day, please let\n");
    printf("the programmer know.\n\n");
#endif

#ifdef	LAST_STABLE_RELEASE
    printf("The last stable version of CFINGERD is %s\n\n", LAST_STABLE_RELEASE);
#endif

    log(LOG_USER, "Version information", " ");

    fflush(stdout);
    exit(PROGRAM_OKAY);
}
