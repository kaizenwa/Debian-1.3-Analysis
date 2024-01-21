/*
 * CFINGERD
 * Starting option routines
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
 * SET_DAEMON_MODE
 *
 * Eventually, this will bind to the finger socket, and stay running as a
 * true daemon.  For the time being, though, it is only INETD compliant.
 */
void set_daemon_mode(void)
{
    syslog(LOG_NOTICE, "Daemon mode initiated");

    printf("CFINGERD %s daemon mode starting.\n", VERSION);
    printf("\nSorry, daemon mode for cfingerd is not yet available in CFINGERD.\n");
    printf("Wait for the next release.\n\n");
    fflush(stdout);
    exit(0);
}

/*
 * GIVE_HELP
 *
 * This gives help about command line options under cfingerd.
 */
void give_help(void)
{
    printf("\nCFINGERD command line options:\n\n");
    printf("\t-c\t\tChecks the configuration to make sure it's okay\n");
    printf("\t-d\t\tRuns the daemon as stand-alone (non-inetd)\n");
    printf("\t-e [user]\tEmulates a local finger from the command line\n");
    printf("\t-o\t\tTurn off all incoming finger queries\n");
    printf("\t-v\t\tRetrieves daemon version information\n\n");
    printf("Any other options will give you this screen.  Commands are not\n");
    printf("case sensitive.\n\n");
    fflush(stdout);

    exit(PROGRAM_OKAY);
}

/*
 * CHECK_OPTIONS
 *
 * This is called at runtime (and is usually the first option).  This takes
 * in the argv pointer, and verifies the options passed.
 */
void check_options(int argc, char *argv[])
{
    if (!strncasecmp(argv[1], "-d", 2))
	set_daemon_mode();
    else if (!strncasecmp(argv[1], "-v", 2))
	show_version_info();
    else if (!strncasecmp(argv[1], "-e", 2)) {
	if (argc > 2) {
	    emulated = TRUE;
	    syslog(LOG_NOTICE, "Emulated: \"%s\"", (char *) argv[2]);
	    printf("[127.0.0.1 : Username: %s]\n", (char *) argv[2]);
	    fflush(stdout);
	    return;
	} else
	    CF_ERROR(E_COMMANDLINE);
    } else if (!strncasecmp(argv[1], "-o", 2)) {
	CF_ERROR(E_FINGEROFF);
    } else if (!strncasecmp(argv[1], "-c", 2)) {
	read_configuration();
	printf("\nConfiguration okay.\n\n");
	fflush(stdout);
	exit(PROGRAM_OKAY);
    }

    give_help();
}
