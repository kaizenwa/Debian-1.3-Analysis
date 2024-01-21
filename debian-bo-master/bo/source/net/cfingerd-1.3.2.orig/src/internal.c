/*
 * CFINGERD
 * Internal finger handler
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

/*
 * SHOW_SERVICES
 *
 * This routine simply shows the available services that your system 
 * provides.
 */
void show_services(void)
{
    int i;

    show_top();

    for (i = 0; i < num_headers; i++)
	if (prog_config.services.header[i] != NULL)
	    printf("%s", prog_config.services.header[i]);

    for (i = 0; i < fakeuser_num; i++)
	if (prog_config.fusers[i].user[0] != '-') {
	    printf(prog_config.services.display_string,
	(prog_config.services.name_pos == 1) ? prog_config.fusers[i].user :
	(prog_config.services.service_pos == 1) ? prog_config.fusers[i].description :
	(prog_config.services.search_pos == 1) ?
		(prog_config.fusers[i].searchable ? "TRUE" : "FALSE") : " ",
	(prog_config.services.name_pos == 2) ? prog_config.fusers[i].user :
	(prog_config.services.service_pos == 2) ? prog_config.fusers[i].description :
	(prog_config.services.search_pos == 2) ?
		(prog_config.fusers[i].searchable ? "TRUE" : "FALSE") : " ",
	(prog_config.services.name_pos == 3) ? prog_config.fusers[i].user :
	(prog_config.services.service_pos == 3) ? prog_config.fusers[i].description :
	(prog_config.services.search_pos == 3) ?
		(prog_config.fusers[i].searchable ? "TRUE" : "FALSE") : " ");

	printf("\n");
    }

    fflush(stdout);

    show_bottom();
}

/*
 * HANDLE_INTERNAL
 *
 * This routine handles the internal fingernames.  If CFINGERD detects a
 * U_INTERNAL finger type, it calls this routine automatically.  Pretty
 * nice.  :)
 */
void handle_internal(char *username)
{
    if (!strncmp(username, "version", 7))
	show_version_info();
    else if (!strncmp(username, "services", 8)) {
	if (prog_config.config_bits3 & SHOW_FAKEUSER) {
	    show_services();
	    syslog(LOG_NOTICE, "%s", prog_config.p_strings[D_SVC_FINGER]);
	    log(LOG_USER, "Service request", " ");
	    exit(PROGRAM_OKAY);
	} else {
	    printf("\n                Sorry, fake users are not enabled on this system!\n\n");
	    log(LOG_WARN, "Services requested, but disabled.", " ");
	    exit(PROGRAM_OKAY);
	}
    } else if (!strncmp(username, "search", 6)) {
	if (prog_config.config_bits2 & SHOW_SEARCHFING)
	    show_search(username);
	else
	    CF_ERROR(E_SEARCH);
	exit(PROGRAM_OKAY);
    } else if (!strncmp(username, "userlist-only", 13)) {
	if (prog_config.config_bits2 & SHOW_ULISTONLY) {
	    safe_exec("userlist | tail +2");
	    fflush(stdout);
	}
	log(LOG_USER, "Userlist-only", " ");
	exit(PROGRAM_OKAY);
    } else if (!strncmp(username, "help", 4)) {
	show_top();
	printf("\ncfingerd Internal Usernames Help\n\n");
	printf("Finger \"version\" for version information.\n");
	printf("Finger \"services\" to list services this system provides.\n");
	printf("Finger \"search\" to search for a username.\n");
	printf("Finger \"userlist-only\" to get only a userlist of who's on.\n");
	printf("Finger \"help\" gives you this help list.\n\n");
	show_bottom();
	fflush(stdout);
	syslog(LOG_NOTICE, "Help fingered");
	exit(PROGRAM_OKAY);
    }
}
