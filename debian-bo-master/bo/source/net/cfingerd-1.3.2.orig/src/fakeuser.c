/*
 * CFINGERD
 * Fakeuser display routine
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
 * GET_PARMS
 *
 * This routine counts the number of parameters that are sent to the
 * fake user.
 */
int get_parms(char *username)
{
    int i = 0, x = 0;

    for (i = 0; i < strlen(username); i++)
	x += (username[i] == '.') ? 1 : 0;

    return(x);
}

/*
 * SEARCH_FAKE_POS
 *
 * This returns the position in the fake user structure that this fake user
 * lies.  If the fake user was not found in the structure, it returns a
 * "-1", meaning that the user doesn't exist.
 */
int search_fake_pos(char *username)
{
    int i, len;

    len = strlen((char *) username);
    for (i = 0; i < fakeuser_num; i++) {
	char *k = prog_config.fusers[i].user;

	if (*k == '-')
	    k++;

	if (!strcmp(username, k))
	    return(i);
    }

    return(-1);
}

/*
 * HANDLE_FAKEUSER
 *
 * This routine simply handles the username specified, and handles all of
 * the scripting information.
 */
void handle_fakeuser(char *username)
{
    char *data[5];
    int num_parms = 0, i, funum;

    show_top();

    num_parms = get_parms(username);
    num_parms++;

    if (prog_config.config_bits3 & SHOW_FAKEUSER) {
	if (num_parms > 5) {
	    printf("\n                    Sorry, you specified too many options.\n\n");
	    fflush(stdout);
	    show_bottom();
	    log(LOG_WARN, "Too many options specified in fake user finger", NULL);
	    return;
 	}

	/* Clear out any garbage */
	for (i = 0; i < 5; i++)
	    data[i] = NULL;

	/* And allocate our garbage.  :) */
	for (i = 0; i < num_parms; i++)
	    data[i] = (char *) malloc(80);

	if (num_parms == 1)
	    sscanf(username, "%[^\r\n]\r\n", data[0]);
	else if (num_parms == 2)
	    sscanf(username, "%[^.].%[^\r\n]\r\n", data[0], data[1]);
	else if (num_parms == 3)
	    sscanf(username, "%[^.].%[^.].%[^\r\n]\r\n", data[0], data[1], data[2]);
	else if (num_parms == 4)
	    sscanf(username, "%[^.].%[^.].%[^.].%[^\r\n]\r\n",
		data[0], data[1], data[2], data[3]);
	else if (num_parms == 5)
	    sscanf(username, "%[^.].%[^.].%[^.].%[^.].%[^\r\n]\r\n",
		data[0], data[1], data[2], data[3], data[4]);

	log(LOG_USER, "Fakeuser: ", username);

	funum = search_fake_pos(data[0]);

	/* FUNUM should NEVER reach -1 ... we already checked this 
	   internally! */
	if (funum != -1) {
	    char send_string[80];

	    if (prog_config.fusers[funum].searchable)
		sprintf(send_string, "%s %s %s %s %s",
		    prog_config.fusers[funum].script,
		    (data[1] != NULL) ? data[1] : "",
		    (data[2] != NULL) ? data[2] : "",
		    (data[3] != NULL) ? data[3] : "",
		    (data[4] != NULL) ? data[4] : "");
	    else
		sprintf(send_string, "%s", prog_config.fusers[funum].script);

	    safe_exec(send_string);
	    fflush(stdout);
	}
    } else {
	printf("\n");
	printf("             Sorry, this system does not have any fake users enabled!\n\n");
	log(LOG_WARN, "Fake user requested, but rejected - disabled.", " ");
    }

    show_bottom();
}
