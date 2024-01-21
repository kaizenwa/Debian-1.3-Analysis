/*
 * CFINGERD
 * Userlist handler
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
#include "privs.h"

/*
 * HANDLE_USERLIST
 * 
 * This routine simply handles the displaying of the userlist.
 */
void handle_userlist(void)
{
    BOOL can_show = FALSE;

    if (local_finger) {
	if (prog_config.local_config_bits2 & SHOW_SYSTEMLIST)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits2 & SHOW_SYSTEMLIST)
	    can_show = TRUE;
    }

    show_top();

    if (can_show) {
	int i;
	char cmdline[128];

	if ((num_finger_sites > 1) &&
	    prog_config.config_bits2 & SHOW_MULTFING) {
	    printf("%s\n", prog_config.p_strings[D_WAIT]);
	    fflush(stdout);
	}

	NOBODY_PRIVS;

	if ((!strstr(prog_config.finger_program, "finger")) &&
	    (!strstr(prog_config.finger_program, "w"))) {
	    printf("Username Real name                      Idletime TTY Remote console location\n");
	    fflush(stdout);
	}

	if (prog_config.config_bits2 & SHOW_MULTFING) {
	    for (i = 0; i < num_finger_sites; i++) {
		if (!strncmp(prog_config.finger_sites[i], "localhost", 9)) {
		    char *st;

		    if ((st = (char *) malloc(strlen(prog_config.finger_program) + 32)) == NULL) {
			printf("cfingerd could not allocate memory for the userlist program!\n");
			fflush(stdout);
		    } else {
			sprintf(st, "%s | tail +2", prog_config.finger_program);

			safe_exec(st);
			free(st);
		    }
		} else {
		    sprintf(cmdline, "finger userlist-only@%s | tail +3",
			prog_config.finger_sites[i]);

		    safe_exec(cmdline);
		}
	    }
	} else {
	    char st[80];

	    if ((!strstr(prog_config.finger_program, "finger")) &&
		(!strstr(prog_config.finger_program, "w")))
		sprintf(st, "%s | tail +2", prog_config.finger_program);
	    else 
		sprintf(st, "%s", prog_config.finger_program);

	    safe_exec(st);
	}

	if ((num_finger_sites > 1) &&
	    prog_config.config_bits2 & SHOW_MULTFING)
	    printf("\n**> This is the complete listing of %d sites total.\n\n",
		    num_finger_sites);

	if ((num_finger_sites > 1) &&
	    prog_config.config_bits2 & SHOW_MULTFING)
	    SEND_RAW_RETURN;

	fflush(stdout);

	if ((num_finger_sites > 1) &&
	    prog_config.config_bits2 & SHOW_MULTFING) {
		printf("*** This is a list of %dsystems.\n",num_finger_sites);
	    }
    } else {
	can_show = FALSE;

	if (local_finger) {
	    if (prog_config.local_config_bits2 & SHOW_NN_BANNER)
		can_show = TRUE;
	} else {
	    if (prog_config.config_bits2 & SHOW_NN_BANNER)
		can_show = TRUE;
	}

	if (can_show)
	    display_file(noname_display);
    }

    show_bottom();

    log(LOG_USER, "Userlist request", " ");
}
