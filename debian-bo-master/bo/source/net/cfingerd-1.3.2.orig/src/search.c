/*
 * CFINGERD
 * Username search routines
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

typedef struct {
    char username[80];
    char realname[80];
} SEARCHLIST;

/*
 * SHOW_SEARCH
 *
 * This routine searches for the specified uname, or any matches in the
 * specified uname, and lists the matches that were made.
 */
void show_search(char *uname)
{
    char search[7], searchname[80];
    int num_searches = 0, fnd = 0;
    SEARCHLIST searches[500];
    FILE *file;

    show_top();
    sscanf(uname, "%[^.].%[^\r\n]\r\n", search, searchname);

    if (strlen((char *) searchname) == 0) {
	printf("\n                    You must supply a name to search for!\n");
	SEND_RAW_RETURN;
	show_bottom();
	exit(PROGRAM_OKAY);
    }

    if ((searchname[0] = '*') && (strlen((char*) searchname) == 1)) {
	printf("\n                    You must supply a name to search for!\n");
	SEND_RAW_RETURN;
	show_bottom();
	log(LOG_USER, "Security breach: finger * from %s", ident_user);
	syslog(LOG_WARNING, "Security breach: finger * from %s", ident_user);
	exit(PROGRAM_OKAY);
    }

    log(LOG_USER, "Search: ", searchname);

    NOBODY_PRIVS;

    file = fopen("/etc/passwd", "r");
    if (file) {
	while(!feof(file)) {
	    char crap[80], un[80], rn[80], crappp[80];
	    char Un[80], Rn[80];
	    char *cp;

	    fnd = FALSE;

	    bzero(crap, 80);
	    bzero(un, 80);
	    bzero(rn, 80);
	    bzero(crappp, 80);

	    fgets(crappp, 80, file);

	    sscanf(crappp, "%[^:\r\n]:\r\n%[^:\r\n]:\r\n%[^:\r\n]:\r\n%[^:\r\n]:\r\n%[^:\r\n]:\r\n%[^:\r\n]:\r\n%[^\r\n]\r\n",
		un, crap, crap, crap, rn, crap, crap);

	    sprintf (Un, "%s", un);
	    sprintf (Rn, "%s", rn);

	    for (cp=rn; *cp; cp++)
		if (isupper(*cp))
		    *cp = tolower (*cp);

	    for (cp=un; *cp; cp++)
		if (isupper(*cp))
		    *cp = tolower (*cp);

	    if ((cp = index(rn,',')))
		*cp = '\0';

	    if (strstr((char *) rn, (char *) searchname))
		fnd = TRUE;
	    else if (wildmat((char *) rn, (char *) searchname))
		fnd = TRUE;
	    else if (wildmat((char *) un, (char *) searchname))
		fnd = TRUE;

	    if (fnd) {
		bzero(searches[num_searches].realname, 80);
		bzero(searches[num_searches].username, 80);

		strcpy(searches[num_searches].realname, Rn);
		strcpy(searches[num_searches].username, Un);
		num_searches++;
	    }
	}
    } else {
	printf("\n                   Could not open the local password file!\n\n");
	show_bottom();
	exit(PROGRAM_OKAY);
    }

    if (num_searches > 0) {
	int i;

	printf("\n %d match%sfound regarding your search criteria:\n\n",
	    num_searches, (num_searches > 1) ? "es " : " ");
	printf(" Username:      Realname:\n");
	printf(" -------------- --------------------------------------------------------------\n");
	for(i = 0; i < num_searches; i++) {
	    printf(" %-14.14s %-61.61s\n", searches[i].username,
		(searches[i].realname[0] != 0) ?
			searches[i].realname : "No realname field available");
	}

	fflush(stdout);
    } else {
	printf("\n               No matches found regarding your search criteria.\n");
	fflush(stdout);
    }

    SEND_RAW_RETURN;

    show_bottom();
}
