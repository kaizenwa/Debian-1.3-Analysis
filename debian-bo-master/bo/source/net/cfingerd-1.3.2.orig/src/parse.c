/*
 * CFINGERD
 * Parsing routines
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
 * SEARCH_ILLEGAL
 *
 * This searches for any illegal characters in a user's name.  If there are
 * any found, it returns a U_ILLEGAL, otherwise it returns a 0.
 */
int search_illegal(char *str)
{
    int i;

    if (str != NULL)
	for (i = 0; i < strlen(str); i++)
	    if (str[i] == '@')
		return U_FORWARD;
	    else if (!isalpha(str[i]) && !isdigit(str[i]) && (str[i] != '.') &&
		(str[i] != '_') && (str[i] != '-') && (str[i] != ',') &&
		(str[i] != '?') && (str[i] != '*') && (str[i] != '/'))
		return U_ILLEGAL;

    return 0;
}

/*
 * INTERNAL_CHECK
 *
 * This checks the username up against an internal username template,
 * for use with cfingerd testing, debugging, and/or other information
 * retrieval.  Please don't disable this routine - this should be
 * included with all of the cfingerd distributions.
 *
 * Returns U_INTERNAL if the username matches an internal username,
 * otherwise, it returns a 0.
 */
int internal_check(char *username)
{
    int length = strlen(username);

    if (!strncmp(username, "version", 7) && length == 7)
	return(U_INTERNAL);
    else if (!strncmp(username, "services", 8) && length == 8)
	return(U_INTERNAL);
    else if (!strncmp(username, "search", 6))
	return(U_INTERNAL);
    else if (!strncmp(username, "userlist-only", 13) && length == 13)
	return(U_INTERNAL);
    else if (!strncmp(username, "help", 4) && length == 4)
	return(U_INTERNAL);

    return(0);
}

/*
 * SEARCH_FAKE
 *
 * Search for a fake user.  Returns U_FAKEUSER if the user is fake, and
 * 0 if it is not.
 */
int search_fake(char *username)
{
    char parsed[80];

    bzero(parsed, 80);
    sscanf(username, "%[^.].%*[^\r\n]\r\n", parsed);

    if (search_fake_pos(parsed) >= 0)
	return(U_FAKEUSER);

    return(0);
}

/*
 * PROCESS_USERNAME
 *
 * This is the grand-daddy of them all.  This routine does all of the
 * sanity checks against the username to make sure it's a legal username.
 * If the username doesn't exist, it returns the proper error message
 * back to the user, and skips the finger-query request altogether.  Other-
 * wise, it returns:
 *
 *	U_ILLEGAL:	Username was illegal in checking
 *	U_FORWARD:	Username is a forward request (contains `@')
 *	U_STANDARD:	Username is a standard (non-fake) username
 *	U_FAKEUSER:	Username matches a fake user
 *	U_INTERNAL:	Username matches an internal (defined) username
 *	U_USERLIST:	Username is not present - it's a userlisting
 */
int process_username(char *username)
{
    int ret = 0;

    if ((username[0] == 13) || (username[0] == 10) ||
	(username[1] == 13) || (username[1] == 10))
	ret = U_USERLIST;

    if (ret != U_USERLIST) {
	ret = search_illegal(username);

	if (ret == U_FORWARD) {
	    printf("%s\n", prog_config.p_strings[D_FORWARD_DENY]);
	    fflush(stdout);
	    log(LOG_USER, "Denied forward: ", username);
	    exit(1);
	}

	if (ret == U_ILLEGAL) {
	    printf("Illegal character in username.\n");
	    fflush(stdout);
	    log(LOG_USER, "Illegal: ", username);
	    exit(1);
	}

	ret = internal_check(username);

	if (ret != U_INTERNAL)
	    ret = search_fake(username);

	if (ret == 0)
	    ret = U_STANDARD;
    }

    return(ret);
}
