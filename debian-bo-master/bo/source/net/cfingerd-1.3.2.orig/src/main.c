/*
 * CFINGERD
 * Main Routine
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

#define MAIN
#include "privs.h"

CONFIG prog_config;
char *remote_addr, *localhost, *ident_user, *ip_address;
FILE *top_display, *bottom_display, *noname_display, *nouser_display,
     *identd_display;
BOOL local_finger, emulated; 
int local_port, remote_port;

/*
 * HAS_MATCH
 *
 * Check to see if there's an occurance of "match" within "string".  You're
 * probably saying that I could have used strncmp, or something else
 * similar.  Not true - because I'd need about 3 calls in here, and this
 * routine does everything I need in *1* routine.  :)
 */
BOOL has_match(char *string, char *match)
{
    int i, x;

    for (i = 0; i < strlen(string); i++)
	for (x = 0; x < strlen(match); x++)
	    if (string[i] == match[x])
		return TRUE;

    return FALSE;
}

/*
 * MAIN
 *
 * The main dispatcher for cfingerd.
 */
int main(int argc, char *argv[])
{
    char username[100], syslog_str[200];
    int un_type;
    char *cp;

    /* Initialize CFINGERD */
    start_handler();
    start_cfingerd();

    /* Clear any extraneous bits that should be cleared */
    can_log = FALSE;

    bzero(username, 100);
    bzero(syslog_str, 200);

    /* Check for any command-line options */
    if (argc > 1) {
	(void) check_options(argc, argv);
	strcpy(username, argv[2]);
    }

    /* Read the configuration, and check status of the program so far */
    read_configuration();
    check_blank_configurations();
    check_stats();
    open_initial_files();

    /*
     * Okay, now that all of the standard processing has taken place, we
     * can switch to the NOBODY UID/GID and run everything else as nobody
     * (to a certain degree.)  Remembering, of course, if we must switch
     * to a user that's being fingered, we MUST call "RELINQUISH_PRIVS"
     * to relinquish the root privileges, then call BECOME_USER(UID,GID)
     * to obtain privileges as that user.  (Read the FAQ for more info.)
     */

    NOBODY_PRIVS;

    /* Make sure there is actually data waiting in the finger port */
    if (!emulated) {
	if (!fgets(username, sizeof(username), stdin)) {
	    syslog(LOG_ERR, "%s: %s", remote_addr, strerror(errno));
	    log(LOG_ERROR, remote_addr, strerror(errno));
	    closelog();
	    exit(PROGRAM_SYSLOG);
	}
    }

    /* If there's a username, trim the CRLF pair */
    if (!emulated)
	sscanf(username, "%[^\r\n]\r\n", username);

    /* If we're not doing emulated stuff, we can assume that we are running
       either as a daemon, or under INETD.  In that case... */
    if (!emulated) {
	struct sockaddr_in socket_addr;
	struct hostent *host_ent;
	int psize = 0;

	/* Can't run from command line (but this should already be checked) */
	psize = sizeof(socket_addr);

	if (getsockname(0, (struct sockaddr *) &socket_addr, &psize)) {
	    syslog(LOG_WARNING, "getsockname: %s", strerror(errno));
	    local_port = 0;
	} else
	    local_port = ntohs(socket_addr.sin_port);

	if (getpeername(0, (struct sockaddr *) &socket_addr, &psize)) {
	    printf("Internal error - not running as either a daemon or under INETD.\n");
	    printf("Fatal - getpeername: %s\n", strerror(errno));
	    closelog();
	    log(LOG_ERROR, "getpeername: ", strerror(errno));
	    exit(PROGRAM_BUG);
	} else
	    remote_port = ntohs(socket_addr.sin_port);

	ip_address = inet_ntoa(socket_addr.sin_addr);

	/* Get our host entry */
	host_ent = (struct hostent *) gethostbyaddr((char *) &socket_addr.sin_addr,
		   sizeof(socket_addr.sin_addr), AF_INET);

	/* And get our local-host name */
#ifndef	ACTUAL_HOSTNAME
	localhost = get_localhost();
#else
	localhost = ACTUAL_HOSTNAME;
#endif

	/* Make sure we can get the remote host's address name */
	if (host_ent == NULL) {
	    remote_addr = inettos(socket_addr.sin_addr.s_addr);
	    syslog(LOG_WARNING, "%s %s", 
		prog_config.p_strings[D_IP_NO_MATCH],
		remote_addr);
	    if (!(prog_config.config_bits2 & SHOW_IP_MATCH))
		CF_ERROR(E_NOIP);
	} else
	    remote_addr = (char *) host_ent->h_name;

	/* Convert any uppercase letters in the hostname to lowercase */
	for (cp = remote_addr; *cp; cp++)
	    if (isupper(*cp))
		*cp = tolower(*cp);

	/* And find out if this is a local finger */
	if (!strncasecmp(remote_addr, "127.0.0.1", 9) ||
	    !strncasecmp(remote_addr, "localhost", 9) ||
	    !strncasecmp(remote_addr, "127.0.0.0", 9) || /* KTH 07/26/96 */
	    !strncasecmp(remote_addr, localhost, strlen(localhost)))
	    local_finger = TRUE;
	else
	    local_finger = FALSE;

	ident_user = get_rfc1413_data();
    } else
	local_finger = TRUE;

    /* We're emulated, so assume we're fingering from loopback */
    if (emulated) {
	local_finger = TRUE;
#ifndef	ACTUAL_LOOPBACK
	remote_addr = "127.0.0.1";
#else
	remote_addr = ACTUAL_LOOPBACK;
#endif
    }

    /* Now, let's check to make sure this site is trusted */
    if ((!local_finger) && !emulated)
	local_finger = check_trusted(remote_addr);

    if ((!local_finger) && !emulated)
	check_rejected(remote_addr);

    /* Check the finger information coming in and return its type */
    un_type = process_username(username);

    /* And check the process the information here. */
    switch(un_type) {
	case U_INTERNAL:
	    if (!emulated) {
		sprintf(syslog_str, "%s fingered (internal) from %s", username,
		    ident_user);
		syslog(LOG_NOTICE, (char *) syslog_str);
	    }

	    handle_internal(username);
	    break;
	case U_STANDARD:
	    if (!emulated) {
		if (strncmp(username, "root", 4))
		    sprintf(syslog_str, "%s fingered from %s", username, ident_user);
		else
		    sprintf(syslog_str, "%s fingered from %s",
			prog_config.p_strings[D_ROOT_FINGER], ident_user);

		syslog(LOG_NOTICE, (char *) syslog_str);
	    }

	    handle_standard(username);
	    break;
	case U_FAKEUSER:
	    if (!emulated) {
		sprintf(syslog_str, "%s %s from %s", username, 
		    prog_config.p_strings[D_FAKE_USER], ident_user);

		syslog(LOG_NOTICE, (char *) syslog_str);
	    }

	    handle_fakeuser(username);
	    break;
	case U_USERLIST:
	    if (!emulated)
		syslog(LOG_NOTICE, "%s from %s", prog_config.p_strings[D_ULIST_FINGER],
		    ident_user);

	    handle_userlist();
	    break;
    }

    return(0);
}
