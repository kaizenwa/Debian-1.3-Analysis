/*
 * CFINGERD
 * Utilities
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
#ifdef	BSD
#include "getutent.h"
#endif
#include "privs.h"

#include <regex.h>

/*
 * CLEAR_CFINGERD_VARS
 *
 * This clears out memory for the program configuration variables.
 */
void clear_cfingerd_vars(void)
{
    int i;

    prog_config.top_display_file = NULL;
    prog_config.bottom_display_file = NULL;
    prog_config.plan_file = NULL;
    prog_config.project_file = NULL;
    prog_config.pgpkey_file = NULL;
    prog_config.xface_file = NULL;
    prog_config.userlog_file = NULL;
    prog_config.no_name_banner_file = NULL;
    prog_config.no_user_banner_file = NULL;
    prog_config.no_finger_file = NULL;
    prog_config.identd_banner_file = NULL;
    prog_config.rejected_file = NULL;
    prog_config.syslog_file = NULL;
    prog_config.finger_program = NULL;
    prog_config.whois_program = NULL;

    for (i = 0; i < 80; i++) {
	prog_config.rejected[i] = NULL;
	prog_config.trusted[i] = NULL;
	prog_config.forward[i] = NULL;
	prog_config.p_strings[i] = NULL;
	prog_config.finger_sites[i] = NULL;
    }

    for (i = 0; i < 24; i++)
	prog_config.siglist[i] = NULL;

    for (i = 0; i < 40; i++) {
	prog_config.fusers[i].user = NULL;
	prog_config.fusers[i].script = NULL;
	prog_config.fusers[i].description = NULL;
	prog_config.fusers[i].searchable = FALSE;
    }

    for (i = 0; i < 20; i++)
	prog_config.services.header[i] = NULL;

    prog_config.services.display_string = NULL;
    prog_config.services.name_pos = 0;
    prog_config.services.service_pos = 0;
    prog_config.services.search_pos = 0;

    prog_config.config_bits1 = 0;
    prog_config.config_bits2 = 0;
    prog_config.config_bits3 = 0;
    prog_config.local_config_bits1 = 0;
    prog_config.local_config_bits2 = 0;
    prog_config.local_config_bits3 = 0;
    prog_config.override_bits1 = 0;
    prog_config.override_bits2 = 0;
}

/*
 * START_CFINGERD
 *
 * This simply clears out memory, and changes the program ID name for
 * syslog.
 */
void start_cfingerd(void)
{
    int pid = getpid();
    char *proc;

    proc = (char *) malloc(80);

    sprintf(proc, "cfingerd[%d]", pid);
    openlog(proc, LOG_NDELAY, LOG_DAEMON);

    clear_cfingerd_vars();
}

/*
 * CHECK_STATS
 *
 * This program checks statistics on the program to make sure the settings
 * on the program are correct.
 */
void check_stats()
{
    if (!emulated) {
	if (isatty(0)) {
	    syslog(LOG_ERR, "cfingerd run from command line/daemon");
	    CF_ERROR(E_INETD);
	}
    }
}

/*
 * PARSE_LINE
 *
 * This routine simply parses a string given to the routine, and displays
 * information accordingly.
 */
void parse_line(char *line)
{
    int pos = 0, newpos = 0, done = FALSE, center_flag = FALSE, exec_line = FALSE;
    char command[80], *displine;

    displine = (char *) malloc(160);
    bzero(displine, 160);

    while(pos < strlen(line)) {
	if (line[pos] != '$') {
	    if (!exec_line) {
		if (center_flag) {
		    char allocation[2];

		    bzero(allocation, 2);
		    sprintf(allocation, "%c", line[pos]);
		    displine = strcat(displine, (char *) allocation);
		} else
		    printf("%c", line[pos]);
	    }

	    pos++;
	} else {
	    done = FALSE;
	    newpos = 0;

	    bzero(command, 80);

	    pos++;

	    while((line[pos] != ' ') && (!done)) {
		command[newpos] = line[pos];

		if (line[pos] == ' ')
		    done = TRUE;

		if (pos >= strlen(line))
		    done = TRUE;

		newpos++;
		pos++;

		if (command[0] == '$') {
		    if (center_flag)
			displine = strcat(displine, "$");
		    else
			printf("$");

		    done = TRUE;
		}

		if (strlen(command) >= 4) {
		    if ((!strncasecmp(command, "time", 4)) &&
			(strlen(command) == 4)) {
			time_t tim = time(NULL);
			char bleah[80];

			strftime(bleah, 80, "%I:%M %p (%Z)",
				localtime(&tim));

			if (center_flag)
			    displine = strcat(displine, (char *) bleah);
			else
			    printf((char *) bleah);

			done = TRUE;
		    }

		    if ((!strncasecmp(command, "date", 4)) &&
			(strlen(command) == 4)) {
			time_t tim = time(NULL);
			char bleah[80];

			strftime(bleah, 80, "%a %b %d",
				localtime(&tim));

			if (center_flag)
			    displine = strcat(displine, (char *) bleah);
			else
			    printf((char *) bleah);

			done = TRUE;
		    }

		    if ((!strncasecmp(command, "ident", 5)) &&
			(strlen(command) == 5)) {
			if (center_flag)
			    displine = strcat(displine, ident_user);
			else
			    printf("%s", ident_user);

			done = TRUE;
		    }

		    if ((!strncasecmp(command, "center", 6)) &&
			(strlen(command) == 6)) {
			center_flag = TRUE;
			done = TRUE;
		    }

		    if ((!strncasecmp(command, "compile_datetime", 16)) &&
			(strlen(command) == 16)) {
			if (center_flag)
			    displine = strcat(displine, (char *) COMPILE_DT);
			else
			    printf("%s", (char *) COMPILE_DT);

			done = TRUE;
		    }

		    if ((!strncasecmp(command, "version", 7)) &&
			(strlen(command) == 7)) {
			if (center_flag)
			    displine = strcat(displine, (char *) VERSION);
			else
			    printf("%s", (char *) VERSION);

			done = TRUE;
		    }

		    if ((!strncasecmp(command, "exec", 4)) &&
			(strlen(command) == 4) &&
			(prog_config.config_bits3 & SHOW_EXEC)) {
			FILE *pfile;
			char crap[160];

			bzero(crap, 160);
			sscanf(line, "$exec %[^\r\n]\r\n", crap);

			exec_line = TRUE;

			pfile = popen(crap, "r");
			if (pfile) {
			    char crap2[160];

			    while(!feof(pfile)) {
				bzero(crap2, 160);
				fscanf(pfile, "%[^\r\n]\r\n", crap2);
				printf("%s\n", crap2);
				fflush(stdout);
			    }
			} else {
			    printf("Exec of %s: %s\n", crap, strerror(errno));
			    fflush(stdout);
			}

			done = TRUE;
		    }
		}
	    }
	}
    }

    if (center_flag) {
	int center_dist = 40 - (strlen(displine) / 2) - 1;
	int x;

	if (center_dist > 0)
	    for(x = 0; x < center_dist; x++)
		printf(" ");

	printf(displine);

	free(displine);
    }
}

/*
 * DISPLAY_FILE
 *
 * This displays the file specified.
 */
void display_file(FILE *file_ent)
{
    char line[128];
    int bsize = sizeof(line);

    if (file_ent) {
	while(fgets(line, bsize, file_ent)) {
	    if (prog_config.config_bits3 & SHOW_PARSING)
		parse_line(line);
	    else
		fputs(line, stdout);
	}

	fflush(stdout);
	fclose(file_ent);
    }
}

/* 
 * SHOW_TOP
 *
 * This shows the top (or header) file.
 */
void show_top(void)
{
    BOOL can_show = FALSE;

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_TOP)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_TOP)
	    can_show = TRUE;
    }

    if (can_show)
	display_file(top_display);
}

/*
 * CHECK_EXIST
 *
 * Check whether a username exists.
 */
BOOL check_exist(char *username)
{
    struct passwd *pwent;

    pwent = getpwnam(username);
    return((pwent != NULL) ? TRUE : FALSE);
}

/*
 * SHOW_NOTEXIST
 *
 * Show the nouser banner if the user doesn't exist.
 */
void show_notexist(void)
{
    BOOL can_show = FALSE;

    if (local_finger) {
	if (prog_config.local_config_bits2 & SHOW_NOUSER)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits2 & SHOW_NOUSER)
	    can_show = TRUE;
    }

    if (can_show)
	display_file(nouser_display);
}

/* 
 * SHOW_BOTTOM
 *
 * Show the bottom (or footer) file.
 */
void show_bottom(void)
{
    BOOL can_show = FALSE;

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_BOTTOM)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_BOTTOM)
	    can_show = TRUE;
    }

    if (can_show)
	display_file(bottom_display);
}

/*
 * INETTOS
 *
 * Change INET address string to the "dot" ordered pair.
 */
char *inettos(long addr)
{
    char *ret;
    int pair1, pair2, pair3, pair4;

    ret = (char *) malloc(20);

    pair1 = (addr & 0x000000FF);
    pair2 = (addr & 0x0000FF00) >> 8;
    pair3 = (addr & 0x00FF0000) >> 16;
    pair4 = (addr & 0xFF000000) >> 24;

    sprintf(ret, "%d.%d.%d.%d", pair1, pair2, pair3, pair4);
    return(ret);
}

/*
 * GET_LOCALHOST
 *
 * Get the name of the local host computer.
 */
char *get_localhost(void)
{
    char *ret;
    char hostname[80], domname[80];

    gethostname((char *) hostname, (size_t) 80);
    getdomainname((char *) domname, (size_t) 80);

    ret = (char *) malloc(strlen((char *) hostname) +
			  strlen((char *) domname) + 2);

    sprintf(ret, "%s.%s", (char *) hostname, (char *) domname);
    return(ret);
}

/*
 * CHECK_UNKNOWN
 *
 * This routine checks if the ident username is unknown, and processes the
 * data as it should normally.
 */
void check_unknown(char *host)
{
    if ((!(prog_config.config_bits2 & SHOW_NOBODY1413)) &&
	(wildmat(ident_user, "unknown@*"))) {
	syslog(LOG_NOTICE, "Finger from %s %s",
	    host, prog_config.p_strings[D_REJECT_HOST]);
/*	if (prog_config.config_bits2 & SHOW_REJECTED)
	    display_file(prog_config.rejected_file); */

	log(LOG_IDENT, "Unknown not allowed from ", host);

	exit(PROGRAM_OKAY);
    }
}

/*
 * CHECK_TRUSTED
 *
 * This routine returns a TRUE or FALSE, depending on if the passed host is
 * in the trusted host array.
 *
 * Additional: This module now checks against the ident username.  If it's
 * unknown, and unknown ident fingers are NOT allowed, it rejects the
 * connection.
 */
BOOL check_trusted(char *host)
{
    int i;

    check_unknown(host);

    for (i = 0; i < trusted_host_num; i++)
	if ((wildmat(host, prog_config.trusted[i])) ||
	    (wildmat(ip_address, prog_config.trusted[i]))) {
	    syslog(LOG_NOTICE, "Finger from %s %s",
		host, prog_config.p_strings[D_TRUST_HOST]);
	    return(TRUE);
	}

    return(FALSE);
}

/*
 * CHECK_REJECTED
 *
 * This routine simply checks whether or not the listed host is part of the
 * list of rejected hosts.  If it's matched, it displays the rejected list
 * file and quits completely.
 */
void check_rejected(char *host)
{
    int i;

    check_unknown(host);

    for (i = 0; i < rejected_host_num; i++)
	if ((wildmat(host, prog_config.rejected[i])) ||
	    (wildmat(ip_address, prog_config.rejected[i]))) {
	    syslog(LOG_NOTICE, "Finger from %s %s",
		host, prog_config.p_strings[D_REJECT_HOST]);
/*	    if (prog_config.config_bits2 & SHOW_REJECTED)
		display_file(prog_config.rejected_file); */

	    log(LOG_REJECTED, "Rejected host finger detected to ", host);

	    exit(PROGRAM_OKAY);
	}
}

/*
 * SAFE_EXEC
 *
 * This is a safe version of the "system", "exec" or "popen" command.  This
 * simply takes on the UID/GID of the NOBODY (or apparent NOBODY) UID/GID,
 * and runs the specified program as that user.  If it ever fails, it will
 * display in syslog the reason WHY it failed, along with a semi-detailed
 * message of the failure.
 */
void safe_exec(char *cmd)
{
    FILE *file;
    char line[600];

#ifdef	NOBODY_UID
#ifdef	NOBODY_GID
    NOBODY_PRIVS;

    file = popen(cmd, "r");

    if (file) {
	while(!feof(file)) {
	    bzero(line, 600);
	    fgets(line, 600, file);
	    printf("%s", line);
	    fflush(stdout);
	}
    } else {
	syslog(LOG_ERR, "With the nobody UID/GID set,");
	syslog(LOG_ERR, "safe_exec failed on %s: %s", cmd, strerror(errno));
    }

    RELINQUISH_PRIVS;
#else
    syslog(LOG_ERR, "No GID for Nobody defined.");
#endif
#else
    syslog(LOG_ERR, "No UID for Nobody defined.");
#endif
}

/*
 * MY_FOPEN
 *
 * Quick way to open files and check for errors at the same time.
 */
FILE *my_fopen(char *filename)
{
    FILE *myfile;

    myfile = fopen(filename, "r");
    return myfile;
}

/*
 * OPEN_INITIAL_FILES
 *
 * This safely opens all files that are specified at startup.  This allows
 * the files to be read, after they have been opened with root access.
 */
void open_initial_files(void)
{
    top_display = my_fopen(prog_config.top_display_file);
    bottom_display = my_fopen(prog_config.bottom_display_file);
    noname_display = my_fopen(prog_config.no_name_banner_file);
    nouser_display = my_fopen(prog_config.no_user_banner_file);
}
