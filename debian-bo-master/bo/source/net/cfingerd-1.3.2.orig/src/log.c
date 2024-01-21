/*
 * CFINGERD
 * Logging module
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

void log(int logtype, char *msg, char *user)
{
    if (can_log && (prog_config.config_bits3 & SHOW_LOG)) {
	time_t tim = time(NULL);

	fprintf(logfile, "Log from %s at %s", ident_user, ctime(&tim));

	switch(logtype) {
	    case LOG_IDENT:	fprintf(logfile, "\tIDENT: %s%s\n\n", msg, user);
				break;
	    case LOG_REJECTED:	fprintf(logfile, "\tREJECTED: %s%s\n\n", msg, user);
				break;
	    case LOG_WARN:	fprintf(logfile, "\tWARNING: %s\n\n", msg);
				break;
	    case LOG_USER:	fprintf(logfile, "\tUSER: %s%s\n\n", msg, user);
				break;
	    case LOG_ERROR:	fprintf(logfile, "\tERROR: %s%s\n\n", msg, user);
				break;
	}

	fflush(logfile);
    }
}

void userlog(uid_t uid, gid_t gid, char *dir, char *user)
{
    FILE *file = NULL;
    char filename[80];

    bzero(filename, 80);
    sprintf(filename, "%s/%s", dir, prog_config.userlog_file);

    printf("Userlog\n");
    fflush(stdout);

    USER_PRIVS(uid,gid)

    /*
     * If the file doesn't already exist, check to make sure we can
     * create it (if the sysadmin allows it).  If the file was created,
     * then they want a finger log, and thus, should show the log here.
     */
    if (!exist(filename)) {
	if (!(prog_config.config_bits3 & SHOW_CREATE_FLG))
	    file = fopen(filename, "a+");
    } else
	file = fopen(filename, "a+");

    NOBODY_PRIVS

    if (file) {
	time_t tim = time(NULL);

	fprintf(file, "Finger from %s at %s", user, ctime(&tim));
	fflush(file);
	fclose(file);
    } else {
	if (!(prog_config.config_bits3 & SHOW_CREATE_FLG)) {
	    syslog(LOG_WARNING, "Userlog: %s (%s)", filename, strerror(errno));
	    log(LOG_ERROR, "Cannot write to userlog: ", strerror(errno));
	}
    }
}
