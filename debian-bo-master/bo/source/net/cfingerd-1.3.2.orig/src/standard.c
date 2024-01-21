/*
 * CFINGERD
 * Standard finger handler
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
#ifdef	BSD
#include "getutent.h"
#endif
#include <lastlog.h>

#include "privs.h"

int columns;
int times_on;

typedef struct {
    char tty[20];
    char locale[16];
    long ip_addr;
    time_t time;
    BOOL writable;
} TTY_FROM;

TTY_FROM tty_list[20];
static int uid, nuid, ngid; 

/*
 * DO_COLUMNS
 *
 * This simply sends a carriage return if there was more than one column
 * already displayed on the screen.
 */
void do_columns(void)
{
    if (columns > 1) {
	printf("\n");
	columns = 0;
    }
}

/*
 * CHECK_ILLEGAL
 *
 * Check to make sure that the specified file given for the user 
 * isn't an illegal file.
 */
BOOL check_illegal(char *str, char *user)
{
    struct stat statbuf;

    if ((lstat(str, (struct stat *) &statbuf)) < 0)
	return(FALSE);

    if (S_ISREG(statbuf.st_mode))
	return(FALSE);
    else
	syslog(LOG_NOTICE, "%s's file %s is NOT A REGULAR FILE.",
		user, str);

    return(TRUE);
}

/*
 * SHOW_STUFF
 *
 * This shows data that is parsed correctly to the number of characters
 * specified in t2.  T1 is the actual bit that is checked for.
 */
void show_stuff(char *user, int t1, int t2)
{
    BOOL can_show = FALSE;
    char formatter[80];

    if (local_finger) {
	if (prog_config.local_config_bits1 & t1)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits1 & t1)
	    can_show = TRUE;
    }

    if (can_show) {
	columns++;
	sprintf(formatter, "%%s%%-%d.%ds", (36 - strlen(prog_config.p_strings[t2])),
		36);
	printf(formatter, prog_config.p_strings[t2], user);
	do_columns();
    }
}

/*
 * EXIST
 *
 * Check whether the specified filename exists or not.
 */
BOOL exist(char *filename)
{
    FILE *f;

    f = fopen(filename, "r");
    if (f) {
	fclose(f);
	return(TRUE);
    } else
	return(FALSE);
}

/*
 * SHOW_PFILE
 *
 * This simply shows specified data for each option that users would
 * normally expect.  (What else was I supposed to say?  :)
 */
void show_pfile(int t1, int t2, int t3, char *dir, char *disp, BOOL send_ret)
{
    BOOL can_show = FALSE;

    if (local_finger) {
	if (prog_config.local_config_bits2 & t1)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits2 & t1)
	    can_show = TRUE;
    }

    if (can_show) {
	char d[128];

	bzero(d, 128);
	sprintf(d, "%s/%s", dir, disp);

	if (!(prog_config.config_bits3 & SHOW_HEADERS_FE)) {
	    printf("%s\n", prog_config.p_strings[t2]);
	    fflush(stdout);
	}

	if (exist(d)) {
	    NOBODY_PRIVS;

	    if (prog_config.config_bits3 & SHOW_HEADERS_FE) {
		printf("%s\n", prog_config.p_strings[t2]);
		fflush(stdout);
	    }

	    if (!check_illegal(d, "CFINGERD")) {
		FILE *file;

		file = fopen(d, "r");
		display_file(file);
		fclose(file);
	    }
	} else {
	    if (!(prog_config.config_bits3 & SHOW_HEADERS_FE))
		printf("%s\n", prog_config.p_strings[t3]);
	    else
		send_ret = 0;
	}
    }

    if (send_ret)
	SEND_RAW_RETURN;
}

/*
 * SHOW_PFILE2
 *
 * This simply shows specified data for each option that users would
 * normally expect.  (What else was I supposed to say?  :)
 */
void show_pfile2(int t1, int t2, int t3, char *dir, char *disp, BOOL send_ret)
{
    BOOL can_show = FALSE;

    if (local_finger) {
	if (prog_config.local_config_bits3 & t1)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits3 & t1)
	    can_show = TRUE;
    }

    if (can_show) {
	char d[128];

	sprintf(d, "%s/%s", dir, disp);

	if (!(prog_config.config_bits3 & SHOW_HEADERS_FE)) {
	    printf("%s\n", prog_config.p_strings[t2]);
	    fflush(stdout);
	}

	if (exist(d)) {
	    NOBODY_PRIVS;

	    if (prog_config.config_bits3 & SHOW_HEADERS_FE) {
		printf("%s\n", prog_config.p_strings[t2]);
		fflush(stdout);
	    }

	    if (!check_illegal(d, "CFINGERD")) {
		FILE *file;

		file = fopen(d, "r");
		display_file(file);
		fclose(file);
	    }
	} else {
	    if (!(prog_config.config_bits3 & SHOW_HEADERS_FE))
		printf("%s\n", prog_config.p_strings[t3]);
	}
    }

    if (send_ret)
	SEND_RAW_RETURN;
}

/*
 * SHOW_LASTTIME_ON
 *
 * This routine simply shows the last time "user" was on.
 */
void show_lasttime_on(char *user)
{
    int fd;
    BOOL found = FALSE, can_show_orig = FALSE;
    char formatted[80];

#ifndef	USE_LASTLOG
    struct utmp ut;
#else
    struct lastlog lastlog;
#endif

    TTY_FROM last_tty;

    NOBODY_PRIVS;
    fd = open(WTMPFILE, O_RDONLY);

#ifndef	USE_LASTLOG
    if (fd != -1) {
	while (!found) {
	    if (read(fd, &ut, sizeof(struct utmp)) != sizeof (struct utmp))
		found = TRUE;

#ifdef	BSD
	    if (!strncmp(user, (char *) ut.ut_name, strlen(user))) {
		last_tty.ip_addr = 0;
#else
	    if (!strncmp(user, (char *) ut.ut_user, strlen(user))) {
		strcpy(last_tty.tty, ut.ut_line);
#endif
		ut.ut_host[16] = 0;
		strcpy(last_tty.locale, ut.ut_host);
		last_tty.ip_addr = ut.ut_addr;
		last_tty.time = ut.ut_time;
	    }
	}
    }
#else
    if (fd != -1) {
	/* Since in lastlog, you can seek to the user's UID and get info,
	   we do that here. */
	if (lseek(fd, (sizeof(lastlog) * uid), SEEK_SET) == -1) {
	    printf("Seek: %s\n", strerror(errno));
	    fflush(stdout);
	}

	if (read(fd, &lastlog, sizeof(lastlog)) == sizeof(lastlog)) {
	    found = TRUE;
	    strcpy(last_tty.tty, lastlog.ll_line);
	    lastlog.ll_host[UT_HOSTSIZE - 1] = 0;
	    strcpy(last_tty.locale, lastlog.ll_host);
	    last_tty.time = lastlog.ll_time;
	}
    }
#endif

    close(fd);

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_FROM)
	    can_show_orig = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_FROM)
	    can_show_orig = TRUE;
    }

    if (last_tty.time > 0L) {
	strftime(formatted, 80, "%a %b %d %I:%M %p (%Z)",
	    localtime(&last_tty.time));
	printf("Last seen %s", (char *) formatted);
    } else {
	printf("This user has never logged in.");
    }

    if ((can_show_orig) && (last_tty.time > 0L))
	printf("%s %s",
	    (strlen((char *) last_tty.locale) > 1) ? " from" : " ",
	    (strlen((char *) last_tty.locale) > 1) ?
		    (char *) last_tty.locale : " ");

    if (!(prog_config.config_bits2 & SHOW_STRICTFMT))
	printf("\n");

    fflush(stdout);
}

/*
 * SHOW_NOT_ONLINE
 *
 * This shows whether or not the user is online.
 */
void show_not_online(char *user)
{
    BOOL can_show = FALSE, can_show_lasttime = FALSE;

    NOBODY_PRIVS;

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_IFON)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_IFON)
	    can_show = TRUE;
    }

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_LTON)
	    can_show_lasttime = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_LTON)
	    can_show_lasttime = TRUE;
    }

    if (can_show) {
	if (can_show_lasttime)
	    show_lasttime_on(user);
	else {
	    printf("This user is not currently logged in.\n");
	    fflush(stdout);
	}
    }
}

/*
 * SHOW_IDLE
 *
 * This routine shows how long the user has been idle on the "tty"
 */
void show_idle(char *tty)
{
    struct stat buf;
    time_t cur_time;
    long diff_time;
    int sec, min, hour, day, has;
    char dev_file[80];

    sprintf(dev_file, "/dev/%s", tty);

    USER_PRIVS(nuid, ngid);
    stat((char *) dev_file, &buf);
    NOBODY_PRIVS;

    cur_time = time(NULL);
    diff_time = (long) cur_time - (long) buf.st_mtime;

    sec = min = hour = day = has = 0;

    if (diff_time > 86400)
	day = hour = has = 1;
    else if (diff_time > 3600)
	hour = min = has = 1;
    else if (diff_time > 59)
	min = sec = has = 1;
    else if (diff_time > 0)
	sec = has = 1;

    if (has)
	printf(" Idle ");

    if (day) {
	day = diff_time / 86400;
	diff_time -= day * 86400;
	printf("%d %s ", day, (day > 1) ? "days" : "day");
    }

    if (hour) {
	hour = diff_time / 3600;
	diff_time -= hour * 3600;
	if (prog_config.config_bits2 & SHOW_TIMESTAMP)
	    printf("%d %s ", hour, (hour > 1) ? "hours" : "hour");
	else
	    printf("%02d:", hour);
    } else
	if (!(prog_config.config_bits2 & SHOW_TIMESTAMP) && has)
	    printf("00:");

    if (min) {
	min = diff_time / 60;
	diff_time -= min * 60;
	if (prog_config.config_bits2 & SHOW_TIMESTAMP)
	    printf("%d %s ", min, (min > 1) ? "minutes" : "minute");
	else
	    printf("%02d:", min);
    } else
	if (!(prog_config.config_bits2 & SHOW_TIMESTAMP) && has)
	    printf("00:");

    if (sec) {
	sec = diff_time;
	if (prog_config.config_bits2 & SHOW_TIMESTAMP)
	    printf("%d %s", sec, (sec > 1) ? "seconds" : "second");
	else
	    printf("%02d", sec);
    } else
	if (!(prog_config.config_bits2 & SHOW_TIMESTAMP) && has)
	    printf("00");
}

/*
 * SHOW_TIMES_ON
 *
 * This shows the number of times the user has logged in, and how long the
 * user has been on on each TTY.  Don't ask why it was a void routine.
 */
void show_times_on(void)
{
    int i;
    BOOL can_show = FALSE, can_show_orig = FALSE;

    NOBODY_PRIVS;

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_IFON)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_IFON)
	    can_show = TRUE;
    }

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_FROM)
	    can_show_orig = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_FROM)
	    can_show_orig = TRUE;
    }

    if (can_show) {
	if (!(prog_config.config_bits2 & SHOW_STRICTFMT))
	    SEND_RAW_RETURN;

	for (i = 0; i < times_on; i++) {
	    char formatted[80];

	    strftime(formatted, 80, "%a %b %d %I:%M%p (%Z)",
		    localtime(&tty_list[i].time));
	    printf("On since %s on %s",
		    formatted, (char *) tty_list[i].tty);

	    show_idle((char *) tty_list[i].tty);

	    if (can_show_orig)
		printf("%s %s",
		    (strlen((char *) tty_list[i].locale) > 1) ? " from" : "",
		    (strlen((char *) tty_list[i].locale) > 1) ?
			    (char *) tty_list[i].locale : "");

	    if (!(tty_list[i].writable))
		printf("[MSG-N]");

	    SEND_RAW_RETURN_NO_FLUSH;
	}
    }

    fflush(stdout);
}

/*
 * SHOW_LOGIN_STATUS
 *
 * This shows the login status of the specified user, showing how many times
 * they are logged in, how long they've been idle, and other necessities.
 */
void show_login_stats(char *user)
{
    struct utmp *ut;

    times_on = 0;

    while((ut = getutent()) != NULL) {
#ifdef	BSD
	if (!strncmp(user, (char *) ut->ut_name, strlen(user)) &&
		(strlen((char *) ut->ut_name) == strlen(user))) {
#else
	if (ut->ut_type == USER_PROCESS)
	    if (!strncmp(user, (char *) ut->ut_user, strlen(user)) &&
		(strlen((char *) ut->ut_user) == strlen(user))) {
#endif
		struct stat buf;
		char devfile[80];

#ifdef	BSD
		ut->ut_line[UT_LINESIZE-1] = 0;
#endif

		strcpy(tty_list[times_on].tty, (char *) ut->ut_line);

		sprintf(devfile, "/dev/%s", (char *) ut->ut_line);

		USER_PRIVS(nuid, ngid);
		stat(devfile, &buf);
		NOBODY_PRIVS;

		if ((buf.st_mode & S_IWGRP) && (buf.st_mode & S_IWOTH))
		    tty_list[times_on].writable = TRUE;
		else
		    tty_list[times_on].writable = FALSE;

#ifdef	BSD
		ut->ut_host[UT_HOSTSIZE-1] = 0;
		tty_list[times_on].ip_addr = 0;
#else
		ut->ut_host[16] = 0;
		tty_list[times_on].ip_addr = ut->ut_addr;
#endif

		strcpy(tty_list[times_on].locale, (char *) ut->ut_host);
		tty_list[times_on].time = ut->ut_time;
		times_on++;
	    }
    }

    if (times_on > 0) {
	show_times_on();

	if (!(prog_config.config_bits2 & SHOW_STRICTFMT))
	    SEND_RAW_RETURN;
    } else {
	show_not_online(user);
	if (!(prog_config.config_bits2 & SHOW_STRICTFMT))
	    SEND_RAW_RETURN;
    }

    fflush(stdout);
}

/*
 * SHOW_MAIL_STATS
 *
 * This shows whether the specified user has mail, and how long ago the
 * user read mail.
 */
void show_mail_stats(char *user)
{
    struct stat buf;
    char mailfile[80];
    BOOL can_show = FALSE, can_show_date = FALSE;

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_LRMAIL)
	    can_show = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_LRMAIL)
	    can_show = TRUE;
    }

    if (local_finger) {
	if (prog_config.local_config_bits1 & SHOW_MRDATE)
	    can_show_date = TRUE;
    } else {
	if (prog_config.config_bits1 & SHOW_MRDATE)
	    can_show_date = TRUE;
    }

    if (can_show) {
	char formatted[80];
	int fm;

	sprintf(mailfile, "%s/%s", MAIL_SPOOL_DIR, user);

	USER_PRIVS(nuid, ngid);
	fm = stat(mailfile, &buf);
	NOBODY_PRIVS;

	if (fm != -1) {
	    if (can_show_date)
		strftime(formatted, 80, "%a %b %d %I:%M%p (%Z)",
		    localtime(&buf.st_atime));
	    else
		strftime(formatted, 80, "%I:%M %p (%Z)",
		    localtime(&buf.st_atime));

	    if ((int) buf.st_size == 0)
		printf("This user has no mail.\n");
	    else {
		printf("This user last read mail %s\n", (char *) formatted);
		if (buf.st_mtime > buf.st_atime) {
		    char formatted2[80];

		    strftime(formatted2, 80, "%a %b %d %I:%M%p (%Z)",
			localtime(&buf.st_mtime));
		    printf("This user has unread mail since %s\n",
			(char *) formatted2);
		}
	    }
	} else
	    printf("This user has no mail or mail spool.\n");
    }
}

/*
 * SHOW_FINGERINFO
 *
 * This shows correct finger information for the specified "user"
 */
void show_fingerinfo(char *user)
{
    struct passwd *pwent;
    FILE *file;
    char username[80], room[80], work_phone[80], home_phone[80],
	 other[80], fn[80];

    columns = 0;
    pwent = getpwnam(user);

    uid = pwent->pw_uid;

    /* For changing to the correct UID/GID */
    nuid = pwent->pw_uid;
    ngid = pwent->pw_gid;

    username[0] = 0;
    room[0] = 0;
    work_phone[0] = 0;
    home_phone[0] = 0;
    other[0] = 0;

    sscanf(pwent->pw_gecos, "%[^,],%[^,],%[^,],%[^,],%[^\r\n]\r\n",
	username, room, work_phone, home_phone, other);

    sprintf(fn, "%s/.nofinger", pwent->pw_dir);

    if (exist(fn)) {
	if (!check_illegal((char *) fn, user)) {
	    show_bottom();
	    exit(PROGRAM_SYSLOG);
	}
    }

    USER_PRIVS(nuid, ngid);
    if ((file = fopen(fn, "r")) != NULL) {
	NOBODY_PRIVS;
	display_file(file);
	fclose(file);
    } else {
	NOBODY_PRIVS;

	show_stuff(user, SHOW_UNAME, D_USERNAME);
	show_stuff(username, SHOW_REALNAME, D_REALNAME);
	show_stuff(pwent->pw_dir, SHOW_DIR, D_DIRECTORY);
	show_stuff(pwent->pw_shell, SHOW_SHELL, D_SHELL);
	show_stuff((char *) room, SHOW_ROOM, D_ROOM);
	show_stuff((char *) work_phone, SHOW_WPHONE, D_WORK_PHONE);
	show_stuff((char *) home_phone, SHOW_HPHONE, D_HOME_PHONE);
	show_stuff((char *) other, SHOW_OTHER, D_OTHER);

	if ((prog_config.config_bits3 & SHOW_USERLOG) &&
	    (prog_config.userlog_file != NULL))
	    userlog(pwent->pw_uid, pwent->pw_gid, pwent->pw_dir, ident_user);

	/* For sanity sake, and to make the display look prettier... */
	columns++;
	do_columns();

	if (!(prog_config.config_bits2 & SHOW_STRICTFMT))
	    SEND_RAW_RETURN;

	show_mail_stats(user);
	show_login_stats(user);

	if (prog_config.config_bits2 & SHOW_STRICTFMT)
	    SEND_RAW_RETURN;

	/* I made a typo here ... thanks to stig@hackvan.com, it was
	   corrected. */

	show_pfile(SHOW_PROJECT, D_PROJECT, D_NO_PROJECT, pwent->pw_dir, 
		prog_config.project_file, 
	(prog_config.config_bits2 & SHOW_STRICTFMT) ? FALSE : TRUE);

	show_pfile(SHOW_PLAN, D_PLAN, D_NO_PLAN, pwent->pw_dir,
		prog_config.plan_file,
	(prog_config.config_bits2 & SHOW_STRICTFMT) ? FALSE : TRUE);

	/* Thanks to Andy Smith */
	show_pfile(SHOW_PGPKEY, D_PGPKEY, D_NO_PGPKEY, pwent->pw_dir, 
		prog_config.pgpkey_file,
	(prog_config.config_bits2 & SHOW_STRICTFMT) ? FALSE : TRUE);

	show_pfile2(SHOW_XFACE, D_XFACE, D_NO_XFACE, pwent->pw_dir,
		prog_config.xface_file, FALSE);
    }
}

/*
 * HANDLE_STANDARD
 *
 * This handles the standard fingering of a user, and checks whether or not
 * the user actually exists.
 */
void handle_standard(char *username)
{
    char uname[600];
    int nums = 0;

    /*
    ** Support to change the "/W" or "-L" listings from Microsoft's
    ** bloated finger program.  This combats their options, and ignores
    ** them.
    **
    ** Ken Hollis 08/06/96
    */
    if (username[0] == '/')
	strcpy(username, username + 2);
    else if (username[0] == '-')
	strcpy(username, username + 2);

    if ((!(prog_config.config_bits2 & SHOW_FINGERFWD)) || 
	check_exist(username))
	show_top();

    if (!check_exist(username)) {
	if (prog_config.config_bits2 & SHOW_FINGERFWD) {
	    while(prog_config.forward[nums] != NULL) {
		bzero(uname, 600);
		printf("\nChecking site %s for the specified user.\n", prog_config.forward[nums]);
		fflush(stdout);
		sprintf(uname, "finger %s\100%s | tail +2", username, prog_config.forward[nums]);
		safe_exec(uname);
		nums++;
		log(LOG_USER, "Forwarded: ", username);
	    }
	} else {
	    show_notexist();
	}
    } else {
	show_fingerinfo(username);
	log(LOG_USER, "Normal: ", username);
    }

    if ((!(prog_config.config_bits2 & SHOW_FINGERFWD)) || 
	check_exist(username)) {
	show_bottom();
    }
}
