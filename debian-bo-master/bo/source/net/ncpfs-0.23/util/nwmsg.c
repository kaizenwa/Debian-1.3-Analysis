/*
 *  nwmsg.c
 *
 *  Fetch NetWare broadcast messages and write to the user
 *
 *  Copyright (C) 1996 by Volker Lendecke
 *
 */

#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <unistd.h>
#include <syslog.h>
#include <signal.h>
#include <paths.h>
#include <utmp.h>
#include <mntent.h>
#include "ncplib.h"

static int search_utmp(char *user, char *tty);

static char *progname;

void
main(int argc, char *argv[])
{
	struct ncp_conn *conn;
	char message[256];
	char *mount_point;
	struct ncp_fs_info info;
	struct passwd *pwd;
	char tty[256];
	char tty_path[256];
	FILE *tty_file;
	FILE *mtab;
	struct mntent *mnt;
	long err;
	

	progname = argv[0];

	openlog("nwmsg", LOG_PID, LOG_LPR);

	if (argc != 2)
	{
		fprintf(stderr, "usage: %s mount-point\n",
			progname);
		exit(1);
	}

	mount_point = argv[1];
	if ((conn = ncp_open_mount(mount_point, &err)) == NULL)
	{
		com_err(progname, err, "in ncp_open_mount");
		exit(1);
	}

	if (ncp_get_broadcast_message(conn, message) != 0)
	{
		fprintf(stderr, "%s: could not get broadcast message\n",
			progname);
		ncp_close(conn);
		exit(1);
	}

	if (strlen(message) == 0)
	{
		syslog(LOG_DEBUG, "no message");
		exit(0);
	}

#if 0
	syslog(LOG_DEBUG, "message: %s", message);
#endif

	info.version = NCP_GET_FS_INFO_VERSION;
	if (ioctl(conn->mount_fid, NCP_IOC_GET_FS_INFO, &info) < 0)
	{
		fprintf(stderr, "%s: could not ioctl on connection: %s\n",
			progname, strerror(errno));
		ncp_close(conn);
		exit(1);
	}

	ncp_close(conn);

	if ((pwd = getpwuid(info.mounted_uid)) == NULL)
	{
		fprintf(stderr, "%s: user %d not known\n",
			progname, info.mounted_uid);
		exit(1);
	}

	if ((mtab = fopen(MOUNTED, "r")) == NULL)
	{
		fprintf(stderr, "%s: can't open %s\n",
			progname, MOUNTED);
		exit(1);
	}

	while ((mnt = getmntent(mtab)) != NULL)
	{
		if (strcmp(mnt->mnt_dir, mount_point) == 0)
		{
			break;
		}
	}

	if (mnt == NULL)
	{
		syslog(LOG_DEBUG, "cannot find mtab entry\n");
	}

	if (search_utmp(pwd->pw_name, tty) != 0)
	{
		exit(1);
	}

	sprintf(tty_path, "/dev/%s", tty);
	if ((tty_file = fopen(tty_path, "w")) == NULL)
	{
		fprintf(stderr, "%s: cannot open %s: %s\n",
			progname, tty_path, strerror(errno));
		exit(1);
	}

	fprintf(tty_file, "\r\n\007\007\007Message from NetWare Server: %s\n",
		mnt->mnt_fsname);
	fprintf(tty_file, "%s\n", message);
	fclose(tty_file);
	fclose(mtab);
	return;
}

/* The following routines have been taken from util-linux-2.5's write.c */

/*
 * term_chk - check that a terminal exists, and get the message bit
 *     and the access time
 */
static int
term_chk(char *tty, int *msgsokP, time_t *atimeP, int *showerror)
{
	struct stat s;
	char path[MAXPATHLEN];

	(void)sprintf(path, "/dev/%s", tty);
	if (stat(path, &s) < 0) {
		if (showerror)
			(void)fprintf(stderr,
			    "write: %s: %s\n", path, strerror(errno));
		return(1);
	}
	*msgsokP = (s.st_mode & (S_IWRITE >> 3)) != 0;	/* group write bit */
	*atimeP = s.st_atime;
	return(0);
}

/*
 * search_utmp - search utmp for the "best" terminal to write to
 *
 * Ignores terminals with messages disabled, and of the rest, returns
 * the one with the most recent access time.  Returns as value the number
 * of the user's terminals with messages enabled, or -1 if the user is
 * not logged in at all.
 *
 * Special case for writing to yourself - ignore the terminal you're
 * writing from, unless that's the only terminal with messages enabled.
 */
static int
search_utmp(char *user, char *tty)
{
	struct utmp u;
	time_t bestatime, atime;
	int ufd, nloggedttys, nttys, msgsok, user_is_me;

	char atty[sizeof(u.ut_line) + 1];

	if ((ufd = open(_PATH_UTMP, O_RDONLY)) < 0) {
		perror("utmp");
		return -1;
	}

	nloggedttys = nttys = 0;
	bestatime = 0;
	user_is_me = 0;
	while (read(ufd, (char *) &u, sizeof(u)) == sizeof(u))
		if (strncmp(user, u.ut_name, sizeof(u.ut_name)) == 0) {
			++nloggedttys;

			(void)strncpy(atty, u.ut_line, sizeof(u.ut_line));
			atty[sizeof(u.ut_line)] = '\0';

			if (term_chk(atty, &msgsok, &atime, 0))
				continue;	/* bad term? skip */
			if (!msgsok)
				continue;	/* skip ttys with msgs off */

                        if (u.ut_type != USER_PROCESS)
			        continue;       /* it's not a valid entry */

			++nttys;
			if (atime > bestatime) {
				bestatime = atime;
				(void)strcpy(tty, atty);
			}
		}

	(void)close(ufd);
	if (nloggedttys == 0) {
		(void)fprintf(stderr, "write: %s is not logged in\n", user);
		return -1;
	}
	return 0;
}
