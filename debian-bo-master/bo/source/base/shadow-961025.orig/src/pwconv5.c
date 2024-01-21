/*
 * pwconv5 - create or update /etc/shadow with information from
 * /etc/passwd.
 *
 * It is more like SysV pwconv, slightly different from Shadow
 * pwconv.  Depends on "x" as password in /etc/passwd which means
 * that the password has already been moved to /etc/shadow.
 * There is no need to move /etc/npasswd to /etc/passwd, password
 * files are updated using library routines with proper locking.
 *
 * Can be used to update /etc/shadow after adding/deleting users
 * by editing /etc/passwd.  There is no man page yet, but this
 * program should be close to pwconv(1M) on Solaris 2.x.
 *
 * Warning: make sure that all users have "x" as the password in
 * /etc/passwd before running this program for the first time on
 * a system which already has shadow passwords.  Anything else
 * (like "*" from old versions of the shadow suite) will replace
 * the user's encrypted password in /etc/shadow.
 *
 * Doesn't currently support pw_age information in /etc/passwd,
 * and doesn't support DBM files.  Add it if you need it...
 *
 * Copyright (C) 1996, Marek Michalkiewicz
 * <marekm@i17linuxb.ists.pwr.wroc.pl>
 * This program may be freely used and distributed.  If you improve
 * it, please send me your changes.  Thanks!
 */

#include <config.h>

#include "rcsid.h"
RCSID("$Id: pwconv5.c,v 1.2 1996/09/25 03:20:03 marekm Exp $")

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>

#include <pwd.h>
#include "prototypes.h"
#include "defines.h"
#include "pwio.h"
#include "shadowio.h"
#include "getdef.h"

static int
	shadow_locked = 0,
	passwd_locked = 0,
	shadow_created = 0;

#ifndef PASSWD_FILE
#define PASSWD_FILE "/etc/passwd"
#endif

#ifndef SHADOW_FILE
#define SHADOW_FILE "/etc/shadow"
#endif

static void
fail_exit(status)
	int status;
{
	if (shadow_locked)
		spw_unlock();
	if (passwd_locked)
		pw_unlock();
	if (shadow_created)
		unlink(SHADOW_FILE);
	exit(status);
}

int
main()
{
	const struct passwd *pw;
	struct passwd pwent;
	const struct spwd *sp;
	struct spwd spent;
	int fd;

	if (!pw_lock()) {
		fprintf(stderr, "can't lock passwd file\n");
		fail_exit(5);
	}
	passwd_locked++;
	if (!pw_open(O_RDWR)) {
		fprintf(stderr, "can't open passwd file\n");
		fail_exit(1);
	}

	/*
	 * Create empty /etc/shadow if it doesn't exist.
	 */
	fd = open(SHADOW_FILE, O_CREAT|O_EXCL|O_WRONLY, 0600);
	if (fd >= 0) {
		close(fd);
		shadow_created++;
	} else if (errno != EEXIST) {
		perror("open /etc/shadow");
		fail_exit(1);
	}

	if (!spw_lock()) {
		fprintf(stderr, "can't lock shadow file\n");
		fail_exit(5);
	}
	shadow_locked++;
	if (!spw_open(O_RDWR)) {
		fprintf(stderr, "can't open shadow file\n");
		fail_exit(1);
	}

	/*
	 * Remove /etc/shadow entries for users not in /etc/passwd.
	 */
	spw_rewind();
	while ((sp = spw_next())) {
		if (pw_locate(sp->sp_namp))
			continue;

		if (!spw_remove(sp->sp_namp)) {
			/*
			 * This shouldn't happen (the entry exists) but...
			 */
			fprintf(stderr, "can't remove shadow entry for %s\n",
				sp->sp_namp);
			fail_exit(3);
		}
	}

	/*
	 * Update shadow entries which don't have "x" as pw_passwd.
	 * Add any missing shadow entries.
	 */
	pw_rewind();
	while ((pw = pw_next())) {
		sp = spw_locate(pw->pw_name);
		if (sp) {
			/* do we need to update this entry? */
			if (strcmp(pw->pw_passwd, "x") == 0)
				continue;
			/* update existing shadow entry */
			spent = *sp;
		} else {
			/* add new shadow entry */
			memset(&spent, 0, sizeof spent);
			spent.sp_namp = pw->pw_name;
			spent.sp_min = getdef_num("PASS_MIN_DAYS", -1);
			spent.sp_max = getdef_num("PASS_MAX_DAYS", -1);
			spent.sp_warn = getdef_num("PASS_WARN_AGE", -1);
			spent.sp_inact = -1;
			spent.sp_expire = -1;
			spent.sp_flag = -1;
		}
		spent.sp_pwdp = pw->pw_passwd;
		spent.sp_lstchg = time((time_t *) 0) / (24L*3600L);
		if (!spw_update(&spent)) {
			fprintf(stderr, "can't update shadow entry for %s\n",
				spent.sp_namp);
			fail_exit(3);
		}
		/* remove password from /etc/passwd */
		pwent = *pw;
		pwent.pw_passwd = "x";
		if (!pw_update(&pwent)) {
			fprintf(stderr, "can't update passwd entry for %s\n",
				pwent.pw_name);
			fail_exit(3);
		}
	}

	if (!spw_close()) {
		fprintf(stderr, "can't update shadow file\n");
		fail_exit(3);
	}
	if (!pw_close()) {
		fprintf(stderr, "can't update passwd file\n");
		fail_exit(3);
	}
	chmod(PASSWD_FILE "-", 0600);  /* /etc/passwd- (backup file) */
	spw_unlock();
	pw_unlock();
	return 0;
}
