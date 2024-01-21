
#include "../_pwdb_internal.h"

static int check_link_count(const char *file)
{
	struct stat sb;

	if (stat(file, &sb) != 0)
		return 0;

	if (sb.st_nlink != 2)
		return 0;

	unlink(file);
	return 1;
}

/*
 * do_lock_file - lock a password file
 *
 *	do_lock_file() encapsulates the lock operation.  it returns
 *	TRUE or FALSE depending on the password file being
 *	properly locked.  the lock is set by creating a semaphore
 *	file, LOCK.  FILE is a temporary file name.
 */
int do_lock_file(const char *file, const char *lock)
{
	int	fd;
	int	pid;
	int	len;
	char	buf[32];

	/*
	 * Create a lock file which can be switched into place
	 */

	if ((fd = open (file, O_CREAT|O_EXCL|O_WRONLY, 0600)) == -1)
		return 0;

	pid = getpid();
	sprintf (buf, "%d", pid);
	len = strlen(buf) + 1;
	if (write (fd, buf, len) != len) {
		(void) close (fd);
		(void) unlink (file);
		return 0;
	}
	close (fd);

	/*
	 * Simple case first -
	 *	Link fails (in a sane environment ...) if the target
	 *	exists already.  So we try to switch in a new lock
	 *	file.  If that succeeds, we assume we have the only
	 *	valid lock.  Needs work for NFS where this assumption
	 *	may not hold.  The simple hack is to check the link
	 *	count on the source file, which should be 2 iff the
	 *	link =really= worked.
	 */

	if (link (file, lock) == 0)
		return check_link_count(file);

	/*
	 * Invalid lock test -
	 *	Open the lock file and see if the lock is valid.
	 *	The PID of the lock file is checked, and if the PID
	 *	is not valid, the lock file is removed.  If the unlink
	 *	of the lock file fails, it should mean that someone
	 *	else is executing this code.  They will get success,
	 *	and we will fail.
	 */

	if ((fd = open (lock, O_RDWR)) == -1 ||
			(len = read (fd, buf, sizeof buf)) <= 0) {
		errno = EINVAL;
		return 0;
	}
	buf[len] = '\0';
	if ((pid = strtol (buf, (char **) 0, 10)) == 0) {
		errno = EINVAL;
		return 0;
	}
	if (kill (pid, 0) == 0)  {
		errno = EEXIST;
		return 0;
	}
	if (unlink (lock)) {
		(void) close (fd);
		(void) unlink (file);

		return 0;
	}

	/*
	 * Re-try lock -
	 *	The invalid lock has now been removed and I should
	 *	be able to acquire a lock for myself just fine.  If
	 *	this fails there will be no retry.  The link count
	 *	test here makes certain someone executing the previous
	 *	block of code didn't just remove the lock we just
	 *	linked to.
	 */

	if (link (file, lock) == 0)
		return check_link_count(file);

	(void) unlink (file);
	return 0;
}

FILE * fopen_with_umask(const char *name, const char *mode, int mask)
{
	FILE *f;

	mask = umask(mask);
	f = fopen(name, mode);
	umask(mask);
	return f;
}

/*
 * Copy fp to backup, set permissions and times from st.
 */
int create_backup_file(FILE *fp, const char *backup, const struct stat *st)
{
	FILE *bkfp;
	int c;

	unlink(backup);
	bkfp = fopen_with_umask(backup, "w", 0777);
	if (bkfp == NULL)
		return -1;
	rewind(fp);
	while ((c = getc(fp)) != EOF) {
		if (putc(c, bkfp) == EOF) {
			fclose(bkfp);
			return -1;
		}
	}
	if (fflush(bkfp)) {
		fclose(bkfp);
		return -1;
	}
	if (fclose(bkfp))
		return -1;
	if (st) {
		struct utimbuf ut;

		chown(backup, st->st_uid, st->st_gid);
		chmod(backup, st->st_mode);
		ut.actime = st->st_atime;
		ut.modtime = st->st_mtime;
		utime(backup, &ut);
	}
	return 0;
}
