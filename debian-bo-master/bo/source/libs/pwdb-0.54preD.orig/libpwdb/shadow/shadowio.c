/*
 *	This file implements a transaction oriented password database
 *	library.  The password file is updated one entry at a time.
 *	After each transaction the file must be logically closed and
 *	transferred to the existing password file.  The sequence of
 *	events is
 *
 *	spw_lock			-- lock shadow file
 *	spw_open			-- logically open shadow file
 *	                       while transaction to process
 *	spw_(locate,update,remove) -- perform transaction
 *	                              done
 *	spw_close			-- commit transactions
 *	spw_unlock			-- remove shadow lock
 */

#include "../_pwdb_internal.h"

struct spw_file_entry {
        char *spwf_line;
        int spwf_changed;
        struct __pwdb_spwd *spwf_entry;
        struct spw_file_entry *spwf_next;
};

static	int	islocked;
static	int	isopen;
static	int	open_modes;
static	FILE	*spwfp;

struct	spw_file_entry	*__spwf_head;
static	struct	spw_file_entry	*spwf_tail;
static	struct	spw_file_entry	*spwf_cursor;
int	__sp_changed;
static	int	lock_pid;

#define	SPW_LOCK	"/etc/shadow.lock"
#define	SPW_TEMP	"/etc/spwd.%d"

static	char	spw_filename[BUFSIZ] = __PWDB_SHADOW_FILE;

/*
 * __spw_dup - duplicate a shadow file entry
 *
 *	__spw_dup() accepts a pointer to a shadow file entry and
 *	returns a pointer to a shadow file entry in allocated
 *	memory.
 */

struct __pwdb_spwd * __pwdb___spw_dup (const struct __pwdb_spwd *spwd)
{
	struct	__pwdb_spwd	*spw;

	if (! (spw = (struct __pwdb_spwd *) malloc (sizeof *spw)))
		return 0;

	*spw = *spwd;
	if ((spw->sp_namp = __pwdb_strdup (spwd->sp_namp)) == 0 ||
			(spw->sp_pwdp = __pwdb_strdup (spwd->sp_pwdp)) == 0)
		return 0;

	return spw;
}

/*
 * spw_free - free a dynamically allocated shadow file entry
 *
 *	spw_free() frees up the memory which was allocated for the
 *	pointed to entry.
 */

static void __pwdb_spw_free (const struct __pwdb_spwd *spwd)
{
	free (spwd->sp_namp);
	free (spwd->sp_pwdp);
}

/*
 * spw_name - change the name of the shadow password file
 */

int __pwdb_spw_name (const char *name)
{
	if (isopen || strlen (name) > (BUFSIZ-10))
		return -1;

	strcpy (spw_filename, name);
	return 0;
}

/*
 * spw_lock - lock a password file
 *
 *	spw_lock() encapsulates the lock operation.  it returns
 *	TRUE or FALSE depending on the password file being
 *	properly locked.  the lock is set by creating a semaphore
 *	file, SPW_LOCK.
 */

int __pwdb_spw_lock (void)
{
	char	file[BUFSIZ];
	char	lock[BUFSIZ];

	if (islocked)
		return 1;

	lock_pid = getpid();
	if (strcmp (spw_filename, __PWDB_SHADOW_FILE) != 0) {
		sprintf (file, "%s.%d", spw_filename, lock_pid);
		sprintf (lock, "%s.lock", spw_filename);
	} else {
		sprintf (file, SPW_TEMP, lock_pid);
		strcpy (lock, SPW_LOCK);
	}

	/*
	 * The rest is common to all four files (see commonio.c).  --marekm
	 */

	if (do_lock_file(file, lock)) {
		islocked = 1;
		return 1;
	}

	return 0;
}

/*
 * spw_unlock - logically unlock a shadow file
 *
 *	spw_unlock() removes the lock which was set by an earlier
 *	invocation of spw_lock().
 */

int __pwdb_spw_unlock (void)
{
	char	lock[BUFSIZ];

	if (isopen) {
		open_modes = O_RDONLY;
		if (! __pwdb_spw_close ())
			return 0;
	}
  	if (islocked) {
  		islocked = 0;
		if (lock_pid != getpid ())
			return 0;

		strcpy (lock, spw_filename);
		strcat (lock, ".lock");
		(void) unlink (lock);
		return 1;
	}
	return 0;
}

/*
 * spw_open - open a password file
 *
 *	spw_open() encapsulates the open operation.  it returns
 *	TRUE or FALSE depending on the shadow file being
 *	properly opened.
 */

int __pwdb_spw_open (int mode)
{
	char	buf[BUFSIZ];
	char	*cp;
	struct	spw_file_entry	*spwf;
	struct	__pwdb_spwd	*spwd;

	if (isopen || (mode != O_RDONLY && mode != O_RDWR))
		return 0;

	if (mode != O_RDONLY && ! islocked &&
			strcmp (spw_filename, __PWDB_SHADOW_FILE) == 0)
		return 0;

	if ((spwfp = fopen (spw_filename, mode == O_RDONLY ? "r":"r+")) == 0)
		return 0;

	__spwf_head = spwf_tail = spwf_cursor = 0;
	__sp_changed = 0;

	while (fgets (buf, sizeof buf, spwfp) != (char *) 0) {
		if ((cp = strrchr (buf, '\n')))
			*cp = '\0';

		if (! (spwf = (struct spw_file_entry *) malloc (sizeof *spwf)))
			return 0;

		spwf->spwf_changed = 0;
		if (! (spwf->spwf_line = __pwdb_strdup (buf)))
			return 0;
		if ((spwd = __pwdb_sgetspent (buf))
		    && ! (spwd = __pwdb___spw_dup (spwd)))
			return 0;

		spwf->spwf_entry = spwd;

		if (__spwf_head == 0) {
			__spwf_head = spwf_tail = spwf;
			spwf->spwf_next = 0;
		} else {
			spwf_tail->spwf_next = spwf;
			spwf->spwf_next = 0;
			spwf_tail = spwf;
		}
	}
	isopen++;
	open_modes = mode;

	return 1;
}

/*
 * spw_close - close the password file
 *
 *	spw_close() outputs any modified password file entries and
 *	frees any allocated memory.
 */

int __pwdb_spw_close (void)
{
	char	backup[BUFSIZ];
	char	newfile[BUFSIZ];
	int	errors = 0;
	struct	spw_file_entry *spwf;
	struct	stat	sb;

	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	if (islocked && lock_pid != getpid ()) {
		isopen = 0;
		islocked = 0;
		errno = EACCES;
		return 0;
	}
	strcpy (backup, spw_filename);
	strcat (backup, "-");
	strcpy (newfile, spw_filename);
	strcat (newfile, "+");

	/*
	 * Create a backup copy of the shadow password file
	 */

	if (open_modes == O_RDWR && __sp_changed) {

		/*
		 * POLICY: /etc/shadow
		 * Any backup copy of the password file shall have the
		 * same protections as the original.
		 */

		if (fstat (fileno (spwfp), &sb))
			return 0;

		if (create_backup_file(spwfp, backup, &sb))
			return 0;

		isopen = 0;
		(void) fclose (spwfp);

		/*
		 * POLICY: /etc/shadow
		 * The shadow password file shall allow write access to
		 * privileged users only.
		 *
		 * The shadow password file is opened with no access
		 * permissions to any user.  This allows the file to be
		 * changed to root ownership and then made readable by the
		 * owner without ever giving any unprivileged user write
		 * access.
		 */

		spwfp = fopen_with_umask(newfile, "w", 0777);
		if (!spwfp)
			return 0;
		if (chown(newfile, sb.st_uid, sb.st_gid) ||
		    chmod(newfile, sb.st_mode))
			return 0;

		/*
		 * Check each member in the list and write out any elements
		 * that have been changed.
		 */

		for (spwf = __spwf_head;errors == 0 && spwf;
						spwf = spwf->spwf_next) {
			if (spwf->spwf_changed) {
				if (__pwdb_putspent (spwf->spwf_entry, spwfp))
					errors++;
			} else {
				if (fputs (spwf->spwf_line, spwfp) == EOF)
					errors++;
				if (putc ('\n', spwfp) == EOF)
					errors++;
			}
		}
		if (fflush (spwfp))
			errors++;
		if (fclose (spwfp))
			errors++;

		if (errors) {
			unlink (newfile);
			return 0;
		}

		/*
		 * POLICY: /etc/shadow
		 * The shadow password file shall be consistent at all
		 * times.
		 *
		 * The new shadow password file is moved into place only
		 * after determining that the file was created without any
		 * errors occuring.
		 */

		if (rename (newfile, spw_filename))
			return 0;
		sync();
	} else
		/*
		 * Just close the file -- there was nothing to change
		 */

		fclose (spwfp);

	spwfp = 0;

	/*
	 * Free up all of the memory in the linked list.
	 */

	while (__spwf_head != 0) {
		spwf = __spwf_head;
		__spwf_head = spwf->spwf_next;

		if (spwf->spwf_entry) {
			__pwdb_spw_free (spwf->spwf_entry);
			free (spwf->spwf_entry);
		}
		if (spwf->spwf_line)
			free (spwf->spwf_line);

		free (spwf);
	}
	spwf_tail = 0;
	isopen = 0;
	return 1;
}

int __pwdb_spw_update (const struct __pwdb_spwd *spwd)
{
	struct	spw_file_entry	*spwf;
	struct	__pwdb_spwd	*nspwd;

	if (! isopen || open_modes == O_RDONLY) {
		errno = EINVAL;
		return 0;
	}
	for (spwf = __spwf_head;spwf != 0;spwf = spwf->spwf_next) {
		if (spwf->spwf_entry == 0)
			continue;

		if (strcmp (spwd->sp_namp, spwf->spwf_entry->sp_namp) != 0)
			continue;

		if (! (nspwd = __pwdb___spw_dup (spwd)))
			return 0;
		else {
			__pwdb_spw_free (spwf->spwf_entry);
			*(spwf->spwf_entry) = *nspwd;
		}
		spwf->spwf_changed = 1;
		spwf_cursor = spwf;
		return __sp_changed = 1;
	}
	spwf = (struct spw_file_entry *) malloc (sizeof *spwf);
	if (!spwf)
		return 0;
	if (! (spwf->spwf_entry = __pwdb___spw_dup (spwd)))
		return 0;

	spwf->spwf_changed = 1;
	spwf->spwf_next = 0;
	spwf->spwf_line = 0;

	if (spwf_tail)
		spwf_tail->spwf_next = spwf;

	if (! __spwf_head)
		__spwf_head = spwf;

	spwf_tail = spwf;

	return __sp_changed = 1;
}

int __pwdb_spw_remove (const char *name)
{
	struct	spw_file_entry	*spwf;
	struct	spw_file_entry	*ospwf;

	if (! isopen || open_modes == O_RDONLY) {
		errno = EINVAL;
		return 0;
	}
	for (ospwf = 0, spwf = __spwf_head;spwf != 0;
			ospwf = spwf, spwf = spwf->spwf_next) {
		if (! spwf->spwf_entry)
			continue;

		if (strcmp (name, spwf->spwf_entry->sp_namp) != 0)
			continue;

		if (spwf == spwf_cursor)
			spwf_cursor = ospwf;

		if (ospwf != 0)
			ospwf->spwf_next = spwf->spwf_next;
		else
			__spwf_head = spwf->spwf_next;

		if (spwf == spwf_tail)
			spwf_tail = ospwf;

		return __sp_changed = 1;
	}
	errno = ENOENT;
	return 0;
}

const struct __pwdb_spwd * __pwdb_spw_locate (const char *name)
{
	struct	spw_file_entry	*spwf;

	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	for (spwf = __spwf_head;spwf != 0;spwf = spwf->spwf_next) {
		if (spwf->spwf_entry == 0)
			continue;

		if (strcmp (name, spwf->spwf_entry->sp_namp) == 0) {
			spwf_cursor = spwf;
			return spwf->spwf_entry;
		}
	}
	errno = ENOENT;
	return 0;
}

int __pwdb_spw_rewind (void)
{
	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	spwf_cursor = 0;
	return 1;
}

const struct __pwdb_spwd * __pwdb_spw_next (void)
{
	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	if (spwf_cursor == 0)
		spwf_cursor = __spwf_head;
	else
		spwf_cursor = spwf_cursor->spwf_next;

	while (spwf_cursor) {
		if (spwf_cursor->spwf_entry)
			return spwf_cursor->spwf_entry;

		spwf_cursor = spwf_cursor->spwf_next;
	}
	return 0;
}
