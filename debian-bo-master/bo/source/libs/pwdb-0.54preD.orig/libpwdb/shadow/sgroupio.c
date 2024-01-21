/*
 *	This file implements a transaction oriented shadow group
 *	database library.  The shadow group file is updated one
 *	entry at a time.  After each transaction the file must be
 *	logically closed and transferred to the existing shadow
 *	group file.  The sequence of events is
 *
 *	sgr_lock			-- lock shadow group file
 *	sgr_open			-- logically open shadow group file
 *	while transaction to process
 *		sgr_(locate,update,remove) -- perform transaction
 *	done
 *	sgr_close			-- commit transactions
 *	sgr_unlock			-- remove shadow group lock
 */

#include "../_pwdb_internal.h"

static	int	islocked;
static	int	isopen;
static	int	open_modes;
static	FILE	*sgrfp;

struct	__pwdb_sg_file_entry	*__sgr_head;
static	struct	__pwdb_sg_file_entry	*sgr_tail;
static	struct	__pwdb_sg_file_entry	*sgr_cursor;
int	__sg_changed;
static	int	lock_pid;

#define	SG_LOCK	"/etc/gshadow.lock"
#define	GR_TEMP "/etc/gshadow.%d"

static	char	sg_filename[BUFSIZ] = __PWDB_SGROUP_FILE ;

/*
 * __sgr_dup - duplicate a shadow group file entry
 *
 *	__sgr_dup() accepts a pointer to a shadow group file entry and
 *	returns a pointer to a shadow group file entry in allocated memory.
 */

static struct __pwdb_sgrp * __pwdb___sgr_dup (const struct __pwdb_sgrp * sgrent)
{
	struct	__pwdb_sgrp	*sgr;
	int	i;

	if (! (sgr = (struct __pwdb_sgrp *) malloc (sizeof *sgr)))
		return 0;

	if ((sgr->sg_name = strdup (sgrent->sg_name)) == 0 ||
			(sgr->sg_passwd = strdup (sgrent->sg_passwd)) == 0)
		return 0;

	for (i = 0;sgrent->sg_mem[i];i++)
		;

	if (! (sgr->sg_mem = (char **) malloc (sizeof (char *) * (i + 1))))
		return 0;
	for (i = 0;sgrent->sg_mem[i];i++)
		if (! (sgr->sg_mem[i] = strdup (sgrent->sg_mem[i])))
			return 0;

	sgr->sg_mem[i] = 0;

	for (i = 0;sgrent->sg_adm[i];i++)
		;

	if (! (sgr->sg_adm = (char **) malloc (sizeof (char *) * (i + 1))))
		return 0;
	for (i = 0;sgrent->sg_adm[i];i++)
		if (! (sgr->sg_adm[i] = strdup (sgrent->sg_adm[i])))
			return 0;

	sgr->sg_adm[i] = 0;

	return sgr;
}

/*
 * sgr_free - free a dynamically allocated shadow group file entry
 *
 *	sgr_free() frees up the memory which was allocated for the
 *	pointed to entry.
 */

static void __pwdb_sgr_free (const struct __pwdb_sgrp *sgrent)
{
	int	i;

	free (sgrent->sg_name);
	free (sgrent->sg_passwd);

	for (i = 0;sgrent->sg_mem[i];i++)
		free (sgrent->sg_mem[i]);

	free (sgrent->sg_mem);

	for (i = 0;sgrent->sg_adm[i];i++)
		free (sgrent->sg_adm[i]);

	free (sgrent->sg_adm);
}

/*
 * sgr_name - change the name of the shadow group file
 */

int __pwdb_sgr_name (const char *name)
{
	if (isopen || (int) strlen (name) > (BUFSIZ-10))
		return -1;

	strcpy (sg_filename, name);
	return 0;
}

/*
 * sgr_lock - lock a shadow group file
 *
 *	sgr_lock() encapsulates the lock operation.  it returns
 *	TRUE or FALSE depending on the shadow group file being
 *	properly locked.  the lock is set by creating a semaphore
 *	file, SG_LOCK.
 */

int __pwdb_sgr_lock (void)
{
	char	file[BUFSIZ];

	if (islocked)
		return 1;

	if (strcmp (sg_filename, __PWDB_SGROUP_FILE) != 0)
		return 0;

	sprintf (file, GR_TEMP, lock_pid = getpid ());

	/*
	 * The rest is common to all four files (see commonio.c).  --marekm
	 */

	if (do_lock_file(file, SG_LOCK)) {
		islocked = 1;
		return 1;
	}

	return 0;
}

/*
 * sgr_unlock - logically unlock a shadow group file
 *
 *	sgr_unlock() removes the lock which was set by an earlier
 *	invocation of sgr_lock().
 */

int __pwdb_sgr_unlock (void)
{
	if (isopen) {
		open_modes = O_RDONLY;
		if (! __pwdb_sgr_close ())
			return 0;
	}
	if (islocked) {
		islocked = 0;
		if (lock_pid != getpid ())
			return 0;

		(void) unlink (SG_LOCK);
		return 1;
	}
	return 0;
}

/*
 * sgr_open - open a shadow group file
 *
 *	sgr_open() encapsulates the open operation.  it returns
 *	TRUE or FALSE depending on the shadow group file being
 *	properly opened.
 */

int __pwdb_sgr_open (int mode)
{
	char	buf[8192];
	char	*cp;
	struct	__pwdb_sg_file_entry	*sgrf;
	struct	__pwdb_sgrp	*sgrent;

	if (isopen || (mode != O_RDONLY && mode != O_RDWR))
		return 0;

	if (mode != O_RDONLY && ! islocked &&
			strcmp (sg_filename, __PWDB_SGROUP_FILE) == 0)
		return 0;

	if ((sgrfp = fopen (sg_filename, mode == O_RDONLY ? "r":"r+")) == 0)
		return 0;

	__sgr_head = sgr_tail = sgr_cursor = 0;
	__sg_changed = 0;

	while (__pwdb_fgetsx (buf, sizeof buf, sgrfp) != (char *) 0) {
		if ((cp = strrchr (buf, '\n')))
			*cp = '\0';

		if (! (sgrf = (struct __pwdb_sg_file_entry *) malloc (sizeof *sgrf)))
			return 0;

		sgrf->sgr_changed = 0;
		if (! (sgrf->sgr_line = strdup (buf)))
			return 0;
		if ((sgrent = __pwdb_sgetsgent (buf)) && ! (sgrent = __pwdb___sgr_dup (sgrent)))
			return 0;

		sgrf->sgr_entry = sgrent;

		if (__sgr_head == 0) {
			__sgr_head = sgr_tail = sgrf;
			sgrf->sgr_next = 0;
		} else {
			sgr_tail->sgr_next = sgrf;
			sgrf->sgr_next = 0;
			sgr_tail = sgrf;
		}
	}
	isopen++;
	open_modes = mode;

	return 1;
}

/*
 * sgr_close - close the shadow group file
 *
 *	sgr_close() outputs any modified shadow group file entries and
 *	frees any allocated memory.
 */

int __pwdb_sgr_close (void)
{
	char	backup[BUFSIZ];
	char	newfile[BUFSIZ];
	int	errors = 0;
	struct	__pwdb_sg_file_entry *sgrf;
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
	strcpy (backup, sg_filename);
	strcat (backup, "-");
	strcpy (newfile, sg_filename);
	strcat (newfile, "+");

	/*
	 * Create a backup copy of the shadow group file.
	 */

	if (open_modes == O_RDWR && __sg_changed) {

		/*
		 * POLICY: /etc/gshadow
		 * Any backup copy of the shadow group file shall have
		 * the same protections as the original.
		 *
		 * Get the permissions from the old file and apply them
		 * to the backup file.
		 */

		if (fstat (fileno (sgrfp), &sb))
			return 0;

		if (create_backup_file(sgrfp, backup, &sb))
			return 0;

		isopen = 0;
		(void) fclose (sgrfp);

		/*
		 * POLICY: /etc/gshadow
		 * The group file shall allow read and write access to
		 * privileged users only.
		 *
		 * The group file is opened with no access permissions
		 * to any user.  This allows the file to be changed to
		 * root ownership and then made readable by the owner
		 * without ever giving any unprivileged user write access.
		 */

		sgrfp = fopen_with_umask(newfile, "w", 0777);
		if (!sgrfp)
			return 0;
		if (chown(newfile, sb.st_uid, sb.st_gid) ||
		    chmod(newfile, sb.st_mode))
			return 0;

		/*
		 * Check each member in the list and write out any elements
		 * that have been changed.
		 */

		for (sgrf = __sgr_head;! errors && sgrf;sgrf = sgrf->sgr_next) {
			if (sgrf->sgr_changed) {
				if (__pwdb_putsgent (sgrf->sgr_entry, sgrfp) == -1)
					errors++;
			} else {
				if (__pwdb_fputsx (sgrf->sgr_line, sgrfp) == -1)
					errors++;

				if (putc ('\n', sgrfp) == EOF)
					errors++;
			}
		}
		if (fflush (sgrfp))
			errors++;
		if (fclose (sgrfp))
			errors++;

		if (errors) {
			unlink (newfile);
			return 0;
		}

		/*
		 * POLICY: /etc/gshadow
		 * The shadow group file shall be consistent at all
		 * times.
		 *
		 * The new group file is moved into place only after
		 * determining that the file was created without any
		 * errors occuring.
		 */

		if (rename (newfile, sg_filename))
			return 0;
		sync();
	} else
		/*
		 * Just close the file -- there was nothing to change
		 */

		fclose (sgrfp);

	sgrfp = 0;

	/*
	 * Free up all of the memory in the linked list.
	 */

	while (__sgr_head != 0) {
		sgrf = __sgr_head;
		__sgr_head = sgrf->sgr_next;

		if (sgrf->sgr_entry) {
			__pwdb_sgr_free (sgrf->sgr_entry);
			free (sgrf->sgr_entry);
		}
		if (sgrf->sgr_line)
			free (sgrf->sgr_line);

		free (sgrf);
	}
	sgr_tail = 0;
	isopen = 0;
	return 1;
}

int __pwdb_sgr_update (const struct __pwdb_sgrp *sgrent)
{
	struct	__pwdb_sg_file_entry	*sgrf;
	struct	__pwdb_sgrp	*nsgr;

	if (! isopen || open_modes == O_RDONLY) {
		errno = EINVAL;
		return 0;
	}
	for (sgrf = __sgr_head;sgrf != 0;sgrf = sgrf->sgr_next) {
		if (sgrf->sgr_entry == 0)
			continue;

		if (strcmp (sgrent->sg_name, sgrf->sgr_entry->sg_name) != 0)
			continue;

		if (! (nsgr = __pwdb___sgr_dup (sgrent)))
			return 0;
		else {
			__pwdb_sgr_free (sgrf->sgr_entry);
			*(sgrf->sgr_entry) = *nsgr;
		}
		sgrf->sgr_changed = 1;
		sgr_cursor = sgrf;
		return __sg_changed = 1;
	}
	sgrf = (struct __pwdb_sg_file_entry *) malloc (sizeof *sgrf);
	if (! (sgrf->sgr_entry = __pwdb___sgr_dup (sgrent)))
		return 0;

	sgrf->sgr_changed = 1;
	sgrf->sgr_next = 0;
	sgrf->sgr_line = 0;

	if (sgr_tail)
		sgr_tail->sgr_next = sgrf;

	if (! __sgr_head)
		__sgr_head = sgrf;

	sgr_tail = sgrf;

	return __sg_changed = 1;
}

int __pwdb_sgr_remove (const char *name)
{
	struct	__pwdb_sg_file_entry	*sgrf;
	struct	__pwdb_sg_file_entry	*osgrf;

	if (! isopen || open_modes == O_RDONLY) {
		errno = EINVAL;
		return 0;
	}
	for (osgrf = 0, sgrf = __sgr_head;sgrf != 0;
			osgrf = sgrf, sgrf = sgrf->sgr_next) {
		if (! sgrf->sgr_entry)
			continue;

		if (strcmp (name, sgrf->sgr_entry->sg_name) != 0)
			continue;

		if (sgrf == sgr_cursor)
			sgr_cursor = osgrf;

		if (osgrf != 0)
			osgrf->sgr_next = sgrf->sgr_next;
		else
			__sgr_head = sgrf->sgr_next;

		if (sgrf == sgr_tail)
			sgr_tail = osgrf;

		return __sg_changed = 1;
	}
	errno = ENOENT;
	return 0;
}

const struct __pwdb_sgrp * __pwdb_sgr_locate (const char *name)
{
	struct	__pwdb_sg_file_entry	*sgrf;

	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	for (sgrf = __sgr_head;sgrf != 0;sgrf = sgrf->sgr_next) {
		if (sgrf->sgr_entry == 0)
			continue;

		if (strcmp (name, sgrf->sgr_entry->sg_name) == 0) {
			sgr_cursor = sgrf;
			return sgrf->sgr_entry;
		}
	}
	errno = ENOENT;
	return 0;
}

int __pwdb_sgr_rewind (void)
{
	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	sgr_cursor = 0;
	return 1;
}

const struct __pwdb_sgrp * __pwdb_sgr_next ()
{
	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	if (sgr_cursor == 0)
		sgr_cursor = __sgr_head;
	else
		sgr_cursor = sgr_cursor->sgr_next;

	while (sgr_cursor) {
		if (sgr_cursor->sgr_entry)
			return sgr_cursor->sgr_entry;

		sgr_cursor = sgr_cursor->sgr_next;
	}
	return 0;
}
