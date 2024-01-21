/*
 *	gr_lock				-- lock group file
 *	gr_open				-- logically open group file
 *	while transaction to process
 *		gr_(locate,update,remove) -- perform transaction
 *	done
 *	gr_close			-- commit transactions
 *	gr_unlock			-- remove group lock
 */

#include "../_pwdb_internal.h"


static	int	islocked;
static	int	isopen;
static	int	open_modes;
static	FILE	*grfp;

struct	__pwdb_gr_file_entry	*__grf_head;
static	struct	__pwdb_gr_file_entry	*grf_tail;
static	struct	__pwdb_gr_file_entry	*grf_cursor;
int	__gr_changed;
static	int	lock_pid;

#define	GR_LOCK	"/etc/group.lock"
#define	GR_TEMP "/etc/grp.%d"

static	char	gr_filename[BUFSIZ] = __PWDB_GROUP_FILE;

/*
 * __gr_dup - duplicate a group file entry
 *
 *	__gr_dup() accepts a pointer to a group file entry and
 *	returns a pointer to a group file entry in allocated
 *	memory.
 */

static struct __pwdb_group * __pwdb___gr_dup (const struct __pwdb_group *grent)
{
	struct	__pwdb_group	*gr;
	int	i;

	if (! (gr = (struct __pwdb_group *) malloc (sizeof *gr)))
		return 0;

	if ((gr->gr_name = strdup (grent->gr_name)) == 0 ||
			(gr->gr_passwd = strdup (grent->gr_passwd)) == 0)
		return 0;

	for (i = 0;grent->gr_mem[i];i++)
		;

	if (! (gr->gr_mem = (char **) malloc (sizeof (char *) * (i + 1))))
		return 0;
	for (i = 0;grent->gr_mem[i];i++)
		if (! (gr->gr_mem[i] = strdup (grent->gr_mem[i])))
			return 0;

	gr->gr_mem[i] = 0;
	gr->gr_gid = grent->gr_gid;

	return gr;
}

/*
 * gr_free - free a dynamically allocated group file entry
 *
 *	gr_free() frees up the memory which was allocated for the
 *	pointed to entry.
 */

static void __pwdb_gr_free (const struct __pwdb_group * grent)
{
	int	i;

	free (grent->gr_name);
	free (grent->gr_passwd);

	for (i = 0;grent->gr_mem[i];i++)
		free (grent->gr_mem[i]);

	free ((char *) grent->gr_mem);
}

/*
 * gr_name - change the name of the group file
 */

int __pwdb_gr_name (const char * name)
{
	if (isopen || (int) strlen (name) > (BUFSIZ-10))
		return -1;

	strcpy (gr_filename, name);
	return 0;
}

/*
 * gr_lock - lock a group file
 *
 *	gr_lock() encapsulates the lock operation.  it returns
 *	TRUE or FALSE depending on the group file being
 *	properly locked.  the lock is set by creating a semaphore
 *	file, GR_LOCK.
 */

int __pwdb_gr_lock (void)
{
	char	file[BUFSIZ];

	if (islocked)
		return 1;

	if (strcmp (gr_filename, __PWDB_GROUP_FILE) != 0)
		return 0;

	sprintf (file, GR_TEMP, lock_pid = getpid ());

	/*
	 * The rest is common to all four files (see commonio.c).  --marekm
	 */

	if (do_lock_file(file, GR_LOCK)) {
		islocked = 1;
		return 1;
	}

	return 0;
}

/*
 * gr_unlock - logically unlock a group file
 *
 *	gr_unlock() removes the lock which was set by an earlier
 *	invocation of gr_lock().
 */

int __pwdb_gr_unlock (void)
{
	if (isopen) {
		open_modes = O_RDONLY;
		if (! __pwdb_gr_close ())
			return 0;
	}
	if (islocked) {
		islocked = 0;
		if (lock_pid != getpid ())
			return 0;

		(void) unlink (GR_LOCK);
		return 1;
	}
	return 0;
}

/*
 * gr_open - open a group file
 *
 *	gr_open() encapsulates the open operation.  it returns
 *	TRUE or FALSE depending on the group file being
 *	properly opened.
 */

int __pwdb_gr_open (int mode)
{
	char	buf[8192];
	char	*cp;
	struct	__pwdb_gr_file_entry	*grf;
	struct	__pwdb_group	*grent;

	if (isopen || (mode != O_RDONLY && mode != O_RDWR))
		return 0;

	if (mode != O_RDONLY && ! islocked &&
			strcmp (gr_filename, __PWDB_GROUP_FILE) == 0)
		return 0;

	if ((grfp = fopen (gr_filename, mode == O_RDONLY ? "r":"r+")) == 0)
		return 0;

	__grf_head = grf_tail = grf_cursor = 0;
	__gr_changed = 0;

	while (__pwdb_fgetsx (buf, sizeof buf, grfp) != (char *) 0) {
		if ((cp = strrchr (buf, '\n')))
			*cp = '\0';

		if (! (grf = (struct __pwdb_gr_file_entry *) malloc (sizeof *grf)))
			return 0;

		grf->grf_changed = 0;
		if (! (grf->grf_line = strdup (buf)))
			return 0;
		if ((grent = __pwdb_sgetgrent (buf)) && ! (grent = __pwdb___gr_dup (grent)))
			return 0;

		grf->grf_entry = grent;

		if (__grf_head == 0) {
			__grf_head = grf_tail = grf;
			grf->grf_next = 0;
		} else {
			grf_tail->grf_next = grf;
			grf->grf_next = 0;
			grf_tail = grf;
		}
	}
	isopen++;
	open_modes = mode;

	return 1;
}

/*
 * gr_close - close the group file
 *
 *	gr_close() outputs any modified group file entries and
 *	frees any allocated memory.
 */

int __pwdb_gr_close (void)
{
	char	backup[BUFSIZ];
	char	newfile[BUFSIZ];
	int	errors = 0;
	struct	__pwdb_gr_file_entry *grf;
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
	strcpy (backup, gr_filename);
	strcat (backup, "-");
	strcpy (newfile, gr_filename);
	strcat (newfile, "+");

	/*
	 * Create a backup copy of the group file.
	 */

	if (open_modes == O_RDWR && __gr_changed) {

		/*
		 * POLICY: /etc/group
		 * Any backup copy of the group file shall have the
		 * same protections as the original.
		 *
		 * Get the permissions from the old file and apply them
		 * to the backup file.
		 */

		if (fstat (fileno (grfp), &sb))
			return 0;

		if (create_backup_file(grfp, backup, &sb))
			return 0;

		isopen = 0;
		(void) fclose (grfp);

		/*
		 * POLICY: /etc/group
		 * The group file shall allow write access to
		 * privileged users only.  The group file shall allow
		 * read access to all users.
		 *
		 * The group file is opened with no access permissions
		 * to any user.  This allows the file to be changed to
		 * root ownership and then made readable by all users
		 * without ever giving any unprivileged user write access.
		 */

		grfp = fopen_with_umask(newfile, "w", 0777);
		if (!grfp)
			return 0;
		if (chown(newfile, sb.st_uid, sb.st_gid) ||
		    chmod(newfile, sb.st_mode))
			return 0;

		/*
		 * Check each member in the list and write out any elements
		 * that have been changed.
		 */

		for (grf = __grf_head;! errors && grf;grf = grf->grf_next) {
			if (grf->grf_changed) {
				if (__pwdb_putgrent (grf->grf_entry, grfp))
					errors++;
			} else {
				if (__pwdb_fputsx (grf->grf_line, grfp))
					errors++;

				if (putc ('\n', grfp) == EOF)
					errors++;
			}
		}
		if (fflush (grfp))
			errors++;
		if (fclose (grfp))
			errors++;

		if (errors) {
			unlink (newfile);
			return 0;
		}

		/*
		 * POLICY: /etc/group
		 * The group file shall be consistent at all times.
		 *
		 * The new group file is moved into place only after
		 * determining that the file was created without any
		 * errors occuring.
		 */

		if (rename (newfile, gr_filename))
			return 0;
		sync();
	} else
		/*
		 * Just close the file -- there was nothing to change
		 */

		fclose (grfp);

	grfp = 0;

	/*
	 * Free up all of the memory in the linked list.
	 */

	while (__grf_head != 0) {
		grf = __grf_head;
		__grf_head = grf->grf_next;

		if (grf->grf_entry) {
			__pwdb_gr_free (grf->grf_entry);
			free ((char *) grf->grf_entry);
		}
		if (grf->grf_line)
			free (grf->grf_line);

		free ((char *) grf);
	}
	grf_tail = 0;
	isopen = 0;
	return 1;
}

/* update an entry */

int __pwdb_gr_update (const struct __pwdb_group * grent)
{
	struct	__pwdb_gr_file_entry	*grf;
	struct	__pwdb_group	*ngr;

	if (! isopen || open_modes == O_RDONLY) {
		errno = EINVAL;
		return 0;
	}
	for (grf = __grf_head;grf != 0;grf = grf->grf_next) {
		if (grf->grf_entry == 0)
			continue;

		if (strcmp (grent->gr_name, grf->grf_entry->gr_name) != 0)
			continue;

		if (! (ngr = __pwdb___gr_dup (grent)))
			return 0;

		__pwdb_gr_free (grf->grf_entry);
		*(grf->grf_entry) = *ngr;

		grf->grf_changed = 1;
		grf_cursor = grf;
		return __gr_changed = 1;
	}
	if (! (grf = (struct __pwdb_gr_file_entry *) malloc (sizeof *grf)))
		return 0;
	if (! (grf->grf_entry = __pwdb___gr_dup (grent)))
		return 0;

	grf->grf_changed = 1;
	grf->grf_next = 0;
	grf->grf_line = 0;

	if (grf_tail)
		grf_tail->grf_next = grf;

	if (! __grf_head)
		__grf_head = grf;

	grf_tail = grf;

	return __gr_changed = 1;
}

int __pwdb_gr_remove (const char * name)
{
	struct	__pwdb_gr_file_entry	*grf;
	struct	__pwdb_gr_file_entry	*ogrf;

	if (! isopen || open_modes == O_RDONLY) {
		errno = EINVAL;
		return 0;
	}
	for (ogrf = 0, grf = __grf_head;grf != 0;
			ogrf = grf, grf = grf->grf_next) {
		if (! grf->grf_entry)
			continue;

		if (strcmp (name, grf->grf_entry->gr_name) != 0)
			continue;

		if (grf == grf_cursor)
			grf_cursor = ogrf;

		if (ogrf != 0)
			ogrf->grf_next = grf->grf_next;
		else
			__grf_head = grf->grf_next;

		if (grf == grf_tail)
			grf_tail = ogrf;

		return __gr_changed = 1;
	}
	errno = ENOENT;
	return 0;
}

const struct __pwdb_group * __pwdb_gr_locate (const char *name)
{
	struct	__pwdb_gr_file_entry	*grf;

	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	for (grf = __grf_head;grf != 0;grf = grf->grf_next) {
		if (grf->grf_entry == 0)
			continue;

		if (strcmp (name, grf->grf_entry->gr_name) == 0) {
			grf_cursor = grf;
			return grf->grf_entry;
		}
	}
	errno = ENOENT;
	return 0;
}

const struct __pwdb_group * __pwdb_gr_locate_id (gid_t gid)
{
	struct	__pwdb_gr_file_entry	*grf;

	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	for (grf = __grf_head;grf != 0;grf = grf->grf_next) {
		if (grf->grf_entry == 0)
			continue;

		if (grf->grf_entry->gr_gid == gid) {
			grf_cursor = grf;
			return grf->grf_entry;
		}
	}
	errno = ENOENT;
	return 0;
}

int __pwdb_gr_rewind (void)
{
	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	grf_cursor = 0;
	return 1;
}

const struct __pwdb_group * __pwdb_gr_next (void)
{
	if (! isopen) {
		errno = EINVAL;
		return 0;
	}
	if (grf_cursor == 0)
		grf_cursor = __grf_head;
	else
		grf_cursor = grf_cursor->grf_next;

	while (grf_cursor) {
		if (grf_cursor->grf_entry)
			return grf_cursor->grf_entry;

		grf_cursor = grf_cursor->grf_next;
	}
	return 0;
}
