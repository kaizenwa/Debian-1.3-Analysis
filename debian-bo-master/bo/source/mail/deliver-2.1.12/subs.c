/* $Id: subs.c,v 1.9 1993/10/28 16:49:51 chip Exp $
 *
 * Miscellaneous subroutines.
 *
 * $Log: subs.c,v $
 * Revision 1.9  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.8  1991/11/26  16:50:42  chip
 * Add TEMPDIR.
 *
 * Revision 1.7  1991/10/23  20:02:03  chip
 * Make all temp files close-on-exec.
 * Add tdup(), which duplicates and sets close-on-exec.
 *
 * Revision 1.6  1991/09/11  17:41:20  chip
 * Fix typo.
 *
 * Revision 1.5  1991/08/27  15:39:37  chip
 * Add tmzone().
 *
 * Revision 1.4  1991/08/21  22:20:37  chip
 * Shut up signed/unsigned warning.
 *
 * Revision 1.3  1991/08/21  22:15:33  chip
 * Careful creation for NFS.
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"
#include <time.h>
#include <sys/stat.h>
#include <errno.h>

/*----------------------------------------------------------------------
 * Report as to whether a file exists or not.
 */

int
exists(path)
char *path;
{
    struct stat st;

    return (stat(path, &st) == 0);
}

/*----------------------------------------------------------------------
 * Return a pointer to a temporary filename, or NULL if error.
 */

char *
tempfile()
{
    static char template[] = "dl.XXXXXX";
    char *f;

    f = zalloc(sizeof(TEMPDIR) + sizeof(template));
    (void) sprintf(f, "%s/%s", TEMPDIR, template);
    if (mktemp(f) == NULL)
    {
	free(f);
	error("can't create temporary file");
	return NULL;
    }
    return f;
}

/*----------------------------------------------------------------------
 * Create a file and return an fd, or complain if it doesn't work.
 * The file is opened for read/write.
 */

int
tcreate(name)
char *name;
{
    int fd;

#ifdef O_CREAT
    fd = open(name, O_RDWR | O_CREAT | O_EXCL, 0);
#else
    fd = creat(name, 0);
#endif
    if (fd == -1)
    {
	syserr("can't create %s", name);
	return -1;
    }

#ifndef O_CREAT
    (void) close(fd);
    if ((fd = open(name, O_RDWR)) == -1)
    {
	syserr("can't re-open %s", name);
	return -1;
    }
#endif

    if (clexec(fd, TRUE) == -1)
    {
	(void) close(fd);
	return -1;
    }

    return fd;
}

/*----------------------------------------------------------------------
 * Create a file and return a FILE *, or complain if it doesn't work.
 * The file is opened for read/write.
 */

FILE *
ftcreate(name)
char *name;
{
    FILE *fp;
    int fd;

    if ((fd = tcreate(name)) == -1)
	return NULL;

    if ((fp = fdopen(fd, "r+")) == NULL)
	(void) close(fd);

    return fp;
}

/*----------------------------------------------------------------------
 * Duplicate a temporary file descriptor.
 */

int
tdup(fd, what)
int fd;
char *what;
{
    int nfd;

    if ((nfd = dup(fd)) == -1)
    {
	syserr("can't dup %s fd %d", what, fd);
	return -1;
    }

    if (clexec(nfd, TRUE) == -1)
    {
	(void) close(nfd);
	return -1;
    }

    return nfd;
}

/*----------------------------------------------------------------------
 * Allocate memory for an environment variable, and putenv() it.
 */

void
alloc_env(name, value)
char *name;
char *value;
{
    char *s;

    if (!name || !value)
	return;

    /* If it's the same value it already has, don't bother. */

    if ((s = getenv(name)) != NULL && strcmp(s, value) == 0)
	return;

    s = zalloc((unsigned) (strlen(name) + strlen(value) + 2));
    (void) sprintf(s, "%s=%s", name, value);
    if (putenv(s))
	nomem();
}

/*----------------------------------------------------------------------
 * Remove an environment variable.
 */

void
del_env(name)
char *name;
{
    unsigned len;
    char **e;

    if (!name)
	return;

    len = strlen(name);

    for (e = environ; *e; ++e)
    {
	char c;

	if (strncmp(*e, name, len) != 0)
	    continue;

	c = (*e)[len];
	if (c == '=' || c == '\0')
	    break;
    }

    for (; *e; ++e)
	*e = *(e + 1);
}

/*----------------------------------------------------------------------
 * Skip the string "From " and return new pointer, or NULL if the
 * parameter doesn't contain "From ".
 */

char *
skipfrom(s)
char *s;
{
    if (s != NULL
	&& s[0] == 'F' && s[1] == 'r' && s[2] == 'o' && s[3] == 'm'
	&& s[4] == ' ')
	return s + 5;

    return NULL;
}

/*----------------------------------------------------------------------
 * Return name of timezone described by given tm structure.
 */

char *
tmzone(tm)
struct tm *tm;
{
#ifdef HAS_TZNAME
    return tzname[tm->tm_isdst ? 1 : 0];
#else
    return tm->tm_zone;
#endif
}

/*----------------------------------------------------------------------
 * Allocate and clear.  If it fails, it takes the emergency exit.
 */

char *
zalloc(size)
unsigned size;
{
    char *p;

    if ((p = (char *) malloc(size)) == NULL)
	nomem();

    Zero(p, size);
    return p;
}

/*----------------------------------------------------------------------
 * Reallocate to new size.  If it fails, it takes the emergency exit.
 */

char *
srealloc(ptr, size)
char *ptr;
unsigned size;
{
    char *p;

    if ((p = (char *) realloc(ptr, size)) == NULL)
	nomem();

    return p;
}

/*----------------------------------------------------------------------
 * Make an allocated copy of a string.
 */

char *
copystr(s)
char *s;
{
    char *p;

    if (s == NULL)
	return NULL;

    if ((p = (char *) malloc((unsigned) strlen(s) + 1)) == NULL)
	nomem();

    (void) strcpy(p, s);
    return p;
}

/*----------------------------------------------------------------------
 * Emergency exit for memory loss.
 */

NORETURN void
nomem()
{
    error("out of memory");
    leave(1);
}

/*----------------------------------------------------------------------
 * Return the last component of the given pathname.
 */

char *
basename(name)
char *name;
{
    char *b;

    if ((b = strrchr(name, '/')) != NULL)
	++b;
    else
	b = name;

    return (b);
}

/*----------------------------------------------------------------------
 * Return an allocated string containing the name of a (probably)
 * unique temporary file in the same directory as the given file.
 */

char *
unique(path)
char *path;
{
    static int uhostlen = 6;
    char *upath, *ubase;
    unsigned sequence, pid4;
    int i;

    upath = zalloc(strlen(path) + 20);
    (void) strcpy(upath, path);
    ubase = basename(upath);

    sequence = 0;
    if (strlen(hostname) > (unsigned)uhostlen)
    {
	/*
	 * Use extra characters of hostname to randomize sequence.
	 * (The magic "33" is brought to you courtesy of Chris Torek.)
	 */

	for (i = uhostlen; hostname[i]; ++i)
	    sequence = (sequence * 33) + hostname[i];
    }

    pid4 = (unsigned)getpid() & (unsigned)0xFFFF;

    for (i = 0; i < 32; ++i)
    {
	struct stat st;

	++sequence;
	(void) sprintf(ubase, "_%02X%04X.%.*s",
		       sequence & 0xFF, pid4, uhostlen, hostname);

	if (stat(upath, &st) == -1)
	{
	    if (errno == ENOENT)
		return upath;

	    syserr("can't stat %s", upath);
	    break;
	}
    }

    free(upath);
    return NULL;
}

/*----------------------------------------------------------------------
 * Return an allocated string containing an absolute pathname equal
 * to the given pathname when interpreted relative to the given
 * directory.  (No fancy checking for "." and ".." -- sorry.)
 */

char *
relpath(dir, file)
char *dir, *file;
{
    char *path;
    unsigned n;

    if (file[0] == '/')
	return copystr(file);

    n = strlen(dir) + strlen(file) + 2;
    path = zalloc(n);

    (void) strcpy(path, dir);
    n = strlen(path);
    if (n == 0 || path[n - 1] != '/')
	path[n++] = '/';
    (void) strcpy(path + n, file);

    return path;
}
