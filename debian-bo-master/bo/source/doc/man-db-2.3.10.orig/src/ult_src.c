/*
 * ult_src.c
 *
 * Copyright (C), 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * code to seek out the original (ultimate) source man file for
 * any specified man file. Soft and hard links and .so inclusions
 * are traced. Use: reduce amount of cat files to a minimum.
 *
 * Mon May  2 11:14:28 BST 1994 Wilf. (G.Wilford@ee.surrey.ac.uk)
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#ifndef STDC_HEADERS
extern int errno;
#endif

#ifndef S_ISLNK /* loosing sys/stat.h */
#  if defined(S_IFMT) && defined(S_IFLNK)
#    define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)
#  endif
#endif

#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#else /* not HAVE_DIRENT_H */
#  define dirent direct
#  ifdef HAVE_SYS_NDIR_H
#    include <sys/ndir.h>
#  endif /* HAVE_SYS_NDIR_H */
#  ifdef HAVE_SYS_DIR_H
#    include <sys/dir.h>
#  endif /* HAVE_SYS_DIR_H */
#  ifdef HAVE_NDIR_H
#    include <ndir.h>
#  endif /* HAVE_NDIR_H */
#endif /* HAVE_DIRENT_H  */

extern char *realpath();

#if defined(STDC_HEADERS)
#  include <string.h>
#  include <stdlib.h>
#elif defined(HAVE_STRING_H)
#  include <string.h>
#elif defined(HAVE_STRINGS_H)
#  include <strings.h>
#else /* no string(s) header */
extern char *strrchr();
#endif /* no string(s) header */

#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

#if defined(HAVE_LIMITS_H) && defined(_POSIX_VERSION)
#  include <limits.h>                     /* for PATH_MAX */
#else /* !(HAVE_LIMITS_H && _POSIX_VERSION) */
#  include <sys/param.h>                  /* for MAXPATHLEN */
#endif /* HAVE_LIMITS_H */

#ifndef PATH_MAX
#  if defined(_POSIX_VERSION) && defined(_POSIX_PATH_MAX)
#    define PATH_MAX _POSIX_PATH_MAX
#  else /* !_POSIX_VERSION */
#    ifdef MAXPATHLEN
#      define PATH_MAX MAXPATHLEN
#    else /* !MAXPATHLEN */
#      define PATH_MAX 1024
#    endif /* MAXPATHLEN */
#  endif /* _POSIX_VERSION */
#endif /* !PATH_MAX */

#define NLS_SET	u_sSet
#include "nls/nls.h"

#include "manconfig.h"
#include "lib/error.h"
#include "ult_src.h"

/* Find minimum value hard link filename for given file and inode */
static __inline__ char *ult_hardlink(char *fullpath, long inode)
{
	DIR *mdir;
	struct dirent *manlist;
	char link[PATH_MAX];
	char dir[PATH_MAX];
	char *t;

	*(t = strrchr(fullpath, '/')) = '\0';
	(void) strcpy(dir, fullpath);
	*t = '/';
	(void) strcpy(link, ++t);

	if ((mdir = opendir(dir)) == NULL) {
		error (0, errno,
		       CATGETS(u_s_OPENDIR, "can't search directory %s"),
		       dir);
		return NULL;
	}

	while ((manlist = readdir(mdir))) {
		if (manlist->d_ino == inode &&
		  strcmp(link, manlist->d_name) > 0) {
		  	(void) strcpy(link, manlist->d_name);
			if (debug)
				fprintf(stderr, "ult_hardlink: (%s)\n", link);
		}
	}
	closedir(mdir);

	/* If we already are the link with the smallest name value */
	/* return NULL */
	
	if (strcmp(link, t) == 0)
		return NULL;

	return strcpy(strrchr(fullpath, '/') + 1, link);
}

#ifdef S_ISLNK
/* use realpath() to resolve all sym links within 'fullpath'. 'mantree' is 
   the man hierarchy */
static __inline__ char *ult_softlink(char *fullpath, const char *mantree)
{
	char resolved_path[PATH_MAX];

	if (realpath(fullpath, resolved_path) == NULL) {

		/* discard the unresolved path */

		if (errno == ENOENT)
			error (0, 0,
			       CATGETS (u_s_DANGLE,
					"warning: %s is a dangling symlink"),
			       resolved_path);
		else
			error (0, errno,
			       CATGETS (u_s_RESOLVE, "can't resolve %s"),
			       resolved_path);
		return NULL;
	}

	if (debug)
		fprintf(stderr, "ult_softlink: (%s)\n", 
		  resolved_path + strlen(mantree) + 1);
		
	/* if symlink is not within this mantree, forget it */

	if (strncmp(mantree, resolved_path, strlen(mantree)) != 0) {
		if (debug)
			fprintf(stderr, "ult_softlink: non-local link\n");
		return NULL;
	}
	
	return strcpy(fullpath, resolved_path);
}
#endif /* S_ISLNK */

/* test `buffer' to see if it contains a .so include, if so and it's not an 
   absolute filename, copy it into `basename' at `rel' and return 1 */
static __inline__ int test_for_include(char *buffer, char *rel)
{
	/* strip out any leading whitespace (if any) */
	while (isspace( (int) *buffer))
		buffer++;

	/* see if the `command' is a .so */
	if (strncmp(buffer, ".so", 3) == 0) {
		buffer += 3;

		/* strip out any whitespace between the command and 
		   it's argumant */
		while (isspace( (int) *buffer))
			buffer++;

		/* If .so's argument is an absolute filename, it could be
		   either (i) a macro inclusion, (ii) a non local manual page
		   or (iii) a (somewhat bogus) reference to a local manual 
		   page.
		   
		   If (i) or (ii), we must not follow the reference. (iii) is
		   a problem with the manual page, thus we don't want to 
		   follow any absolute inclusions in our quest for the 
		   ultimate source file */
		if (*buffer != '/') {
			/* copy filename into rel address */
			while (*buffer && !isspace( (int) *buffer))
				*(rel++) = *(buffer++);
			
			*rel = '\0';
			return 1;
		}
	} 
	return 0;
}

/*
 * recursive function which finds the ultimate source file by following
 * any ".so filename" directives in the first line of the man pages.
 * Also (optionally) traces symlinks and hard links(!).
 *
 * name is full pathname, path is the MANPATH directory (/usr/man)
 * flags is a combination of SO_LINK | SOFT_LINK | HARD_LINK
 */
char *ult_src(char *name, const char *path, struct stat *buf, int flags)
{

	static char basename[2048];	/* must be static */
	static short recurse; 		/* must be static */
	static char *relative; 		/* must be static */

	/* initialise the function */

	/* as ult_softlink() & ult_hardlink() do all of their respective
	   resolving in one call, only need to sort them out once */
	   
	if (recurse == 0) {
		struct stat new_buf;
		(void) strcpy(basename, name);
		relative = basename + strlen(path) + 1;

		if (debug)
			fprintf(stderr, "\nult_src: File %s\n", name);

		/* If we don't have a buf, allocate and assign one */
		if ( !buf && ((flags & SOFT_LINK) || (flags & HARD_LINK)) ) {
			buf = &new_buf;
			if (lstat(basename, buf) == -1) {
				error (0, errno,
				       CATGETS (u_s_RESOLVE, "can't resolve %s"),
				       basename);
				return NULL;
			}
		}
	
#ifdef S_ISLNK
		/* Permit semi local (inter-tree) soft links */
		
		if (flags & SOFT_LINK) {
			if (S_ISLNK(buf->st_mode))
				/* Is a symlink, resolve it. */
				(void) ult_softlink(basename, path); 
		}
#endif /* S_ISLNK */
	
		/* Only deal with local (inter-dir) HARD links */
		
		if (flags & HARD_LINK) {
			if (buf->st_nlink > 1)
				/* Has HARD links, find least value */
				(void) ult_hardlink(basename, buf->st_ino); 
		}
	}

	/* keep a check on recursion level */
	else if (recurse == 10) {
		error (0, 0,
		       CATGETS(u_s_SELF_REF, "%s is self referencing"),
		       name);
		return NULL;
	}

	if (flags & SO_LINK) {
		char buffer[1024];
		int val;
		FILE *fp;
#ifdef COMP_SRC
		struct compression *comp;

		/* get rid of the previous ztemp file (if any) */
		remove_ztemp();
		
		/* if we are handed the name of a compressed file, remove
		   the compression extension? */
		if ( (comp = comp_info(basename)) ) {
			*comp->file = '\0';
		}

		/* if the open fails, try looking for compressed */
		if ((fp = fopen (basename, "r")) == NULL) {
			char *filename;
			
			if ( (comp = comp_file(basename)) ) {
				filename = decompress(comp->file, comp);
				free(comp->file);
				(void) strcat(basename, ".");
				(void) strcat(basename, comp->ext);
				fp = fopen (filename, "r");
			} else
				filename = basename;

			if (!fp) {
				error (0, errno,
				       CATGETS(u_s_OPEN, "can't open %s"),
				       filename);
				return NULL;
			}
		}
#else
		if ((fp = fopen (basename, "r")) == NULL) {
			error (0, errno,
			       CATGETS(u_s_OPEN, "can't open %s"),
			       basename);
			return NULL;
		}
#endif
		/* make sure that we skip over any comments */
		do { 
			(void) fgets(buffer, 1024, fp);
		}
			while (buffer && strncmp(buffer, ".\\\"", 3) == 0);
		 
		fclose(fp);
		
		if (buffer)
			val = test_for_include(buffer, relative);
		else
			val = EOF;

		if (val == 1) {			/* keep on looking... */
			char *ult;

			if (debug)
				fprintf(stderr, "ult_src: points to %s\n", basename);
	
			recurse++;
			ult = ult_src(basename, path, NULL, flags);
			recurse--;
	
			return ult;
		}
	}
	
	/* We have the ultimate source */
	return basename;
}
