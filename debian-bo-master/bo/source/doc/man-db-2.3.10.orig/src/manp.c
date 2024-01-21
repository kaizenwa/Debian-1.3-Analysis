/*
 * manp.c
 *
 * Copyright (c) 1990, 1991, John W. Eaton.
 * Copyright (C) 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * John W. Eaton
 * jwe@che.utexas.edu
 * Department of Chemical Engineering
 * The University of Texas at Austin
 * Austin, Texas  78712
 *
 * Wed May  4 15:44:47 BST 1994 Wilf. (G.Wilford@ee.surrey.ac.uk): changes
 * to get_dirlist() and manpath().
 *
 * This whole code segment is unfriendly and could do with a complete 
 * overhaul.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <errno.h>

#if defined(STDC_HEADERS)
#  include <stdlib.h>
#  include <string.h>
#elif defined(HAVE_STRING_H)
#  include <string.h>
#elif defined(HAVE_STRINGS_H)
#  include <strings.h>
#else /* no string(s) header */
extern char *strtok();
extern char *strpbrk();
extern char *strchr();
extern char *strstr();
#endif

#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#else
#  ifdef HAVE_GETCWD_H
extern char *getcwd();
#  else
extern char *getwd();
#  endif
#endif /* HAVE_UNISTD_H */

#ifndef STDC_HEADERS
extern char *getenv();
extern int errno;
#endif

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

#define NLS_SET	manpSet
#include "nls/nls.h"

#include "manconfig.h"
#include "lib/error.h"
#include "manp.h"

typedef struct {
	char mandir[PATH_MAX];
	char bin[PATH_MAX];
	int mandatory;
} DIRLIST;

#define MANDB_MAP       -1
#define MANPATH_MAP      0
#define MANDATORY        1

DIRLIST list[MAXDIRS];
char *tmplist[MAXDIRS];
char *manpathlist[MAXDIRS];

static __inline__ int get_dirlist(void);
static __inline__ char *get_manpath(char *path);
static __inline__ char *has_subdirs(char *p);
static __inline__ char *fsstnd(char *path);
static char *def_path(int flag);
static void add_dir_to_list(char **lp, char *dir);
static char **add_dir_to_path_list(char **mp, char *p);


static __inline__ void gripe_reading_mp_config (char *file)
{
	if (!quiet)
		error (FAIL, 0,
		       CATGETS(manp_PARSE_CONF,
			       "can't make sense of the manpath configuration file %s"), 
		       file);
}

static __inline__ void gripe_stat_file(char *file)
{
	if (!quiet)
		error (0, errno,
		       CATGETS(manp_STAT, "warning: %s"), file);
}

static __inline__ void gripe_not_directory(char *dir)
{
	if (!quiet)
		error (0, 0,
		       CATGETS(manp_NOT_DIR, "warning: %s isn't a directory"),
		       dir);
}

/* accept a manpath list, separated with ':', return the associated 
   catpath list */
char *cat_manpath(char *manp)
{
	DIRLIST *manlist;
	size_t len = 0;
	char *catp, *path;

	catp = xstrdup(manp);

	for (path = strtok(catp, ":"); path; path = strtok(NULL, ":")) {
		for (manlist = list; manlist->mandir[0] != '\0'; manlist++)
			if (manlist->mandatory == MANDB_MAP
			  && strcmp(path, manlist->mandir) == 0) {
				len += strlen (manlist->bin) + 1;
				break;
			}
		if (manlist->mandir[0] == '\0')
			len += strlen(path) + 1;
	}

	catp = (char *) xrealloc (catp, len + 1);

	*catp = '\0';

	for (path = strtok(manp, ":"); path; path = strtok(NULL, ":")) {
		(void) strcat(catp, ":");
		for (manlist = list; manlist->mandir[0] != '\0'; manlist++)
			if (manlist->mandatory == MANDB_MAP
			  && strcmp(path, manlist->mandir) == 0) {
				(void) strcat(catp, manlist->bin);
				break;
			}
		if (manlist->mandir[0] == '\0') 
			(void) strcat(catp, path);
	}
	
	return ++catp;
}		

char *add_nls_manpath (char *manpathlist, const char *locale)
{
#ifdef HAVE_SETLOCALE
	char *manpath = NULL;
	char *path;

	if (locale == NULL || *locale == '\0')
		return manpathlist;

	for (path = strtok(manpathlist, ":"); path; 
	     path = strtok(NULL, ":") ) {
	     
		char *testdir;
		int test;
	     	
		testdir = strappend (NULL, path, "/", locale, NULL);
	     	test = is_directory(testdir);

	     	if ( test == 1 ) {
	     		if (debug)
	     			fprintf(stderr, 
	     				"add_nls_manpath(): adding %s\n",
	     				testdir);
	     		if (!manpath)
	     			manpath = xstrdup (testdir);
	     		else
	     			manpath = strappend (manpath, ":", testdir,
	     					     NULL);
	     	} else if ( test == 0 ) 
	     		gripe_not_directory(testdir);
	     	
	     	free(testdir);

		if (!manpath)
			manpath = xstrdup (path);
		else
			manpath = strappend (manpath, ":", path, NULL);

	 }
	 free(manpathlist);
#if 0
	 if (debug)
	 	fprintf(stderr, "path inc nls = %s\n", manpath);
#endif
	 return manpath;
#else /* !HAVE_SETLOCALE */
	return manpathlist;
#endif /* HAVE_SETLOCALE */
}

/* Add to the manpathlist system dependent subdirs */
char *add_system_manpath (char *systems, char *manpathlist)
{
	const char *system;
	char *path;
	char *manpath = NULL;
	size_t manpathsize = 0;

	if (systems == NULL)
		if ((systems = getenv ("SYSTEM")) == NULL)
			return manpathlist;
	if (*systems == '\0')
		return manpathlist;

	/* For each manpathlist[] component */

	for (system = strtok (systems, ",:"); system;
	     system = strtok (NULL, ",:")) {
		char *next;

		/* For each systems component */

		if (strcmp(system, "man") != 0) {
			for (path = manpathlist; path; 
			     path = next) {
				size_t pathlen;
				size_t newdirlen;
				char *newdir;
				int status;
	
				next = strpbrk (path, ":" );
				pathlen = next ? next++ - path : strlen (path);
				newdirlen = strlen (system) + pathlen + 3;
				newdir = (char *) xmalloc (newdirlen);
	
				/* add it to the manpath component */
	
				strncpy (newdir, path, pathlen);
				*(newdir + pathlen) = '\0';
				strcat (newdir, "/");
				strcat (newdir, system);
	
				/* and see if it exists */
	
				status = is_directory (newdir);
	
				if (status == 0)
					gripe_not_directory(newdir);
				else if (status == 1) {
					if (debug)
						fprintf (stderr,
							 "adding %s to manpathlist\n",
							 newdir);
					manpath = (char *) xrealloc (manpath,
							   manpathsize + newdirlen);
					/* Add to manpath */
					if (manpathsize)
						strcat (manpath, ":");
					else
						*manpath = '\0';
					strcat (manpath, newdir);
					manpathsize += newdirlen;
				} else if (debug) {
					fputs ("can't stat ", stderr);
					perror (newdir);
				}
				free (newdir);
			}
		} else {
			/* Add base directory */
			size_t pathlen;

			pathlen = strlen(manpathlist) + 1;
			manpath = (char *) xrealloc (manpath, manpathsize + pathlen);
			if (manpathsize)
				strcat (manpath, ":");
			else
				*manpath = '\0';
			strcat (manpath, manpathlist);
			manpathsize += pathlen;
		}
	}
	if (!manpath)
		error(FAIL, 0, CATGETS(manp_NULL_MANPATH,
				    "internal manpath equates to NULL"));
	return manpath;
}

/*
 * Always add system and locale directories to pathlist.
 * If the environment variable MANPATH is set, return it.
 * If the environment variable PATH is set and has a nonzero length,
 * try to determine the corresponding manpath, otherwise, return the
 * default manpath.
 *
 * The manpath.config file is used to map system wide /bin directories
 * to top level man page directories.
 *
 * For directories which are in the user's path but not in the
 * manpath.config file, see if there is a subdirectory `man' or `MAN'.
 * If so, add that directory to the path.  Example:  user has
 * $HOME/bin in his path and the directory $HOME/bin/man exists -- the
 * directory $HOME/bin/man will be added to the manpath.
 */
char *manpath (char *systems)
{
	int len;
	char *manpathlist;
	char *path;

	/* need to read config file even if MANPATH set, for mandb(8) */
	
	if (get_dirlist ())
		gripe_reading_mp_config (CONFIG_FILE);

	if ((manpathlist = getenv ("MANPATH")) ) {
		/*
    		 * This must be it.
    		 */
		if (!quiet)
			error (0, 0,
			       CATGETS(manp_MANPATH,
				       "warning: $MANPATH set, ignoring %s"),
			       CONFIG_FILE);
		/* xstrdup() required below so that we don't hack up the
		   environment */

		manpathlist = add_system_manpath (systems, xstrdup (manpathlist));

		return manpathlist;
	}

	if ((path = getenv ("PATH")) == NULL) {
		/*
		 * Things aren't going to work well, but hey...
		 */
		if (!quiet)
			error (0, 0, CATGETS(manp_NO_PATH,
					     "warning: $PATH not set"));
		
		path = add_system_manpath (systems, def_path (MANDATORY));

		return path;
	} else {
		if ((len = strlen (path)) == 0) {
			/*
			 * Things aren't going to work well here either...
			 */
			if (!quiet)
				error (0, 0,
				       CATGETS(manp_MT_PATH,
					       "warning: empty $PATH"));
			
			path = add_system_manpath (systems, def_path (MANDATORY));

			return path;
		}

		path = add_system_manpath (systems, get_manpath (path));

		return path;
	}
}

/* Parse the manpath.config file, extracting appropriate information. */
static __inline__ int get_dirlist (void)
{
	char *bp;
	char buf[BUFSIZ];
	DIRLIST *dlp = list;
	FILE *config;
	int converted;

	if ((config = fopen (CONFIG_FILE, "r")) == NULL)
		error (FAIL, 0,
		       CATGETS(manp_OPEN_CONF, "can't open the manpath configuration file %s"),
		       CONFIG_FILE);

	while ( (bp = fgets (buf, BUFSIZ, config)) ) {

		while ( isspace(*bp) )
			bp++;
			
		if (*bp == '#' || *bp == '\0') {
			continue;
		} else if (sscanf(bp, "MANBIN %*s") == 1) {
			continue;
		} else if (sscanf(bp, "MANDATORY_MANPATH %s", dlp->mandir) == 1) {
			dlp->mandatory = MANDATORY;
			if (debug)
				fprintf (stderr, "found mandatory man directory %s\n",
				  dlp->mandir); 
			dlp++;
		} else if (sscanf(bp, "MANPATH_MAP %s %s", dlp->bin, 
		  dlp->mandir) == 2) {
			dlp->mandatory = MANPATH_MAP;
			if (debug)
				fprintf (stderr, "found manpath map %s --> %s\n",
				  dlp->bin, dlp->mandir);
			dlp++;
		} else if ( (converted = 
		  sscanf(bp, "MANDB_MAP %s %s", dlp->mandir, dlp->bin)) > 0) {
		  	dlp->mandatory = MANDB_MAP;
		  	if (is_directory(dlp->mandir) != 1)
		  		continue;
		  	if (converted == 1) {
		  		(void) strcpy(dlp->bin, dlp->mandir);
		  	} else if (converted == 2 && 
		  	         strcmp(dlp->bin, "FSSTND") == 0) {
		  	        char *fsstnd_catpath;
		  		(void) strcpy(dlp->bin, 
		  		              fsstnd_catpath = fsstnd(dlp->mandir));
		  		free(fsstnd_catpath);
		  	}
		  	if (debug)
		  		fprintf (stderr, 
		  		  "found global mandir %s mapped to catdir %s\n",
		  		  dlp->mandir, dlp->bin);
		  	dlp++;
	 	} else {
			error (0, 0,
			       CATGETS(manp_PARSE_DIRS,
				       "can't parse directory list `%s'"),
			       bp);
			gripe_reading_mp_config (CONFIG_FILE);
		}
	}

	dlp->bin[0] = dlp->mandir[0] = '\0';
	dlp->mandatory = 0;
	fclose(config);

	return 0;
}

/*
 * Construct the default manpath.  This picks up mandatory manpaths
 * only.
 */
static char *def_path (int flag)
{
	int len = 0;
	char *manpathlist; 
	DIRLIST *dlp;

	dlp = list;
	while (dlp->mandatory == flag) {
		  len += strlen (dlp->mandir) + 1;
		  dlp++;
	}

	/* If we have complete config file failure... */
	if (len == 0)
		return xstrdup("/usr/man");

	manpathlist = (char *) xmalloc (len);
	*manpathlist = '\0';

	dlp = list;
	while (dlp->mandatory == flag) {
		int status;

		status = is_directory (dlp->mandir);

		if (status < 0) {
			gripe_stat_file(dlp->mandir);
		} else if (status == 0 && !quiet) {
			error (0, 0,
			       CATGETS(manp_MISS_DIR,
				       "warning: mandatory directory %s doesn't exist"),
			       dlp->mandir);
		} else if (status == 1){
			(void) strcat(manpathlist, dlp->mandir);
			(void) strcat(manpathlist, ":"); 
			dlp++;
		}
	}
	return manpathlist;
}

/*
 * For each directory in the user's path, see if it is one of the
 * directories listed in the manpath.config file.  If so, and it is
 * not already in the manpath, add it.  If the directory is not listed
 * in the manpath.config file, see if there is a subdirectory `man' or
 * `MAN'.  If so, and it is not already in the manpath, add it.
 * Example:  user has $HOME/bin in his path and the directory
 * $HOME/bin/man exists -- the directory $HOME/bin/man will be added
 * to the manpath.
 */
static __inline__ char *get_manpath (char *path)
{
	int len;
	char *tmppath;
	char *t;
	char *p;
	char **lp;
	char *end;
	char *manpathlist;
	DIRLIST *dlp;

	tmppath = xstrdup (path);

	for (p = tmppath;; p = end + 1){
	
		if ((end = strchr (p, ':')))
			*end = '\0';

		if (debug)
			fprintf (stderr, "\npath directory %s ", p);

		/*
      		 * The directory we're working on is in the config file.
      		 * If we haven't added it to the list yet, do.
      		 */

		for (dlp = list; dlp->mandir[0] != '\0'; dlp++)
			if (dlp->bin[0] != '\0' && !strcmp (p, dlp->bin)) {
				if (debug)
					fprintf (stderr, "is in the config file\n");

				 add_dir_to_list (tmplist, dlp->mandir);
				 goto found;
			}

		/*
      		 * The directory we're working on isn't in the config file.  See
      		 * if it has man or MAN subdirectories.  If so, and it hasn't
      		 * been added to the list, do.
      		 */

		 if (debug)
			fprintf (stderr, "is not in the config file\n");

		 t = has_subdirs (p);
		 if (t != NULL) {
			if (debug)
				fprintf (stderr, "but it does have a man or MAN subdirectory\n");

			 add_dir_to_list (tmplist, t);
			 free (t);
		  } else
			if (debug)
				fprintf (stderr, "and doesn't have man or MAN subdirectories\n");

	found:

		  if (!end)
			break;
	}

	free(tmppath);

	if (debug)
		fprintf (stderr, "\nadding mandatory man directories\n\n");

	dlp = list;
	while (dlp->mandatory == MANDATORY) {
		add_dir_to_list (tmplist, dlp->mandir);
		dlp++;
	}

	len = 0;
	lp = tmplist;
	while (*lp != NULL) {
		len += strlen (*lp) + 1;
		lp++;
	}

	assert (len);
	manpathlist = (char *) xmalloc (len);
	*manpathlist = '\0';

	lp = tmplist;
	p = manpathlist;
	while (*lp != NULL) {
		len = strlen (*lp);
		memcpy (p, *lp, len);
		p += len;
		*p++ = ':';
		lp++;
	}

	p[-1] = '\0';

	return manpathlist;
}

/*
 * Add a directory to the manpath list if it isn't already there.
 */
static void add_dir_to_list (char **lp, char *dir)
{
	int status;

	while (*lp != NULL){

		if (!strcmp (*lp, dir)){
			if (debug)
				fprintf (stderr, "%s is already in the manpath\n", dir);
			return;
		}
		lp++;
	}

	/*
  	 * Not found -- add it.
  	 */
  	 
	status = is_directory (dir);

	if (status < 0)
		gripe_stat_file(dir);
	else if (status == 0)
		gripe_not_directory(dir);
	else if (status == 1) {
		if (debug)
			fprintf (stderr, "adding %s to manpath\n", dir);

		*lp = xstrdup (dir);
	}
}

/*
 * Check to see if the current directory has man or MAN
 * subdirectories.
 *
 * Unsure if this is such a good idea... Should at least do a realpath()
 * on anything picked up!
 */
static __inline__ char *has_subdirs (char *p)
{
	size_t len;
	char *t;

	len = strlen (p);

	t = (char *) xmalloc (len + 5);

	(void) strcpy(t, p);
	(void) strcpy(t + len, "/man");

	if (is_directory (t) == 1)
		return t;

#if 0
	(void) strcpy (t + len, "/MAN");

	if (is_directory (t) == 1)
		return t;

	free (t);
#endif
	return NULL;
}

static __inline__ char **add_dir_to_path_list (char **mp, char *p)
{
	int status;
	char wd[PATH_MAX];
	char *cwd = wd;

	status = is_directory (p);

	if (status < 0)
		gripe_stat_file(p);
	else if (status == 0)
		gripe_not_directory(p);
	else {
		/* deal with relative paths */

		if (*p != '/') {
#ifdef HAVE_GETCWD
			if ( !getcwd(cwd, PATH_MAX - 2 - strlen(p)) )
#else /* no GETCWD */
			if ( !getwd(cwd) )
#endif /* HAVE_GETCWD */
				error (FATAL, errno,
				       CATGETS(manp_CWD, "can't determine current directory"));
			(void) strcat(wd, "/");
			(void) strcat(wd, p);
			*mp = xstrdup(wd);
		} else 
			*mp = xstrdup (p);

		if (!strstr(*mp, "/man") && !strstr(*mp, "/MAN")) {

			/* not really a man tree after all */

			if (!quiet)
				error (0, 0,
				       CATGETS(manp_NO_TREE, "warning: %s does not have a man tree component"), 
				       *mp);
			free(*mp);
		} else {
			if (debug)
				fprintf (stderr, 
				  "adding %s to manpathlist\n", 
				  *mp);
			mp++;
		}
	}
	return mp;
}

void create_pathlist(char *manp, char **mp)
{
	char *p, *end;

	/*
	 * Expand the manpath into a list for easier handling.
	 */

	for (p = manp;; p = end + 1) {
		if ( (end = strchr (p, ':')) )
			*end = '\0';

		mp = add_dir_to_path_list (mp, p);
		if (end == NULL)
			break;

		*end = ':';
	}
	*mp = NULL;
	free(manp);
}

/* routine to get global manpath list */
char *get_mandb_manpath(void)
{
	size_t len = 0;
	char *manpathlist;
	DIRLIST *dlp;
	/* extern DIRLIST list[]; mappings set up by manpath() */

	for (dlp = list; dlp->mandir[0] != '\0'; dlp++)
		if (dlp->mandatory == MANDB_MAP)
			len += strlen (dlp->mandir) + 1;

	/* see if we have anything at all */

	if (!len)
		return NULL;

	manpathlist = (char *) xmalloc (len + 1);
	*manpathlist = '\0';

	for (dlp = list; dlp->mandir[0] != '\0'; dlp++) {
		if (dlp->mandatory == MANDB_MAP) {
			(void) strcat(manpathlist, ":");
			(void) strcat(manpathlist, dlp->mandir);
		}
	}
	return ++manpathlist;
}

/*
 * Take manpath or manfile path as arg, return catdir mapping or NULL if
 * it isn't a global mandir.
 */
/* This routine would seem to work correctly for nls subdirs and would 
   specify the (correct) consistent catpath even if not defined in the 
   config file */
char *global_catpath(char *name)
{
	DIRLIST *dlp;

	for (dlp = list; dlp->mandir[0] != '\0'; dlp++) {
		size_t mandir_length = strlen (dlp->mandir);

		if (dlp->mandatory == MANDB_MAP &&
		    strncmp(name, dlp->mandir, mandir_length) == 0) {
			char *suffix = name + mandir_length;
			char *catpath;

			catpath = xmalloc (  strlen (dlp->bin)
					   + strlen (suffix) + 1);
			(void) strcpy (catpath, dlp->bin);
			if (*suffix == '/') {
				++suffix;
				(void) strcat (catpath, "/");
			}
			if (!strncmp (suffix, "man", 3)) {
				suffix += 3;
				(void) strcat (catpath, "cat");
			}
			(void) strcat (catpath, suffix);
		  	return catpath;
		}
	}
	return NULL;
}

/* check to see if the supplied man directory is considered `global' */
int is_global_mandir(const char *dir)
{
	DIRLIST *dlp;
	
	for (dlp = list; dlp->mandir[0] != '\0'; dlp++)
		if (dlp->mandatory == MANDB_MAP &&
		    strncmp(dir, dlp->mandir, strlen(dlp->mandir)) == 0)
			return 1;
	return 0;
}

/* Accept a manpath (not a full pathname to a file) and return an FSSTND 
   equivalent catpath */
static __inline__ char *fsstnd(char *path)
{
	char *manpath;
	char *catpath;
	char *element;
	
	if (strncmp(path, MAN_ROOT, sizeof MAN_ROOT - 1) != 0) {
		if (!quiet)
			error (0, 0,
			       CATGETS(manp_PREFIX,
				       "warning: %s does not begin with %s"),
			       path, MAN_ROOT);
		return xstrdup(path);
	}
	/* get rid of initial "/usr" */
	path += sizeof MAN_ROOT - 1;
	manpath = xstrdup (path);
	catpath = (char *) xmalloc (strlen(path) + sizeof CAT_ROOT -  3);
#if 0
	if (debug)
		fprintf(stderr, "fsstnd size = %d\n", strlen(path) + 
			sizeof CAT_ROOT - 4);
#endif
	/* start with CAT_ROOT */ 
	(void) strcpy (catpath, CAT_ROOT);

	/* split up path into elements and deal with accordingly */
	for (element = strtok(manpath, "/"); element; element = strtok(NULL, "/")) {
		if (strncmp(element, "man", 3) == 0) {
			if (*(element + 3)) { 
#if 0
				*element -= 10;		/* [Mm] -> [Cc] */
				*(element + 2) += 6;	/* [Nn] -> [Tt] */
#endif
				*element = 'c';
				*(element + 2) = 't';
			} else
				continue;
		} 
		(void) strcat (catpath, "/");
		(void) strcat (catpath, element);
	}
	free(manpath);
#if 0
	if (debug)
		fprintf(stderr, "fsstnd: %s -> %s\n", path, catpath);
#endif
	return catpath;
}
