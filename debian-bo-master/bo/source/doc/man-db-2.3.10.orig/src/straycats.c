/*
 * straycats.c: find and process stray cat files
 *
 * Copyright (C), 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * Tue May  3 21:24:51 BST 1994 Wilf. (G.Wilford@ee.surrey.ac.uk)
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <signal.h>
#include <errno.h>

#ifndef STDC_HEADERS
extern int errno;
#endif

#if defined(STDC_HEADERS)
#  include <string.h>
#  include <stdlib.h>
#elif defined(HAVE_STRING_H)
#  include <string.h>
#elif defined(HAVE_STRINGS_H)
#  include <strings.h>
#else /* no string(s) header */
extern char *strrchr();
#endif /* STDC_HEADERS */

#include <sys/types.h>
#include <sys/stat.h>

#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

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

#define NLS_SET	straySet
#include "nls/nls.h"

#include "manconfig.h"
#include "libdb/mydbm.h"
#include "libdb/db_storage.h"
#include "lib/error.h"
#include "manp.h"

static char *temp_name;
static char *catdir, *mandir;

/* prototype here as it uses struct mandata which is defined in db_storage.h */
extern int splitline(char *raw_whatis, struct mandata *info, char *base_name);

static __inline__ int check_for_stray(void)
{
	DIR *cdir;
	struct dirent *catlist;
	size_t lenman, lencat;
	int strays = 0;
	
	if ( !(cdir = opendir(catdir))) {
		error (0, errno,
		       CATGETS(stray_OPENDIR, "can't search directory %s"),
		       catdir);
		return 0;
	}

	mandir = strappend(mandir, "/", NULL);
	catdir = strappend(catdir, "/", NULL);
	lenman = strlen(mandir);
	lencat = strlen(catdir);
		
	while ((catlist = readdir(cdir))) {
		struct mandata info;
		char *ext, *section;
		short found;
		struct stat buf;
#ifdef COMP_SRC
		struct compression *comp;
#endif
		
		if (*catlist->d_name == '.' && 
		     strlen(catlist->d_name) < (size_t) 3)
			continue;

		*(mandir + lenman) = *(catdir + lencat) = '\0';
		mandir = strappend(mandir, catlist->d_name, NULL);
		catdir = strappend(catdir, catlist->d_name, NULL);

		ext = strrchr(mandir, '.');
		if (!ext) {
			if (!quiet)
				error (0, 0,
				       CATGETS(stray_BOGUS, "warning: %s: ignoring bogus filename"),
				       catdir);
			continue;
			
#if defined(COMP_SRC) || defined(COMP_CAT)

#  if defined(COMP_SRC)
		} else if (comp_info(ext)) {
#  elif defined(COMP_CAT)
		} else if (strcmp(ext + 1, COMPRESS_EXT) == 0) {
#  endif /* COMP_* */
			*ext = '\0';
			info.comp = ext + 1;
#endif /* COMP_SRC || COMP_CAT */

		} else
			info.comp = NULL;
			
		ext = strrchr(mandir, '.');
		*(mandir + lenman - 1) = '\0';
		section = xstrdup(strrchr(mandir, '/') + 4);
		*(mandir + lenman - 1) = '/';

		/* check for bogosity */
		
		if (!ext || strncmp(ext + 1, section, strlen(section)) != 0) {
		  	if (!quiet)
				error (0, 0,
				       CATGETS(stray_BOGUS, "warning: %s: ignoring bogus filename"),
			  	       catdir);
			free(section);
			continue;
		}

		/* 
		 * now that we've stripped off the cat compression
		 * extension (if it has one), we can try some of ours.
		 */

		if (debug)
			fprintf(stderr, "Testing for existence: %s\n", mandir);

		if (stat(mandir, &buf) == 0) 
			found = 1;
#ifdef COMP_SRC 
		else if ( (comp = comp_file(mandir)) ) {
			found = 1;
			free(comp->file);
		}
#endif
		else 
			found = 0;
			
		if (!found) { 
			char *filter, *whatis;
			struct mandata *exists;

			/* we have a straycat. Need to filter it and get
			   its whatis (if necessary)  */

			*(ext++) = '\0';
			info.ext = ext;

			/* see if we already have it, before going any 
			   further */
			exists = dblookup_exact(basename(mandir), info.ext);
#ifndef FAVOUR_STRAYCATS
			if (exists && exists->id != WHATIS_CAT) {
#else /* FAVOUR_STRAYCATS */
			if (exists && exists->id != WHATIS_CAT &&
			    exists->id != WHATIS_MAN) {
#endif /* !FAVOUR_STRAYCATS */
				free_mandata_struct(exists);
				free(section);
				continue;
			}
			if (debug)
				fprintf(stderr, "%s(%s) is not in the db.\n",
					basename(mandir), info.ext);

			/* fill in the missing parts of the structure */
			info.sec = section;
			info.id = STRAY_CAT;
			info.pointer = NULL;
			info._st_mtime = 0L;

			/* Check to see how to filter the cat file */
#if defined(COMP_SRC)
			if (info.comp)
				filter = strappend(NULL, 
					           comp_info(catdir)->prog, " ",
					           catdir, " | " COL " -bx > ",
					           temp_name, NULL);
			else
#elif defined (COMP_CAT)
			if (info.comp)
				filter = strappend(NULL, DECOMPRESSOR " ",
						   catdir, " | " COL " -bx > ",
						   temp_name, NULL);
			else
#endif /* COMP_* */
				filter = strappend(NULL, COL " -bx < ",
						   catdir, " > ", temp_name,
						   NULL);
					
			if (do_system(filter) != 0) {
				remove(temp_name);
				if (debug)
					perror(filter);
				exit (CHILD_FAIL);
			}
			
			free(filter);

			strays++;
			
			if ( (whatis = find_name(temp_name, basename(catdir),
						 CATPAGE)) == NULL)
				if (!quiet)
					error (0, 0,
					       CATGETS(stray_NO_WHATIS,
						       "warning: %s: whatis parse for %s(%s) failed"),
					       catdir, basename(mandir), info.sec);

			(void) splitline(whatis, &info, basename(mandir));

			if (whatis)
				free(whatis);
		} 
		free(section);
	}
	closedir(cdir);
	return strays;
}

static int open_catdir(void)
{
	DIR *cdir;
	struct dirent *catlist;
	size_t catlen, manlen;
	int strays = 0;

	if ( !(cdir = opendir(catdir))) {
		error (0, errno,
		       CATGETS(stray_OPENDIR, "can't search directory %s"),
		       catdir);
		return 0;
	}

	if (!quiet)
		printf(CATGETS(stray_CHECK,
			       "Checking for stray cats under %s...\n"), catdir);

	catdir = strappend(catdir, "/", NULL);
	mandir = strappend(mandir, "/", NULL);
	catlen = strlen(catdir);
	manlen = strlen(mandir);
		
	/* should make this case insensitive */
	while ((catlist = readdir(cdir))) {
		char *t1;

		if (strncmp(catlist->d_name, "cat", 3) != 0)
			continue;

		catdir = strappend(catdir, catlist->d_name, NULL);
		mandir = strappend(mandir, catlist->d_name, NULL);

		*(t1 = mandir + manlen) = 'm';
		*(t1 + 2) = 'n';
		
		strays += check_for_stray();

		*(catdir + catlen) = *(mandir + manlen) = '\0';
	}
	closedir(cdir);
	return strays;
}

int straycats(char *manpath)
{
	char *catpath;
	int strays;

	if (!temp_name)
		temp_name = tempnam(NULL, "zcat");
	
	if ( !(dbf = MYDBM_RWOPEN(database)) || dbver_rd(dbf)) {
		error (0, errno,
		       CATGETS(stray_UPDATE_DB, "warning: can't update index cache %s"),
		       database);
		return 0;
	}
	
	catpath = global_catpath(manpath);
	
	/* look in the usual catpath location */
	mandir = xstrdup(manpath);
	catdir = xstrdup(manpath);
	strays = open_catdir(); 

	/* look in the alternate catpath location if we have one 
	   and it's different from the usual catpath */
	
	if (debug && catpath)
		fprintf(stderr, "catpath: %s, manpath: %s\n", catpath, manpath);
		
	if (catpath && strcmp(catpath, manpath) != 0) {
		*mandir = *catdir = '\0';
		mandir = strappend(mandir, manpath, NULL);
		catdir = strappend(catdir, catpath, NULL);
		strays += open_catdir();
	}

	free(mandir);
	free(catdir);

	if (catpath)
		free(catpath);

	MYDBM_CLOSE(dbf);
	remove (temp_name);
	return strays;
}
