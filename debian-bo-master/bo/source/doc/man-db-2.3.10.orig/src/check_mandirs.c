/*
 * check_mandirs.c: used to auto-update the database caches
 *
 * Copyright (C), 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * Mon May  2 17:36:33 BST 1994  Wilf. (G.Wilford@ee.surrey.ac.uk)
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <errno.h>

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

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

#if defined(STDC_HEADERS)
#  include <string.h>
#  include <stdlib.h>
#elif defined(HAVE_STRING_H)
#  include <string.h>
#elif defined(HAVE_STRINGS_H)
#  include <strings.h>
#else /* no string(s) header */
extern char *strchr(), *strrchr(), *strstr();
#endif /* no string(s) header */

#ifndef STDC_HEADERS
extern time_t time();
extern int errno;
#endif

#define NLS_SET	c_mSet
#include "nls/nls.h"

#include "manconfig.h"
#include "libdb/mydbm.h"
#include "libdb/db_storage.h"
#include "lib/error.h"
#include "ult_src.h"
#include "hashtable.h"

int pages;

static void gripe_bogus_manpage(char *manpage)
{
	if (!quiet)
		error (0, 0,
		       CATGETS(c_m_BOGUS, "warning: %s: ignoring bogus filename"), 
		       manpage);
}	  	  

static void gripe_multi_extensions(const char *path, const char *sec, 
				   const char *name, const char *ext)
{
	if (!quiet)
		error (0, 0,
		       CATGETS(c_m_MULT_EXT, 
			       "warning: %s/man%s/%s.%s*: competing extensions"),
		       path, sec, name, ext);
}

char *make_filename(const char *path, const char *name, 
		    struct mandata *in, char *type)
{
	static char *file;
	
	file = (char *) xrealloc (file, sizeof "//." + strlen(path) + 
				  strlen(type) + strlen(in->sec) +
				  strlen(name) + strlen(in->ext));
				   
	(void) sprintf(file, "%s/%s%s/%s.%s", path, type, in->sec, name, 
		       in->ext);

	if (*in->comp != '-')	/* Is there an extension ? */
		file = strappend(file, ".", in->comp, NULL);

	return file;
}

int splitline(char *raw_whatis, struct mandata *info, char *base_name)
{
	char *comma;
	int ret;

	if (raw_whatis) {
		char *whatis;

		if ( (whatis = strstr(raw_whatis, " - ")) ) {
			info->whatis = whatis + 3;
			*whatis = '\0';
		} else {
			raw_whatis = NULL; /* kill entire whatis line */
			info->whatis = NULL;
		}
	} else
		info->whatis = NULL;
	
	/* Here we store the direct reference */
	if (debug)
		fprintf(stderr, "%s\n", base_name);

	ret = dbstore(info, base_name);
	if (ret > 0)
		return ret;

	/* if there are no indirect references, just go on to the 
	   next file */

	if (!raw_whatis || strchr(raw_whatis, ',') == NULL)
		return 0;

	/* If there are...  */
		
	if (info->id < STRAY_CAT)
		info->id = WHATIS_MAN;
	else
		info->id = WHATIS_CAT;

	/* don't waste space storing the whatis in the db */
	info->whatis = NULL;
	info->pointer = base_name; 
	
	while( (comma = strrchr(raw_whatis, ',')) != NULL) {
		*comma = '\0';
		comma += 2;

		/* If we've already dealt with it, ignore */
		
		if (strcmp(comma, base_name) != 0) {
			if (debug)
				fprintf(stderr, "%s\n", comma);
			ret = dbstore(info, comma);
			if (ret > 0)
				return ret;
		}
	}

	/* If we've already dealt with it, ignore */
		
	if (strcmp(raw_whatis, base_name) == 0)
		return 0;
		
	if (debug)
		fprintf(stderr, "%s\n", raw_whatis);
	ret = dbstore(info, raw_whatis);
	if (ret > 0)
		return ret;

	return 0;
}

/* take absolute filename and path (for ult_src) and do sanity checks on 
   file. Also check that file is non zero in length and is not already in
   the db. If not, find it's ult_src() and see if we have the whatis cached, 
   otherwise cache it incase we trace another manpage back to it. Next, store 
   it in the db along with any references found in the whatis. */
void test_manfile(char *file, const char *path)
{
	char *base_name, *othername, *ult, *sep, *whatis;
	char *manpage;
	struct mandata info, *exists;
	struct nlist *in_cache;
	struct stat buf;

#ifdef COMP_SRC
	struct compression *comp;
	size_t len;
#endif /* COMP_SRC */

	manpage = xstrdup(file);
	base_name = basename(manpage);

	/* Bogus files either have (i) no period, ie no extension, (ii)
	   a compression extension, but no sectional extension, (iii)
	   a missmatch between the section they are under and the
	   sectional part of their extension. */

#ifdef COMP_SRC
	if ( (comp = comp_info(base_name)) ) {
		info.comp = comp->ext;
		*(comp->file) = '\0';		/* to strip the comp ext */
	} else
		info.comp = NULL;

	len = strlen(manpage);
#else /* !COMP_SRC */	
	info.comp = NULL;
#endif /* COMP_SRC */

	if ( !(info.ext = strrchr(base_name, '.'))) {

		/* no section extension */
		gripe_bogus_manpage(file);
		free(manpage);
		return;
	}

	*(info.ext++) = '\0';			/* set section ext */
	*(base_name - 1) = '\0';		/* strip '/base_name' */ 
	info.sec = strrchr(manpage, '/') + 4;	/* set section name */

	if (strncmp(info.sec, info.ext, strlen(info.sec)) != 0) {

		/* missmatch in extension */
		gripe_bogus_manpage(file);
		free(manpage);
		return;
	}

	/* to get mtime info */
	(void) lstat(file, &buf);
	info._st_mtime = buf.st_mtime;
	
	/* check that our file actually contains some data */
	if (buf.st_size == 0) {
		/* man_db pre 2.3 place holder ? */
		free(manpage);
		return;
	}

	/* see if we already have it, before going any further, this will
	   save both an ult_src() a find_name(), amongst other time wastes */
	exists = dblookup_exact(base_name, info.ext);

	/* Ensure we really have the actual page. Gzip keeps the mtime
	   the same when it compresses, so we have to compare comp 
	   extensions also */

	if (exists) {
		if (strcmp(exists->comp, info.comp ? info.comp : "-") == 0) {
			if (exists->_st_mtime == info._st_mtime 
			    && exists->id < WHATIS_MAN) {
				free_mandata_struct(exists);
				free(manpage);
				return;
			}
		} else {
			struct stat physical;
			
			/* see if the cached file actually exists. It's 
			   evident at this point that we have multiple 
			   comp extensions */
			if (debug)
				fprintf(stderr, "test_manfile(): stat %s\n",
					make_filename(path, base_name, exists, "man"));
			if (stat(make_filename(path, base_name, exists, "man"), 
				 &physical) == -1)
				 	dbdelete(base_name, exists);
			else {
				gripe_multi_extensions(path, exists->sec,
						       base_name, exists->ext);
				free_mandata_struct(exists);
				free(manpage);
				return;
			}
		}
		free_mandata_struct(exists);
	}

	/* Trace the file to its ultimate source, else we'll be looking
	   for whatis info in files containing only '.so manx/foo.x', which
	   will give us an unobtainable whatis for the entry. */
	ult = ult_src(file, path, &buf, SO_LINK | SOFT_LINK | HARD_LINK);

	if (!ult) {
		error (0, 0,
		       CATGETS(c_m_BAD_INCL,
			       "warning: %s: bad symlink or ROFF `.so' request"),
		       file);
		free(manpage);
		return;
	}

	pages++;			/* pages seen so far */
	info.pointer = NULL;		/* we have a direct page (so far) */

#ifdef COMP_SRC
	if (strncmp(ult, file, len) == 0)
#else /* not COMP_SRC */
	if (strcmp(ult, file) == 0)
#endif /* COMP_SRC */

		info.id = ULT_MAN;	/* ultimate source file */
	else
		info.id = SO_MAN;	/* .so, sym or hard linked file */

	/* Ok, here goes: Use a hash tree to store the ult_srcs with
	   their whatis. Anytime after, check the hash tree, if it's there, 
	   use it. This saves us a find_name() which is a real hog */

	/* could use strrchr(ult, '/') + 1 as hash text, but not worth it */

	in_cache = lookup(ult);

	if (in_cache) {		/* cache hit */
		whatis = in_cache->defn ? xstrdup(in_cache->defn) : NULL;
	} else {		/* cache miss */
		/* go get the whatis info in its raw state */
#ifdef COMP_SRC
		/* if the nroff was compressed, an uncompressed version is
		   shown by a call to get_ztemp(), grog this for a whatis
		   rather than ult. This is a bit difficult to follow, sorry:
		   ult_src() will leave the last uncompressed nroff file it
		   has to deal with in get_ztemp() */
		char *ztemp;
		
		if ( (ztemp = get_ztemp()) ) {
			whatis = find_name(ztemp, basename(file), MANPAGE);
			remove_ztemp();	/* get rid of temp file identifier */
		} else
#endif /* COMP_SRC */
			whatis = find_name(ult, basename(file), MANPAGE);
			
		(void) install_text(ult, whatis);
	}

	if (debug)
		fprintf(stderr, "\"%s\"\n", whatis);

	/* split up the raw whatis data and store references */

	if (whatis) {
		char save_id = info.id;
		info.id = WHATIS_MAN;
		while ((sep = strrchr(whatis, 0x11))) {
			*(sep++) = '\0';
			othername = xstrdup(sep);
			*(othername + strcspn(othername, " -")) = '\0';
			(void) splitline(sep, &info, othername);
			free(othername);
		}
		info.id = save_id;
	} else {
		(void) lstat(ult, &buf);
		if (buf.st_size == 0) {
			if (!quiet)
				error (0, 0,
				       CATGETS(c_m_MT_MAN,
					       "warning: %s: ignoring empty file"),
				       ult);
			free(manpage);
			return;
		}
		if (!quiet)
			error (0, 0,
			       CATGETS(c_m_NO_WHATIS,
				       "warning: %s: whatis parse for %s(%s) failed"),
			       ult, base_name, info.ext);
	}
	if (splitline(whatis, &info, base_name) == 1)
		gripe_multi_extensions(path, info.sec, base_name, info.ext);

	free(manpage);
	if (whatis)
		free(whatis);
}

static __inline__ void add_dir_entries(const char *path, char *infile)
{
	char *manpage;
	int len;
	struct dirent *newdir;
	DIR *dir;

	manpage = strappend(NULL, path, "/", infile, "/", NULL);
	len = strlen(manpage);

	/*
	 * All filename entries in this dir should either be valid manpages
	 * or . files (such as current, parent dir).
	 */

	if ( !(dir = opendir(infile)) ) {
		error (0, errno,
		       CATGETS(c_m_OPENDIR, "can't search directory %s"),
		       manpage);
		free(manpage);
                return;
        }
        
        /* strlen(newdir->d_name) could be replaced by newdir->d_reclen */
        
	while ( (newdir = readdir(dir)) )
		if ( !(*newdir->d_name == '.' && 
		       strlen(newdir->d_name) < (size_t) 3) ) {
			manpage = strappend(manpage, newdir->d_name, NULL);
			test_manfile(manpage, path);
			*(manpage + len) = '\0';
		}
		
	free(manpage);
	closedir(dir);
}

/*
 * accepts the raw man dir tree eg. "/usr/man" and the time stored in the db
 * any dirs of the tree that have been modified (ie added to) will then be
 * scanned for new files, which are then added to the db.
 */
static short testmandirs(const char *path, time_t last)
{
	DIR *dir;
	struct dirent *mandir;
	struct stat stbuf;
	short amount = 0;

	if (debug)
		fprintf(stderr, "Testing %s for new files\n", path);

	if ( !(dir = opendir(path)) ) {
		error (0, errno,
		       CATGETS(c_m_OPENDIR, "can't search directory %s"), path);
		return 0;
	}

	chdir(path);

	while( (mandir = readdir(dir)) ) {
		if ( strncmp(mandir->d_name, "man", 3) != 0 )
			continue;
			
		if ( stat(mandir->d_name, &stbuf) == 0 
		  && (stbuf.st_mode&S_IFDIR) && stbuf.st_mtime > last) {

			if (debug)
				fprintf(stderr,
				  "\tsubdirectory %s has been 'modified'\n",
				  mandir->d_name);

			if ( (dbf = MYDBM_RWOPEN(database)) == NULL) {
				/* rwopen(database); */
#ifdef MAN_DB_UPDATES
				if (!quiet)
#endif /* MAN_DB_UPDATES */
				  error (0, errno,
				         CATGETS(c_m_UPDATE_DB, 
				         "can't update index cache %s"),
				         database);
				return 0;
			}

			if (quiet)
				fprintf(stderr, 
					CATGETS(c_m_UPDATE_DB_MSG, 
						"\rUpdating index cache for path `%s'. Wait..."), 
						path);
		  	add_dir_entries(path, mandir->d_name);
		  	MYDBM_CLOSE(dbf);
		  	amount++;
		}
	}
	closedir(dir);

	/* clean out the whatis hashtable for new hierarchy */
	if (amount > 0)
		free_hashtab();

	return amount;
}

/* update the time key stored within `database' */
void update_db_time(void)
{
	datum key, content;
#ifdef FAST_BTREE
	datum key1, content1;
#endif /* FAST_BTREE */

	key.dptr = KEY;
	key.dsize = sizeof KEY;
	content.dptr = (char *) xmalloc(16); /* 11 is max long with '\0' */
	sprintf(content.dptr, "%ld", (long)time(NULL));
	content.dsize = strlen(content.dptr) + 1;

	/* Open the db in RW to store the $mtime$ ID */
	/* we know that this should succeed because we just updated the db! */
	if ( (dbf = MYDBM_RWOPEN(database)) == NULL) {
#ifdef MAN_DB_UPDATES
		if (!quiet)
#endif /* MAN_DB_UPDATES */
		  error (0, errno,
		         CATGETS(c_m_UPDATE_DB, 
		         "can't update index cache %s"),
		         database);
		free(content.dptr);
		return;
	}
#ifndef FAST_BTREE
	MYDBM_REPLACE(dbf, key, content);
#else /* FAST_BTREE */
	key1.dptr = KEY;
	key1.dsize = sizeof KEY;

	(dbf->seq)(dbf, (DBT *) &key1, (DBT *) &content1, R_CURSOR);
	
	if (strcmp(key1.dptr, key.dptr) == 0)
		(dbf->put)(dbf, (DBT *) &key, (DBT *) &content, R_CURSOR);
	else
		(dbf->put)(dbf, (DBT *) &key, (DBT *) &content, 0);
#endif /* !FAST_BTREE */

	MYDBM_CLOSE(dbf);
	free(content.dptr);
}

/* remove the db's time key - called prior to update_db if we want
   to `force' a full consistency check */
void reset_db_time(void)
{
	datum key;

	key.dptr = KEY;
	key.dsize = sizeof KEY;

	/* we don't really care if we can't open it RW - it's not fatal */
	if ( (dbf = MYDBM_RWOPEN(database)) == NULL) {
		if (debug) {
			fprintf(stderr, "reset_db_time(): ");
			perror("can't open db");
		}
		return;
	}

	MYDBM_DELETE(dbf, key);
	if (debug)
		fprintf(stderr, "reset_db_time()\n");
	MYDBM_CLOSE(dbf);
}

/* routine to prepare/create the db prior to calling testmandirs() */
short create_db(const char *manpath)
{
	short amount;
	
	if (debug)
		fprintf(stderr, "create_db(%s): %s\n", manpath, database);

	/* Open the db in CTRW mode to store the $ver$ ID */

	if ( (dbf = MYDBM_CTRWOPEN(database)) == NULL) {
		error (0, errno,
		       CATGETS(c_m_CREATE_DB, "can't create index cache %s"),
		       database);
		return 0;
		/* should really return EOF */
	}

	dbver_wr(dbf);
	MYDBM_CLOSE(dbf);

	amount = testmandirs(manpath, (time_t) 0);

	if (amount) {
		update_db_time();
		if (quiet)
			fputs(CATGETS(c_m_DONE, "done.\n"), stderr);
	}

	return amount;
}

/* routine to update the db, ensure that it is consistent with the 
   filesystem */
short update_db(const char *manpath)
{
	if ( (dbf = MYDBM_RDOPEN(database)) && !dbver_rd(dbf)) {
		datum key, content;
		short new;

		key.dptr = KEY;
		key.dsize = sizeof KEY;
		content = MYDBM_FETCH(dbf, key);
		MYDBM_CLOSE(dbf);

		if (debug)
			fprintf(stderr, "update_db(): %ld\n", content.dptr ? 
				atol(content.dptr) :
				0L);
		if (content.dptr) {
			new = testmandirs(manpath, (time_t) atol(content.dptr) );
			MYDBM_FREE(content.dptr);
		} else
			new = testmandirs(manpath, (time_t) 0);

		if (new) {
			update_db_time();
			if (quiet)
				fputs(CATGETS(c_m_DONE, "done.\n"), stderr);
		}
		
		return new;
	}
		
	if (debug)
		fprintf(stderr, "failed to open %s O_RDONLY\n", database);
		
	return EOF;
} 
