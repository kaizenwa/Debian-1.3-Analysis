/* @(#) mkdbm.c,v 1.9 1996/02/16 14:57:44 woods Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * mkdbm.c:
 *	Take a list of lines with a key followed by a colon character
 *	or white space followed by data and build a dbm database from
 *	them.  If the flag -f is given, downcase the keys.
 *
 *	Usage:  mkdbm [-o database] [-fvndy] [input_file ...]
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "smail.h"
#include "dys.h"
#include "exitcodes.h"
#ifdef	UNIX_BSD
# include <sys/file.h>
#endif
#ifdef	UNIX_SYS5
# include <fcntl.h>
#endif
#include "dbm_compat.h"
#ifndef DEPEND
# include "extern.h"
#endif

#if !defined(HAVE_GETHOSTNAME) && defined(HAVE_UNAME)
#include <sys/utsname.h>
#endif

char *program;				/* argv[0] from main */
/* variables local to this file */
static char *out_name = NULL;
static char *temp_dir = NULL;
static char *temp_pag = NULL;
static int downcase = FALSE;
static int ypstamps = FALSE;
static int verbose = FALSE;
static int no_at_record = FALSE;
static int no_nul_byte = FALSE;
static int reccnt = 0;
static int maxlen = 0;

/* functions local to this file */
static void done();
static char **read_args();
static char *build_temp_name();
static void create_database();
static void add_to_database();
static void add_ending_record();
static char *getline();
static void rename_database();

/*ARGSUSED*/
void
main(argc, argv)
    int argc;
    char **argv;
{
    char **files;
    char *temp_name;

    program = *argv++;

    files = read_args(argv);

    if (out_name == NULL) {
	if (files[0] && ! EQ(files[0], "-")) {
	    out_name = files[0];
	} else {
	    out_name = "dbm";
	}
    }

    temp_name = build_temp_name(out_name);
    create_database(temp_name);

    if (*files) {
	while (*files) {
	    FILE *f;

	    if (EQ(*files, "-")) {
		add_to_database(stdin);
		files++;
		continue;
	    }
	    f = fopen(*files, "r");
	    if (f == NULL) {
		(void) fprintf(stderr, "%s: cannot open ", program);
		(void) perror(*files);
		done(EX_NOINPUT);
	    }
	    add_to_database(f);
	    files++;
	}
    } else {
	add_to_database(stdin);
    }


    if (ypstamps) {
	add_yp_stamps();
    }

    if (! no_at_record) {
	add_ending_record();
    }

    rename_database(temp_name, out_name);

    if (verbose) {
	(void) printf("Added %d records, longest record was %d bytes\n",
		      reccnt, maxlen);
    }

    dbmclose();
    exit(EX_OK);
}

add_yp_stamps()
{
	datum key_lastmod, val_lastmod;
	datum key_master,  val_master;
	long now;
	char nows[32];

#ifdef HAVE_GETHOSTNAME
	char host[256];

	if(gethostname(host, sizeof(host)) < 0) {
		done(EX_DATAERR);
	}
#else
#ifdef HAVE_UNAME
	struct utsname utsname;
	char *host;

	(void) uname(&utsname);
	host = utsname.nodename;
#else
#ifdef SITENAME_FILE
	char host[4096];
	FILE *f;
	char *p;

	f = fopen(SITENAME_FILE, "r");
	if (f == NULL) {
		fprintf(stderr, "%s: cannot open ", program);
		perror(SITENAME_FILE);
		done(EX_OSFILE);
	}
	host[0] = '\0';
	fgets(host, sizeof(host), f);
	if (host[0] == '\0') {
		fprintf(stderr, "%s: no hostname found in %s\n",
			program, SITENAME_FILE);
		exit(EX_OSFILE);
	}
	p = strchr(host, '\n');
	if (p)
		*p = '\0';
	fclose(f);
#else
	fprintf(stderr, "%s: No known method for computing hostname\n",
		program);
	done(EX_USAGE);
#endif	/* SITENAME_FILE */
#endif	/* HAVE_UNAME */
#endif	/* HAVE_GETHOSTNAME */

	(void) time(&now);
	sprintf(nows, "%10.10ld", now);

	key_lastmod.dptr  = "YP_LAST_MODIFIED";
	key_lastmod.dsize = strlen(key_lastmod.dptr);
	val_lastmod.dptr  = nows;
	val_lastmod.dsize = strlen(val_lastmod.dptr);

	key_master.dptr   = "YP_MASTER_NAME";
	key_master.dsize  = strlen(key_master.dptr);
	val_master.dptr   = host;
	val_master.dsize  = strlen(val_master.dptr);

	if (store(key_lastmod, val_lastmod) < 0) {
		(void) fprintf(stderr, "%s: store failed for %s\n",
			       program, key_lastmod.dptr);
		done(EX_DATAERR);
	}

	if (store(key_master, val_master) < 0) {
		(void) fprintf(stderr, "%s: store failed for %s\n",
			       program, key_master.dptr);
		done(EX_DATAERR);
	}
}

/*
 * for errors or signals, remove the temp files
 */
static void
done(ex)
    int ex;
{
    dbmclose();
    (void) unlink(temp_dir);
    (void) unlink(temp_pag);
    exit(ex);
}

/*
 * read through the arglist and turn argv into list of input files
 */
static char **
read_args(args)
    char **args;
{
    char **filev = args;
    char **filep = args;

    while (*args) {
	if ((*args)[0] == '-' && (*args)[1] != '\0') {
	    static char *end_arg = "";
	    char *s = &(*args)[1];

	    while (*s) {
		switch (*s++) {
		case 'y':
		    ypstamps = TRUE;
		    break;

		case 'f':
		    downcase = TRUE;
		    break;

		case 'v':
		    verbose = TRUE;
		    break;

		case 'n':
		    no_nul_byte = TRUE;
		    break;

		case 'd':
		    no_at_record = TRUE;
		    break;

		case 'o':
		    if (*s != '\0') {
			out_name = s;
			s = end_arg;
		    } else {
			out_name = *++args;
		    }
		    break;

		default:
		    (void) fprintf(stderr,
			      "Usage: %s [-yfvnd] [-o database] [file ...]\n",
				   program);
		    exit(EX_USAGE);
		}
	    }
	} else {
	    *filep++ = *args;
	}
	args++;
    }

    *filep = NULL;
    return filev;
}

/*
 * build a temp filename in the same directory as the given file
 */
static char *
build_temp_name(name)
    char *name;
{
    char *fn;
    char *slash = rindex(name, '/');
    extern char *mktemp();

    if (slash) {
	fn = xmalloc((unsigned)(slash - name + sizeof("/dbmXXXXXX")));
	(void) strncpy(fn, name, slash - name);
	(void) strcat(fn, "/dbmXXXXXX");
    } else {
	fn = xmalloc(sizeof("dbmXXXXXX"));
	strcpy(fn, "dbmXXXXXX");
    }
    return mktemp(fn);
}

/*
 * create a DBM database with the given basename
 */
static void
create_database(dbm_name)
    char *dbm_name;
{
    int fd;
    int temp_len = strlen(dbm_name);

#ifdef DBM_SUFFIX	/* db emulation of ndbm */

    temp_dir = xmalloc(temp_len + sizeof(DBM_SUFFIX));
    (void) sprintf(temp_dir, "%s%s", dbm_name, DBM_SUFFIX);

#else

    temp_dir = xmalloc(temp_len + sizeof(".dir"));
    temp_pag = xmalloc(temp_len + sizeof(".pag"));
    (void) sprintf(temp_dir, "%s.dir", dbm_name);
    (void) sprintf(temp_pag, "%s.pag", dbm_name);

    fd = creat(temp_dir, 0644);
    if (fd < 0) {
	(void) fprintf(stderr, "%s: cannot creat ", program);
	(void) perror(temp_dir);
	exit(EX_TEMPFAIL);
    }
    (void) close(fd);

    fd = creat(temp_pag, 0644);
    if (fd < 0) {
	(void) fprintf(stderr, "%s: cannot creat ", program);
	(void) perror(temp_pag);
	(void) unlink(temp_dir);
	exit(EX_TEMPFAIL);
    }
    (void) close(fd);

#endif

    /* open the new DBM database */
    if (dbminit(dbm_name) < 0) {
	(void) fprintf(stderr, "%s: dbminit failed ", program);
	(void) perror(dbm_name);
	done(EX_TEMPFAIL);
    }
}

/*
 * add all entries in the file to the DBM database
 */
static void
add_to_database(f)
    FILE *f;
{
    register char *s;

    while (s = getline(f)) {
	register char *data;
	datum key, content;

	for (data = s; *data && *data != ':' && !isspace(*data); data++) ;

	if (*data) {
	    *data++ = '\0';
	    key.dptr = s;
	    key.dsize = data - s - 1;
	    content.dptr = data;
	    content.dsize = strlen(data);
	    if (! no_nul_byte) {
		key.dsize++;
		content.dsize++;
	    }
	    if (downcase) {
		register char *sp;

		for (sp = s; *sp; sp++) {
		    *sp = lowercase(*sp);
		}
	    }
	    if (store(key, content) < 0) {
		(void) fprintf(stderr, "%s: store failed for %s\n",
			       program, s);
		done(EX_DATAERR);
	    }
	    reccnt++;
	    if (content.dsize + key.dsize > maxlen) {
		maxlen = content.dsize + key.dsize;
	    }
	} else {
	    (void) fprintf(stderr, "%s: no value for key %s, ignored\n",
			   program, s);
	}
    }
}

/*
 * add the ending `@' record, to make sendmail happy.
 */
static void
add_ending_record()
{
    datum key, content;

    content.dptr = key.dptr = "@";
    content.dsize = key.dsize = (no_nul_byte? 1: 2);
    if (store(key, content) < 0) {
	(void) fprintf(stderr, "%s: store failed for end record\n", program);
    }
}

/*
 * read and return one line from the given file.
 *
 * return NULL on end of input.
 */
static char *
getline(f)
    register FILE *f;
{
    static struct str str;
    static int inited = FALSE;
    register int c;

    if (! inited) {
	STR_INIT(&str);
	inited = TRUE;
    } else {
	str.i = 0;
    }
    while ((c = getc(f)) != EOF && c != '\n') {
	STR_NEXT(&str, c);
    }
    if (c == EOF && str.i == 0) {
	return NULL;
    }
    STR_NEXT(&str, '\0');
    return str.p;
}

/*
 * rename the database from the old name to a new one.
 *
 * Allow some time after unlinking the old name so that smail will not
 * open the .dir file of the old database and the .pag file of the new
 * one accidentally.
 */
static void
rename_database(from, to)
    char *from;
    char *to;
{
    unsigned int fromlen = (unsigned)strlen(from);
    unsigned int tolen = (unsigned)strlen(to);
    char *from_dir = xmalloc(fromlen + sizeof(".dir"));
    char *from_pag = xmalloc(fromlen + sizeof(".pag"));
    char *to_dir = xmalloc(tolen + sizeof(".dir"));
    char *to_pag = xmalloc(tolen + sizeof(".pag"));

#ifdef DBM_SUFFIX
    (void) sprintf(from_dir, "%s%s", from, DBM_SUFFIX);
    (void) sprintf(to_dir, "%s%s", to, DBM_SUFFIX);
#else
    (void) sprintf(from_dir, "%s.dir", from);
    (void) sprintf(from_pag, "%s.pag", from);
    (void) sprintf(to_dir, "%s.dir", to);
    (void) sprintf(to_pag, "%s.pag", to);

    (void) unlink(to_pag);
#endif

    (void) unlink(to_dir);
    (void) sleep(2);			/* sleep at least 1 second */
    if (link(from_dir, to_dir) < 0) {
	(void) fprintf(stderr, "%s: cannot link %s to ", program, from_dir);
	(void) perror(to_dir);
	done(EX_CANTCREAT);
    }
#ifndef DBM_SUFFIX
    if (link(from_pag, to_pag) < 0) {
	(void) fprintf(stderr, "%s: cannot link %s to ", program, from_pag);
	(void) perror(to_pag);
	done(EX_CANTCREAT);
    }
    (void) unlink(from_pag);
#endif
    (void) unlink(from_dir);
}

/*
 * standalone versions of some referenced routines
 */
char *
xmalloc(len)
    unsigned int len;
{
    char *malloc();
    register char *ret = malloc(len);

    if (ret == NULL) {
	(void) fprintf(stderr, "%s: out of memory!\n", program);
	exit(EX_OSERR);
    }
    return ret;
}

char *
xrealloc(s, len)
    char *s;
    unsigned int len;
{
    char *realloc();
    register char *ret = realloc(s, len);

    if (ret == NULL) {
	(void) fprintf(stderr, "%s: out of memory!\n", program);
	exit(EX_OSERR);
    }
    return ret;
}
