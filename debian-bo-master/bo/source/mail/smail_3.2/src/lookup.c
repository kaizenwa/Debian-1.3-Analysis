/*
#ident	"@(#)smail/src:RELEASE-3_2:lookup.c,v 1.27 1996/06/25 23:34:23 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * lookup.c:
 *	search for values corresponding to keys using a specified
 *	access method.
 *
 *	external functions:  open_database, close_database, lookup_database
 *
 * NeXT NetInfo aliases support added by Dan Danz <dan@az.stratus.com>
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#include "defs.h"
#ifdef	UNIX_BSD
# include <sys/file.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef	HAVE_YP
# include "jump.h"
# include <rpcsvc/ypclnt.h>
#endif
#include "smail.h"
#include "lookup.h"
#include "dys.h"
#include "exitcodes.h"
#ifdef HAVE_NIALIAS
# include <aliasdb.h>
#endif
#ifdef  HAVE_NISPLUS
# include <rpcsvc/nis.h>
#endif
#ifdef USE_LSEARCH_REGEXCMP
# ifdef UNIX_SYS5_4
#  include <regexpr.h>			/* simple form of the following */
# else
#  if defined(UNIX_SYS5_3) || defined(UNIX_SYS5_2) || defined(USE_REGEXP_H)
    /* most use "INIT" before other declaritions, Linux uses "INIT;" as the
     * last, luckily */
#   define INIT		register char *sp = instring;
#   define ESIZE	BUFSIZ
#   define GETC()	(*sp++)
#   define PEEKC()	(*sp)
#   define UNGETC(c)	(--sp)
#   define RETURN(c)	return (c);
#   define ERROR(c)	{ regerr = c; return ((char *) NULL); }
static int	regerr = 0;		/* error code from RE compiler */
#   include <regexp.h>			/* from UNIX SysV (incl. SunOS) or GNU */
#  else
#   include <regex.h>			/* P1003.2 C, Henry Spencer, 4.4BSD, XPG3 */
#  endif
# endif
#endif /* USE_LSEARCH_REGEXCMP */
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
#endif

#if defined(HAVE_DBM) && !defined(HAVE_NDBM)
# undef NULL
# include <dbm.h>
# undef NULL
# define NULL 0
#else
# ifdef HAVE_NDBM
#  include <ndbm.h>
# else
#  include "sdbm.h"
#  define HAVE_NDBM
# endif
#endif

#ifdef STANDALONE
# define xmalloc malloc
# define xrealloc realloc
# define xfree free
#endif	/* STANDALONE */

/* variables imported from libc */
extern int errno;

/* functions local to this file */
static int bsearch_open();
static void bsearch_close();
static int bsearch_lookup();

static int lsearch_open();
static void lsearch_close();
static int lsearch_lookup();

static int dbmbase_open();
static void dbmbase_close();
static int dbmbase_lookup();

#ifdef	HAVE_YP
static int yp_open();
static void yp_close();
static int yp_lookup();

static int aliasyp_open();
static void aliasyp_close();
static int aliasyp_lookup();
#endif

#ifdef HAVE_NIALIAS
static int aliasni_open();
static void aliasni_close();
static int aliasni_lookup();
#endif

#ifdef  HAVE_NISPLUS
static int nisplus_open();
static void nisplus_close();
static int nisplus_lookup();
#endif


/*
 * the following structure defines the available access methods.
 * Methods are identified by name when open_database is called.
 */
static struct proto {
    char *proto;			/* name of the access method */
    int (*open)();			/* open database function */
    void (*close)();			/* close database function */
    int (*lookup)();			/* lookup in database function */
} protos[] = {
    { "bsearch",			/* binary search */
      bsearch_open, bsearch_close, bsearch_lookup },
    { "lsearch",
      lsearch_open, lsearch_close, lsearch_lookup },
    { "dbm",				/* DBM database search */
      dbmbase_open, dbmbase_close, dbmbase_lookup },
#ifdef	HAVE_YP
    { "yp",				/* YP remote database search */
      yp_open, yp_close, yp_lookup },
    { "aliasyp",			/* mail.aliases-style YP database */
      aliasyp_open, aliasyp_close, aliasyp_lookup },
#endif
#ifdef HAVE_NIALIAS
    { "nialias",			/* NetInfo remote alias search */
      aliasni_open, aliasni_close, aliasni_lookup },
#endif /* HAVE_NIALIAS */
#ifdef  HAVE_NISPLUS
    { "nisplus",			/* NIS+ remote database search */
      nisplus_open, nisplus_close, nisplus_lookup },
#endif /* HAVE_NISPLUS */
#ifdef FIXME
    { NULL, NULL, NULL, NULL }
#endif
};

/* FIXME:  avoid this ugly stuff -- use null-entry termination semantics... */
/* point to end of protos table */
struct proto *end_protos = protos + sizeof(protos)/sizeof(protos[0]);

/* generic form of structure returned by database open calls */
struct generic_db {
    struct proto *proto;		/* access method */
};

/*
 * open_database - open a database of the specified type
 *
 * given a database name and an access method return a pointer to opaque
 * data that can be used to access that database.
 *
 * return:
 *   FILE_SUCCEED
 *		if the open was successful.  The database info will
 *		be stored in *db.
 *   FILE_AGAIN	if the open failed but may succeed at a later time
 *		(e.g., a remote host is down right now).  An error will
 *		be stored in *error.
 *   FILE_FAIL	if an unrecoverable failure occured.  An error will be
 *		stored in *error.
 *   FILE_NOMATCH
 *		if the database does not appear to exist.
 */
int
open_database(name, proto, retries, interval, statp, db, error)
    char *name;				/* name of database */
    char *proto;			/* access method name */
    int retries;			/* retry count */
    int interval;			/* retry interval */
    struct stat *statp;			/* return a stat buffer */
    char **db;				/* store open database info here */
    char **error;			/* store error message here */
{
    register struct proto *pp;

    for (pp = protos; proto && pp < end_protos; pp++) {
	if (EQ(proto, pp->proto)) {
	    /* found the requested access method */
	    return (*pp->open)(name, pp, retries, interval, statp, db, error);
	}
    }

    /* access method was not found */
    *error = "unknown access protocol";
    return FILE_FAIL;
}

/*
 * close_database - close an open database and free its resources
 */
void
close_database(priv)
    char *priv;				/* database's private data */
{
    register struct generic_db *gp = (struct generic_db *)priv;

    (*gp->proto->close)(gp);
}

/*
 * lookup_database - find a value corresponding to a key
 *
 * given an open database, perform a lookup operation to find a match for
 * a given key.
 *
 * Return:
 *   DB_SUCCEED if the lookup was successful.  The matched value will
 * 		be stored in *value.
 *   DB_NOMATCH	if the lookup operation did not find a match for the key.
 *   DB_AGAIN	if the lookup operation failed but may succeed at a later
 *		time (e.g., a remote host is down right now).  An error
 *		will be stored in *error.
 *   DB_FAIL	if an unrecoverable failure occured.  An error will be
 *		stored in *error.
 *   FILE_AGAIN	if the lookup operation failed because of an error accessing
 *		the file.  The file should be considered unreachable until
 *		some later time.
 *   FILE_FAIL	if the lookup operation failed because of a permanent error
 *		accessing the file.  Retrying at a later time is not assumed
 *		to be possible.
 */
int
lookup_database(db, key, value, error)
    char *db;				/* open database */
    char *key;				/* search key */
    char **value;			/* store value here */
    char **error;			/* store error message here */
{
    register struct generic_db *gdb = (struct generic_db *)db;

    return (*gdb->proto->lookup)(gdb, key, value, error);
}

/*
 * bsearch access method:
 *
 *	access a file containing sorted lines of data.  keys
 *	are at the start of each line followed by a colon and/or
 *	white space.
 */

/* private data: */
struct bsearch_db {
    struct proto *proto;		/* access method table entry */
    char *name;				/* name of file */
    FILE *f;				/* open file */
    long size;				/* size of file */
};

/* bsearch_open - open a sorted file */
static int
bsearch_open(name, proto, retries, interval, statp, db, error)
    char *name;				/* name of file */
    struct proto *proto;		/* access method */
    int retries;			/* retry count */
    int interval;			/* retry interval */
    struct stat *statp;			/* save stat results here */
    char **db;				/* store open database info here */
    char **error;			/* store error message here */
{
    register struct bsearch_db *priv;
    register FILE *f;

    name = make_lib_fn(name);
    if (name == NULL) {
	*error = "No directory for file";
	return FILE_FAIL;
    }
    f = fopen(name, "r");
    if (f == NULL) {
	int left = retries;

	while (left-- > 0) {
	    DEBUG2(DBG_DRIVER_HI, "bsearch_open: will retry open(%s) after %d seconds\n", name, interval);
	    (void) sleep(interval);
	    if ((f = fopen(name, "r"))) break;
	}
	if (f == NULL) {
	    if (errno == ENOENT) {
		return FILE_NOMATCH;
	    }
	    *error = strerror(errno);
	    return FILE_FAIL;
	}
    }

#ifdef lock_fd_rd_wait
    if (lock_fd_rd_wait(fileno(f)) < 0) {
	*error = "database is locked, try again later";
	return FILE_AGAIN;
    }
#endif

    /* seek to the end of the file */
    (void) fseek(f, 0L, 2);

    /* build the private data */
    priv = (struct bsearch_db *)xmalloc(sizeof(*priv));
    priv->proto = proto;
    priv->name = name;
    priv->f = f;
    priv->size = ftell(f);		/* remember the fseek() */
    if (statp) {
	(void) fstat(fileno(f), statp);
    }

    *db = (char *)priv;
    return FILE_SUCCEED;
}

/* bsearch_close - close the file */
static void
bsearch_close(db)
    struct bsearch_db *db;
{
    (void) fclose(db->f);
    xfree((char *)db);
}

/*
 * bsearch_lookup - look up key in ascii sorted key/data line database.
 *
 * This routine taken from smail version 2.3, though it has been
 * modified to work with the new version and has also been
 * generalized from the original routine getpath().
 */
/*ARGSUSED*/
static int
bsearch_lookup(db, key, value, error)
    register struct bsearch_db *db;
    char *key;
    char **value;
    char **error;
{
    long middle, hi, lo;
    int c;
    int flag;
    static struct str str;		/* string in which to store data */
    static int str_inited = FALSE;	/* TRUE if str has been STR_INIT'd */
    int i;				/* temp */

    DEBUG1(DBG_DRIVER_HI, "bsearch_lookup: looking for <%s>\n", key);

    if (!str_inited) {
	/*
	 * note, the string is reused in each call to bsearch.
	 */
	str_inited = TRUE;
	STR_INIT(&str);
    }

    lo = 0;
    hi = db->size;

    /*
     * "Binary search routines are never written right the first time around."
     * - Robert G. Sheldon.
     * << above comment retained 'cause I thought it was cute -- tron >>
     * << it is also true -- tron >>
     */
    for (;;) {
	middle = (hi + lo + 1)/2;
	(void) fseek(db->f, middle, 0); /* find midpoint */
	if (middle != 0) {		/* to beginning of next line */
	    while((c = getc(db->f)) != EOF && c != '\n') ;
	    if (c == EOF && ferror(db->f)) {
		*error = strerror(errno);
		clearerr(db->f);
		return FILE_FAIL;
	    }
	}
	str.i = 0;
	while ((c = getc(db->f)) != EOF && !isspace(c) && c != ':') {
	    STR_NEXT(&str, c);
	}
	if (c == EOF && ferror(db->f)) {
	    *error = strerror(errno);
	    clearerr(db->f);
	    return FILE_FAIL;
	}
	STR_NEXT(&str, '\0');
	flag = strcmpic(str.p, key);
	if (flag == 0)
	    break;
	if ( lo>=middle ) {		/* failure? */
	    return DB_NOMATCH;
	}
	if ( c != EOF && flag < 0 ) {	/* close window */
	    lo = middle;
	} else {
	    hi = middle - 1;
	}
    }

    /*
     * Now just copy the result.
     */
    i = -1;				/* index to last non-space */
    str.i = 0;				/* clear out the region */
    while(((c  = getc(db->f)) != EOF) && (c != '\n')) {
	if (!isspace(c)) {
	    if (c == '#') {
		/* comment puts an end to the entry */
		break;
	    }
	    i = str.i;
	}
	STR_NEXT(&str, c);
    }
    if (c == EOF && ferror(db->f)) {
	*error = strerror(errno);
	clearerr(db->f);
	return FILE_FAIL;
    }
    str.i = i + 1;			/* backup to last interesting char */
    /* backup to last non-space character */
    STR_NEXT(&str, '\0');

    DEBUG2(DBG_DRIVER_HI, "bsearch_lookup: found <%s> for <%s>\n", str.p, key);
    *value = str.p;
    return DB_SUCCEED;
}

/*
 * lsearch access method:
 *
 *	access a file containing lines of data, in the format expected
 *	by read_entry() in parse.c.  Keys are at the start of each line
 *	followed by a colon and/or white space.
 */

/* private data: */
struct lsearch_db {
    struct proto *proto;		/* access method table entry */
    char *name;				/* name of file */
    FILE *f;				/* open file */
};

/* lsearch_open - open a file */
static int
lsearch_open(name, proto, retries, interval, statp, db, error)
    char *name;				/* name of file */
    struct proto *proto;		/* access method */
    int retries;			/* retry count */
    int interval;			/* retry interval */
    struct stat *statp;			/* save stat results here */
    char **db;				/* store open database info here */
    char **error;			/* store error message here */
{
    register struct lsearch_db *priv;
    register FILE *f;

    name = make_lib_fn(name);
    if (name == NULL) {
	*error = "No directory for file";
	return FILE_FAIL;
    }
    f = fopen(name, "r");
    if (f == NULL) {
	int left = retries;

	while (left-- > 0) {
	    DEBUG2(DBG_DRIVER_HI, "lsearch_open: will retry open(%s) after %d seconds\n", name, interval);
	    (void) sleep(interval);
	    if ((f = fopen(name, "r"))) break;
	}
	if (f == NULL) {
	    if (errno == ENOENT) {
		return FILE_NOMATCH;
	    }
	    *error = strerror(errno);
	    return FILE_FAIL;
	}
    }

#ifdef lock_fd_rd_wait
    if (lock_fd_rd_wait(fileno(f)) < 0) {
	*error = "database is locked, try again later";
	return FILE_AGAIN;
    }
#endif

    /* build the private data */
    priv = (struct lsearch_db *)xmalloc(sizeof(*priv));
    priv->proto = proto;
    priv->name = name;
    priv->f = f;
    if (statp) {
	(void) fstat(fileno(f), statp);
    }

    *db = (char *)priv;
    return FILE_SUCCEED;
}

/* lsearch_close - close the file */
static void
lsearch_close(db)
    struct lsearch_db *db;
{
    (void) fclose(db->f);
    xfree((char *)db);
}

#ifdef USE_LSEARCH_REGEXCMP
/*
 * lsearch_regexcmp - do a regular expression matching
 */
static int
lsearch_regexcmp(entry, key, len)
    char *entry;
    char *key;
    int *len;
{
    static int inited = FALSE;
    static struct str exp;
# if defined(UNIX_SYS5_4)
    char *rcstr;
# else
#  if defined(UNIX_SYS5_3) || defined(UNIX_SYS5_2) || defined(USE_REGEXP_H)
    char expbuf[ESIZE];	
    char *rcstr;
#  else /* P1003.2 / 4.4bsd / Spencer regex */
    regex_t preg;
#  endif
# endif
    char *s;
    int  rc;
	
    if (*entry != '\"') {
	panic(EX_SOFTWARE, "lsearch_lookup: called for non-RE <%s>", entry);
	/*NOTREACHED*/
    }
    s = &entry[1];			/* note this style may allow for multibyte */
    if (!strchr(s, '\"')) {
	DEBUG1(DBG_DRIVER_MID, "lsearch_lookup: un-terminated RE <%s>\n", entry);
	return (-1);
    }
    if (!inited) {
	STR_INIT(&exp);
	inited = TRUE;
    } else {
	STR_LEN(&exp) = 0;
    }
    STR_NEXT(&exp, '^');
    while (*s && *s != '\"')
	STR_NEXT(&exp, *s++);
    *len = s + 1 - entry;		/* mark char following closing quote.... */
    STR_NEXT(&exp, '$');
    STR_NEXT(&exp, '\0');
    DEBUG2(DBG_DRIVER_HI, "lsearch_regexcmp: matching <%s> against expression <%s>\n", key, STR(&exp));

# if defined(UNIX_SYS5_4)
    if (!(rcstr = compile(STR(&exp), (char *) NULL, (char *) NULL))) {
	DEBUG1(DBG_DRIVER_MID, "lsearch_regexcmp: bad regular expression <%s>\n", STR(&exp));
	if (rcstr)
	    free(rcstr);
	return (-1);
    }
# else
#  if defined(UNIX_SYS5_3) || defined(UNIX_SYS5_2) || defined(USE_REGEXP_H)
    if (!(rcstr = compile(STR(&exp), expbuf, &expbuf[ESIZE], '\0'))) {
	DEBUG2(DBG_DRIVER_MID, "lsearch_regexcmp: bad regular expression <%s>, regerr = %d\n",
	       STR(&exp), regerr);
	return (-1);
    }
#  else
    if ((rc = regcomp(&preg, STR(&exp), (REG_BASIC | REG_ICASE | REG_NOSUB)))) {
	char re_errmsg[BUFSIZ];

	(void) regerror(rc, &preg, re_errmsg, sizeof(re_errmsg));
	DEBUG2(DBG_DRIVER_MID, "lsearch_regexcmp: bad regular expression <%s>: %s\n",
	       STR(&exp), re_errmsg);
	regfree(&preg);
	return (-1);
    }
#  endif
# endif

# if defined(UNIX_SYS5_4)
    /* non-zero indicates match */
    rc = (!(advance(key, rcstr)));	/* advance() forces leading '^' */
    if (rcstr)
	free(rcstr);			/* no reusing this one....  :-( */
# else
#  if defined(UNIX_SYS5_3) || defined(UNIX_SYS5_2) || defined(USE_REGEXP_H)
    /* non-zero indicates match */
    rc = (!(advance(key, expbuf)));	/* advance() forces leading '^' */
#  else /* P1003.2 / 4.4bsd / Spencer regex */
    /* zero indicates match */
    rc = regexec(&preg, key, (size_t) 0, (regmatch_t *) NULL, 0);
    regfree(&preg);
#  endif
# endif

    return (rc);
}
#endif /* USE_LSEARCH_REGEXCMP */

/*
 * lsearch_lookup - look up key with a linear search
 */
/*ARGSUSED*/
static int
lsearch_lookup(db, key, value, error)
    register struct lsearch_db *db;
    char *key;
    char **value;
    char **error;
{
    int len = strlen(key);		/* length of comparison */
    register char *entry;		/* entry from read_entry() */
    register char *p;			/* temp */

    DEBUG1(DBG_DRIVER_HI, "lsearch_lookup: looking for <%s>\n", key);

    /* always start from the beginning of the file */
    (void) fseek(db->f, 0L, 0);

    while ((entry = read_entry(db->f))) {
	int rc;

#ifdef USE_LSEARCH_REGEXCMP
	if (*entry == '\"')
	    rc = lsearch_regexcmp(entry, key, &len);
	else
#endif
	    rc = strncmpic(entry, key, len);

	if ((rc == 0) && (entry[len] == ':' || isspace(entry[len]))) {
	    char *ret;			/* value to be returned */

	    /* found a matching entry in the file, find the data */
	    entry += len;
	    /*
	     * skip <whitespace>:<whitespace>
	     */
	    while (isspace(*entry)) entry++;
	    if (*entry == ':') {
		entry++;
		while (isspace(*entry)) entry++;
	    }

	    /*
	     * skip comments
	     */
	    while (*entry == '#') {
		while (*entry && *entry != '\n') entry++;
		while (isspace(*entry)) entry++;
	    }
	    ret = entry;		/* return from this point on */

	    /* though do some more processing to the remainder */
	    p = entry - 1;
	    /* find the last character which is not white-space or comment */
	    while (*entry) {
		if (!isspace(*entry)) {
		    if (*entry == '#') {
			while (*entry && *entry != '\n') entry++;
			continue;
		    }
		    p = entry;
		}
		entry++;
	    }
	    /* throw away after the last interesting character */
	    p[1] = '\0';

	    DEBUG1(DBG_DRIVER_HI, "lsearch_lookup: return <%s>\n", ret);

	    *value = ret;
	    return DB_SUCCEED;
	}
    }
    if (ferror(db->f)) {
	*error = strerror(errno);
	return FILE_FAIL;
    }
    DEBUG1(DBG_DRIVER_HI, "lsearch_lookup: did not find <%s>\n", key);
    return DB_NOMATCH;
}

/*
 * dbmbase access method:
 *
 *	access a database stored as a DBM database.  Note that the
 *	DBM semantics only allow for one DBM file in the life of
 *	a process.  As a result, these files should never be closed
 *	and only one reference to a DBM-type database can exist in
 *	the application.
 *
 *	BUGS:  extensive use of #ifdef within functions is ugly.
 */

/*
 * form for private data:
 */
struct dbmbase_db {
    struct proto *proto;		/* access method table entry */
    char *name;				/* name of database */
#ifdef HAVE_NDBM
    DBM *db;				/* open database */
#endif
};

#ifndef HAVE_NDBM
static struct dbmbase_db *opendb = NULL;
#endif

/* dbmbase_open - open a DBM database */
static int
dbmbase_open(name, proto, retries, interval, statp, db, error)
    char *name;				/* name of database */
    struct proto *proto;		/* access method */
    int retries;			/* retry count */
    int interval;			/* retry interval */
    struct stat *statp;			/* return a stat structure */
    char **db;				/* store open database info here */
    char **error;			/* store error message here */
{
    char *pag_file;			/* name of .pag file */
    register struct dbmbase_db *priv;

    name = make_lib_fn(name);
    if (name == NULL) {
	*error = "No directory for file";
	return FILE_FAIL;
    }
    priv = (struct dbmbase_db *)xmalloc(sizeof(*priv));

#ifndef HAVE_NDBM
    if (opendb != NULL) {
	dbmclose();
	opendb = NULL;
    }
#endif

#ifdef HAVE_NDBM
    if ((priv->db = dbm_open(name, 0, 0)) == NULL)
#else
    if (dbminit(name) < 0)
#endif
    {
	int succeed = FAIL;
	int left = retries;

	if (left < 1) {
	    /* DBM databases cannot be moved atomicly, so require
	     * at least two retries */
	    left = 2;
	}
	if (interval < 2) {
	    /* require a somewhat reasonable interval as well */
	    interval = 2;
	}

	while (left-- > 0) {
	    DEBUG2(DBG_DRIVER_HI, "dbmbase_open: will retry open(%s) after %d seconds\n", name, interval);
	    (void) sleep(interval);
#ifdef HAVE_NDBM
	    if ((priv->db = dbm_open(name, 0, 0)) != NULL)
#else
	    if (dbminit(name) == 0)
#endif
	    {
		succeed = SUCCEED;
		break;
	    }
	    if (errno != ENOENT) {
		break;
	    }
	}
	if (succeed != DB_SUCCEED) {
	    if (errno == ENOENT)
		return FILE_NOMATCH;
	    *error = strerror(errno);
	    return FILE_FAIL;
	}
    }

#ifndef HAVE_NDBM
    opendb = priv;
#endif

    /* if required, get a stat structure from the .pag file */
    if (statp) {
	pag_file = xmalloc(strlen(name) + sizeof(".pag"));
	(void) sprintf(pag_file, "%s.pag", name);
	(void) stat(pag_file, statp);
	xfree(pag_file);
    }
    priv->proto = proto;
    priv->name = name;
    *db = (char *)priv;
#if defined(HAVE_NDBM) && defined(lock_fd_rd_wait) && defined(HAVE_DBM_PAGFNO)
    if (lock_fd_rd_wait(dbm_pagfno(priv->db)) < 0) {
	*error = "database is locked, try again later";
	return FILE_AGAIN;
    }
#endif /* HAVE_NDBM && lock_fd_rd_wait && HAVE_DBM_PAGFNO */
    return FILE_SUCCEED;
}

/* dbmbase_close - close the database */
static void
dbmbase_close(db)
    struct dbmbase_db *db;
{
#ifdef HAVE_NDBM
    (void) dbm_close(db->db);
#else
    if (opendb == db) {
	dbmclose();
	opendb = NULL;
    }
#endif
    xfree((char *)db);
    return;
}

/* dbmbase_lookup - call on the DBM routines to find that data */
static int
dbmbase_lookup(db, key, value, error)
    register struct dbmbase_db *db;
    char *key;
    char **value;
    char **error;
{
    datum the_key;
    datum the_value;
    static int temp_size;		/* size of temp_key area */
    static char *temp_data = NULL;	/* growable temp area */
    int len;				/* length of key */
    register char *p;

#ifndef HAVE_NDBM
    if (opendb != db) {
	if (opendb) {
	    dbmclose();
	    opendb = NULL;
	}
	if (dbminit(db->name) < 0)
	    return FAIL;
    }
#endif

    /*
     * convert the key to lower case, as fetch() is case-sensitive
     * for efficiency, keep around the malloc'd region used to store
     * the down-cased key.
     */
    len = strlen(key) + 1;
    if (temp_data == NULL) {
	temp_size = len;
	temp_data = xmalloc(temp_size);
    } else if (temp_size < len) {
	temp_size = len;
	temp_data = xrealloc(temp_data, temp_size);
    }
    for (p = temp_data; *key; p++, key++) {
	*p = lowercase(*key);
    }
    *p = '\0';

    the_key.dptr = temp_data;
    the_key.dsize = len;
#ifdef HAVE_NDBM
    the_value = dbm_fetch(db->db, the_key);
    if (dbm_error(db->db)) {
	*error = xprintf("Error in fetching %s", the_key);
	return FILE_FAIL;
    }
#else
    the_value = fetch(the_key);
#endif

    if (the_value.dptr) {
	if (temp_size < the_value.dsize + 1) {
	    temp_size = the_value.dsize + 1;
	    temp_data = xrealloc(temp_data, temp_size);
	}
	(void) strcpy(temp_data, the_value.dptr);
	*value = temp_data;
	return DB_SUCCEED;
    }
    return DB_NOMATCH;
}

#ifdef	HAVE_YP
/*
 * The YP database routines takes names of the form:
 *
 *	domain:database_name
 * or	database_name
 *
 * in the former case, the `domain' specified is the YP domain to use
 * for yp_match operations.  In the second case, the default YP domain
 * is used.
 *
 * There are two forms for the lookup: regular yp and aliasyp.  The
 * second form is used for accessing the standard Sun mail.aliases map
 * format, which does not fit the form of other YP maps.  The
 * difference is that a nul-byte is counted in the length of a key for
 * mail.aliases, while it is not counted for other maps.
 */

/*
 * form for private data:
 */
struct yp_db {
    struct proto *proto;		/* access method table entry */
    char *map;				/* name of database */
    char *domain;			/* yp domain */
};

static char *default_yp_domain = NULL;	/* from yp_get_default_domain(3N) */
static int common_yp_lookup();

#ifdef notyet
static JUMP_ENVBUF alarm_jmp;		/* jump here on SIGALRM */

static void yp_sigalrm();		/* catch SIGALRM for YP timeouts */

#define YP_TIMEOUT	30		/* 30 second timeout for YP */
#endif

/*
 * yp_open, aliasyp_open - create a yp_private structure for a database
 *
 * yp_order(3N) is called to verify that the database is accessible.
 */
static int
aliasyp_open(name, proto, retries, interval, statp, db, error)
    char *name;				/* name of database */
    struct proto *proto;		/* access method */
    int retries;			/* retry count */
    int interval;			/* retry interval */
    struct stat *statp;			/* return a stat structure */
    char **db;				/* store open database info here */
    char **error;			/* store error message here */
{
    return yp_open(name, proto, retries, interval, statp, db, error);
}

/*ARGSUSED*/
static int
yp_open(name, proto, retries, interval, statp, db, error)
    char *name;				/* name of database */
    struct proto *proto;		/* access method */
    int retries;			/* retry count */
    int interval;			/* retry interval */
    struct stat *statp;			/* return a stat structure */
    char **db;				/* store open database info here */
    char **error;			/* store error message here */
{
    register struct yp_db *priv;
    register char *p;
    int err;				/* error from yp functions */
    /* XXX: third arg to yp_order() is (ulong *) on Solaris-2 */
    int order;				/* output from yp_order */
    char *domain;
#ifdef notyet
    VOLATILE int save_time;		/* saved value from alarm() */
    VOLATILE JUMPSIG save_sigalrm;	/* previous SIGALRM handler */
#endif

    priv = (struct yp_db *)xmalloc(sizeof(*priv));

    /* is a YP domain specified? */
    p = index(name, ':');
    if (p == NULL) {
	/* no, use the default YP domain */
	priv->domain = NULL;
	priv->map = name;
    } else {
	/* yes, make a copy and use it */
	priv->domain = xmalloc(p - name + 1);
	(void) memcpy(priv->domain, name, p - name);
	priv->domain[p - name] = '\0';
	priv->map = p + 1;
    }

    /* if stat required, just zero it out, there is nothing to put there */
    if (statp) {
	(void) bzero((char *)statp, sizeof(*statp));
    }

    if (priv->domain) {
	domain = priv->domain;
    } else {
	if (default_yp_domain == NULL &&
	    (err = yp_get_default_domain(&default_yp_domain)))
	{
	    /* this should only fail if the domainname is not set, right? */
	    *error = yperr_string(err);
	    return FILE_FAIL;
	}
	domain = default_yp_domain;
    }

#ifdef notyet
    save_time = alarm(0);

    /* verify that we can access the database */
    if (JUMP_SETJMP(alarm_jmp)) {
	(void) alarm(0);
	JUMP_CLEARSIG(SIGALRM, &save_sigalrm);
	if (save_time) {
	    (void) alarm(save_time);
	}

	*error = "YP timeout";
	return FILE_AGAIN;
    } else {

	/* arrange to timeout if the operation blocks */
	JUMP_SETSIG(SIGALRM, yp_sigalrm, &save_sigalrm);
	(void) alarm(YP_TIMEOUT);
#endif

	/* potentially blocking operation */
	/* XXX: third arg to yp_order() is (ulong *) on Solaris-2 */
	/* to make smail work with nis+ (?)  SL */	
	/* err = yp_order(domain, priv->map, &order); */

#ifdef notyet
	/* restore previous alarm setting */
	(void) alarm(0);
	JUMP_CLEARSIG(SIGALRM, &save_sigalrm);
	if (save_time) {
	    (void) alarm(save_time);
	}
#endif

	if (err) {
	    /* analyze the reason for the failure */
	    *error = yperr_string(err);
	    switch (err) {
	    case YPERR_RPC:		/* cases where failure is temporary */
	    case YPERR_YPERR:
	    case YPERR_RESRC:
	    case YPERR_PMAP:
	    case YPERR_YPBIND:
	    case YPERR_YPSERV:
		return FILE_AGAIN;

	    case YPERR_MAP:		/* no such map */
	    case YPERR_DOMAIN:		/* no such domain */
		return FILE_NOMATCH;
	    }
	    return FILE_FAIL;
	}
#ifdef notyet
    }
#endif

    priv->proto = proto;
    *db = (char *)priv;
    return FILE_SUCCEED;
}

/* yp_close, aliasyp_close - free up data allocated for to a YP database */
static void
aliasyp_close(db)
    struct yp_db *db;
{
    yp_close(db);
}

/*ARGSUSED*/
static void
yp_close(db)
    struct yp_db *db;
{
    if (db->domain) {
	xfree(db->domain);
    }
    xfree((char *)db);
    return;
}

/* yp_lookup, aliasyp_lookup - call on the YP routines to find that data */
static int
yp_lookup(db, key, value, error)
    register struct yp_db *db;		/* open YP database */
    char *key;				/* search key */
    char **value;			/* return value here */
    char **error;			/* return error message here */
{
    return common_yp_lookup(db, key, value, error, FALSE);
}

static int
aliasyp_lookup(db, key, value, error)
    register struct yp_db *db;		/* open YP database */
    char *key;				/* search key */
    char **value;			/* return value here */
    char **error;			/* return error message here */
{
    return common_yp_lookup(db, key, value, error, TRUE);
}

/*ARGSUSED*/
static int
common_yp_lookup(db, key, value, error, aliasflag)
    register struct yp_db *db;		/* open YP database */
    char *key;				/* search key */
    char **value;			/* return value here */
    char **error;			/* return error message here */
    int aliasflag;			/* TRUE for Sun mail.aliases format */
{
    int keylen;				/* length of key */
    static char *temp_key = NULL;	/* area for lower-case conversion */
    static int temp_size;		/* size of temp area */
    register char *p;
    char *matchval;			/* matched data */
    int matchlen;			/* length of matched data */
    int err;				/* yp error */
    char *domain;
#ifdef notyet
    VOLATILE int save_time;		/* saved value from alarm() */
    VOLATILE JUMPSIG save_sigalrm;	/* previous SIGALRM handler */
#endif

    /*
     * convert the key to lower case, as fetch() is case-sensitive
     * for efficiency, keep around the malloc'd region used to store
     * the down-cased key.
     */
    keylen = strlen(key);
    if (temp_key == NULL) {
	temp_size = keylen + 1;
	temp_key = xmalloc(temp_size);
    } else if (temp_size <= keylen) {
	temp_size = keylen + 1;
	temp_key = xrealloc(temp_key, temp_size);
    }
    for (p = temp_key; *key; p++, key++) {
	*p = lowercase(*key);
    }
    *p = '\0';

    if (db->domain) {
	domain = db->domain;
    } else {
	domain = default_yp_domain;
    }

    if (aliasflag) {
	keylen++;
    }

#ifdef notyet
    save_time = alarm(0);

    if (JUMP_SETJMP(alarm_jmp)) {
	(void) alarm(0);
	JUMP_CLEARSIG(SIGALRM, &save_sigalrm);
	if (save_time) {
	    (void) alarm(save_time);
	}

	*error = "YP timeout";
	return FILE_AGAIN;
    } else {

	/* arrange to timeout if the operation blocks */
	JUMP_SETSIG(SIGALRM, yp_sigalrm, &save_sigalrm);
	(void) alarm(YP_TIMEOUT);
#endif

	/* potentially blocking operation */
	err = yp_match(domain, db->map, temp_key, keylen,
		       &matchval, &matchlen);

#ifdef notyet
	/* restore previous alarm setting */
	(void) alarm(0);
	JUMP_CLEARSIG(SIGALRM, &save_sigalrm);
	if (save_time) {
	    (void) alarm(save_time);
	}
#endif

	if (err) {
	    /* analyze the reason for the failure */
	    *error = yperr_string(err);
	    switch (err) {
	    case YPERR_RPC:		/* cases where failure is temporary */
	    case YPERR_DOMAIN:
	    case YPERR_YPERR:
	    case YPERR_RESRC:
	    case YPERR_PMAP:
	    case YPERR_YPBIND:
	    case YPERR_YPSERV:
		return FILE_AGAIN;

	    case YPERR_KEY:		/* key not in database */
		return DB_NOMATCH;
	    }
	    return FILE_FAIL;
	}
#ifdef notyet
    }
#endif

    matchval[matchlen] = '\0';		/* we don't want the extra newline */
    *value = matchval;
    return DB_SUCCEED;
}

#ifdef notyet
static void
yp_sigalrm()
{
    JUMP_LONGJMP(alarm_jmp, 1);
}
#endif

#endif	/* HAVE_YP */

#ifdef        HAVE_NIALIAS

/*
 * nialias access method
 *
 *            access the netinfo alias database.  
 *
 */

/* private data: */
struct aliasni_db {
    struct proto *proto;
    char *expansion_area;
    int expansion_size;
};

/* aliasni_open - open the netinfo database */

static int
aliasni_open(name, proto, retries, interval, statp, db, error)
    char *name;			/* name of file */
    struct proto *proto;	/* access method */
    int retries;		/* retry count */
    int interval;		/* retry interval */
    struct stat *statp;		/* save stat results here */
    char **db;			/* store open database info here */
    char **error;		/* store error message here */
{
    register struct aliasni_db *priv;

    /* build the private area */

    alias_setent();

    priv = (struct aliasni_db *)xmalloc(sizeof(*priv));
    priv->proto = proto;
    priv->expansion_area = NULL;
    priv->expansion_size = 0;

    /* if stat required, just zero it out, there is nothing to put there */
    if (statp) {
	(void) bzero((char *)statp, sizeof(*statp));
    }

    *db = (char *)priv;
    /* always succeeds */
    return FILE_SUCCEED;
}

/* aliasni_close - close the NetInfo alias database */
static void
aliasni_close(db)
    struct aliasni_db *db;
{
    alias_endent();
    xfree(db->expansion_area);
    xfree((char *)db);
}

/* aliasni_lookup - search for a key in the NetInfo alias database */

/*ARGSUSED*/
static int
aliasni_lookup(db, key, value, error)
    register struct aliasni_db *db;
    char *key;
    char **value;
    char **error;
{
    aliasent    *a;
    int i,n;
    char        *p;

    DEBUG1(DBG_DRIVER_HI, "aliasni_lookup: looking for <%s>\n",key);

    a = alias_getbyname(key);
    if ((!a) || (a->alias_members_len == 0)) {
	DEBUG1(DBG_DRIVER_HI,"aliasni_lookup: looking for <%s>\n",
	       lc_fold(key));
	a = alias_getbyname(lc_fold(key));
	if ((!a) || (a->alias_members_len == 0)) {
	    DEBUG1(DBG_DRIVER_HI,"aliasni_lookup: did not find <%s>\n",key);
	    return DB_NOMATCH;
	}
    }

    /* Since this routine can return multiple member lists, then */
    /* we must expand it into a single string and then let the caller */ 
    /* parse it into the individual fields */

    /* calculate the size of the expansion area  */
    for (n=2,i=0;i < a->alias_members_len; i++)
	n += (strlen(a->alias_members[i])+1);

    if (db->expansion_size < n) {
	db->expansion_size = n;
	if (db->expansion_area)
	    db->expansion_area=xrealloc(db->expansion_area, db->expansion_size);
	else
	    db->expansion_area=xmalloc(db->expansion_size);
    }

    for (p = db->expansion_area, i=0; i < a->alias_members_len; i++)
	if (n = strlen(a->alias_members[i])) {
	    *p++ = ',';				/* add a separator */
	    memcpy(p,a->alias_members[i],n);	/* add these member(s) */
	    p += n;
        }
    *p = '\0';					/* terminate the string */

    /* if anything in the string, skip the opening separator */
    *value = (p == db->expansion_area)?p:db->expansion_area+1;

    DEBUG3(DBG_DRIVER_HI, "aliasni_lookup: returns %d %s  <%s>\n",
	   a->alias_members_len, (a->alias_local?"local":"network"),*value);

    return DB_SUCCEED;
}

#endif /* HAVE_NIALIAS */

#ifdef  HAVE_NISPLUS
/*
 * NIS+ remote access method
 * (This is NIS version 3
 * for NIS version 2, it's called Yellow Pages so use HAVE_YP)
 *
 * NIS+ tables can be partially or fully qualified.  For example:
 *   mail_aliases.org_dir
 *   mail_aliases.org_dir.oes
 *   mail_aliases.org_dir.oes.amdahl.com.
 * The trailing dot "." indicates a fully-qualified NIS+ name
 *
 * If the system is properly configured, the server, domain, and search path
 * should already be established external to smail.  There may even be
 * mirrors and caches, all of which are transparent unless specifically
 * disabled.  (We won't disable them.)
 */

/* private data structure */
struct nisplus_db {
    struct proto	*proto;
    char		*table;
    int			fieldnum;
    int			retries,	/* saved retry/interval for lookup */
			interval;
};

/* open a NIS+ database */
static int
nisplus_open(name, proto, retries, interval, statp, db, error)
    char *name;				/* name of file */
    struct proto *proto;		/* access method */
    int retries;			/* retry count */
    int interval;			/* retry interval */
    struct stat *statp;			/* save stat results here */
    char **db;				/* store open database info here */
    char **error;			/* store error message here */
{
    register struct nisplus_db	*priv;
    nis_result			*result;
    int				flags;
    char			*domain;

    priv = (struct nisplus_db *)xmalloc(sizeof(*priv));
    priv->proto = proto;
    priv->fieldnum = 1;
    priv->retries = retries;
    priv->interval = interval;

    /* check for directive to change field number */
    /* we use @ in character 0 because NIS+ strings cannot start with @ */
    if ( name[0] == '@' ) {
	char	*offset;

        priv->fieldnum = atoi(name+1);
	offset = index(name,',');
	if ( offset ) {
            priv->table = COPY_STRING(offset+1);
        } else {
	    /* Yech!  No commas!  This is a failure condition */
	    *error = "syntax error in name given to nisplus proto";
	    xfree(priv);
	    return FILE_FAIL;
	}
    } else {
        priv->fieldnum = 1;
        priv->table = name;
    }

    /* if stat required, just zero it out, there is nothing to put there */
    if (statp) {
	(void) bzero((char *)statp, sizeof(*statp));
    }

    /* find offset to domain name, advance past the last comma */
    if ( !(domain = rindex(name,','))) {
	domain = name;
    } else {
	domain++;
    }

    DEBUG1(DBG_DRIVER_HI, "nisplus_open: verifying <%s>\n", domain);

    /* we'll just check if the table exists and then proceed */
    flags = FOLLOW_LINKS | EXPAND_NAME;
    do {
	result = nis_lookup ( domain, flags );
	switch ( result->status ) {

	    /* success - FILE_SUCCEED */
	    case NIS_SUCCESS:
	    case NIS_S_SUCCESS:
		/* we found it, but is it a table? */
		if ( result->objects.objects_len > 1 ) {
		    *error = "multiple results - NIS+ table name is ambiguous";
		    nis_freeresult(result);
		    DEBUG(DBG_DRIVER_HI, "nisplus_open: more than 1 value\n");
	            xfree(priv);
		    return FILE_FAIL;
		}
		if ( result->objects.objects_val[0].zo_data.zo_type != TABLE_OBJ ) {
		    *error = "table open attempted on non-table NIS+ object";
		    nis_freeresult(result);
		    DEBUG(DBG_DRIVER_HI, "nisplus_open: not a table\n");
	            xfree(priv);
		    return FILE_FAIL;
		}

		nis_freeresult(result);
		*db = (char*) priv;
	        DEBUG1(DBG_DRIVER_HI, "nisplus_open: found <%s>\n", domain);
		return FILE_SUCCEED;

	    /* immediate retry on cache expiration */
	    case NIS_CACHEEXPIRED:
		flags |= NO_CACHE;
		break;

	    /* immediate retry on server resource exhaustion */
	    case NIS_TRYAGAIN:
	    case NIS_NOMEMORY:
		break;

	    /* failure - not found - FILE_NOMATCH */
	    case NIS_NOTFOUND:
	    case NIS_NOSUCHNAME:
	    case NIS_NOSUCHTABLE:
	    case NIS_BADNAME:
	    case NIS_LINKNAMEERROR:
	    case NIS_FOREIGNNS:
		*error = nis_sperror(result->status, "nisplus_open");
		nis_freeresult(result);
	        DEBUG1(DBG_DRIVER_HI, "nisplus_open: <%s> not found\n", domain);
	        xfree(priv);
		return FILE_NOMATCH;

	    /* failure - try again later - FILE_AGAIN */
	    case NIS_NAMEUNREACHABLE:
	    case NIS_SYSTEMERROR:
	    case NIS_NOT_ME:
	    case NIS_RPCERROR:
		*error = nis_sperror(result->status, "nisplus_open");
		nis_freeresult(result);
	        DEBUG1(DBG_DRIVER_HI, "nisplus_open: try again <%s>\n", domain);
	        xfree(priv);
		return FILE_AGAIN;

	    /* failure - permanent failure on this object - FILE_FAIL */
	    case NIS_UNKNOWNOBJ:
	    case NIS_INVALIDOBJ:
	    default:
		*error = nis_sperror(result->status, "nisplus_open");
		nis_freeresult(result);
	        DEBUG1(DBG_DRIVER_HI, "nisplus_open: <%s> not found\n", domain);
	        xfree(priv);
		return FILE_FAIL;
	}

	/* sleep interval for retry */
	if ( retries > 0 ) {
	    DEBUG2(DBG_DRIVER_HI, "nisplus_open: will retry open(%s) after %d seconds\n", name, interval);
	    sleep ( interval );
	}
    } while ( retries-- > 0 );

    /* if we get here, we ran out of immediate retries */
    /* the director should be told to try again later */
    *error = nis_sperror(result->status, "nisplus_open");
    nis_freeresult(result);
    DEBUG1(DBG_DRIVER_HI, "nisplus_open: try again <%s>\n", domain);
    xfree(priv);
    return FILE_AGAIN;
}

/* close a NIS+ database */
static void
nisplus_close(db)
    struct nisplus_db *db;
{
    xfree((char *)db);
    return;
}

/* look up an element of a NIS+ database */
/*ARGSUSED*/
static int
nisplus_lookup(db, key, value, error)
    register struct nisplus_db *db;
    char *key;
    char **value;
    char **error;
{
    nis_result			*result;
    struct nis_object		*objbuf;
    int				flags,
				col,
				retries,
				valsize;
    char			*name,
				*valbuf;

    valsize=0;
    retries = db->retries;

    /* make the name to look up */
    /* prepend the search string as a "subdomain" of the table */
    name = (char*) xmalloc(strlen(key)+strlen(db->table)+2);
    sprintf(name,db->table,key);

    DEBUG1(DBG_DRIVER_HI, "nisplus_lookup: looking for <%s>\n", key);
    DEBUG1(DBG_DRIVER_HI, "nisplus_lookup: asking NIS+ for <%s>\n", name);

    /* look up the object */
    flags = FOLLOW_LINKS | EXPAND_NAME | RETURN_RESULT;
    do {
	result = nis_list ( name, flags, NULL, NULL );
	switch ( result->status ) {

	    /* success - DB_SUCCEED */
	    /* with two exceptions... */
	    case NIS_SUCCESS:
	    case NIS_S_SUCCESS:
		/* OK, we found it, but did we find too many? */
		if ( result->objects.objects_len > 1 ) {
		    *error = "multiple results - NIS+ entry name is ambiguous";
		    nis_freeresult(result);
		    DEBUG(DBG_DRIVER_HI, "nisplus_lookup: more than 1 value\n");
		    xfree(name);
		    return DB_FAIL;
		}

		/* ...but did we really find anything? */
		if ( result->objects.objects_len < 1 ) {
		    *error = "empty results - NIS+ search returned nothing";
		    nis_freeresult(result);
		    DEBUG(DBG_DRIVER_HI, "nisplus_lookup: returned nothing\n");
		    xfree(name);
		    return DB_FAIL;
		}

		/* ...but is it a table entry? */
		if ( result->objects.objects_val[0].zo_data.zo_type != ENTRY_OBJ ) {
		    *error = "open attempted on non-entry NIS+ object";
		    nis_freeresult(result);
		    DEBUG(DBG_DRIVER_HI, "nisplus_lookup: not a table entry\n");
		    xfree(name);
		    return DB_FAIL;
		}

		/* ...but are there enough fields? */
		if ( result->objects.objects_val[0].zo_data.objdata_u.en_data.en_cols.en_cols_len <= db->fieldnum ) {
		    *error = "NIS+ result had too few fields for the request";
		    nis_freeresult(result);
		    DEBUG(DBG_DRIVER_HI, "nisplus_lookup: not enough fields\n");
		    xfree(name);
		    return DB_FAIL;
		}

		/* assemble the return value from the columns */
		objbuf = &(result->objects.objects_val[0]);
		valbuf = xmalloc(objbuf->zo_data.objdata_u.en_data.en_cols.en_cols_val[db->fieldnum].ec_value.ec_value_len+1);
		strncpy(valbuf, objbuf->zo_data.objdata_u.en_data.en_cols.en_cols_val[db->fieldnum].ec_value.ec_value_val,
				objbuf->zo_data.objdata_u.en_data.en_cols.en_cols_val[db->fieldnum].ec_value.ec_value_len);
		valsize = objbuf->zo_data.objdata_u.en_data.en_cols.en_cols_val[db->fieldnum].ec_value.ec_value_len+1;

		/* back up over any trailing whitespace */
		for (valsize--; valsize > 0 && isspace(valbuf[valsize]); valsize--);
		    valsize++;
		    if (isspace(valbuf[valsize])) {
			valbuf[valsize] = '\0';
		}

		DEBUG1(DBG_DRIVER_HI, "nisplus_lookup: got <%s>\n", valbuf);
		*value=valbuf;
		nis_freeresult(result);
		xfree(name);
		return DB_SUCCEED;

	    /* immediate retry on cache expiration */
	    case NIS_CACHEEXPIRED:
		flags |= NO_CACHE;
		break;

	    /* immediate retry on server resource exhaustion */
	    case NIS_TRYAGAIN:
	    case NIS_NOMEMORY:
		break;

	    /* failure - not found - DB_NOMATCH */
	    case NIS_NOTFOUND:
	    case NIS_S_NOTFOUND:
	    case NIS_NOSUCHNAME:
	    case NIS_NOSUCHTABLE:
	    case NIS_BADNAME:
	    case NIS_LINKNAMEERROR:
	    case NIS_FOREIGNNS:
	    case NIS_PARTIAL:
		*error = nis_sperror(result->status, "nisplus_lookup");
		nis_freeresult(result);
		DEBUG2(DBG_DRIVER_HI, "nisplus_lookup: <%s> not found: %s\n",
		    key, *error);
		xfree(name);
		return DB_NOMATCH;

	    /* failure - try again later - FILE_AGAIN */
	    case NIS_NAMEUNREACHABLE:
	    case NIS_SYSTEMERROR:
	    case NIS_NOT_ME:
	    case NIS_RPCERROR:
		*error = nis_sperror(result->status, "nisplus_lookup");
		nis_freeresult(result);
		DEBUG2(DBG_DRIVER_HI, "nisplus_lookup: try again <%s>: %s\n",
		    key, *error);
		xfree(name);
		return FILE_AGAIN;

	    /* failure - permanent failure on this object - DB_FAIL */
	    case NIS_UNKNOWNOBJ:
	    case NIS_INVALIDOBJ:
	    case NIS_BADATTRIBUTE:
	    case NIS_NOTSEARCHABLE:
	    case NIS_TOOMANYATTRS:
	    default:
		*error = nis_sperror(result->status, "nisplus_lookup");
		nis_freeresult(result);
		DEBUG2(DBG_DRIVER_HI, "nisplus_lookup: <%s> not found: %s\n",
		    key, *error);
		xfree(name);
		return DB_FAIL;
	}

	/* sleep interval for retry */
	if (retries > 0) {
	    DEBUG2(DBG_DRIVER_HI,
		   "bsearch_lookup: will retry open(%s) after %d seconds\n",
		   name, db->interval);
	    sleep(db->interval);
	}
    } while (retries-- > 0);

    /* if we get here, we ran out of immediate retries */
    /* the director should be told to try again later */
    *error = nis_sperror(result->status, "nisplus_lookup");
    nis_freeresult(result);
    DEBUG2(DBG_DRIVER_HI, "nisplus_lookup: try again <%s>: %s\n",
	   key, *error);
    xfree(name);
    return FILE_AGAIN;
    
}

#endif /* HAVE_NISPLUS */

#ifdef STANDALONE

int debug = 0;
FILE *errfile = stderr;

void
main(argc, argv)
    int argc;
    char **argv;
{
    char *program = (--argc, *argv++);
    char *proto;
    char *name;
    char *db;
    char *error;
    int interval = 3;
    int retries = 0;
    int success;

    for (;;) {
	if (*argv && EQ(*argv, "-r")) {
	    argv++;
	    retries = atoi(*argv++);
	    argc -= 2;
	    continue;
	}
	if (*argv && EQ(*argv, "-i")) {
	    argv++;
	    interval = atoi(*argv++);
	    argc -= 2;
	    continue;
	}
	break;
    }

    if (argc < 3) {
	fprintf(stderr,
		"Usage: %s [-r #retries] [-i interval] proto name key ...\n",
		program);
	exit(EX_USAGE);
    }

    proto = *argv++;
    name = *argv++;

    success = open_database(name, proto, retries, interval,
			    (struct stat *)NULL, &db, &error);
    switch (success) {
    case DB_AGAIN:
	fprintf(stderr, "%s: try again later: %s\n", program, error);
	exit(EX_TEMPFAIL);

    case DB_FAIL:
	fprintf(stderr, "%s: open failed: %s\n", program, error);
	exit(EX_UNAVAILABLE);
    }

    while (*argv) {
	char *value;

	switch (lookup_database(db, *argv, &value, &error)) {
	case DB_AGAIN:
	    fprintf(stderr, "%s: %s: try again later: %s\n",
		    program, *argv, error);
	    break;

	case DB_FAIL:
	    fprintf(stderr, "%s: %s: failed: %s\n", program, *argv, error);
	    break;

	case DB_NOMATCH:
	    fprintf(stderr, "%s: %s: no match\n", program, *argv);
	    break;

	case DB_SUCCEED:
	    printf("%s --> %s", *argv, value);
	    break;
	}
	argv++;
    }

    close_database(db);
    exit(EX_OK);
}

#endif	/* STANDALONE */
