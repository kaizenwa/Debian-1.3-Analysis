/*
#ident	"@(#)smail/src:RELEASE-3_2:expand.c,v 1.19 1996/03/09 22:26:15 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * expand.c:
 *	expand filenames used by directors.
 *
 *	external functions:  expand_string, build_cmd_line
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include "defs.h"
#include "smail.h"
#include "addr.h"
#include "direct.h"
#include "route.h"
#include "transport.h"
#include "lookup.h"
#include "log.h"
#include "alloc.h"
#include "dys.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "debug.h"
# include "extern.h"
#endif

/* library functions */
extern long time();

/* functions local to this file */
static int expand_string_to();
static int if_clause();
static int lookup_clause();
static int test_cond();
static char **build_argv();
static char *substitute();
static char *strip_fold();
static char *uc_fold();
static char *lc_fold();
static char *quote_fold();
static char *shell_quote_fold();
static char *skip_ident();
static int skip_nesting();
static int clause_token();
#ifndef NODEBUG
static void bad_subst();
#endif


/*
 * expand_string - expand a string containing parts to be expanded
 *
 * This function does ~user and ~/ expansion and also performs expansions
 * of the form $name or ${name}.  See substitute() for the possible
 * substitutions.
 *
 * If `addr' is NULL, then a dummy addr structure is formed with all
 * items NULL except for home and user, which are taken from the
 * arguments to expand_string().
 *
 * return NULL on error attempting expansion.  The area returned may
 * be reused on subsequent calls.  If the caller wishes to retain the
 * returned data, it should be copied elsewhere.
 */
char *
expand_string(string, addr, home, user)
    register char *string;		/* unexpanded string */
    struct addr *addr;			/* addr structure with values */
    char *home;				/* home directory */
    char *user;				/* user name for $user */
{
    static struct str str;		/* build strings here */
    static int inited = FALSE;		/* TRUE if str inited */
    static struct addr *dummy_addr = NULL; /* dummy addr for home and user */

    DEBUG3(DBG_DRIVER_HI, "expand_string(%s, %s, %s) called\n",
	   string, home, user);

    if (! inited) {
	STR_INIT(&str);
	inited = TRUE;
    } else {
	STR_CHECK(&str);
	str.i = 0;
    }

    if (addr == NULL) {
	/* no addr structure given, setup a dummy one */
	if (dummy_addr == NULL) {
	    dummy_addr = alloc_addr();
	}
	addr = dummy_addr;
	addr->home = home;
	addr->next_addr = user;
    }

    /*
     * do the grunt work of expansion, appending to str
     */
    if (expand_string_to(&str, string, addr, (char *)NULL) == FAIL) {
	return NULL;
    }

    /*
     * expansion was done, finish up the string and return it
     */
    STR_NEXT(&str, '\0');
    DEBUG1(DBG_DRIVER_HI, "expand_string returns %s\n", str.p);
    return str.p;
}

static int
expand_string_to(str, string, addr, found)
    struct str *str;			/* output string */
    char *string;			/* unexpanded string */
    struct addr *addr;			/* addr structure with values */
    char *found;			/* value for $value */
{
    if (string[0] == '~') {
	/* do some kind of twiddle expansion */
	if (string[1] == '/') {
	    /* ~/ turns into home/ */
	    if (addr->home) {
		string++;
		STR_CAT(str, addr->home);
	    } else {
		/* no home directory, so ~/ is not valid */
		DEBUG(DBG_DRIVER_MID, "no home directory, ~/ invalid\n");
		return FAIL;
	    }
	} else {
	    /* ~user turns into home director for the given user */
	    char *p = string + 1;
	    struct passwd *pw;
	    extern struct passwd *getpwbyname();

	    while (*string && *string != '/') {
		string++;
	    }
	    if (*string) {
		*string = '\0';
		pw = getpwbyname(p);
		*string = '/';
	    } else {
		pw = getpwbyname(p);
	    }
	    if (pw == NULL) {
		/* ~user but username isn't valid */
		DEBUG1(DBG_DRIVER_MID, "user not found, ~%s invalid\n", p);
		return FAIL;
	    }
	    STR_CAT(str, pw->pw_dir);
	}
    }

    /*
     * we have the silly ~ business out of the way, now
     * get all of the rest of the silly business out of the way
     */
    while (*string) {
	if (*string == '$') {
	    /* do a $-substitution */
	    string++;
	    if (*string == '{') {
		/*
		 * handle expansions of the form ${name}
		 */
		char *p, *q, *new;

		p = ++string;
		if (skip_nesting(&string) == FAIL)
		    return FAIL;
		--string;
		/*
		 * process conditional expansion
		 */
		q = skip_ident(p, string);
		if ((q - p) == 2 && strncmpic(p, "if", 2) == 0) {
		    if (if_clause(addr, found, str, q, string) == FAIL) {
#ifndef NODEBUG
			bad_subst(p, string - p);
#endif
			return FAIL;
		    }
		}
		/*
		 * process database lookup
		 */
		else if ((q - p) == 6 && strncmpic(p, "lookup", 6) == 0) {
		    if (lookup_clause(addr, str, q, string) == FAIL) {
#ifndef NODEBUG
			bad_subst(p, string - p);
#endif
			return FAIL;
		    }
		} else {
		    /* not conditional, just a variable */
		    new = substitute(addr, (char *)NULL, found, p, string - p);
		    if (new) {
			STR_CAT(str, new);
		    } else {
			/* unrecognized substitution */
#ifndef NODEBUG
			bad_subst(p, string - p);
#endif
			return FAIL;
		    }
		}
		string++;
	    } else {
		/*
		 * handle $name expansions
		 */
		char *p = string;
		char *new;

		string = skip_ident(string + 1, (char *) NULL);
		new = substitute(addr, (char *) NULL, found, p, string - p);
		if (new) {
		    STR_CAT(str, new);
		} else {
		    /* unrecognized substitution */
#ifndef NODEBUG
		    bad_subst(p, string - p);
#endif
		    return FAIL;
		}
	    }
	} else {
	    /*
	     * regular character, copy it into the result
	     */
	    STR_NEXT(str, *string++);
	}
    }
    return SUCCEED;
}

/*
 * if_clause - plause ${if ...} expansion
 */

static int
if_clause(addr, found, str, s, es)
    struct addr *addr;
    char *found;
    struct str *str;
    char *s;
    char *es;
{
    int result;
    char *then_clause, *then_end, *else_clause, *else_end;
    char *q = s;

    while (isspace(*q))
	q++;
    result = test_cond(addr, found, &q);
    if (result == FAIL || q >= es) {
	return FAIL;
    }
    while (isspace(*q))
	q++;

    /*
     * find then and else clauses of if.  Two forms
     * are possible:
     *	${if : condition [:]then-clause}
     *  ${if : condition {then-clause} [else] [{else-clause}]}
     */

    switch (*q) {
    case '{':
	then_clause = ++q;
	if (skip_nesting(&q) == FAIL)
	    return FAIL;
	then_end = q - 1;
	while (isspace(*q))
	    q++;
	if (*q != '{' && strncmpic(q, "else", 4) == 0) {
	    q += 4;
	    while (isspace(*q))
		q++;
	}
	if (*q == '}') {
	    else_clause = es;
	    else_end = es;
	    break;
	}
	if (*q != '{') {
	    return FAIL;
	}
	else_clause = ++q;
	if (skip_nesting(&q) == FAIL)
	    return FAIL;
	else_end = q - 1;
	while (isspace(*q))
	    q++;
	if (q != es) {
	    return FAIL;
	}
	break;

    case ':':
	q++;
	/* FALLTHRU */

    default:
	then_clause = q;
	then_end = es;
	else_clause = es;
	else_end = es;
	break;
    }

    /*
     * expand the then clause of the result matches
     * the indicated test sense if: is a true sense,
     * if! is a false sense; otherwise, expand the
     * else clause.
     */

    if (result)
	s = rcopy(then_clause, then_end);
    else
	s = rcopy(else_clause, else_end);
    if (expand_string_to(str, s, addr, found) == FAIL) {
	xfree(s);
	return FAIL;
    }
    xfree(s);
    return SUCCEED;
}

/*
 * test_cond - test a conditional within an ${if: ...} or ${if! ...}
 *
 * Possible conditionals:
 *
 *	!condition	- reverse sense of test
 *	def:variable	- true if variable is defined and non-empty
 *	header:string	- true if indicated header exists in message
 *	origin:local	- true if message origin is local
 *	origin:remote	- true if message origin is remote
 *	dest:local	- true if message is being delivered locally
 *	dest:remote	- true if message is being delivered to remote host
 *	xform:local	- transform into a local header
 *	xform:inet	- transform into a RFC822-conformant envelope
 *	xform:uucp	- transform into a UUCP-zone envelope
 *	xform:none	- no specified transformation
 *	eq{var}{value}	- test for match of variable with a value
 *	gt{var}{value}	- test for variable greater than value
 *	lt{var}{value}	- test for variable less than value
 *	or{{cond}...}	- logical or of conditionals
 *	and{{cond}...}	- logical and of conditionals
 */

static int
test_cond(addr, found, sp)
    struct addr *addr;
    char *found;
    char **sp;
{
#define RESULT(res)	((negate)? ((res)?FALSE:TRUE): ((res)?TRUE:FALSE))

    char *q = *sp;
    char *s, *s2;
    char *new;
    int len, len2;
    struct list *hdr;
    long tpflags;
    struct str str;
    int result;
    int negate = FALSE;
    int compare_op;

    while (isspace(*q))
	q++;
    if (*q == '!') {
	negate = TRUE;
	q++;
    }
    *sp = q;
    if (clause_token(sp, &s, &len) == FAIL)
	return FAIL;
    if (len == 3 && strncmpic(s, "def", 3) == 0) {
	if (clause_token(sp, &s, &len) == FAIL)
	    return FAIL;
	new = substitute(addr, (char *)NULL, found, s, len);
	return (new && *new) ? RESULT(TRUE) : RESULT(FALSE);
    }

    if (len == 6 && strncmpic(s, "header", 6) == 0) {
	if (clause_token(sp, &s, &len) == FAIL)
	    return FAIL;
	for (hdr = header; hdr; hdr = hdr->succ) {
	    if (strncmpic(s, hdr->text, len) == 0 &&
		(hdr->text[len] == ':' || isspace(hdr->text[len])))
	    {
		return RESULT(TRUE);
	    }
	}
	return RESULT(FALSE);
    }

    if (len == 6 && strncmpic(s, "origin", 6) == 0) {
	if (clause_token(sp, &s, &len) == FAIL)
	    return FAIL;
	if (len == 5 && strncmpic(s, "local", 5) == 0)
	    return RESULT(islocal);
	if (len == 6 && strncmpic(s, "remote", 6) == 0)
	    return RESULT(! islocal);
	return FAIL;
    }

    if (len == 4 && strncmpic(s, "dest", 4) == 0) {
	if (clause_token(sp, &s, &len) == FAIL)
	    return FAIL;
	if (addr == NULL || addr->transport == NULL)
	    return RESULT(FALSE);
	tpflags = addr->transport->flags;
	if (len == 5 && strncmpic(s, "local", 5) == 0)
	    return RESULT(tpflags & LOCAL_TPORT);
	if (len == 6 && strncmpic(s, "remote", 6) == 0)
	    return RESULT(! (tpflags & LOCAL_TPORT));
	return FAIL;
    }

    if (len == 5 && strncmpic(s, "xform", 5) == 0) {
	if (clause_token(sp, &s, &len) == FAIL)
	    return FAIL;
	if (addr == NULL || addr->transport == NULL)
	    return FALSE;
	tpflags = addr->transport->flags;
	if (len == 5 && strncmpic(s, "local", 5) == 0)
	    return RESULT(tpflags & LOCAL_XFORM);
	if (len == 6 && strncmpic(s, "uucp", 6) == 0)
	    return RESULT(tpflags & UUCP_XFORM);
	if (len == 6 && strncmpic(s, "inet", 6) == 0)
	    return RESULT(tpflags & INET_XFORM);
	if (len == 6 && strncmpic(s, "none", 6) == 0)
	    return RESULT(! (tpflags & (LOCAL_XFORM|UUCP_XFORM|INET_XFORM)));
	return FAIL;
    }

    compare_op = 0;
    if (len == 2 && ( (strncmpic(s, "eq", 2) == 0) ||
		     ((strncmpic(s, "gt", 2) == 0) && (compare_op = 1)) ||
		     ((strncmpic(s, "lt", 2) == 0) && (compare_op = -1)))) {
	if (clause_token(sp, &s, &len) == FAIL)
	    return FAIL;
	if (clause_token(sp, &s2, &len2) == FAIL)
	    return FAIL;
	s2 = rcopy(s2, s2 + len2);
	STR_INIT(&str);
	if (expand_string_to(&str, s2, addr, found) == FAIL) {
	    result = FAIL;
	} else {
	    s = substitute(addr, (char *)NULL, found, s, len);
	    STR_NEXT(&str, '\0');
	    switch(compare_op) {
	      case 0:
		result = (strcmp(s, str.p) == 0) ? RESULT(TRUE) : RESULT(FALSE);
		break;
	      case 1:
		result = (strcmp(s, str.p) > 0) ? RESULT(TRUE) : RESULT(FALSE);
		break;
	      case -1:
		result = (strcmp(s, str.p) < 0) ? RESULT(TRUE) : RESULT(FALSE);
		break;
	      default:
		result = FAIL;
	    }
	}
	STR_FREE(&str);
	xfree(s2);
	return result;
    }

    if (len == 3 && strncmpic(s, "and", 3) == 0) {
	if (clause_token(sp, &s, &len) == FAIL)
	    return FAIL;
	s = rcopy(s, s + len);
	q = s;
	result = TRUE;
	for (;;) {
	    while (isspace(*q))
		q++;
	    if (*q == '\0')
		break;
	    if (clause_token(&q, &s2, &len2) == FAIL) {
		result = FAIL;
		break;
	    }
	    result = test_cond(addr, found, &s2);
	    if (result != TRUE)
		break;
	}
	xfree(s);
	return result == FAIL? FAIL: RESULT(result);
    }

    if (len == 2 && strncmpic(s, "or", 2) == 0) {
	if (clause_token(sp, &s, &len) == FAIL)
	    return FAIL;
	s = rcopy(s, s + len);
	q = s;
	result = FALSE;
	for (;;) {
	    while (isspace(*q))
		q++;
	    if (*q == '\0')
		break;
	    if (clause_token(&q, &s2, &len2) == FAIL) {
		result = FAIL;
		break;
	    }
	    result = test_cond(addr, found, &s2);
	    if (result != FALSE)
		break;
	}
	xfree(s);
	return result == FAIL? FAIL: RESULT(result);
    }

    return FAIL;
#undef RESULT
}

/*
 * lookup_clause - expand a lookup expansion
 *
 * Form:
 *	${lookup:key:proto{file-expansion}
 *		[then] {then-clause}
 *		[else] {else-clause}}
 * or:
 *	${lookup:key:proto:file-expansion:then-clause}
 */

static int
lookup_clause(addr, str, s, es)
    struct addr *addr;
    struct str *str;
    char *s;
    char *es;
{
    char *q = s;
    char *key, *proto, *file;
    char *xkey = NULL;
    char *xproto = NULL;
    char *xfile = NULL;
    int key_len, proto_len, file_len;
    char *then_clause, *then_end, *else_clause, *else_end;
    struct str file_str;
    char *dbinfo;
    char *error;
    char *found = NULL;
    int success;

    /*
     * get the key, proto, and database
     */

    if (clause_token(&q, &key, &key_len) == FAIL ||
	clause_token(&q, &proto, &proto_len) == FAIL ||
	clause_token(&q, &file, &file_len) == FAIL)
    {
	return FAIL;
    }

    /*
     * locate then and else clauses
     */

    while (isspace(*q))
	q++;
    if (strncmpic(q, "then", 4) == 0) {
	q += 4;
	while (isspace(*q))
	    q++;
    }
    switch (*q) {
    case '{':
	then_clause = ++q;
	if (skip_nesting(&q) == FAIL)
	    return FAIL;
	then_end = q - 1;
	while (isspace(*q))
	    q++;
	if (*q != '{' && strncmpic(q, "else", 4) == 0) {
	    q += 4;
	    while (isspace(*q))
		q++;
	}
	if (*q == '}') {
	    else_clause = es;
	    else_end = es;
	    break;
	}
	if (*q != '{')
	    return FAIL;
	else_clause = ++q;
	if (skip_nesting(&q) == FAIL)
	    return FAIL;
	else_end = q - 1;
	while (isspace(*q))
	    q++;
	if (q != es)
	    return FAIL;
	break;

    case ':':
	q++;
	/* FALLTHRU */

    default:
	then_clause = q;
	then_end = es;
	else_clause = es;
	else_end = es;
	break;
    }

    /*
     * expand the key, file, and proto
     */

    xkey = substitute(addr, (char *)NULL, (char *)NULL, key, key_len);
    if (xkey == NULL)
	goto do_else_clause;
    xkey = COPY_STRING(xkey);
    STR_INIT(&file_str);
    xfile = rcopy(file, file + file_len);
    file = NULL;
    if (expand_string_to(&file_str, xfile, addr, (char *)NULL) == FAIL) {
	xfree(xkey);
	xfree(xfile);
	STR_FREE(&file_str);
	return FAIL;
    }
    STR_NEXT(&file_str, '\0');
    xproto = rcopy(proto, proto + proto_len);
    proto = NULL;

    /*
     * open the database and do a lookup with the key
     */

    success = open_database(file_str.p, xproto, 1, 2, (struct stat *) NULL,
			    &dbinfo, &error);
    switch (success) {
    case FILE_FAIL:
	DEBUG3(DBG_DRIVER_LO, "Warning: open of %s:%s failed: %s\n",
	       xproto, file_str.p, error);
	goto do_else_clause;

    case FILE_AGAIN:
    case FILE_NOMATCH:
	goto do_else_clause;
    }

    success = lookup_database(dbinfo, xkey, &found, &error);
    close_database(dbinfo);

    switch (success) {
    case FILE_FAIL:
    case DB_FAIL:
	DEBUG4(DBG_DRIVER_LO, "Warning: lookup of %s in %s:%s failed: %s\n",
	       xkey, xproto, file_str.p, error);
	goto do_else_clause;

    case FILE_AGAIN:
    case FILE_NOMATCH:
    case DB_AGAIN:
    case DB_NOMATCH:
	goto do_else_clause;
    }

    /*
     * remove white-space from beginning and end of found string
     */
    while (isspace(*found))
	found++;
    for (q = found; *q; q++) {
	if (*q == '#') {
	    break;
	}
    }
    while (q > found && isspace(*(q - 1)))
	--q;
    *q = '\0';

    /*
     * expand the then-clause
     */

    s = rcopy(then_clause, then_end);
    goto do_expand;

do_else_clause:
    s = rcopy(else_clause, else_end);
    /* FALLTHRU */

do_expand:
    success = expand_string_to(str, s, addr, found);
    if (xkey)
	xfree(xkey);
    if (xproto)
	xfree(xproto);
    if (xfile)
	xfree(xfile);
    if (s)
	 xfree(s);
    STR_FREE(&file_str);
    return success;
}


/*
 * build_cmd_line - build up an arg vector suitable for execv
 *
 * transports can call this to build up a command line in a standard
 * way.  Of course, if they want to they can build up a command line in
 * a totally different fashion.
 *
 * Caution: return value points to a region which may be reused by
 *	    subsequent calls to build_cmd_line()
 *
 * Notes on the replacement algorithm:
 *   o	Within a $( and $) pair, substitutions are made once for
 *	each address on the input list.
 *   o	Otherwise the substitution is made relative to the first
 *	address on the input list.
 *   o	Substitutions:
 *	o  grade ==> $grade
 *	o  addr->next_host ==> $host
 *	o  addr->next_addr ==> $addr or $user
 *	o  addr->home ==> $home or $HOME
 *	o  sender ==> $from or $sender
 *	o  file ==> $file
 *	o  message_id ==> $message_id
 *	o  unix_date() ==> $ctime
 *	o  get_arpa_date() ==> $date
 *	o  getpid() ==> $$
 *	o  uucp_name ==> $uucp_name
 *	o  visible_name ==> $visible_name
 *	o  primary_name ==> $primary_name
 *	o  VERSION ==> $version
 *	o  version() ==> $version_string
 *   o	single quotes, double quotes and backslash work as with /bin/sh
 *
 * return NULL for parsing errors, and load `error' with a message
 * explaining the error.
 */
char **
build_cmd_line(cmd, addr, file, error)
    register char *cmd;			/* input command line */
    struct addr *addr;			/* list of remote addresses */
    char *file;				/* substitution for $file */
    char **error;			/* error message */
{
    static struct str str;		/* generated region */
    static int inited = FALSE;		/* TRUE if str has been inited */
    char *mark;				/* temp mark in cmd line */
    char *new;				/* new string from substitute */
    int ct = 1;				/* count of args, at least one */
    int state = 0;			/* notes about parse state */
    struct addr *save_addr = addr;	/* replace addr from this after $) */
    char *save_cmd;			/* start of a $( ... $) group */
    int last_char = '\0';		/* hold last *cmd value */
#define DQUOTE	0x01			/* double quote in effect */
#define GROUP	0x02			/* $( ... $) grouping in effect */

    /* initialize for building up the arg vectors */
    if (! inited) {
	STR_INIT(&str);
	inited = TRUE;
    } else {
	STR_CHECK(&str);
	str.i = 0;
    }

    while (*cmd) {
	switch (*cmd) {
	case '\'':
	    /* after "'" copy literally to before next "'" char */
	    mark = index(cmd+1, '\'');
	    if (mark == NULL) {
		panic(EX_DATAERR, "no matching ' for cmd in transport %s",
		      addr->transport->name);
		/*NOTREACHED*/
	    }
	    *mark = '\0';		/* put null in for copy */
	    STR_CAT(&str, cmd+1);
	    *mark = '\'';		/* put quote back */
	    last_char = '\'';
	    cmd = mark;
	    break;

	case '\\':
	    /*
	     * char after \ is literal, unless in quote, in which case
	     * this is not so if the following char is not " or $ or \
	     */
	    if (*cmd++ == '\0') {
		*error = "\\ at end of command";
		return NULL;
	    }
	    if (!(state&DQUOTE) ||
		*cmd == '\\' || *cmd == '"' || *cmd == '$')
	    {
		STR_NEXT(&str, *cmd);
	    } else {
		STR_NEXT(&str, '\\');
		STR_NEXT(&str, *cmd);
	    }
	    last_char = '\\';
	    break;

	case '"':			/* double quote is a toggle */
	    state ^= DQUOTE;
	    last_char = '"';
	    break;

	case '$':			/* perform parameter substitution */
	    cmd++;
	    if (*cmd == '\0') {
		*error = "$ at end of command";
		return NULL;
	    }
	    if (*cmd == '(') {
		if (state&GROUP) {
		    *error = "recursive $( ... $)";
		    return NULL;
		}
		if (state&DQUOTE) {
		    *error = "$( illegal inside \"...\"";
		    return NULL;
		}
		save_cmd = cmd;
		state |= GROUP;
		break;
	    }
	    if (*cmd == ')') {
		if ((state&GROUP) == 0) {
		    *error = "no match for $)";
		    return NULL;
		}
		if (state&DQUOTE) {
		    *error = "$) illegal inside \"...\"";
		    return NULL;
		}
		if (!isspace(last_char)) {
		    /* end previous vector, create a new one */
		    ct++;
		    STR_NEXT(&str, '\0');
		}
		addr = addr->succ;
		if (addr) {
		    cmd = save_cmd;
		} else {
		    /* no more addrs to put in group */
		    addr = save_addr;
		    state &= ~GROUP;
		}
		last_char = ' ';	/* don't create an extra vector */
		break;
	    }
	    if (*cmd == '{') {
		mark = cmd+1;
		cmd = index(mark, '}');
		if (cmd == NULL) {
		    *error =  "no match for {";
		    return NULL;
		}
	    } else {
		/* use at least one char after $ for substitute name */
		mark = cmd;
		cmd = skip_ident(cmd, cmd + strlen(cmd));
		/* cmd now one beyond where it should be */
	    }
	    new = substitute(addr, file, (char *)NULL, mark, cmd - mark);
	    if (new == NULL) {
		int c_save = mark[cmd-mark];

		mark[cmd-mark] = '\0';
		/* TODO: This is a memory leak */
		*error = xprintf("bad substition: $%s", mark);
		mark[cmd-mark] = c_save;
		return NULL;
	    }
	    STR_CAT(&str, new);
	    if (*cmd != '}') {
		--cmd;			/* correct next char pointer */
	    }
	    last_char = '$';
	    break;

	case ' ':			/* when not in a quote */
	case '\t':			/* white space separates words */
	case '\n':
	    if (state&DQUOTE) {
		STR_NEXT(&str, *cmd);
	    } else if (!isspace(last_char)) {
		/* end the previous arg vector */
		STR_NEXT(&str, '\0');
		ct++;			/* start a new one */
	    }
	    last_char = *cmd;
	    break;

	default:
	    STR_NEXT(&str, *cmd);
	    last_char = *cmd;
	}
	cmd++;				/* advance to next char */
    }
    if (state&DQUOTE) {
	*error = "no match for opening \"";
	return NULL;
    }
    if (state&GROUP) {
	*error = "no match for $(";
	return NULL;
    }

    if (isspace(last_char)) {
	--ct;				/* don't count just blanks */
    }
    STR_NEXT(&str, '\0');		/* null terminate the strings */
    return build_argv(str.p, ct);
}

/*
 * build_argv - build arg vectors from inline strings
 *
 * build_cmd_line produces chars with null characters separating
 * strings.  build_argv takes these chars and turns them into
 * an arg vector suitable for execv.
 *
 * Caution: the value returned by build_argv() points to a region
 *	    which may be reused on subsequent calls to build_argv().
 */
static char **
build_argv(p, ct)
    register char *p;			/* strings, one after another */
    register int ct;			/* count of strings */
{
    static char **argv = NULL;		/* reusable vector area */
    static int argc;
    register char **argp;

    if (argv == NULL) {
	argc = ct + 1;
	argv = (char **)xmalloc(argc * sizeof(*argv));
    } else {
	if (ct + 1 > argc) {
	    X_CHECK(argv);
	    argc = ct + 1;
	    argv = (char **)xrealloc((char *)argv, argc * sizeof(*argv));
	}
    }
    argp = argv;
    DEBUG(DBG_REMOTE_MID, "cmd =");
    while (ct--) {
	*argp++ = p;
	DEBUG1(DBG_REMOTE_MID, " '%s'", p);
	if (ct) {
	    while (*p++) ;		/* scan for next string */
	}
    }
    DEBUG(DBG_REMOTE_MID, "\n");
    *argp = NULL;			/* terminate vectors */
    return argv;
}

/*
 * substitute - relace a $paramater with its value
 *
 * panic on errors, see build_cmd_line for details.
 */
static char *
substitute(addr, file, found, var, len)
    struct addr *addr;			/* source for $host, $addr, $user */
    char *file;				/* source for $file */
    char *found;			/* $value value */
    register char *var;			/* start of variable */
    register int len;			/* length of variable */
{
    static char buf[50];

#define MATCH(x) (len==sizeof(x)-1 && strncmpic(var, x, sizeof(x)-1) == 0)

    if (strncmpic(var, "lc:", sizeof("lc:") - 1) == 0) {
	return lc_fold(substitute(addr, found, file, var + 3, len - 3));
    }
    if (strncmpic(var, "uc:", sizeof("uc:") - 1) == 0) {
	return uc_fold(substitute(addr, found, file, var + 3, len - 3));
    }
    if (strncmpic(var, "quote:", sizeof("quote:") - 1) == 0) {
	return quote_fold(substitute(addr, found, file, var + 6, len - 6));
    }
    if (strncmpic(var, "shquote:", sizeof("shquote:") - 1) == 0) {
	return shell_quote_fold(substitute(addr, found, file, var + 8, len - 8));
    }
    if (strncmpic(var, "strip:", sizeof("strip:") - 1) == 0) {
	return strip_fold(substitute(addr, found, file, var + 6, len - 6));
    }
    if (strncmpic(var, "parent:", sizeof("parent:") - 1) == 0) {
	struct addr *parent = addr->parent;

	if (parent == NULL) {
	    return NULL;
	}
	return substitute(parent, found, file, var + 7, len - 7);
    }
    if (strncmpic(var, "top:", sizeof("top:") - 1) == 0) {
	struct addr *top = addr;

	while (top->parent) {
	    top = top->parent;
	}
	return substitute(top, found, file, var + 4, len - 4);
    }
    if (MATCH("value")) {
	return found;
    }
    if (MATCH("grade")) {
	static char grade_str[2] = { 0, 0 };
	grade_str[0] = msg_grade;
	return grade_str;
    }
    if (MATCH("user") || MATCH("addr")) {
	return addr? addr->next_addr: NULL;
    }
    if (MATCH("input_addr")) {
	return addr? addr->in_addr: NULL;
    }
    if (MATCH("host")) {
	return addr? addr->next_host: NULL;
    }
#ifdef USE_TARGET_DOMAIN
    if (MATCH("target_domain")) {
	return addr? (addr->target? addr->target: primary_name): NULL;
    }
#endif
    if (MATCH("HOME") || MATCH("home")) {
	return addr? addr->home: NULL;
    }
    if (MATCH("sender") || MATCH("from")) {
	return sender;
    }
    if (MATCH("sender_name") || MATCH("fullname")) {
	if (sender_name == NULL && islocal) {
	    getfullname();
	}
	return sender_name;
    }
    if (MATCH("file")) {
	return file;
    }
    if (MATCH("message_id") || MATCH("id")) {
	return message_id;
    }
    if (MATCH("message_size") || MATCH("size")) {
	sprintf(buf, "%ld", msg_size);
	return buf;
    }
    if (MATCH("message_body_size") || MATCH("body_size")) {
	sprintf(buf, "%ld", msg_body_size);
	return buf;
    }
    if (MATCH("ctime")) {
	return unix_date();
    }
    if (MATCH("date")) {
	/* get the current date in ARPA format */
	return get_arpa_date(time((long *)0));
    }
    if (MATCH("spool_date")) {
	/* get the spool date in ARPA format */
	return get_arpa_date(message_date());
    }
    if (MATCH("$") || MATCH("pid")) {
	static char pidbuf[10];

	(void) sprintf(pidbuf, "%d", getpid());
	return pidbuf;
    }
    if (MATCH("uucp_name")) {
	return uucp_name;
    }
    if (MATCH("visible_name") || MATCH("name")) {
	return visible_name;
    }
    if (MATCH("primary_name") || MATCH("primary")) {
	return primary_name;
    }
    if (MATCH("version")) {
	return version_number;
    }
    if (MATCH("version_string")) {
	return version();
    }
    if (MATCH("release_date") || MATCH("release")) {
	return release_date;
    }
    if (MATCH("compile_num") || MATCH("ld_num")) {
	static char s_compile_num[10];
	(void) sprintf(s_compile_num, "%d", compile_num);
	return s_compile_num;
    }
    if (MATCH("compile_date") || MATCH("ld_date")) {
	return compile_date;
    }
    if (MATCH("smail_lib_dir") || MATCH("lib_dir")) {
	return smail_lib_dir;
    }
    if (MATCH("sender_host")) {
	return sender_host;
    }
    if (MATCH("sender_host_addr")) {
	return sender_host_addr;
    }
    if (MATCH("sender_proto")) {
	return sender_proto;
    }
    if (MATCH("ident_sender")) {
	return ident_sender;
    }
    if (MATCH("ident_method")) {
	return ident_method;
    }
    if (MATCH("program")) {
	return program;
    }
    if (MATCH("sender_program")) {
	return sender_program? sender_program: program;
    }
    if (MATCH("director")) {
	if (addr && addr->director)
	    return addr->director->name;
	return NULL;
    }
    if (MATCH("router")) {
	if (addr && addr->router)
	    return addr->router->name;
	return NULL;
    }
    if (MATCH("transport")) {
	if (addr && addr->transport)
	    return addr->transport->name;
	return NULL;
    }
    return NULL;			/* no match */
#undef	MATCH
}

/*
 * lc_fold - meta substitution to convert value to lower case
 */
static char *
lc_fold(value)
    register char *value;
{
    static int lc_size;			/* keep size of allocated region */
    int value_size;
    static char *lc = NULL;		/* retained malloc region */
    register char *p;			/* for scanning through lc */

    if (value == NULL) {
	return NULL;
    }
    value_size = strlen(value) + 1;

    /* get a region at least large enough for the value */
    if (lc == NULL) {
	lc = xmalloc(lc_size = value_size);
    } else if (value_size > lc_size) {
	X_CHECK(lc);
	lc = xrealloc(lc, lc_size = value_size);
    }
    p = lc;
    while (*value) {
	*p++ = lowercase(*value++);
    }
    *p = '\0';
    return lc;
}

/*
 * uc_fold - meta substitution to convert value to upper case
 */
static char *
uc_fold(value)
    register char *value;
{
    static int uc_size;			/* keep size of allocated region */
    int value_size;
    static char *uc = NULL;		/* retained malloc region */
    register char *p;			/* for scanning through lc */

    if (value == NULL) {
	return NULL;
    }
    value_size = strlen(value) + 1;

    /* get a region at least large enough for the value */
    if (uc == NULL) {
	uc = xmalloc(uc_size = value_size);
    } else if (value_size > uc_size) {
	X_CHECK(uc);
	uc = xrealloc(uc, uc_size = value_size);
    }
    p = uc;
    while (*value) {
	*p++ = uppercase(*value++);
    }
    *p = '\0';
    return uc;
}

/*
 * shell_quote_fold - meta substitution to quote value for shell
 *
 * Tidies up a string to allow it to be passed to a shell command
 * without nasty effects.  This makes the whole string single quoted
 * (ie one shell argument), and escapes any internal quote characters.
 *
 */
static char * 
shell_quote_fold(value)
     register char * value;
{
    static struct str wkstr;
    static int initialised = FALSE;
    char * ptr;
    register char chr;

    if (!initialised) {
	initialised = TRUE;
	STR_INIT(&wkstr);
    }
  
    wkstr.i = 0;			/* zero length string */
    STR_NEXT(&wkstr, '\'');
    for (ptr = value;((chr = *ptr) != 0); ptr++) {
	switch(chr) {
	  case '\\':
	    STR_NEXT(&wkstr, chr);
	    if (! (chr = *++ptr)) {	/* premature end of string */
		STR_NEXT(&wkstr, '\\');
		ptr--;			/* will force loop to terminate */
	    } else {
		STR_NEXT(&wkstr, chr);
	    }
	    break;
	  case '\'':
	    STR_NEXT(&wkstr, '\\');
	    /* FALLTHRU */
	  default:
	    STR_NEXT(&wkstr, chr);
	}
    }
    STR_NEXT(&wkstr, '\'');		/* Trailing quotes */
    STR_NEXT(&wkstr, '\0');		/* Trailing null */
    return(wkstr.p);
}

/*
 * quote_fold - meta substitution to quote value if required
 *
 * If string requires quoting, then quote it, else return original string.
 * Quoting means putting double quotes around the string, and escaping any
 *   backslashes or double quote characters with a backslash.
 * A string "requires quoting" if it starts with a non-alphanumeric, or
 *   contains any characters other than alphanumerics, hyphens or underscores.
 *
 */
static char * 
quote_fold(value)
     register char * value;
{
    static struct str wkstr1;		/* There are 2 of these to prevent you */
    static struct str wkstr2;		/* messing up your own workspace! */
    static int initialised = FALSE;
    static int which_str = FALSE;
    struct str * curwkstr;
    int quote_needed = FALSE;
    char * ptr;
    register char chr;

    if (!initialised) {
	initialised = TRUE;
	STR_INIT(&wkstr1);
	STR_INIT(&wkstr2);
    }
  
    /* Check if quoting is necessary */
    if (isalnum(*value)) {		/* Make sure that first char is alphanumeric */
	for (ptr=value; ((!quote_needed) && (chr = *ptr)); ptr++) {
	    if ( !(   (isalnum(chr)) 
		   || (chr == '_')
		   || (chr == '-')
		   ) ) {
		quote_needed = TRUE;
	    }
	}
    } else
      quote_needed = TRUE;

    if (!quote_needed)			/* No work, so just drop out.... */
      return(value);

    /* Set up work string */
    if (which_str = !which_str)
      curwkstr = &wkstr1;
    else
      curwkstr = &wkstr2;

    curwkstr->i = 0;
    STR_NEXT(curwkstr, '"');		/* Leading quotes */
    while (chr = *value++) {
	switch(chr) {			/* Switch is overkill here! */
	  case '\\':
	  case '"':
	    STR_NEXT(curwkstr, '\\');
	    STR_NEXT(curwkstr, chr);
	    break;
	  default:
	    STR_NEXT(curwkstr, chr);
	    break;
	}
    }
    STR_NEXT(curwkstr, '"');		/* Trailing quotes */
    STR_NEXT(curwkstr, '\0');		/* Trailing null */
    return(curwkstr->p);
}

/*
 * strip_fold - strip quotes and collapse spaces and dots
 *
 * strip quotes from the input string and collapse any sequence of one
 * or more white space and `.' characters into a single `.'.
 */
static char *
strip_fold(value)
    char *value;
{
    static int strip_size;		/* keep size of allocated region */
    int value_size;
    static char *strip_buf = NULL;	/* retained malloc region */
    register char *p;			/* for scanning through strip_buf */
    register char *q;			/* also for scanning strip_buf */

    if (value == NULL) {
	return NULL;
    }
    value_size = strlen(value) + 1;
    if (strip_buf == NULL) {
	strip_buf = xmalloc(strip_size = value_size);
    } else if (value_size > strip_size) {
	X_CHECK(strip_buf);
	strip_buf = xrealloc(strip_buf, strip_size = value_size);
    }
    (void) strcpy(strip_buf, value);
    (void) strip(strip_buf);

    /* q reads and p writes */
    p = q = strip_buf;
    /* strip initial -'s */
    while (*q == '-') {
	q++;
    }
    while (*q) {
	/* collapse multiple white-space chars and .'s into single dots */
	if (isspace(*q) || *q == '.') {
	    while (isspace(*++q) || *q == '.') ;
	    *p++ = '.';
	    continue;
	}
	*p++ = *q++;
    }
    *p = '\0';			/* finish off strip_buf */
    return strip_buf;
}

/*
 * skip_ident - skip a variable identifier
 */
static char *
skip_ident(s, lim)
    char *s, *lim;
{
    char *p = s;
    while (*p && (lim == NULL || p < lim)) {
	if (p != s && !isalnum(*p & 0xFF) && *p != '_')
	    break;
	++p;
    }
    return p;
}

/*
 * clause_token - extract a name or {...} token from the expansion string
 *
 * Used within ${...} substrings.
 */

static int
clause_token(sp, startp, lenp)
    char **sp;
    char **startp;
    int *lenp;
{
    char *s = *sp;

    while (isspace(*s))
	s++;
    switch (*s) {
    case '\0':
    case '}':
	return FAIL;

    case '{':
	s++;
	*startp = s;
	if (skip_nesting(&s) == FAIL)
	    return FAIL;
	*lenp = s - *startp - 1;
	*sp = s;
	return SUCCEED;

    case ':':
	s++;
	while (isspace(*s))
	    s++;
	/* FALLTHRU */

    default:
	*startp = s;
	if (*s == ':' || *s == '{' || *s == '}')
	    return FAIL;
	s++;
	while (*s) {
	    switch (*s) {
	    case '_':
	    case '-':
	    case '.':
	    case '/':
		s++;
		continue;

	    default:
		if (isalnum(*s)) {
		    s++;
		    continue;
		}
		break;
	    }
	    break;
	}
	*lenp = s - *startp;
	*sp = s;
	return SUCCEED;

    }
}

static int
skip_nesting(sp)
    char **sp;
{
    int nesting = 1;
    char *s = *sp;

    while (nesting) {
	switch (*s++) {
	  case '\0':
	    DEBUG1(DBG_DRIVER_LO, "expand_string: no matching } for {%s\n",
		   *sp);
	    return FAIL;
	  case '{':
	    ++nesting;
	    break;
	  case '}':
	    --nesting;
	    break;
	}
    }
    *sp = s;
    return SUCCEED;
}

#ifndef NODEBUG
/*
 * bad_subst - generate a debugging message for a failed substitution.
 *
 * note that we can't use "%*.*s" here since dprintf() is simple-minded.
 */
static void
bad_subst(var, len)
    char *var;
    int len;
{
    char *saved;

    saved = xmalloc(len + 1);
    strncpy(saved, var, (size_t) len);
    saved[len] = '\0';
    DEBUG1(DBG_DRIVER_MID, "expand_string: expansion failed for ${%s\n", saved);
    xfree(saved);
}
#endif
