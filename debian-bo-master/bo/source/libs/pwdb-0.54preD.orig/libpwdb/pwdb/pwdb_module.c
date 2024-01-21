/*
 * $Id: pwdb_module.c,v 1.1 1996/10/16 22:16:04 morgan Exp morgan $
 *
 * $Log: pwdb_module.c,v $
 * Revision 1.1  1996/10/16 22:16:04  morgan
 * Initial revision
 *
 */

#include "pwdb_module.h"
#include "../_pwdb_internal.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/fcntl.h>

#include <pwdb/_pwdb_macros.h>

/*
 * This file contains the module dispatching functions
 */

/*
 * The table of entries for each supported module
 */

#include "helpers.c"
#include "interface/unix.c"
#include "interface/shadow.c"
#include "interface/nis.c"
#include "interface/radius.c"

const static struct _pwdb_module *modules[] = {

    &_pwdb_unix_struct,
    &_pwdb_unix_g_struct,

    &_pwdb_shadow_struct,
    &_pwdb_shadow_g_struct,

    &_pwdb_nis_struct,
    &_pwdb_nis_g_struct,

    &_pwdb_radius_struct,
    &_pwdb_radius_g_struct,

    /* that's all */

    NULL
};

/* return a string describing the database associated with the pwdb_type */

const char *pwdb_db_name(pwdb_type src)
{
    D(("called."));
    if (src >= PWDB_UNIX) {
	int i;

	for (i=0; modules[i] ; ++i) {
	    if (modules[i]->type == src) {
		D(("returning %s", modules[i]->name));
		return modules[i]->name;
	    }
	}
    }
    D(("returning (unknown)"));
    return "(unknown)";
}

/* READ the database conf file */

#define PWDB_MAX_TOKEN_BUFFER 100

static int _pwdb_read_token(char *buffer, FILE *f)
{
    static int backup=0;
    int len;
    int c;

    D(("called."));
    while ((c=backup) || (c=fgetc(f)) != EOF) {
        backup=0;
        if (!isspace(c)) {
            if (c == '#') {                /* read to end of line */
                while ((c=fgetc(f)) != EOF) {
                    if (c == '\n')
			break;
                }
                if (c == EOF)
                    break;
            } else {
                break;          /* not a comment and not a space */
            }
        }
    }

    /* are we just checking for a '+'? */

    if (buffer == NULL) {
        if (c == '+') {
            return 0;
        } else {
            backup = c;
            return 1;
        }
    }

    /* now we read until a space or EOF or buffer full */

    if (c == '+') {
        *buffer++ = '+';
    } else {
        for (len=0; len < PWDB_MAX_TOKEN_BUFFER &&
		 c != EOF &&
		 !isspace(c) &&
		 (c != '+') ;
             c=fgetc(f)) {
            *buffer++ = c;
        }
        /* remember last character for next time around */
        if (c == '+' || c == '#')
            backup = c;
    }

    *buffer = '\0';                              /* <NUL> terminate */
    return (c == EOF ? EOF:0);
}

static int grow_buffer(pwdb_type **buf, int length)
{
    pwdb_type *junk;

    D(("called."));
    junk = (pwdb_type *) realloc(*buf, length*sizeof(pwdb_type));
    if (junk == NULL) {
        return PWDB_MALLOC;
    } else {
        *buf = junk;
        return PWDB_SUCCESS;
    }
}

static int grow_list(pwdb_type ***list, int nlist)
{
    pwdb_type **tmp;

    D(("called."));
    tmp = realloc(*list, nlist*sizeof(**list));
    if (tmp == NULL) {                       /* memory trouble: clean up */
	tmp = *list;
	while (*tmp) {
	    free(*tmp);
	    *tmp++ = NULL;
	}
	free(*list);
	*list = NULL;
	return PWDB_MALLOC;
    }
    *list = tmp;

    return PWDB_SUCCESS;
}

static int fill_buffer(FILE *conf, const char *end, pwdb_type ***list
               , char *buf)
{
    int nlist=0;
    int retval;
     
    D(("called."));
    do {
	pwdb_type *dblist=NULL;                     /* new list to make */
	int length = 0,ret,no_plus;

	do {
	    pwdb_type db;
	    int i;

	    /* read the next token */

	    retval = _pwdb_read_token(buf, conf);
	    if ( strcmp(end, buf) == 0 ) {
		break;                       /* db section is ended */
	    }

	    /* read a plus? */
	    no_plus = _pwdb_read_token(NULL, conf);

	    /* identify the corresponding database */

	    db = -1;
	    for (i=0; modules[i]->type != _PWDB_MAX_TYPES; ++i) {
		if ( strcmp(modules[i]->name, buf) == 0 ) {
		    db = modules[i]->type;
		    break;
		}
	    }
	    if (db == -1) {
		continue;                      /* entry not recognized */
	    }

	    /* realloc a bigger buffer? */

	    if ( (length % 4) == 0 ) {
		ret = grow_buffer(&dblist, (int) length+4);
		if (ret != PWDB_SUCCESS) {
		    return PWDB_MALLOC;
		}
	    }

	    dblist[length++] = db;         /* register this database */

	} while (retval != EOF && !no_plus);

	/* did we make a list this time? */

	if ( dblist ) {

	    /* truncate list */

	    if ( (length % 4) == 0 ) {
		ret = grow_buffer(&dblist, (int) length+1);
		if (ret != PWDB_SUCCESS) {
		    return PWDB_MALLOC;
		}
	    }
	    dblist[length++] = _PWDB_MAX_TYPES;

	    /* now add this list to array of lists */

	    ret = grow_list(list, nlist+1);
	    if (ret != PWDB_SUCCESS)
		return PWDB_MALLOC;

	    (*list)[nlist++] = dblist;

	}

	if ( strcmp(end, buf) == 0 ) {
	    break;                       /* db section is ended */
	}

    } while (retval != EOF);

    /* truncate the list of lists */

    retval = grow_list(list, nlist+1);
    if (retval != PWDB_SUCCESS)
	return retval;

    (*list)[nlist] = NULL;

    /*
    for (retval=0; retval <= nlist; ++retval) {
	pwdb_type *dl;

	fprintf(stderr, "read: %p\n", (*list)[retval]);
	for (dl=(*list)[retval]; dl && *dl != _PWDB_MAX_TYPES; ++dl)
	    fprintf(stderr, " %d[%p]", *dl,dl);
	fprintf(stderr,"\n");
    }
    */

    return PWDB_SUCCESS;
}

static pwdb_type **_pwdb_policy=NULL;
static pwdb_type **_pwdb_group_policy=NULL;

const pwdb_type **pwdb_policy=NULL;
const pwdb_type **pwdb_group_policy=NULL;

/*
 * run down the list of databases...
 *
 * XXX: this is set to become *much* more general in the future
 */

const pwdb_type **_pwdb_get_policy(const char *class)
{
    D(("called."));
    if (strcmp("user", class) == 0) {
	return pwdb_policy;
    } else if (strcmp("group", class) == 0) {
	return pwdb_group_policy;
    }

    return NULL;
}

void _pwdb_delete_policy(void)
{
    D(("called."));
    while (_pwdb_policy && *_pwdb_policy) {
	free(*_pwdb_policy++);
    }
    pwdb_policy = NULL;
    _pwdb_policy = NULL;

    while (_pwdb_group_policy && *_pwdb_group_policy) {
	free(*_pwdb_group_policy++);
    }
    pwdb_group_policy = NULL;
    _pwdb_group_policy = NULL;
}

int _pwdb_read_conf(void)
{
    char buf[PWDB_MAX_TOKEN_BUFFER+1];
    FILE *conf;
    int retval;

    D(("called."));
    /* have we already set the policies? */

    if (_pwdb_policy != NULL || _pwdb_group_policy != NULL) {
	return PWDB_ABORT;
    }

    /* open PWDB_CONF file */

    conf = fopen(PWDB_CONF, "r");
    if (conf == NULL) {
	return PWDB_CONF_ERR;
    }

    /* this is a fixed syntax file.
     *
     * Newlines are ignored except where they terminate comments
     * comments are preceeded with '#' characters
     * we expect:
     *
     *          user: list1 list2 list3 ...
     *          group: list1 list2 ...
     *
     * In this file listN is of the following form
     *
     *    name1+name2+name3 ...
     *
     * the names are to be found in the module_header table.
     */

    /* read token, expect "user:" */

    retval = _pwdb_read_token(buf, conf);
    if (retval == EOF || strcmp("user:", buf)) {
	fclose(conf);
	return PWDB_CONF_ERR;
    }

    /* loop through tokens until "group:" read */

    retval = fill_buffer(conf, "group:", &_pwdb_policy, buf);
    if (retval != PWDB_SUCCESS) {
	fclose(conf);
	_pwdb_delete_policy();
	return PWDB_CONF_ERR;
    }

    /* last token should be "group:" */

    if (retval == EOF || strcmp("group:", buf)) {
	fclose(conf);
	_pwdb_delete_policy();
	return PWDB_CONF_ERR;
    }

    /* loop through tokens until EOF found */

    retval = fill_buffer(conf, "", &_pwdb_group_policy, buf);
    if (retval != PWDB_SUCCESS) {
	D(("group db reading failed"));
	fclose(conf);
	_pwdb_delete_policy();
	return PWDB_CONF_ERR;
    }

    /* all done */

    D(("seemed to work"));
    pwdb_policy = (const pwdb_type **) _pwdb_policy;
    pwdb_group_policy = (const pwdb_type **) _pwdb_group_policy;
    fclose(conf);

    return PWDB_SUCCESS;
}

/* locate the indicated database */

static int _pwdb_db_number(const char *class, const pwdb_type src)
{
    int i;

    D(("called for class %s", class));
    /* search for this module's entry */

    for (i=0; modules[i]; ++i) {
	if ( modules[i]->type == src && !strcmp(class, modules[i]->class ) ) {
	    /* we've found it! */
	    break;
	}
    }
    return i;
}

int _pwdb_dispatch(const char *class
		   , const pwdb_type src
		   , const pwdb_fn fn
		   , const char *name, const int id
		   , const struct pwdb **p
		   , pwdb_flag *flag_p)
{
    int retval,i;

    D(("called."));
    D(("%s,%d,%d,%s,%d,%p,%p",class, src, fn, name, id, p, flag_p));
    if (src < PWDB_UNIX || src >= _PWDB_MAX_TYPES)
	return PWDB_ABORT;       /* this should never happen */

    /* search for this module's entry */

    i = _pwdb_db_number(class, src);
    if (modules[i] == NULL) {
	D(("aborting..."));
	return PWDB_ABORT;
    }

    /*
     * now we have found the module.. call the appropriate function
     */

    retval = PWDB_UNSUPPORTED;

    switch (fn) {
    case _PWDB_LOCATE:
	D(("calling LOCATE"));
	if (modules[i]->locate)
	    retval = modules[i]->locate(name, id, p);
	break;
    case _PWDB_REPLACE:
	D(("calling REPLACE"));
	if (modules[i]->replace)
	    retval = modules[i]->replace(name, id, p);
	break;
    case _PWDB_REMOVE:
	D(("calling REMOVE"));
	if (modules[i]->delete)
	    retval = modules[i]->delete(name, id, p);
	break;
    case _PWDB_REQUEST:
	D(("calling REQUEST"));
	if (modules[i]->request)
	    retval = modules[i]->request(name, p);
	break;
    case _PWDB_SUPPORT:
	D(("calling SUPPORT"));
	if (modules[i]->support)
	    retval = modules[i]->support(name);
	break;
    case _PWDB_FLAGS:
	D(("calling FLAGS"));
	if (modules[i]->flags)
	    retval = modules[i]->flags(flag_p);
	break;
    case _PWDB_CLEANUP:
	D(("calling CLEANUP"));
	if (modules[i]->cleanup)
	    retval = modules[i]->cleanup(id);
	break;
    default:
	D(("unknown function request"));
	/* what is the caller after? */
	return PWDB_ABORT;
    }

    /* indicate success or failure to caller */
    D(("about to return %d (%s)", retval, pwdb_strerror(retval)));
    return retval;
}

/* -- end of file -- */
