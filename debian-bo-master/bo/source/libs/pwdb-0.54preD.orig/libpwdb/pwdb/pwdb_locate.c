/*
 * $Id: pwdb_locate.c,v 1.1 1996/10/16 22:16:04 morgan Exp morgan $
 *
 * $Log: pwdb_locate.c,v $
 * Revision 1.1  1996/10/16 22:16:04  morgan
 * Initial revision
 *
 */

#include "pwdb_module.h"
#include <pwdb/_pwdb_macros.h>
#include <stdio.h>

/*
 * database reading
 *
 * All databases are assumed to contain a numeric-id <-> name relationship.
 * This function searches for the entry of a given 'name' having the
 * indicated 'id'. Either 'name' OR 'id' can be supplied. However, one of
 * these fields must be given.
 *
 * PWDB_ID_UNKNOWN is the value for id that indicates no id is specified
 * PWDB_NAME_UNKNOWN is the value for name that indicates we search for an id
 *
 * these functions return the entry in a given database that
 * corresponds to the indicated name and or id. The return value
 * indicates an error.
 *
 * The application should ensure that *p = NULL prior to calling this
 * function. If it is set to some value, the database module may
 * choose to read it. This is useful in the case that the application
 * is requesting 'additional' information, or a pass_phrase is required
 * to access the database.
 */

static int _pwdb_do(const char *class
		    , const pwdb_type *src
		    , const pwdb_fn fn
		    , const char *name, const int id
		    , const struct pwdb **p
		    , pwdb_flag *flag_p)
{
    const pwdb_type *sample[2];
    const pwdb_type **lpp;

    D(("called."));
    /* safety first */

    D(("called: %s %p %p %s %d %p %p"
       ,class,src,fn,name,id,p,flag_p));

    if ( (p == NULL
	  || (name == PWDB_NAME_UNKNOWN && id == PWDB_ID_UNKNOWN))
	 && !(fn == _PWDB_FLAGS || fn == _PWDB_SUPPORT) ) {
        return PWDB_BAD_REQUEST;
    }

    /* what databases ? */

    if (src != PWDB_DEFAULT) {    /* we are searching a specific database */
        sample[0] = src;
        sample[1] = NULL;
        lpp = sample;
    } else {
	D(("finding class[%s] of database", class));
	lpp = _pwdb_get_policy(class);
	if (lpp == NULL) {
	    D(("failed to find class: %s", class));
	    return PWDB_BAD_REQUEST;
	}
    }

    /*
     * now we have a list of lists, search each of them
     */

    while (lpp && *lpp) {
        const pwdb_type *lp;
        int retval=PWDB_NOT_FOUND;

        lp = *lpp;

	/* if a new locate request, we search the cache first... */

	if (fn == _PWDB_LOCATE && *p == NULL) {
	    if ( pwdb_cached(class, lp, name, id, p) == PWDB_SUCCESS )
		return PWDB_SUCCESS;
	    *p = NULL;                     /* this should not be required */
	}

	/* no valid cached data, so we pass the request to the dispatcher */

        while (lp && *lp != _PWDB_MAX_TYPES) {
            int ret;

            /* try this policy database */

            D(("class:%s lp:%d fn:%d name:%s id:%d p:%p flag_p:%p",
	       class, *lp, fn, name, id, p, flag_p));
            ret = _pwdb_dispatch(class, *lp, fn, name, id, p, flag_p);
	    D(("-> %d (%s)", ret, pwdb_strerror(ret)));
#ifdef DEBUG
	    if (p) {
		debug_pwdb_struct(*p);
	    }
#endif
            if ( p && !*p ) {    /* the first database must know this name */
		D(("first database ignorant of name"));
                break;
            }

            /*
             * Try to record the first return. PWDB_PASS_PHRASE_REQD
             * is, however, more urgent.
             */

            if ( (retval == PWDB_NOT_FOUND) || 
                 (ret == PWDB_PASS_PHRASE_REQD) ) {
                retval = ret;
            }

            /*
             * now search the next db in this list.
             */
            lp++;                                     /* next entry in list */
        }

	/* was this list successful? */

        if ( (p && *p) || (retval != PWDB_NOT_FOUND) ) {

	    D(("p=%p[%p]\n", p, p?*p:NULL));
	    D(("(*p)->source=%p\n", p?(*p)->source:NULL));
#ifdef DEBUG
	    if (p) {
		debug_pwdb_struct(*p);
	    }
#endif
	    if ( p && *p && (*p)->source == NULL ) {     /* set pwdb origin */
		int ret;

                ret = pwdb_source(*p, *lpp, class, name, id);
		D(("pwdb_source -> %s\n", pwdb_strerror(ret)));
#ifdef DEBUG
		if (p) {
		    debug_pwdb_struct(*p);
		}
#endif
                if (ret != PWDB_SUCCESS)
                    return ret;                        /* an error occurred */

	    }

	    return retval;                                /* time to return */

	}

        ++lpp;                                             /* try next list */
    }

    /* we didn't accomplish anything */

    return PWDB_NOT_FOUND;
}

/* locate the requested user in the requested database */

int pwdb_locate(const char *class, const pwdb_type *src
		, const char *name, const int id
		, const struct pwdb **p)
{
    D(("called."));
    return _pwdb_do(class, src, _PWDB_LOCATE, name, id, p, NULL);
}

/*
 * database writing
 *
 * These functions replace/add an entry to the indicated database
 * If the database does not exist it may be created with this entry.
 */

int pwdb_replace(const char *class, const pwdb_type *src
		 , const char *name, const int id
		 , const struct pwdb **p)
{
    D(("called."));
    return _pwdb_do(class, src, _PWDB_REPLACE, name, id, p, NULL);
}

/*
 * This function removes an entry from a database
 */

int pwdb_remove(const char *class, const pwdb_type *src
		, const char *name, const int id
		, const struct pwdb **p)
{
    D(("called."));
    return _pwdb_do(class, src, _PWDB_REMOVE, name, id, p, NULL);
}

/*
 * this function attempts to have the following entry filled in the
 * pwdb structure from the indicated database
 */

int pwdb_request(const char *class, const pwdb_type *src
		 , const char *name, const struct pwdb **p)
{
    D(("called."));
    return _pwdb_do(class, src, _PWDB_REQUEST, name
		    , PWDB_ID_UNKNOWN, p, NULL);
}

/*
 * the return from this function indicates whether "name" is supported by
 * this database.
 */

int pwdb_support(const char *class, const pwdb_type *src, const char *name)
{
    D(("called."));
    return _pwdb_do(class, src, _PWDB_SUPPORT, name
		    , PWDB_ID_UNKNOWN, NULL, NULL);
}

/*
 * this function returns the privilege flags for a given database with
 * respect to the current process' euid, the time, locking files etc..
 */

int pwdb_flags(const char *class, const pwdb_type *src, pwdb_flag *flag_p)
{
    D(("called."));
    if (flag_p == NULL)
	return PWDB_BAD_REQUEST;

    *flag_p = 0;                                           /* reset flags */
    return _pwdb_do(class, src, _PWDB_FLAGS, PWDB_NAME_UNKNOWN
		    , PWDB_ID_UNKNOWN, NULL, flag_p);
}

/* -- end of file -- */

