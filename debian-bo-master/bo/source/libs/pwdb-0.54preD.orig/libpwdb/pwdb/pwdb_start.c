/*
 * $Id: pwdb_start.c,v 1.1 1996/10/16 22:16:04 morgan Exp morgan $
 *
 * $Log: pwdb_start.c,v $
 * Revision 1.1  1996/10/16 22:16:04  morgan
 * Initial revision
 *
 */

#include <pwdb/pwdb_public.h>
#include <pwdb/_pwdb_macros.h>
#include "pwdb_module.h"
#include <stdio.h>
#include <string.h>
#include <sys/resource.h>

/*
 * This file contains the initialization and termination code for the
 * database. It also contains code for managing the memory structures
 */

/* structures needed only by this file: */

struct _pwdb_list {
    struct _pwdb_list *next;    /* linked list of pwdb structures */
    struct pwdb *db;            /* this pwdb pointer */

    /* this is information used for cached lookup of pwdb structure */

    time_t cache_expire;        /* non-zero value indicates time to
                                 * this pwdb structure expires */
    char *class;                /* primary class of pwdb, user/group
				 * etc.. */
    char *name;                 /* name used to locate this pwdb */
    int id;                     /* id used to locate this pwdb */
};

/* STATIC data initialized by pwdb_start and cleaned up by pwdb_end */

static int __pwdb__open__count=0;

static struct _pwdb_entry_list *root_pwdb_entry_list=NULL;
static struct _pwdb_list *root_pwdb_list=NULL;

static long core_limit;

/*
 * pwdb structure allocation
 *
 * These functions maintain a linked list of allocated memory. That is
 * cleaned up by pwdb_end(). They return pointers to const structures.
 * this is to ensure that the application uses the library functions to
 * alter their entries.
 */

/*
 * only honor requests for structures allocated by this library
 * This function returns the non-const version of the const struct argument
 * or NULL if it is not found. This way we can ensure that the library is
 * the only thing that changes the elements of the pwdb structures
 */

static struct _pwdb_list *pwdb_old=NULL;    /* this is used by _pwdb_delete */

/* clean up an entry */

static void _pwdb_drop_entry(struct pwdb_entry **e)
{
    struct pwdb_entry *entry;
    entry = *e;
    *e = NULL;

    D(("called."));
    entry->name = NULL;
    if (entry->malloced && entry->value) {  /* name & value were malloc()'d */
        entry->name = _pwdb_delete_string(entry->name);
        memset(entry->value, 0, entry->length);
        free(entry->value);
    } else {                        /* name and value from process memory */
        entry->name = NULL;
    }
    entry->value = NULL;
    entry->compare = NULL;
    entry->strvalue = NULL;
    entry->max_strval_size = 0;
    entry->length = 0;
    free(entry);
    entry = NULL;
}

static struct _pwdb_list *_pwdb_list_check(const struct pwdb *p)
{
    struct _pwdb_list *t;

    D(("called."));
    for (pwdb_old=NULL, t=root_pwdb_list; t; pwdb_old=t, t=t->next) {
        if (t->db == p)
            break;
    }
    return t;
}

static int _pwdb_check(const struct pwdb *p, struct pwdb **np)
{
    struct _pwdb_list *t;

    D(("called."));
    t = _pwdb_list_check(p);
    if (t != NULL) {
        /* has it expired ? */
        if ((t->cache_expire > 0) && (t->cache_expire < time(NULL))) {
            *np = NULL;
            return PWDB_EXPIRED;
        }
        /* found and still valid */
        *np = t->db;
        return PWDB_SUCCESS;
    }
    return PWDB_NOT_FOUND;
}

static void _pwdb_delete(struct _pwdb_list *o)
{
    /* unlink o from the list */

    D(("called."));
    if (pwdb_old) {
        pwdb_old->next = o->next;
    } else {
        root_pwdb_list = o->next;
    }

    /* delete strings: class, name, and reset id */

    o->class = _pwdb_delete_string(o->class);
    o->name = _pwdb_delete_string(o->name);
    o->id = PWDB_ID_UNKNOWN;

    /* delete the list of entries in o */

    if (o->db != NULL) {
        struct _pwdb_entry_list *l;
        for (l=o->db->data; l; ) {
            struct _pwdb_entry_list *t;

            t = l;
            l = l->next;
            _pwdb_drop_entry(&t->entry);
            free(t);
            t = NULL;
        }
    }
    o->db->data = NULL;
     
    /* liberate the memory for 'o' */
    if (o->db->source) {
        free(o->db->source);
        o->db->source = NULL;
    }
    free(o->db);
    free(o);
}


/*
 *  PUBLIC FUNCTIONS..
 */

int pwdb_delete(const struct pwdb **old)
{
    struct _pwdb_list *p;

    D(("called."));
    p = _pwdb_list_check(*old);
    if ( p == NULL ) {
        return PWDB_BAD_REQUEST;
    }

    *old = NULL;

    /* now we delete this structure */

    _pwdb_delete(p);

    return PWDB_SUCCESS;
}

int pwdb_source(const struct pwdb *old, const pwdb_type *src
		, const char *class, const char *name, const int id)
{
    struct _pwdb_list *p;
    int len;

    D(("called."));
    p = _pwdb_list_check(old);
    if ( p == NULL ) {
        return PWDB_BAD_REQUEST;
    }
    
    /* free the old name, class, id, source */

    p->name = _pwdb_delete_string(p->name);
    p->class = _pwdb_delete_string(p->class);
    p->id = PWDB_ID_UNKNOWN;

    /* free the old source list */

    if (p->db->source != NULL) {
        free(p->db->source);
	p->db->source = PWDB_DEFAULT;
    }

    /* find length of src */

    for (len=0; src[len++] != _PWDB_MAX_TYPES; );

    /* set new class, name and id */

    p->class = _pwdb_dup_string(class);
    p->name = _pwdb_dup_string(name);
    p->id = id;
    if ((class && p->class == NULL) || (name && p->name == NULL)) {
	return PWDB_MALLOC;
    }

    /* get some memory */

    p->db->source = calloc(len, sizeof(pwdb_type));
    if ( p->db->source == NULL ) {
        return PWDB_MALLOC;
    }

    /* copy list into source of pwdb */

    memcpy(p->db->source, src, len * sizeof(pwdb_type));

    return PWDB_SUCCESS;
}

int pwdb_new(const struct pwdb **new, int life_sec)
{
    struct _pwdb_list *l;
    struct pwdb *p;

    /* get a pwdb structure */

    D(("called."));
    p = (struct pwdb *) malloc(sizeof(struct pwdb));
    if (p == NULL) {
        return PWDB_MALLOC;
    }

    /* get an element for placing in list */

    l = (struct _pwdb_list *)malloc(sizeof(struct _pwdb_list));
    if (l == NULL) {
        free(p);
        return PWDB_MALLOC;
    }
     
    /* initialize this struct */

    p->source = NULL;
    p->data = NULL;

    /* set caching info */

    l->cache_expire = life_sec ? (time(NULL)+life_sec) : 0 ;  /* set life */
    l->class = NULL;
    l->name = NULL;
    l->id = PWDB_ID_UNKNOWN;

    /* link together with root list */

    l->db = p;
    l->next = root_pwdb_list;
    root_pwdb_list = l;

    /* success */

    *new = p;
    return PWDB_SUCCESS;
}

/*
 * pwdb_expire: set expiry time for the pwdb structure to be less than
 * life_sec from now. If expiry is presently before life_sec passes,
 * nothing is changed.
 */

int pwdb_expire(const struct pwdb *old, int life_sec)
{
    struct _pwdb_list *l;
    time_t when;

    D(("called."));
    l = _pwdb_list_check(old);
    if ( l == NULL ) {
        return PWDB_BAD_REQUEST;
    }

    when = time(NULL);
    if (life_sec > 0)
	when += life_sec;

    if ( !l->cache_expire || l->cache_expire > when ) {
	l->cache_expire = when;
    }

    return PWDB_SUCCESS;
}

/*
 * this function takes a constant argument looks it up in the
 * root_pwdb list and then makes a copy of the entry and adds it to
 * the root_pwdb_entry_list before returning it to the application
 * as a const *. This way we ensure that the application must use
 * official functions to maintain the pwdb structures.
 */
 
int pwdb_get_entry(const struct pwdb *p, const char *name
           , const struct pwdb_entry **e)
{
    struct pwdb *pw;
    struct _pwdb_entry_list *list_entry, *entry;
    struct pwdb_entry *new;
    int retval;

    D(("called."));
    if ( ((retval = _pwdb_check(p, &pw)) != PWDB_SUCCESS) ||
         (retval=PWDB_BAD_REQUEST, pw == NULL) ) {
        return retval;
    }

    /* now search for the requested item in this pwdb structure's list */

    for (entry=pw->data; entry; entry = entry->next) {
        if (strcmp(entry->entry->name,name) == 0)
            break;
    }
    if (entry == NULL)
        return PWDB_BAD_REQUEST;
     
    /* this function makes a copy of the entry->entry, in pw */

    list_entry = (struct _pwdb_entry_list *)
    malloc(sizeof(struct _pwdb_entry_list));
    if (list_entry == NULL)
        return PWDB_MALLOC;

    new = (struct pwdb_entry *) malloc( sizeof(struct pwdb_entry) );
    if (new == NULL) {
        free(list_entry);
        return PWDB_MALLOC;
    }

    new->value = malloc(entry->entry->length);
    if (new->value == NULL) {
        free(new);
        free(list_entry);
        return PWDB_MALLOC;
    }

    new->name = _pwdb_dup_string(entry->entry->name);
    if (new->name == NULL) {
        free(new->value);
        free(new);
        free(list_entry);
        return PWDB_MALLOC;
    }

    /* copy entry */

    new->malloced = 1;
    new->max_strval_size = entry->entry->max_strval_size;
    new->strvalue = entry->entry->strvalue;
    new->compare = entry->entry->compare;
    new->length = entry->entry->length;
    memcpy(new->value, entry->entry->value, new->length);

    /* link entry to list */

    list_entry->entry = new;
    list_entry->next = root_pwdb_entry_list;
    root_pwdb_entry_list = list_entry;

    /* inform application */

    *e = new;
    return PWDB_SUCCESS;
}

/*
 * this function (re)sets an entry in the given const pwdb * structure
 * if the length of the new datum is negative, the entry is deleted
 */

int pwdb_set_entry(const struct pwdb *p, const char *entry
           , const void *datum, const int length
           , int (*compare)(const void *, const void *, int)
           , int (*strvalue)(const void *, char *, int)
           , int max_strval_size)
{
    struct pwdb *pw;
    struct _pwdb_entry_list *last=NULL, *this;
    int retval;

    D(("called."));
    if ( ((retval = _pwdb_check(p, &pw)) != PWDB_SUCCESS) ||
         (retval=PWDB_BAD_REQUEST, pw == NULL) ) {
        return retval;
    }

    /* do we just need to replace some existing entry? */

    for ( this=pw->data; this; last=this, this=this->next ) {
        if ( this->entry == NULL ) {
            return PWDB_ABORT; /* list corrupted */
        } else if ( strcmp(this->entry->name, entry) == 0 ) {
            if ( (length >= 0) && (datum != NULL) ) {  /* replace this entry */
                void *tmp;

                memset(this->entry->value, 0, this->entry->length);
                tmp = realloc(this->entry->value, length);
                if (tmp) {
                    this->entry->length = length;
                    this->entry->compare = compare;
                    this->entry->strvalue = strvalue;
                    this->entry->max_strval_size = max_strval_size;
                    memcpy(this->entry->value=tmp, datum,
                           this->entry->length);
                    return PWDB_SUCCESS;
                } else {
                    if (last)
                        last->next = this->next;
                    else
                        pw->data = this->next;
                    _pwdb_drop_entry(&this->entry);
                    free(this);
                    return PWDB_MALLOC;
                }
            }
            if ( length < 0 ) {            /* delete the entry */
                if (last)
                    last->next = this->next;
                else
                    pw->data = this->next;
                _pwdb_drop_entry(&this->entry);
                free(this);
            } else if ( datum == NULL ) {      /* setting entry to NULL */
                if (this->entry->malloced) {
                    memset(this->entry->value, 0, this->entry->length);
                    free(this->entry->value);
                }
                this->entry->value = NULL;
                this->entry->compare = NULL;
                this->entry->strvalue = NULL;
                this->entry->max_strval_size = 0;
                this->entry->length = 0;   /* ignore length.. set to 0 */
            }
            return PWDB_SUCCESS;  /* these cannot fail */
        }
    }

    /* we have to add a new entry */

    this = (struct _pwdb_entry_list *)
    malloc(sizeof(struct _pwdb_entry_list));
    if (this == NULL)
        return PWDB_MALLOC;

    this->entry = (struct pwdb_entry *) malloc( sizeof(struct pwdb_entry) );
    if (this->entry == NULL) {
        free(this);
        return PWDB_MALLOC;
    }

    this->entry->name = _pwdb_dup_string(entry);
    if (this->entry->name == NULL) {
        free(this->entry);
        free(this);
        return PWDB_MALLOC;
    }

    if (datum != NULL) {
        this->entry->value = malloc(length);
        if (this->entry->value == NULL) {
            this->entry->name = _pwdb_delete_string(this->entry->name);
            free(this->entry);
            free(this);
            return PWDB_MALLOC;
        }
        memcpy(this->entry->value, datum, length);
        this->entry->malloced = 1;
        this->entry->length = length;
    } else {
        this->entry->value = NULL;
        this->entry->malloced = 0;
        this->entry->length = 0;
    }        
        
    /* initialize other fields */
    this->entry->compare = compare;
    this->entry->strvalue = strvalue;
    this->entry->max_strval_size = max_strval_size;

    /* link to list */

    this->next = pw->data;
    pw->data = this;

    return PWDB_SUCCESS;
}

/*
 * This function deletes an entry returned from pwdb_get_entry.
 */

int pwdb_entry_delete(const struct pwdb_entry **e)
{
    struct _pwdb_entry_list *last=NULL;
    struct _pwdb_entry_list *this;

    D(("called."));
    for (this=root_pwdb_entry_list; this; last=this, this=this->next) {
        if (*e == this->entry) {
            *e = NULL;                  /* break link to application */
    
            _pwdb_drop_entry(&this->entry);           /* delete contents */
            if (last)                             /* close link in chain */
                last->next = this->next;
            else
                root_pwdb_entry_list = this->next;
            this->next = NULL;
            free(this);                                /* free list item */
            this = NULL;
            return PWDB_SUCCESS;
        }
    }

    return PWDB_NOT_FOUND;
}

static int _pwdb_same_src(const pwdb_type *src1, const pwdb_type *src2)
{
    D(("called."));
    if (src1 == src2) {

	return PWDB_SUCCESS;

    } else if (src1 == NULL || src2 == NULL) {

	return PWDB_NOT_FOUND;

    } else {

	do {
	    if ( *src1 != *src2 )
		return PWDB_NOT_FOUND;
	    src1++;
	} while ( *src2++ != _PWDB_MAX_TYPES );

	return PWDB_SUCCESS;
    }
}

static int _strsame(const char *s1, const char *s2)
{
    D(("called."));
    if (s1 == s2)
	return PWDB_SUCCESS;

    if (s1 == NULL || s2 == NULL)
	return PWDB_BAD_REQUEST;

    return ( strcmp(s1,s2) );
}

/*
 * This function attempts to copy the entries of *q into *p. If overwrite
 * is not set, then the elements of p will overwrite those of q where there
 * is a clash -- otherwise these elements of p will survive.
 */

/*
 * XXX - This is not very efficient. Perhaps we should store entries
 * alphabetically, or better yet with some sort of hash table? Later.
 */

int pwdb_merge(const struct pwdb *p, const struct pwdb *q, int overwrite)
{
    struct _pwdb_list *true_lp, *true_lq;
    struct pwdb *true_p, *true_q;
    register struct _pwdb_entry_list *tlq;
    int copied_one = PWDB_FALSE, retval = PWDB_SUCCESS;
    time_t now;

    D(("called."));
    /* locate true_p and true_q - check neither has expired */
    (void) time(&now);
    true_lp = _pwdb_list_check(p);
    true_lq = _pwdb_list_check(q);
    D(("checking if returned pwdb's are good"));
    if ( (retval = PWDB_BAD_REQUEST, true_lp == NULL
	  || true_lq == NULL || true_lq == true_lp )
	 || (retval = PWDB_EXPIRED, now > true_lp->cache_expire)
	 || now > true_lq->cache_expire ) {
	true_lp = true_lq = NULL;
	p = q = NULL;
	D(("failed because: %s", pwdb_strerror(retval)));
	return retval;
    }
    true_p = true_lp->db;
    true_q = true_lq->db;

    /* loop through the elements of q */
    for (tlq=true_q->data; tlq; tlq = tlq->next) {
	register struct _pwdb_entry_list *entry;

	/* check if we should skip this entry */
	if (!overwrite) {

	    /* does true_p already have an entry of this name? */
	    for (entry=true_p->data; entry; entry = entry->next) {
		if (strcmp(entry->entry->name,tlq->entry->name) == 0)
		    break;
	    }
	    if (entry != NULL) {
		entry = NULL;
		continue;               /* we don't want to overwrite it */
	    }
	}

        /* write this element to true_p */
	entry = true_q->data;

	retval = pwdb_set_entry(p, entry->entry->name
				, entry->entry->value, entry->entry->length
				, entry->entry->compare, entry->entry->strvalue
/*
 * XXX - how about a
 *
 *            void *valuestr(const char *str, int *value_length)
 *
 * entry? To do the reverse of the above and malloc() a void *entry of the
 * returned length. This could be useful for applications like adduser, which
 * typically take strings of some type and place the corresponding entries
 * in the user's database.
 */
				, entry->entry->max_strval_size);

	if (retval != PWDB_SUCCESS) {
	    break;
	}

	/* note we have copied an element - needed so we can align lifetime */
	copied_one = PWDB_TRUE;
    }

    /*
     * if we took any information from the pointer q then the caching
     * time for p should not be greater than q's
     */

    if (copied_one && true_lq->cache_expire
	&& true_lp->cache_expire > true_lq->cache_expire) {
	D(("resetting p's cache to expire in %d seconds"
	   , true_lq->cache_expire-time(NULL)));
	true_lp->cache_expire = true_lq->cache_expire;
    }

    return retval;
}

int pwdb_cached(const char *class, const pwdb_type *src
		, const char *name, const int id
		, const struct pwdb **p)
{
    struct _pwdb_list *list;

    D(("called."));
    if (p == NULL || *p != NULL)
	return PWDB_BAD_REQUEST;

    for (list=root_pwdb_list; list; list = list->next) {
	if ( id != list->id ) {
	    continue;                        /* id's don't match */
	} else if ( _strsame(class,list->class) != PWDB_SUCCESS ) {
	    continue;                        /* classes are different */
	} else if ( _strsame(name,list->name) != PWDB_SUCCESS ) {
	    continue;                        /* names don't match */
	} else if ( _pwdb_same_src(src, list->db->source) != PWDB_SUCCESS ) {
	    continue;                        /* different sources */
	} else {

	    /* this is the correct record. We should test it for age
	     * and return it if appropriate. Otherwise continue
	     * searching */

	    if ( !list->cache_expire || (time(NULL) < list->cache_expire)) {
		*p = list->db;
		return PWDB_SUCCESS;
	    }
	}
    }

    return PWDB_NOT_FOUND;
}

/*
 * These functions initialize the library functions.
 *  - pwdb_start reads the config file and creates the array
 *    const pwdb_type pwdb_policy[]. This array represents the
 *    preferred order that the various databases are to be accessed
 *    in.
 *  - note, pwdb_end free()'s all of the memory allocated by the library.
 */

int pwdb_end(void)
{
    /* maintain a count of the number of times the library is started */

    D(("called. (count=%d)", __pwdb__open__count));
    if (__pwdb__open__count-- > 1) {
        return PWDB_SUCCESS;
    } else if (__pwdb__open__count < 0) {            /* ended too often */
	__pwdb__open__count = 0;
        return PWDB_ABORT;
    }

    while (root_pwdb_entry_list) {                 /* clean out entries */
        const struct pwdb_entry *t;

        t = root_pwdb_entry_list->entry;
        pwdb_entry_delete(&t);       /* this steps along root_..._list */
    }

    while (root_pwdb_list) {                       /* clean out entries */
        const struct pwdb *t;

        t = root_pwdb_list->db;
        pwdb_delete(&t);             /* this steps along root_..._list */
    }

    _pwdb_delete_policy();

    /* handle limits */
    {
        struct rlimit rlim;

        getrlimit(RLIMIT_CORE, &rlim);
        rlim.rlim_cur = core_limit;

        /* set core limit to previous value */
        rlim.rlim_cur=0;
        if (setrlimit(RLIMIT_CORE, &rlim) < 0)
            /* setting the soft core limit to prev value failed */
            ; /* XXX: log a warning ? */
    }

    return PWDB_SUCCESS;
}

/* start the database management */

int pwdb_start(void)
{
    int retval;

    D(("called."));
    /* maintain a count of the number of times the library is started */

    if (__pwdb__open__count++) {
	D(("opened %d", __pwdb__open__count));
        return PWDB_SUCCESS;
    }

    if (root_pwdb_list != NULL) {
	D(("curious root_pwdb_list non-null"));
        return PWDB_BLOCKED;
    }
    if (root_pwdb_entry_list != NULL) {
	D(("curious root_pwdb_entry_list non-null"));
        return PWDB_BLOCKED;
    }

    /* initialize the default database arrays */

    D(("reading conf file"));
    retval = _pwdb_read_conf();

    if (retval != PWDB_SUCCESS) {
	D(("failed conf file read"));
        __pwdb__open__count=0;
    }

    /* handle limits */
    {
        struct rlimit rlim;
        
        getrlimit(RLIMIT_CORE, &rlim);
        core_limit = rlim.rlim_cur;
        
        /* set core limit to 0 == turn off core dumps */
        rlim.rlim_cur=0;
        if (setrlimit(RLIMIT_CORE, &rlim) < 0)
            /* setting the soft core limit to 0 failed */
            ; /* XXX: log a warning ? */
    }            

    /* return success or failure */

    D(("got(count=%d): %s", __pwdb__open__count, pwdb_strerror(retval)));
    return retval;
}

/* -- end of file -- */
