/*
 * external defines for generic calls to the pwdb libraries
 *
 * <pwdb/pwdb_public.h>
 */

#ifndef PWDB_PUBLIC_H
#define PWDB_PUBLIC_H

#include <time.h>                      /* we need this for time_t */

/* types */

/* database types */

enum {
     PWDB_UNIX,                        /* read the UNIX entry */
     PWDB_SHADOW,                      /* read the UNIX+SHADOW entry */
     PWDB_NIS,                         /* read the NIS database */
     PWDB_RADIUS,                      /* read the RADIUS database */
/* ---- */
     _PWDB_MAX_TYPES                   /* upper edge of list */
};
typedef int pwdb_type;
#define PWDB_DEFAULT ((pwdb_type *)0)  /* library should search listed types */

/* access flags type */

typedef int pwdb_flag;

#define PWDB_F_NOACCESS     01 /* insufficient privilege to access database */
#define PWDB_F_NOUPDATE     02 /* insufficient privilege to alter an entry */
#define PWDB_F_PASS_PHRASE  04 /* to access the database, the process must
                                * supply a "pass_phrase" entry with a pre-
                                * allocated pwdb structure (use pwdb_new()
                                * call) */

/* some logic */

#define PWDB_TRUE                    1
#define PWDB_FALSE                   0

/* public structures */

struct pwdb_entry {
    char *name;                /* name of attribute */
    int malloced;              /* flag to indicate if next
				* field is to be free()'d */
    void *value;               /* data for value of attribute */
    int length;                /* length in bytes of attribute */
    int (*compare)(const void *data1, const void *data2, int length);
                               /* comparison function */
    int (*strvalue)(const void *data, char *strbuffer, int length);
                               /* get a char representation of the
				* value in the strbuffer pre-alocated
				* by the calling application */
    int max_strval_size;       /* the maximum value the above function will
				* will produce, so that the app will know
				* how much space to allocate */
};

struct _pwdb_entry_list {
    struct pwdb_entry *entry;         /* this attribute entry pointer */
    struct _pwdb_entry_list *next;    /* linked list of entries       */
};

struct pwdb {
    pwdb_type *source;                /* origin of attribute data */
    struct _pwdb_entry_list *data;    /* OPAQUE attribute data */
};

/*
 * FUNCTIONS
 */

/* return codes */

#define PWDB_SUCCESS            0  /* "task completed successfully" */
#define PWDB_BAD_REQUEST        1  /* "request not recognized" */
#define PWDB_TOO_WEAK           2  /* "insufficient privilege for operation" */
#define PWDB_ABORT              3  /* "internal failure - seek help" */
#define PWDB_BLOCKED            4  /* "another process has locked resource" */
#define PWDB_MALLOC             5  /* "insufficient memory for operation" */
#define PWDB_NOT_FOUND          6  /* "requested item was not found" */
#define PWDB_PASS_PHRASE_REQD   7  /* "pass_phrase needed to satisfy request"*/
#define PWDB_CONF_ERR           8  /* "file " PWDB_CONF " needs to be fixed" */
#define PWDB_EXPIRED            9  /* "pwdb structure is no longer valid" */
#define PWDB_UNSUPPORTED       10  /* this function is not yet supported */
#define PWDB_TIMEOUT           11  /* request timed out - for networked databases */

/*
 * pwdb_type pointer lists, containing references to the preferred
 * order of databses [initialized by pwdb_start()]
 */

extern const pwdb_type **pwdb_policy;
extern const pwdb_type **pwdb_group_policy;

/*
 * These functions initialize the library functions:
 *
 * - pwdb_start reads the config file and creates the array const
 *   pwdb_type *pwdb_policy[]. This array represents the preferred
 *   order that sequences of various databases are to be accessed
 *   in. It is also responsible for creating the pwdb_group_policy
 *   list
 *
 * - note, pwdb_end free()'s all of the memory allocated by the library.
 */

int pwdb_start(void);
int pwdb_end(void);

/*
 * pwdb structure allocation
 *
 * These functions maintain a linked list of allocated memory. That is
 * cleaned up by pwdb_end()
 */

int pwdb_new(const struct pwdb **new, int life_sec);
int pwdb_delete(const struct pwdb **old);

/*
 * This function is used to set the source of the indicate pwdb structure
 */

int pwdb_source(const struct pwdb *old, const pwdb_type *src
		, const char *class, const char *name, const int id);

/*
 * This function resets the lifetime of this structure. It _cannot_
 * extend the structure's life.
 */

int pwdb_expire(const struct pwdb *old, int life_sec);

/*
 * This function free()'s the memory associated with an entry returned
 * by pwdb_get_entry.
 */

int pwdb_delete_entry(const struct pwdb_entry **old);

/*
 *
 * These functions read/set entries in the pwdb structure. To delete an
 * entry pwdb_set_entry should be passed an entry of length -1.
 */

int pwdb_get_entry(const struct pwdb *p, const char *entry
		   , const struct pwdb_entry **e);
int pwdb_set_entry(const struct pwdb *p, const char *entry, const void *datum
		   , const int length
		   , int (*compare)(const void *,const void *, int)
		   , int (*strvalue)(const void *, char *, int)
		   , int max_strval_size);

/* this function deletes a pwdb_entry returned from pwdb_get_entry */

int pwdb_entry_delete(const struct pwdb_entry **entry_p);

/*
 * support functions
 */

const char *pwdb_strerror(int errorno);
const char *pwdb_db_name(pwdb_type src);
void pwdb_print_pwdb_struct(const struct pwdb *);
void debug_pwdb_struct(const struct pwdb *);

/*
 * database reading
 *
 * this function returns the entry in a given database that
 * corresponds to the indicated name or id (or both).
 *
 * at least id or name must be specified.
 *
 * NULL indicates error.
 */

#define PWDB_ID_UNKNOWN      -3
#define PWDB_NAME_UNKNOWN    ((const char *)0)

/* functions to probe behavior of the database(s) */
	   
int pwdb_support(const char *class, const pwdb_type *db
		 , const char *entry_name);

int pwdb_flags(const char *class, const pwdb_type *db
	       , pwdb_flag *flag_p);

/* functions to obtain information from a database */

int pwdb_locate(const char *class, const pwdb_type *db
		, const char *name, const int id
		, const struct pwdb **p);

int pwdb_request(const char *class, const pwdb_type *db
		 , const char *entry_name
                 , const struct pwdb **p);
                 
/*
 * database writing
 *
 * These functions replace/add/delete an entry to the indicated database
 * If the database does not exist it may be created with this entry.
 */

/* copy all the entries of q into p (overwrite -> replace old entries in p) */
int pwdb_merge(const struct pwdb *p, const struct pwdb *q, int overwrite);

int pwdb_replace(const char *class, const pwdb_type *
		 , const char *, const int, const struct pwdb **);
int pwdb_remove(const char *class, const pwdb_type *
		, const char *, const int, const struct pwdb **);

/*
 * Misc. helper functions
 */

char *_pwdb_delete_string(char *s);
char *_pwdb_dup_string(const char *x);

/* test if a flag is set */

#define pwdb_on(m, flag)             ( ((m) & (flag)) ? 1:0 )


/* --------------------------------------------------------------------- *
   POSIX compliance. This is very minimal currently. Essentially, we
   map the only four functions defined by POSIX.1 into four POSIX
   compliant front end functions for libpwdb.  The functions are only
   useful for probing the user-attributes defined by POSIX (username,
   uid, gid, shell, and home; goupname, gid, and members).

   It is anticipated that the remaining attributes are only needed by
   code that knows about the generic interface to libpwdb -- code that
   can handle the more general API.
 * --------------------------------------------------------------------- */

#include <sys/types.h>
#include <pwd.h>
#include <grp.h>

/* a list of still to be implemented functions */

/*
  should #define misc functions -> _posix_undefined (for runtime warning)
 */

/* now a list of functions that have been implemented */

extern struct passwd *pwdb_posix_getpwnam(const char *user);
#define getpwnam(user)       pwdb_posix_getpwnam(user)

extern struct passwd *pwdb_posix_getpwuid(uid_t uid);
#define getpwuid(uid)       pwdb_posix_getpwuid(uid)

extern struct group *pwdb_posix_getgrnam(const char *group);
#define getgrnam(group)      pwdb_posix_getgrnam(group)

extern struct group *pwdb_posix_getgrgid(gid_t gid);
#define getgrgid(gid)        pwdb_posix_getgrgid(gid)

extern char *pwdb_posix_getlogin(void);
#define getlogin             pwdb_posix_getlogin

/* All other cases get this... */
extern void _posix_undefined(void);

/* --------------------------------------------------------------------- */

#endif /* PWDB_PUBLIC_H */
