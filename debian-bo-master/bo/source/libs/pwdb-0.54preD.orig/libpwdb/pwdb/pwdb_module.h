#ifndef PWDB_MODULE_H
#define PWDB_MODULE_H

/*
 * $Id: pwdb_module.h,v 1.1 1996/10/16 22:16:04 morgan Exp morgan $
 *
 * This file contains the generic interface private defines, that are
 * needed to be shared by different compiled files in libpwdb. It is
 * used to build the library
 */

/*
 * $Log: pwdb_module.h,v $
 * Revision 1.1  1996/10/16 22:16:04  morgan
 * Initial revision
 *
 */

#include "pwdb_config.h"
#include <pwdb/pwdb_public.h>
#include <time.h>
#include <stdlib.h>


/* ******************************************************************* *
 * module linker functions
 * ******************************************************************* */

struct _pwdb_module {
    pwdb_type type;
    const char *name;
    const char *class;
    int (*locate)(const char *name, const int id, const struct pwdb **p);
    int (*request)(const char *entry_name, const struct pwdb **p);
    int (*replace)(const char *name, const int id, const struct pwdb **p);
    int (*delete)(const char *name, const int id, const struct pwdb **p);
    int (*support)(const char *entry_name);
    int (*flags)(pwdb_flag *flags);
    int (*cleanup)(int status);
};

/*
 * this function invokes the corresponding function for the given
 * source and location type.
 */

typedef enum {
    _PWDB_LOCATE,         /* find an entry and return it */
    _PWDB_REPLACE,        /* replace an entry - or create it */
    _PWDB_REMOVE,         /* remove an entry */
    _PWDB_REQUEST,        /* request an additional entry from the database */
    _PWDB_FLAGS,          /* get a flags summary of the facilities/strength
			   * of the current process with respect to the
			   * indicated database */
    _PWDB_SUPPORT,         /* indicate if the given entry is supported
			   * by the database */
    _PWDB_CLEANUP          /* used to signal module is about to be dumped */
} pwdb_fn;

int _pwdb_dispatch(const char *class
		   , const pwdb_type src
		   , const pwdb_fn fn
		   , const char *name, const int id
		   , const struct pwdb **p
		   , pwdb_flag *flag_p);

int pwdb_cached(const char *class, const pwdb_type *src, const char *name
		, const int id, const struct pwdb **p);

const pwdb_type **_pwdb_get_policy(const char *class);
void _pwdb_delete_policy(void);
int _pwdb_read_conf(void);

#endif /* PWDB_MODULE_H */
