
#ifndef PWDB_INTERFACE_UNIX
#define PWDB_INTERFACE_UNIX

/*
 * common defines
 */
#define TIME_TO_LIVE           1000           /* lifetime of unix-pwdb /sec */
#define UNIX_MAGIC_PASSWD      "U"            /* passwd is in /etc/passwd */

/*
 * user functions
 */
#include "unix/user.c"
const static struct _pwdb_module _pwdb_unix_struct = {
    PWDB_UNIX,
    "unix",
    "user",                           /* class of this module */
    _pwdb_unix_locate,
    _pwdb_unix_request,
    _pwdb_unix_replace,
    _pwdb_unix_delete,
    _pwdb_unix_support,
    _pwdb_unix_flags,
    NULL                              /* clean up function */
};

/*
 * group functions
 */
#include "unix/group.c"
const static struct _pwdb_module _pwdb_unix_g_struct = {
    PWDB_UNIX,
    "unix",
    "group",                          /* class of this module */
    _pwdb_unix_glocate,
    _pwdb_unix_grequest,
    _pwdb_unix_greplace,
    _pwdb_unix_gdelete,
    _pwdb_unix_gsupport,
    _pwdb_unix_flags,
    NULL                              /* clean up function */
};

#endif
