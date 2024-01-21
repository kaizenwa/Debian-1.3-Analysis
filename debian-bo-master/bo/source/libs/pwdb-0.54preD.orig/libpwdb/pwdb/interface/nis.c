
#ifndef PWDB_INTERFACE_NIS
#define PWDB_INTERFACE_NIS

/*
 * user functions
 */

#include "nis/user.c"
static struct _pwdb_module _pwdb_nis_struct = {
    PWDB_NIS,
    "nis",
    "user",
    _pwdb_nis_locate,
    _pwdb_nis_request,
    _pwdb_nis_replace,
    _pwdb_nis_delete,
    _pwdb_nis_support,
    _pwdb_nis_flags
};

/* 
 * group functions
 */

#include "nis/group.c"
static struct _pwdb_module _pwdb_nis_g_struct = {
    PWDB_NIS,
    "nis",
    "group",
    _pwdb_nis_glocate,
    _pwdb_nis_grequest,
    _pwdb_nis_greplace,
    _pwdb_nis_gdelete,
    _pwdb_nis_gsupport,
    _pwdb_nis_gflags
};

#endif /* PWDB_INTERFACE_NIS */

