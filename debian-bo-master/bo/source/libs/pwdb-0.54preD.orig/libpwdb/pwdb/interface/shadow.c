
#ifndef PWDB_INTERFACE_SHADOW
#define PWDB_INTERFACE_SHADOW

/* 
 * user functions
 */

#include "shadow/user.c"
static struct _pwdb_module _pwdb_shadow_struct = {
    PWDB_SHADOW,
    "shadow",
    "user",                           /* class of this module */
    _pwdb_shadow_locate,
    _pwdb_shadow_request,
    _pwdb_shadow_replace,
    _pwdb_shadow_delete,
    _pwdb_shadow_support,
    _pwdb_shadow_flags,
    NULL
};

/*
 * group functions
 */

#include "shadow/group.c"
static struct _pwdb_module _pwdb_shadow_g_struct = {
    PWDB_SHADOW,
    "shadow",
    "group",                           /* class of this module */
    _pwdb_shadow_glocate,
    _pwdb_shadow_grequest,
    _pwdb_shadow_greplace,
    _pwdb_shadow_gdelete,
    _pwdb_shadow_gsupport,
    _pwdb_shadow_flags,
    NULL
};

#endif /* PWDB_INTERFACE_SHADOW */
