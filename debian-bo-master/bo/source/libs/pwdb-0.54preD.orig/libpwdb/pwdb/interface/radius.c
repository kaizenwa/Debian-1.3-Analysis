
#ifndef PWDB_INTERFACE_RADIUS
#define PWDB_INTERFACE_RADIUS

/*
 * user functions
 */

#include "radius/user.c"
static struct _pwdb_module _pwdb_radius_struct = {
    PWDB_RADIUS,
    "radius",
    "user",
    _pwdb_radius_locate,
    _pwdb_radius_request,
    _pwdb_radius_replace,
    _pwdb_radius_delete,
    _pwdb_radius_support,
    _pwdb_radius_flags
};

/*
 * group functions
 */

#include "radius/group.c"
static struct _pwdb_module _pwdb_radius_g_struct = {
    PWDB_RADIUS,
    "radius",
    "group",
    _pwdb_radius_glocate,
    _pwdb_radius_grequest,
    _pwdb_radius_greplace,
    _pwdb_radius_gdelete,
    _pwdb_radius_gsupport,
    _pwdb_radius_gflags
};

#endif /* PWDB_INTERFACE_RADIUS */

