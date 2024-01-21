/* This file contains the necessary interface functions to
 * the pwdb modules. They are declared as static and this file
 * should be included in the module interface file, because
 * the function addresses are used in the structures from
 * pwdb_module.c file, and I have no idea how to feed a pointer
 * to an external function in that struct...
 * CG
 */

/************************************************
 * RADIUS Server interface (GROUP)
 ************************************************/

static int _pwdb_radius_glocate(const char *name,
                                const int id,
                                const struct pwdb **p)
{
    return PWDB_UNSUPPORTED;
}

static int _pwdb_radius_grequest(const char *entry_name,
                                 const struct pwdb **p)
{
        return PWDB_UNSUPPORTED;
}

static int _pwdb_radius_greplace(const char *name,
                                 const int id,
                                 const struct pwdb **p)
{
    return PWDB_UNSUPPORTED;
}

static int _pwdb_radius_gdelete(const char *name,
                                const int id,
                                const struct pwdb **p)
{
    return PWDB_UNSUPPORTED;
}

static int _pwdb_radius_gsupport(const char *entry_name)
{
    return PWDB_UNSUPPORTED;
}

static int _pwdb_radius_gflags(pwdb_flag *flags)
{
    return PWDB_UNSUPPORTED;
}

