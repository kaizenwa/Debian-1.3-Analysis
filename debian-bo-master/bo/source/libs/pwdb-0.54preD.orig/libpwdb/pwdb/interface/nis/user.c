/* This file contains the necessary interface functions to
 * the pwdb modules. They are declared as static and this file
 * should be included in the module interface file, because
 * the function addresses are used in the structures from
 * pwdb_module.c file, and I have no idea how to feed a pointer
 * to an external function in that struct...
 * CG
 */

/*************************************************
 * NIS USER INTERFACE FUNCTIONS
 *************************************************/

static int _pwdb_nis_locate(const char *name,
                            const int id,
                            const struct pwdb **p)
{
    int retval;
    struct __pwdb_passwd * pwd;
    int tid, tgid; /* temporary locations for filling pwdb_entry. (safety) */
    const struct pwdb_entry * pwe;
    int name_malloced;
            
    if (p == NULL) {
        return PWDB_ABORT;           /* were are we supposed to put pwdb ? */
    }

    name_malloced = 0;
    if ((name == PWDB_NAME_UNKNOWN) && (id == PWDB_ID_UNKNOWN)) {
        /* Try to make use of struct pwdb, if available */
        if (!*p)
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            name = (const char *) pwe->value;
            name_malloced = 1;
        } else
            return PWDB_BAD_REQUEST;
        if (!name) { /* we've got a NULL pointer */
            pwdb_entry_delete(&pwe);
            return PWDB_BAD_REQUEST;
        }
    }
    
    if (!*p) {
        retval = pwdb_new(p, 0);
        if (retval != PWDB_SUCCESS) {
            DO_DEBUG;
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return retval;
        }
    }
 
    if (id == PWDB_ID_UNKNOWN) {
        pwd = __pwdbNIS_getpwnam(name);
        if (!pwd) {
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return PWDB_NOT_FOUND;
        }
    } else if (name == PWDB_NAME_UNKNOWN) {
        pwd = __pwdbNIS_getpwuid(id);
        if (!pwd)
            return PWDB_NOT_FOUND;
    } else {
        /* both name and id where specified. */
        pwd = __pwdbNIS_getpwnam(name);
        if (!pwd) {
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return PWDB_NOT_FOUND;
        }
        if (pwd->pw_uid != id) {
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return PWDB_NOT_FOUND;
        }
    }
    
    /*
     * Now we have the user info in the pwd pointer. Fill out the
     * pwdb_entry with this info
     */

    tid = pwd->pw_uid;
    tgid = pwd->pw_gid;

    /* 
     * Start the show
     */

    retval = pwdb_set_entry(*p,"user", name, 1+strlen(name), NULL, NULL, 0);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        if (name_malloced)
            pwdb_entry_delete(&pwe);
        return retval;
    }

    retval = pwdb_set_entry(*p, "uid", &tid, sizeof(int), NULL, NULL, 0);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        if (name_malloced)
            pwdb_entry_delete(&pwe);
        return retval;
    }

    retval = pwdb_set_entry(*p, "gid", &tgid, sizeof(int), NULL, NULL, 0);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        if (name_malloced)
            pwdb_entry_delete(&pwe);
        return retval;
    }

    retval = pwdb_set_entry(*p, "passwd", pwd->pw_passwd,
                            1+strlen(pwd->pw_passwd), NULL, NULL, 0);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        if (name_malloced)
            pwdb_entry_delete(&pwe);
        return retval;
    }

    retval = pwdb_set_entry(*p, "gecos", pwd->pw_gecos,
                            1+strlen(pwd->pw_gecos), NULL, NULL, 0);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        if (name_malloced)
            pwdb_entry_delete(&pwe);
        return retval;
    }

    retval = pwdb_set_entry(*p, "dir", pwd->pw_dir,
                            1+strlen(pwd->pw_dir), NULL, NULL, 0);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        if (name_malloced)
            pwdb_entry_delete(&pwe);
        return retval;
    }

    retval = pwdb_set_entry(*p, "shell", pwd->pw_shell,
                            1+strlen(pwd->pw_shell), NULL, NULL, 0);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        if (name_malloced)
            pwdb_entry_delete(&pwe);
        return retval;
    }

    if (name_malloced)
        pwdb_entry_delete(&pwe);
    return PWDB_SUCCESS;
}

static int _pwdb_nis_request(const char *entry_name,
    const struct pwdb **p)
{
    return PWDB_UNSUPPORTED;
}

/*
 * The user entry update function
 * We try to deal with the particular case of changing the user
 * login name by setting the account name to the value of the
 * pwdb_entry "name".
 */
static int _pwdb_nis_replace(const char *name,
    const int id,
    const struct pwdb **p)
{
    const struct __pwdb_passwd * pw;
    struct __pwdb_passwd pwent;
    const struct pwdb_entry * ent;
    int retval;
    const struct pwdb_entry * pwe;
    int name_malloced;
                            
    /* do the necessary checks */
    if (p == NULL) {
        return PWDB_ABORT;           /* were are we supposed to put pwdb ? */
    }
    if (!*p) /* we need some data to change ... */
        return PWDB_BAD_REQUEST;

    name_malloced = 0;
    if ((name == PWDB_NAME_UNKNOWN) && (id == PWDB_ID_UNKNOWN)) {
        /* Try to make use of struct pwdb, if available */
        if (!*p)
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            name = (const char *) pwe->value;
            name_malloced = 1;
        } else
            return PWDB_BAD_REQUEST;
        if (!name) { /* we've got a NULL pointer */
            pwdb_entry_delete(&pwe);
            return PWDB_BAD_REQUEST;
        }
    }

    /* see if the user exists in the database 
     * we open first the database read only
     */
    if (id == PWDB_ID_UNKNOWN) {
        /* name should be okay - tested previously */
        pw = __pwdbNIS_getpwnam(name);
        if (!pw) {
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return PWDB_NOT_FOUND;
        }
    } else if (name == PWDB_NAME_UNKNOWN) {
        /* locate by id */
        /* XXX: how to handle multiple equal IDs in this case ... ? */
        pw = __pwdbNIS_getpwuid(id);
        if (!pw)
            return PWDB_NOT_FOUND;
    } else {
        /* we have both name and ID passed */
        pw = __pwdbNIS_getpwnam(name);
        if (!pw) {
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return PWDB_NOT_FOUND;
        }
        if (pw->pw_uid != id) {
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return PWDB_NOT_FOUND;
        }
    }
    
    /* now try to extract some data from pwdb_entry structure */
    /* NAME */
    if (!name_malloced)
        retval = pwdb_get_entry(*p,"user", &ent);
    else
        retval = PWDB_BAD_REQUEST;
    if (retval == PWDB_SUCCESS) {
        /* we've got the name */
        pwent.pw_name=_pwdb_dup_string(ent->value);
        pwdb_entry_delete(&ent);
        if (!pwent.pw_name)
            retval = PWDB_MALLOC;
    } else if (retval == PWDB_BAD_REQUEST) {
        /* name is not available in pwdb_entry */
        pwent.pw_name=_pwdb_dup_string(pw->pw_name);
        if (pwent.pw_name)
            retval = PWDB_SUCCESS;
        else
            retval = PWDB_MALLOC;
    }
    if (retval != PWDB_SUCCESS) { /* some other odd error */
        if (name_malloced)
            pwdb_entry_delete(&pwe);
        return retval;
    }

    /* Dispose the pwe entry */
    if (name_malloced) {
        pwdb_entry_delete(&pwe);
        name = (const char *) pwent.pw_name;
    }

    /* PASSWORD */
    retval = pwdb_get_entry(*p,"passwd", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        pwent.pw_passwd=_pwdb_dup_string(ent->value);
        pwdb_entry_delete(&ent);
        if (!pwent.pw_passwd)
            retval = PWDB_MALLOC;
    } else if (retval == PWDB_BAD_REQUEST) {
        /* name is not available in pwdb_entry */
        pwent.pw_passwd=_pwdb_dup_string(pw->pw_passwd);
        if (pwent.pw_passwd)
            retval = PWDB_SUCCESS;
        else
            retval = PWDB_MALLOC;
    }
    if (retval != PWDB_SUCCESS) { /* some other odd error */
        _pwdb_delete_string(pwent.pw_name);
        return retval;
    }

    /* UID */
    retval = pwdb_get_entry(*p,"uid", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        pwent.pw_uid=*((uid_t *)ent->value);
        pwdb_entry_delete(&ent);
    } else if (retval == PWDB_BAD_REQUEST) {
        /* name is not available in pwdb_entry */
        pwent.pw_uid=pw->pw_uid;
    } else { /* some other odd error */
        _pwdb_delete_string(pwent.pw_name);
        _pwdb_delete_string(pwent.pw_passwd);
        return retval;
    }

    /* GID */
    retval = pwdb_get_entry(*p,"gid", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        pwent.pw_gid=*((gid_t *)ent->value);
        pwdb_entry_delete(&ent);
    } else if (retval == PWDB_BAD_REQUEST) {
        /* name is not available in pwdb_entry */
        pwent.pw_gid=pw->pw_gid;
    } else { /* some other odd error */
        _pwdb_delete_string(pwent.pw_name);
        _pwdb_delete_string(pwent.pw_passwd);
        return retval;
    }

    /* GECOS */
    retval = pwdb_get_entry(*p,"gecos", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        pwent.pw_gecos=_pwdb_dup_string(ent->value);
        pwdb_entry_delete(&ent);
        if (!pwent.pw_gecos)
            retval = PWDB_MALLOC;
    } else if (retval == PWDB_BAD_REQUEST) {
        /* name is not available in pwdb_entry */
        pwent.pw_gecos=_pwdb_dup_string(pw->pw_gecos);
        if (pwent.pw_gecos)
            retval = PWDB_SUCCESS;
        else
            retval = PWDB_MALLOC;
    } 
    if (retval != PWDB_SUCCESS) { /* some other odd error */
        _pwdb_delete_string(pwent.pw_name);
        _pwdb_delete_string(pwent.pw_passwd);
        return retval;
    }

    /* HOME DIR */
    retval = pwdb_get_entry(*p,"dir", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        pwent.pw_dir=_pwdb_dup_string(ent->value);
        pwdb_entry_delete(&ent);
        if (!pwent.pw_dir)
            retval = PWDB_MALLOC;
    } else if (retval == PWDB_BAD_REQUEST) {
        /* name is not available in pwdb_entry */
        pwent.pw_dir=_pwdb_dup_string(pw->pw_dir);
        if (pwent.pw_dir)
            retval = PWDB_SUCCESS;
        else
            retval = PWDB_MALLOC;
    }
    if (retval != PWDB_SUCCESS) { /* some other odd error */
        _pwdb_delete_string(pwent.pw_name);
        _pwdb_delete_string(pwent.pw_passwd);
        _pwdb_delete_string(pwent.pw_gecos);
        return retval;
    }

    /* SHELL */
    retval = pwdb_get_entry(*p,"shell", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        pwent.pw_shell=_pwdb_dup_string(ent->value);
        pwdb_entry_delete(&ent);
        if (!pwent.pw_shell)
            retval = PWDB_MALLOC;
    } else if (retval == PWDB_BAD_REQUEST) {
        /* name is not available in pwdb_entry */
        pwent.pw_shell=_pwdb_dup_string(pw->pw_shell);
        if (pwent.pw_shell)
            retval = PWDB_SUCCESS;
        else
            retval = PWDB_MALLOC;
    }
    if (retval != PWDB_SUCCESS)  { /* some other odd error */
        _pwdb_delete_string(pwent.pw_name);
        _pwdb_delete_string(pwent.pw_passwd);
        _pwdb_delete_string(pwent.pw_gecos);
        _pwdb_delete_string(pwent.pw_dir);
        return retval;
    }

    /* NOW we are ready. Try to update */
    /* XXX: not supported yet */
    return PWDB_UNSUPPORTED;
}

static int _pwdb_nis_delete(const char *name,
    const int id, const struct pwdb **p)
{
    const struct __pwdb_passwd * pw = NULL;
    const struct pwdb_entry * pwe;
    int retval ;
    int name_malloced;
            
    name_malloced = 0;
    if ((name == PWDB_NAME_UNKNOWN) && (id == PWDB_ID_UNKNOWN)) {
        /* Try to make use of struct pwdb, if available */
        if (!p || !*p)
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            name = (const char *) pwe->value;
            name_malloced = 1;
        } else
            return PWDB_BAD_REQUEST;
        if (!name) { /* we've got a NULL pointer */
            pwdb_entry_delete(&pwe);
            return PWDB_BAD_REQUEST;
        }
    }

    /* locate for delete */
    if (id == PWDB_ID_UNKNOWN) {
        /* locate by name */
        pw = __pwdbNIS_getpwnam(name);
        if (!pw) {
            __pwdb_pw_unlock();
            if (name_malloced)
                pwdb_entry_delete(&pwe);
        }    
    } else if (name == PWDB_NAME_UNKNOWN) {
        /* locate by uid */
        pw = __pwdbNIS_getpwuid(id);
        if (!pw)
            return PWDB_NOT_FOUND;
    } else {
        /* both name and uid supplied. */
        pw = __pwdbNIS_getpwnam(name);
        if (!pw) {
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return PWDB_NOT_FOUND;
        }
        if (pw->pw_uid != id) {
            if (name_malloced)
                pwdb_entry_delete(&pwe);
            return PWDB_NOT_FOUND;
        }
    }
    
    if (name_malloced)
        pwdb_entry_delete(&pwe);
    /* XXX: not supported yet */
    return PWDB_UNSUPPORTED;
}

static int _pwdb_nis_support(const char *entry_name)
{
    int i = 0;
    const char *supp_entry[] = {
        "user",
        "password",
        "uid",
        "gid",
        "gecos",
        "dir",
        "shell",
        NULL };

    while (supp_entry[i] != NULL) {
        if (!strcmp(supp_entry[i], entry_name))
            return PWDB_SUCCESS;
        i++;
    }
    return PWDB_UNSUPPORTED;
}

static int _pwdb_nis_flags(pwdb_flag *flags)
{
    return PWDB_UNSUPPORTED;
}
