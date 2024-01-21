/* This file contains the necessary interface functions to
 * the pwdb modules. They are declared as static and this file
 * should be included in the module interface file, because
 * the function addresses are used in the structures from
 * pwdb_module.c file, and I have no idea how to feed a pointer
 * to an external function in that struct...
 * CG
 */

#define SHADOW_MAGIC_PASSWD    "x"           /* passwd is in /etc/passwd */

/*************************************************
 * SHADOW UNIX USER INTERFACE
 *************************************************/

static int _pwdb_shadow_locate(const char *name,
                   const int id,
                   const struct pwdb **p)
{
    int retval;
    const struct pwdb_entry *pwe=NULL;
    struct __pwdb_spwd * spwd;
    struct __pwdb_spwd spent;
    char * pwdb_entry_user = NULL;
        
    if (p == NULL) {
        return PWDB_BAD_REQUEST;  /* were are we supposed to put pwdb ? */
    }

    /*
     * Lookup by uid in the shadow file is not "directly" supported,
     * since the shadow file does not contian a field for a user's uid.
     * However, if the "user" entry is set in the pwdb strucuture then
     * it is legitimate for this module to use it to search the shadow
     * file
     */

    if (name == PWDB_NAME_UNKNOWN) {
        if (*p == NULL)
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_user = _pwdb_dup_string((char *) pwe->value);
            if ((pwdb_entry_user == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);
            if (pwdb_entry_user == NULL)
                return PWDB_BAD_REQUEST;
            name = pwdb_entry_user;
        } else {
            DO_DEBUG;
            return retval;
        }
    }

    /*
     * Is there an entry for this user?
     */

    spwd = __pwdb_getspnam(name);
    if (!spwd) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_NOT_FOUND;
    }
    spent = *spwd;
   
    /*
     * Start the show
     */

    if (!*p) {
        retval = pwdb_new(p, 0);
        if (retval != PWDB_SUCCESS) {
            DO_DEBUG;
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return retval;
        }
    }

    retval = pwdb_set_entry(*p,"user", name,
                            1+strlen(name), NULL, txtcpy, 1+strlen(name));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    /* if a password and not some magic pointer set the passwd entry */
    if (spent.sp_pwdp[1]) {
	retval = pwdb_set_entry(*p, "passwd", spent.sp_pwdp,
                            1+strlen(spwd->sp_pwdp), NULL, txtcpy,
                            1+strlen(spwd->sp_pwdp));
	if (retval == PWDB_SUCCESS) {
	    retval = pwdb_set_entry(*p, "defer_pass", SHADOW_MAGIC_PASSWD,
				    sizeof(SHADOW_MAGIC_PASSWD), NULL, txtcpy,
				    sizeof(SHADOW_MAGIC_PASSWD));
	}
	if (retval != PWDB_SUCCESS) {
	    DO_DEBUG;
	    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
	    return retval;
	}
    }

    retval = pwdb_set_entry(*p, "last_change", &spent.sp_lstchg,
                            sizeof(__pwdb_sptime), NULL, str_long,
                            STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    retval = pwdb_set_entry(*p, "min_change", &spent.sp_min,
                            sizeof(__pwdb_sptime), NULL, str_long, 
                            STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    retval = pwdb_set_entry(*p, "max_change", &spent.sp_max,
                            sizeof(__pwdb_sptime), NULL, str_long, 
                            STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    retval = pwdb_set_entry(*p, "warn_change", &spent.sp_warn,
                            sizeof(__pwdb_sptime), NULL, str_long, 
                            STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    retval = pwdb_set_entry(*p, "defer_change", &spent.sp_inact,
                            sizeof(__pwdb_sptime), NULL, str_long, 
                            STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }
    retval = pwdb_set_entry(*p, "expire", &spent.sp_expire,
                            sizeof(__pwdb_sptime), NULL, str_long, 
                            STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);

    return PWDB_SUCCESS;
}


static int _pwdb_shadow_request(const char *entry_name,
                                const struct pwdb **p)
{
    return PWDB_UNSUPPORTED;
}


/*
 * The user entry update function
 * We try to deal with the particular case of changing the user
 * login name by setting the account name to the value of the
 * pwdb_entry "name".
 * Also, the last_change field is automatically calculated if not
 * supplied as a pwdb_entry
 */
static int _pwdb_shadow_replace(const char *name,
                                const int id,
                                const struct pwdb **p)
{
    int i;
    const struct __pwdb_spwd *spw;
    struct __pwdb_spwd spwent;
    const struct pwdb_entry *pwe=NULL;
    const struct pwdb_entry *ent;
    int retval;
    char *pwdb_entry_user = NULL;
    /* this is a very ugly hack to shut off gcc warning about discarding a 
     * const pointer. Don't you like clean code ? :-) --cristiang */
    const char **p_sp_namp = (const char **) &spwent.sp_namp;

    /* WARNING: id field if different from PWDB_ID_UNKNOWN is ignored */

    /* do the necessary checks */
    if (!p || !*p) /* we need some data to change ... */
        return PWDB_BAD_REQUEST;

    /*
     * Lookup by uid in the shadow file is not "directly" supported,
     * since the shadow file does not contian a field for a user's uid.
     * However, if the "user" entry is set in the pwdb strucuture then
     * it is legitimate for this module to use it to search the shadow
     * file
     */
     retval = pwdb_get_entry(*p, "user", &pwe);
     if (retval == PWDB_SUCCESS) {
        pwdb_entry_user = _pwdb_dup_string((char *)pwe->value);
        if ((pwdb_entry_user == NULL) && (pwe->value != NULL)) {
            pwdb_entry_delete(&pwe);
            return PWDB_MALLOC;
        }
        pwdb_entry_delete(&pwe);
        if (!strlen(pwdb_entry_user))
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
    }

    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_user == NULL))
        return PWDB_BAD_REQUEST;
    else if (name == PWDB_NAME_UNKNOWN) /* pwdb_entry_user != NULL */
        name = pwdb_entry_user;

    /* see if the user exists in the database 
     * we open first the database read only
     */

    if (!__pwdb_spw_open(O_RDONLY)) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }

    /* database open */
    
    /* try a lookup. the name argument is the primary key. If
     * the login name is about to be changed, it will be changed to 
     * the value supplied in the user entry of pwdb struct */
    spw = __pwdb_spw_locate(name);
    if (!spw) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_NOT_FOUND;
    }
    
    /* now try to extract some data from pwdb_entry structure */

    /* NAME */
    /* we set the entry to the name argument, and decide later if
     * we change the login name ... --cristiang */
    if (pwdb_entry_user == NULL)
        *p_sp_namp = name; /* don't change the login name */
    else
        spwent.sp_namp = pwdb_entry_user;
    
    /* PASSWORD */
    retval = pwdb_get_entry(*p,"defer_pass",&ent);
    if (retval == PWDB_SUCCESS) {
	if (strcmp(SHADOW_MAGIC_PASSWD, ent->value)) {
	    spwent.sp_pwdp=_pwdb_dup_string(ent->value);
	} else {
	    pwdb_entry_delete(&ent);
	    retval = pwdb_get_entry(*p, "passwd", &ent);
	    if (retval == PWDB_SUCCESS) {
		/* we've got it */
		spwent.sp_pwdp=_pwdb_dup_string(ent->value);
		if ((spwent.sp_pwdp == NULL) && (ent->value != NULL)) {
		    pwdb_entry_delete(&ent);
		    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
		    __pwdb_spw_close();
		    return PWDB_MALLOC;
		}
	    } else {
		/* not available in pwdb_entry */
		spwent.sp_pwdp=_pwdb_dup_string(spw->sp_pwdp);
		if (spwent.sp_pwdp == NULL) {
		    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
		    __pwdb_spw_close();
		    return PWDB_MALLOC;
		}
	    }
	}
	pwdb_entry_delete(&ent);
    } else {
	D(("no defer_pass entry"));
	pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
	__pwdb_spw_close();
	return PWDB_ABORT;
    }

    /* LAST CHANGE */
    retval = pwdb_get_entry(*p,"last_change", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        spwent.sp_lstchg=*((__pwdb_sptime *)ent->value);
        pwdb_entry_delete(&ent);
    } else {
        /* not available in pwdb_entry */
        spwent.sp_lstchg=(__pwdb_sptime) time(NULL)/(24*60*60);
    }

    /* MIN CHANGE */
    retval = pwdb_get_entry(*p,"min_change", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        spwent.sp_min=*((__pwdb_sptime *)ent->value);
        pwdb_entry_delete(&ent);
    } else
        /* not available in pwdb_entry */
        spwent.sp_min=spw->sp_min;

    /* MAX CHANGE */
    retval = pwdb_get_entry(*p,"max_change", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        spwent.sp_max=*((__pwdb_sptime *)ent->value);
        pwdb_entry_delete(&ent);
    } else
        /* not available in pwdb_entry */
        spwent.sp_max=spw->sp_max;

    /* WARN CHANGE */
    retval = pwdb_get_entry(*p,"warn_change", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        spwent.sp_warn=*((__pwdb_sptime *)ent->value);
        pwdb_entry_delete(&ent);
    } else
        /* not available in pwdb_entry */
        spwent.sp_warn=spw->sp_warn;

    /* DEFER CHANGE */
    retval = pwdb_get_entry(*p,"defer_change", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        spwent.sp_inact=*((__pwdb_sptime *)ent->value);
        pwdb_entry_delete(&ent);
    } else
        /* not available in pwdb_entry */
        spwent.sp_inact=spw->sp_inact;

    /* EXPIRE */
    retval = pwdb_get_entry(*p,"expire", &ent);
    if (retval == PWDB_SUCCESS) {
        /* we've got it */
        spwent.sp_expire=*((__pwdb_sptime *)ent->value);
        pwdb_entry_delete(&ent);
    } else 
        /* not available in pwdb_entry */
        spwent.sp_expire=spw->sp_expire;

    if (!__pwdb_spw_close()) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        spwent.sp_pwdp = _pwdb_delete_string(spwent.sp_pwdp);
        return PWDB_ABORT;
    }
    /* database closed */

    /* NOW we are ready. Try to update */

    /* try for PWDB_LOCK_TIME seconds to lock the shadow file */
    for (i = 0;i < PWDB_LOCK_TIME;i++)
        if (__pwdb_spw_lock())
            break;
        else
            sleep (1);
    /* if we ran out of time ... */
    if (i == PWDB_LOCK_TIME) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        spwent.sp_pwdp = _pwdb_delete_string(spwent.sp_pwdp);
        return PWDB_BLOCKED;
    }

    /* open the shadow file ReadWrite */
    if (!__pwdb_spw_open(O_RDWR)) {
        __pwdb_spw_unlock();
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        spwent.sp_pwdp = _pwdb_delete_string(spwent.sp_pwdp);
        return PWDB_ABORT;
    }
    /* locate again, this time for update */

    /* NOTE: the 'name' arg is the primary argument. If the pwdb
     * struct contains an entry for the 'name' and it is different from
     * the 'name' arg passed to this function, the user login name will
     * be changed.
     */
    spw = __pwdb_spw_locate(name);
    if (!spw) {
        /* really ODD. it shouldn't happen ! */
        __pwdb_spw_close();
        __pwdb_spw_unlock();
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        spwent.sp_pwdp = _pwdb_delete_string(spwent.sp_pwdp);
        return PWDB_ABORT;
    }
    if (!__pwdb_spw_update(&spwent)) {
        __pwdb_spw_close();
        __pwdb_spw_unlock();
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        spwent.sp_pwdp = _pwdb_delete_string(spwent.sp_pwdp);
        return PWDB_ABORT;
    }

    /* Now check if we have changed the login name */
    if ((pwdb_entry_user != NULL) && strcmp(pwdb_entry_user,name))
        /* the login name was changed */
        if (!__pwdb_spw_remove(name)) { /* remove the old entry */
            __pwdb_spw_close();
            __pwdb_spw_unlock();
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            spwent.sp_pwdp = _pwdb_delete_string(spwent.sp_pwdp);
            return PWDB_ABORT;
        }

    if (!__pwdb_spw_close()) {
        /* why would this ever fail ? */
        __pwdb_spw_unlock();
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        spwent.sp_pwdp = _pwdb_delete_string(spwent.sp_pwdp);
        return PWDB_ABORT;
    }
    /* The End. */
    __pwdb_spw_unlock();
    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
    spwent.sp_pwdp = _pwdb_delete_string(spwent.sp_pwdp);
                
    return PWDB_SUCCESS;
}

static int _pwdb_shadow_delete(const char *name,
                               const int id,
                               const struct pwdb **p)
{
    const struct pwdb_entry *pwe;
    const struct __pwdb_spwd * spw = NULL;
    int i;
    int retval;
    char *pwdb_entry_user = NULL;
            
    /* WARNING: this function ignores the id parameter */
    
    /*
     * Lookup by uid in the shadow file is not "directly" supported,
     * since the shadow file does not contian a field for a user's uid.
     * However, if the "user" entry is set in the pwdb strucuture then
     * it is legitimate for this module to use it to search the shadow
     * file
     */

    if (name == PWDB_NAME_UNKNOWN) {
        if ((p == NULL) || (*p == NULL))
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_user = _pwdb_dup_string((char *)pwe->value);
            if ((pwdb_entry_user == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);
        } else {
            DO_DEBUG;
            return retval;
        }
    }
    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_user == NULL))
        return PWDB_BAD_REQUEST;
    else if (name == PWDB_NAME_UNKNOWN)
        name = pwdb_entry_user;

    /* try for PWDB_LOCK_TIME seconds to lock the shadow file */
    for (i = 0;i < 10;i++)
        if (__pwdb_spw_lock())
            break;
        else
            sleep (1);
    /* if we ran out of time ... */
    if (i == PWDB_LOCK_TIME) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_BLOCKED;
    }

    /* open the shadow file ReadWrite */
    if (!__pwdb_spw_open(O_RDWR)) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        __pwdb_spw_unlock();
        return PWDB_ABORT;
    }
    /* locate for delete */
    spw = __pwdb_spw_locate(name);
    if (!spw) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        __pwdb_spw_close();
        __pwdb_spw_unlock();
        return PWDB_NOT_FOUND;
    }
    
    /* after all this, spw now is 'located' somewhere */
    if (!__pwdb_spw_remove(spw->sp_namp)) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        __pwdb_spw_close();
        __pwdb_spw_unlock();
        return PWDB_ABORT;
    }
        
    if (!__pwdb_spw_close()) {
        __pwdb_spw_unlock();
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }
    
    /* Done */
    __pwdb_spw_unlock();
    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
    return PWDB_SUCCESS;
}

static int _pwdb_shadow_support(const char *entry_name)
{
    int i = 0;
    const char *supp_entry[] = {
        "user", "passwd", "last_change",
        "min_change", "max_change", "warn_change",
        "defer_change", "expire", "defer_pass",
        NULL };

    while (supp_entry[i] != NULL) {
        if (!strcmp(supp_entry[i], entry_name))
            return PWDB_SUCCESS;
        i++;
    }

    return PWDB_NOT_FOUND;
}

static int _pwdb_shadow_flags(pwdb_flag *flags)
{
    pwdb_flag all=0;

    if ( geteuid() != 0 ) {
        all |= PWDB_F_NOUPDATE;
    }

    *flags |= all;                         /* merge flags with current ones */

    return PWDB_SUCCESS;
}
