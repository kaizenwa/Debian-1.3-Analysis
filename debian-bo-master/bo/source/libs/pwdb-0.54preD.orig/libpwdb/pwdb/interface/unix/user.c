/* This file contains the necessary interface functions to
 * the pwdb modules. They are declared as static and this file
 * should be included in the module interface file, because
 * the function addresses are used in the structures from
 * pwdb_module.c file, and I have no idea how to feed a pointer
 * to an external function in that struct...
 * CG
 */

/*************************************************
 * STANDARD UNIX USER INTERFACE FUNCTIONS
 *************************************************/

static int _pwdb_unix_locate(const char *name,
    const int id,
    const struct pwdb **p)
{
    int retval=PWDB_SUCCESS;
    struct __pwdb_passwd * pwd;
    /*
     * temporary locations for filling pwdb_entry. (safety)
     */
    int tid, tgid;
    const struct pwdb_entry * pwe;
    char *pwdb_entry_user=NULL;
   
    D(("called."));

    tid = id;
    if (p == NULL) {
        return PWDB_ABORT;           /* were are we supposed to put pwdb ? */
    }
                
    if ((name == PWDB_NAME_UNKNOWN) && (tid == PWDB_ID_UNKNOWN)) {
        /* Try to make use of struct pwdb, if available */
        if (!p || !*p)        
            return PWDB_BAD_REQUEST;
        /* First try to get the username out of it */
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_user = _pwdb_dup_string((const char *) pwe->value);
            if ((pwdb_entry_user == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);
            if (pwdb_entry_user == NULL)
                retval = PWDB_NOT_FOUND;
        }
        if (retval != PWDB_SUCCESS) {        
            /* if "user" lookup did not succeed, try "uid" */                
            retval = pwdb_get_entry(*p, "uid", &pwe);
            if (retval == PWDB_SUCCESS) {
                tid = *(const int *)pwe->value;
                pwdb_entry_delete(&pwe);
            } else
                return PWDB_BAD_REQUEST;
        }
    }

    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_user != NULL))
        name = pwdb_entry_user;
        
    if (tid == PWDB_ID_UNKNOWN) {
        pwd = __pwdb_getpwnam(name);
        if (!pwd) {
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user); /* won't fail if NULL */
            return PWDB_NOT_FOUND;
        }
    } else if (name == PWDB_NAME_UNKNOWN) {
        pwd = __pwdb_getpwuid(tid);
        if (!pwd)
            return PWDB_NOT_FOUND;
    } else {
            /* both name and id where specified. */
        pwd = __pwdb_getpwnam(name);
        if (!pwd) {
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return PWDB_NOT_FOUND;
        }
        if (pwd->pw_uid != tid) {
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return PWDB_NOT_FOUND;
        }
    }
   
    /* Now we have the user info in the pwd pointer. Fill out the
     * pwdb_entry with this info */
    
    if (!*p) {
        retval = pwdb_new(p, TIME_TO_LIVE);
    } else {
        retval = pwdb_expire(*p, TIME_TO_LIVE);
    }
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    tid = pwd->pw_uid;
    tgid = pwd->pw_gid;
   
    /* Start the show */
    retval = pwdb_set_entry(*p, "user", pwd->pw_name, 1+strlen(pwd->pw_name)
                            , NULL, txtcpy, 1+strlen(pwd->pw_name));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }
    retval = pwdb_set_entry(*p, "uid", &tid, sizeof(uid_t), NULL, dump_shorts, STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }
    retval = pwdb_set_entry(*p, "gid", &tgid, sizeof(gid_t), NULL, dump_shorts, STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    /*
     * only set the passwd if it is not a single character -- indicating
     * that the passwd is to be found in another database
     */
    if (pwd->pw_passwd && strlen(pwd->pw_passwd) != 1) {
        retval = pwdb_set_entry(*p, "passwd", pwd->pw_passwd
                                , 1+strlen(pwd->pw_passwd)
                                , NULL, txtcpy, 1+strlen(pwd->pw_passwd));
        if (retval != PWDB_SUCCESS) {
            DO_DEBUG;
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return retval;
        }
        retval = pwdb_set_entry(*p, "defer_pass"
                                , UNIX_MAGIC_PASSWD, 2, NULL, txtcpy, 2);
        if (retval != PWDB_SUCCESS) {
            DO_DEBUG;
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return retval;
        }
    }

    retval = pwdb_set_entry(*p, "gecos", pwd->pw_gecos,
                            1+strlen(pwd->pw_gecos)
                            , NULL, txtcpy, 1+strlen(pwd->pw_gecos));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    retval = pwdb_set_entry(*p, "dir", pwd->pw_dir,
                            1+strlen(pwd->pw_dir)
                            , NULL, txtcpy, 1+strlen(pwd->pw_dir));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    retval = pwdb_set_entry(*p, "shell", pwd->pw_shell,
                            1+strlen(pwd->pw_shell)
                            , NULL, txtcpy, 1+strlen(pwd->pw_shell));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }

    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
    return PWDB_SUCCESS;
}

static int _pwdb_unix_request(const char *entry_name,
                              const struct pwdb **p)
{
    int retval ;
    const struct pwdb_entry *pwe;
    const struct __pwdb_passwd *pw;
    int gid;
    char *members = NULL; /* comma separated member names string */
    uid_t *memberids = NULL; /* array of pointers to uids, last one NULL */
    int nmembers = 0; /* number of members */
    int lmembers = 0; /* string length of comma separated member names */
                                
    D(("called."));

    if (!p || !*p || !entry_name)
        return PWDB_BAD_REQUEST;
        
    if (strcmp(entry_name,"members") &&
        strcmp(entry_name,"memberids"))
        return PWDB_BAD_REQUEST;
    
    /*
     * Extract the gid for which we are doing the lookup
     */
     
    retval = pwdb_get_entry(*p, "gid", &pwe);
    if (retval != PWDB_SUCCESS || pwe == NULL || pwe->value == NULL)
        return PWDB_BAD_REQUEST;
    gid = * (gid_t *) pwe->value;
    pwdb_entry_delete(&pwe);
    
    /* Now open the database */
    if (!__pwdb_pw_open(O_RDONLY))
        return PWDB_ABORT;
    if (!__pwdb_pw_rewind())
        return PWDB_ABORT;
    while ((pw = __pwdb_pw_next()))
        if (pw->pw_gid == gid) {
            /* a new member */
            char *tmembers;
            int tmp;
            
            if (lmembers) /* we already have a member, add ',' */
                members[lmembers++]=',';
            tmp = lmembers + strlen(pw->pw_name);
            tmembers = realloc(members, 1 + tmp);
            if (!tmembers) {
                free(memberids);
                memberids = NULL;
                members = _pwdb_delete_string(members);
                return PWDB_MALLOC;
            }
            members = tmembers;
            strcpy(members+lmembers, pw->pw_name);
            lmembers = tmp;

            if (!(nmembers % 4)) {
                uid_t *tuids;

                /* add some more memory - user ids */
                tuids = realloc(memberids, sizeof(uid_t)*(nmembers+4));
                if (tuids == NULL) {
                    free(memberids);
                    memberids = NULL;
                    members = _pwdb_delete_string(members);
                    return PWDB_MALLOC;
                }
                memberids = tuids;
            }
            memberids[nmembers++] = pw->pw_uid;
        }

    /* Close the passwd file */
    if (!__pwdb_pw_close()) {
        /* Why this should ever fail ?! --cristiang */
        members = _pwdb_delete_string(members);
        free(memberids);
        memberids = NULL;
        return PWDB_ABORT;
    }

    /* Now we are finished. update the pwdb struct */            
    retval = pwdb_set_entry(*p, "members", members, 1+lmembers
                                , NULL, txtcpy, 1+lmembers);
    members = _pwdb_delete_string(members);

    if (retval == PWDB_SUCCESS) {
        /* add the gids entry to pwdb */
        retval = pwdb_set_entry(*p, "memberids", memberids, nmembers*sizeof(uid_t)
                                    , NULL, dump_shorts, nmembers*STRLEN_INT);
    }
    free(memberids);
    return retval;
}

/*
 * The user entry update function
 * We try to deal with the particular case of changing the user
 * login name by setting the account name to the value of the
 * pwdb_entry "name".
 */
static int _pwdb_unix_replace(const char *name,
                              const int id,
                              const struct pwdb **p)
{
    int i;
    const struct __pwdb_passwd * pw;
    struct __pwdb_passwd pwent;
    const struct pwdb_entry *ent;
    char *tmp_txt;
    int retval;
    char *pwdb_entry_user=NULL;
    uid_t pwdb_entry_uid= PWDB_ID_UNKNOWN;
    /* this is a very ugly hack to shut off gcc warning about discarding a 
     * const pointer. Don't you like clean code ? :-) --cristiang */
    const char **p_pw_name = (const char **) &pwent.pw_name;
                                   
    D(("called."));

    /* do the necessary checks */
    if (p == NULL) {
        return PWDB_ABORT;           /* were are we supposed to put pwdb ? */
    }
    if (!*p) /* we need some data to change ... */
        return PWDB_BAD_REQUEST;

    /* Try to make use of struct pwdb, if available */

    /* look for uid entry, we don't need to keep pwdb struct 
     * around after this lookup --cristiang */
    retval = pwdb_get_entry(*p, "uid", &ent);
    if (retval == PWDB_SUCCESS) {
        pwdb_entry_uid = *(uid_t *)ent->value;
        pwdb_entry_delete(&ent);
    } else
        DO_DEBUG;

    /* try to get the username out of it */
    retval = pwdb_get_entry(*p, "user", &ent);
    if (retval == PWDB_SUCCESS) {
        pwdb_entry_user = _pwdb_dup_string((const char *)ent->value);
        if ((pwdb_entry_user == NULL) && (ent->value != NULL)) {
            pwdb_entry_delete(&ent);
            return PWDB_MALLOC;
        }
        pwdb_entry_delete(&ent);
        if (!strlen(pwdb_entry_user))
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
    } else
        DO_DEBUG;

    /* Figure out the user name */
    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_user == NULL)) {
        const struct __pwdb_passwd *temp_pw;
        uid_t use_uid;
        
        /* No name, try with uid */
        
        /* id should be the old UID, in case we change it */
        if (id != PWDB_ID_UNKNOWN)
            use_uid = id;
        else if (pwdb_entry_uid != (uid_t) PWDB_ID_UNKNOWN)
            use_uid = pwdb_entry_uid;
        else /* no name, no uid ... sad. */
            return PWDB_BAD_REQUEST;
            
        temp_pw = __pwdb_getpwuid(use_uid);
        if (!temp_pw)
            /* lookup failed */
            return PWDB_NOT_FOUND;
        pwdb_entry_user = _pwdb_dup_string(temp_pw->pw_name);
        name = pwdb_entry_user;
    }
            
    /* Open the database */
    if (!__pwdb_pw_open(O_RDONLY)) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }

    /* 
     * Database tests:
     *  - old_name exists in the database
     *  - if change_name, new_name does not exist
     */
    /* database open */
    pw = NULL;
    pw = __pwdb_pw_locate(name); /* this is the current login name */
    if (!pw) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_NOT_FOUND;
    }
    /* sanity check */
    if (
        ((id != PWDB_ID_UNKNOWN) && (pw->pw_uid != id)) ||
        ((id == PWDB_ID_UNKNOWN) && (pwdb_entry_uid != (uid_t) PWDB_ID_UNKNOWN) && 
         (pw->pw_uid != pwdb_entry_uid))) {
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return PWDB_NOT_FOUND;
    }

    /* Now we have in pw the old entry. We need to modify it... */
   
    /*
     * NAME - if we already got the name from pwdb struct, skip over...
     */
    if (pwdb_entry_user == PWDB_NAME_UNKNOWN)
        *p_pw_name =  name;
    else
        pwent.pw_name = pwdb_entry_user;
        
    /* 
     * PASSWORD 
     */
    tmp_txt = NULL;
    retval = pwdb_get_entry(*p, "defer_pass", &ent);
    if ((retval != PWDB_SUCCESS) || (ent->value == NULL) || 
        !strcmp(UNIX_MAGIC_PASSWD, ent->value)) {
            /* we store the password in the /etc/passwd file */
            (void) pwdb_entry_delete(&ent);                 /* tidy up */
            retval = pwdb_get_entry(*p, "passwd", &ent);
            if (retval == PWDB_SUCCESS && ent && ent->value) {
                tmp_txt = _pwdb_dup_string(ent->value);     /* we've got it */
            } else if (retval == PWDB_BAD_REQUEST) {
                /* name is not available in pwdb_entry -- keep old one */
                tmp_txt = _pwdb_dup_string(pw->pw_passwd);
            }
    } else {
        /*
         * the passwd is stored elsewhere. So place pointer to it in
         * the /etc/passwd file entry for this user
         */
            tmp_txt = _pwdb_dup_string(ent->value);      /* defer_pass entry */
    }

    pwent.pw_passwd = tmp_txt;
    if (tmp_txt == NULL) {                          /* error with memory */
        tmp_txt = pwent.pw_name = _pwdb_delete_string(pwent.pw_name);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_MALLOC;
    }

    /* UID */
    if (pwdb_entry_uid != (uid_t) PWDB_ID_UNKNOWN)
        pwent.pw_uid = pwdb_entry_uid;
    else
        pwent.pw_uid = pw->pw_uid;

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
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
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
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
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
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
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
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
        pwent.pw_dir = _pwdb_delete_string(pwent.pw_dir);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return retval;
    }
    if (!__pwdb_pw_close()) {
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
        pwent.pw_dir = _pwdb_delete_string(pwent.pw_dir);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }
    /* database closed */

    /* NOW we are ready. Try to update */

    /* try for PWDB_LOCK_TIME seconds to lock the passwd file */
    for (i = 0;i < PWDB_LOCK_TIME;i++)
        if (__pwdb_pw_lock())
            break;
        else
            sleep (1);
    /* if we ran out of time ... */
    if (i == PWDB_LOCK_TIME) {
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
        pwent.pw_dir = _pwdb_delete_string(pwent.pw_dir);
        pwent.pw_shell = _pwdb_delete_string(pwent.pw_shell);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_BLOCKED;
    }
    /* open the passwd file ReadWrite */
    if (!__pwdb_pw_open(O_RDWR)) {
        __pwdb_pw_unlock();
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
        pwent.pw_dir = _pwdb_delete_string(pwent.pw_dir);
        pwent.pw_shell = _pwdb_delete_string(pwent.pw_shell);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }

    /* Updating an user entry takes two steps. We first call pw_update 
     * for the new entry found in pwent. Then if name != pwent.pw_name
     * we have to delete the old entry.
     */
    if (!__pwdb_pw_update(&pwent)) {
        __pwdb_pw_unlock();
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
        pwent.pw_dir = _pwdb_delete_string(pwent.pw_dir);
        pwent.pw_shell = _pwdb_delete_string(pwent.pw_shell);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }

    /* Do we have to dispose the entry for the old user name ? */
    if ((pwdb_entry_user != NULL) && strcmp(pwdb_entry_user, name)) /* different */
        if (! __pwdb_pw_remove(name)) { /* dispose old entry */
            __pwdb_pw_unlock();
            pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
            pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
            pwent.pw_dir = _pwdb_delete_string(pwent.pw_dir);
            pwent.pw_shell = _pwdb_delete_string(pwent.pw_shell);
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return PWDB_ABORT;
        }
            
    /* Close the file and write back the new version */
    if (!__pwdb_pw_close()) {
        /* why would this ever fail ? */
        __pwdb_pw_unlock();
        pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
        pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
        pwent.pw_dir = _pwdb_delete_string(pwent.pw_dir);
        pwent.pw_shell = _pwdb_delete_string(pwent.pw_shell);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }
    /* The End. */
    __pwdb_pw_unlock();
    pwent.pw_passwd = _pwdb_delete_string(pwent.pw_passwd);
    pwent.pw_gecos = _pwdb_delete_string(pwent.pw_gecos);
    pwent.pw_dir = _pwdb_delete_string(pwent.pw_dir);
    pwent.pw_shell = _pwdb_delete_string(pwent.pw_shell);
    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
               
    return PWDB_SUCCESS;
}

static int _pwdb_unix_delete(const char *name,
    const int id, const struct pwdb **p)
{
    const struct __pwdb_passwd * pw = NULL;
    int i;
    const struct pwdb_entry * pwe;
    int retval ;
    int tid;
    char *pwdb_entry_user = NULL;
        
    D(("called."));

    tid = id;
    if ((name == PWDB_NAME_UNKNOWN) && (tid == PWDB_ID_UNKNOWN)) {
        /* Try to make use of struct pwdb, if available */
        if (!p || !*p)        
            return PWDB_BAD_REQUEST;
        /* First try to get the username out of it */
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_user = _pwdb_dup_string((char *)pwe->value);
            if ((pwdb_entry_user == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);            
            if (!pwdb_entry_user)   /* we've got a NULL pointer */
                retval = PWDB_NOT_FOUND;
        }
        if (retval != PWDB_SUCCESS) {        
            /* if "user" lookup did not succeed, try "uid" */                
            retval = pwdb_get_entry(*p, "uid", &pwe);
            if (retval == PWDB_SUCCESS) {
                tid = *(const int *)pwe->value;
                pwdb_entry_delete(&pwe);
            } else
                return PWDB_BAD_REQUEST;
        }
    }

    if ((name==PWDB_NAME_UNKNOWN) && (pwdb_entry_user != NULL))
        name = pwdb_entry_user;
        
    /* try for PWDB_LOCK_TIME seconds to lock the passwd file */
    for (i = 0;i < 10;i++)
        if (__pwdb_pw_lock())
            break;
        else
            sleep (1);
    /* if we ran out of time ... */
    if (i == PWDB_LOCK_TIME) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_BLOCKED;
    }

    /* open the passwd file ReadWrite */
    if (!__pwdb_pw_open(O_RDWR)) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        __pwdb_pw_unlock();
        return PWDB_ABORT;
    }
    /* locate for delete */
    if (tid == PWDB_ID_UNKNOWN) {
        /* locate by name */
        pw = __pwdb_pw_locate(name);
        if (!pw) {
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            __pwdb_pw_unlock();
            return PWDB_NOT_FOUND;
        }
    } else if (name == PWDB_NAME_UNKNOWN) {
        /* locate by uid */
        pw = __pwdb_pw_locate_id(tid);
        if (!pw) {
            __pwdb_pw_unlock();
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return PWDB_NOT_FOUND;
        }
    } else {
        /* both name and uid supplied. */
        pw = __pwdb_pw_locate(name);
        if (!pw) {
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            __pwdb_pw_unlock();
            return PWDB_NOT_FOUND;
        }
        if (pw->pw_uid != tid) {
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            __pwdb_pw_unlock();
            return PWDB_NOT_FOUND;
        }
    }
    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
    
    if (!__pwdb_pw_remove(pw->pw_name)) {
        __pwdb_pw_unlock();
        return PWDB_ABORT;
    }
        
    if (!__pwdb_pw_close()) {
        __pwdb_pw_unlock();
        return PWDB_ABORT;
    }
    
    /* Done */
    __pwdb_pw_unlock();
    return PWDB_SUCCESS;
}

static int _pwdb_unix_support(const char *entry_name)
{
    int i = 0;

    static const char *supp_entry[] = {
        "user", "passwd",
        "uid", "gid",
        "gecos", "dir", "shell",
        "members", "memberids", /* by request only */
        NULL };

    D(("called."));

    while (supp_entry[i] != NULL) {
        if (!strcmp(supp_entry[i], entry_name))
            return PWDB_SUCCESS;
        i++;
    }

    return PWDB_NOT_FOUND;
}

static int _pwdb_unix_flags(pwdb_flag *flags)
{
    pwdb_flag all=0;

    D(("called."));

    if ( geteuid() != 0 ) {
        all |= PWDB_F_NOUPDATE;
    }

    *flags |= all;                         /* merge flags with current ones */

    return PWDB_SUCCESS;
}

