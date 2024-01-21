/* This file contains the necessary interface functions to
 * the pwdb modules. They are declared as static and this file
 * should be included in the module interface file, because
 * the function addresses are used in the structures from
 * pwdb_module.c file, and I have no idea how to feed a pointer
 * to an external function in that struct...
 * CG
 */

/**************************************************
 * STANDARD UNIX GROUP INTERFACE FUNCTIONS
 **************************************************/
static int _pwdb_unix_glocate(const char *name,
                              const int id,
                              const struct pwdb **p)
{
    struct __pwdb_group *grp;
    const struct pwdb_entry *pwe;
    int retval;
    int tgid;
    char *pwdb_entry_group=NULL;
    
    D(("called."));
    
    if (p == NULL) {
        return PWDB_ABORT;           /* were are we supposed to put pwdb ? */
    }

    tgid = id;
    if ((name == PWDB_NAME_UNKNOWN) && (id == PWDB_ID_UNKNOWN)) {
        /* Try to make use of struct pwdb, if available */
        if ( *p == NULL )
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "group", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_group = _pwdb_dup_string((const char *) pwe->value);
            if ((pwdb_entry_group == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);
            if (pwdb_entry_group == NULL)
                retval = PWDB_NOT_FOUND;
        }
        if (retval != PWDB_SUCCESS) {
            /* if group lookup did not succeed, try gid */
            retval = pwdb_get_entry(*p, "gid", &pwe);
            if (retval == PWDB_SUCCESS) {
                tgid = *(const int *)pwe->value;
                pwdb_entry_delete(&pwe);
            } else
                return PWDB_BAD_REQUEST;
        }
    }

    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_group != NULL))
        name = pwdb_entry_group;

    if (tgid == PWDB_ID_UNKNOWN) {
        grp = __pwdb_getgrnam(name);
        if (!grp) {
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            return PWDB_NOT_FOUND;
        }
    } else if (name == PWDB_NAME_UNKNOWN) {
        grp = __pwdb_getgrgid(tgid);
        if (!grp)
            return PWDB_NOT_FOUND;
    } else {
            /*
         * both name and id where specified.
         *
         * XXX: perhaps we should continue search through the whole file
         * in case there are two entries with the same number/name?
         */
        grp = __pwdb_getgrnam(name);
        if (!grp) {
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            return PWDB_NOT_FOUND;
        }
        if (grp->gr_gid != tgid) {
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            return PWDB_NOT_FOUND;
        }
    }

    /* Now we have the user info in the grp pointer. Fill out the
     * pwdb_entry with this info */
    
    if (!*p) {
	D(("obtaining new pwdb structure"));
        retval = pwdb_new(p, TIME_TO_LIVE);
    } else {
	D(("adjusting expiry date of pwdb structure"));
        retval = pwdb_expire(*p, TIME_TO_LIVE);
    }
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return retval;
    }

    tgid = grp->gr_gid;
   
    /* Start the show */
    retval = pwdb_set_entry(*p,"group", grp->gr_name
                            , 1+strlen(grp->gr_name)
                            , NULL, txtcpy, 1+strlen(grp->gr_name));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return retval;
    }
    retval = pwdb_set_entry(*p, "gid", &tgid, sizeof(gid_t)
			    , NULL, dump_shorts, STRLEN_INT);
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return retval;
    }
    retval = pwdb_set_entry(*p, "passwd", grp->gr_passwd,
                            1+strlen(grp->gr_passwd)
                            , NULL, txtcpy, 1+strlen(grp->gr_passwd));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return retval;
    }

    {
        char *mem;
        int glen,tlen;

        for (glen=tlen=0; grp->gr_mem[glen]; ++glen)
            tlen += 1+strlen(grp->gr_mem[glen]);

        /* reserve enough memory for user-list */

        mem = malloc(tlen);
        if (mem == NULL) {
            return PWDB_MALLOC;
        }

        /*
	 * copy usernames into 'mem' separated by commas
	 *  'tlen' is the length of the text, 'glen' is the number
	 *  of groups.
	 */

        for (glen=tlen=0; grp->gr_mem[glen]; ++glen) {
            strcpy(mem+tlen, grp->gr_mem[glen]);
            tlen += strlen(grp->gr_mem[glen]);
            mem[tlen++] = ',';
        }
	if (tlen > 0)
	    --tlen;
        mem[tlen] = '\0';                       /* <NUL> terminate */

        retval = pwdb_set_entry(*p, "users", mem, tlen
				, NULL, txtcpy, tlen + 1);
        if (retval != PWDB_SUCCESS) {
            DO_DEBUG;
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            return retval;
        }
        mem=_pwdb_delete_string(mem);
    }

    pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
    
    D(("successful return"));
    return PWDB_SUCCESS;
}

static int _pwdb_unix_grequest(const char *entry_name,
                               const struct pwdb **p)
{
    int retval = PWDB_BAD_REQUEST;

    D(("called."));

    if (p == NULL || *p == NULL) {
        ;
    } else if (strcmp("groups", entry_name) == 0
        || strcmp("groupids", entry_name) == 0) {

        /* we need to build a list of groups that this user is a member of */
        const struct pwdb_entry *pwe;
        const struct __pwdb_group *grp;
        char *groups=NULL;
        const char *user;
        gid_t *gids=NULL;
        int ngroups=0, lgroups=0;

        /* which user are we interested in? */

        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval != PWDB_SUCCESS || pwe == NULL || pwe->value == NULL) {
            return PWDB_BAD_REQUEST;
        }
        user = (const char *) pwe->value;

        /* open the group file */
        __pwdb_setgrent();

        while ((grp = __pwdb_getgrent()) != NULL) {       /* read an entry */
            int i;
            /*
             * if the user is in this group add the group name/number
             * to list
             */
            for (i=0; grp->gr_mem[i]; ++i) {
                if ( strcmp(grp->gr_mem[i], user) == 0 ) {
                    char *tgroups;
                    int nlgroups;

                    if (lgroups) {      /* already have one so need comma */
                        groups[lgroups++] = ',';
                    }
                    nlgroups = lgroups + strlen(grp->gr_name);
                    tgroups = realloc(groups, 1 + nlgroups);
                    if (tgroups == NULL) {
                        free(gids);
                        gids = NULL;
                        groups = _pwdb_delete_string(groups);
                        (void) pwdb_entry_delete(&pwe);
                        return PWDB_MALLOC;
                    }
                    groups = tgroups;

                    if (!(ngroups % 4)) {
                        gid_t *tgids;

                        /* add some more memory - group ids */

                        tgids = realloc(gids, sizeof(gid_t)*(ngroups+4));
                        if (tgids == NULL) {
                            free(gids);
                            gids = NULL;
                            groups = _pwdb_delete_string(groups);
                            (void) pwdb_entry_delete(&pwe);
                            return PWDB_MALLOC;
                        }
                        gids = tgids;
                    }

                    /* copy the group and gid */
                    gids[ngroups++] = grp->gr_gid;
                    strcpy(groups+lgroups, grp->gr_name);
                    lgroups = nlgroups;
                    break;                   /* finished with this group */
                }
                /* check next user */
            }
            /* try the next group */
        }

        (void) pwdb_entry_delete(&pwe);              /* no longer needed */

        /* add the groups entry */
        retval = pwdb_set_entry(*p, "groups", groups, 1+lgroups
                                , NULL, txtcpy, 1+lgroups);

        groups = _pwdb_delete_string(groups);

        if (retval == PWDB_SUCCESS) {
            /* add the gids entry to pwdb */
            retval = pwdb_set_entry(*p, "groupids", gids, ngroups*sizeof(gid_t)
                                    , NULL, dump_shorts, ngroups*STRLEN_INT);
        }

        free(gids);
    } else {
        retval = PWDB_NOT_FOUND;
    }


    return retval;
}


static int _pwdb_unix_greplace(const char *name,
                               const int id,
                               const struct pwdb **p)
{
    const struct pwdb_entry *pwe;
    const struct __pwdb_group *gr;
    struct __pwdb_group grent;
    int retval;
    int i;
    int pwdb_entry_gid = PWDB_ID_UNKNOWN;
    char *pwdb_entry_group = NULL;
    char *pwdb_entry_passwd = NULL;
    char *pwdb_entry_users = NULL;
    char **mem = NULL; /* temporary holdspace for member list */
    int nr_users = 0; /* number of members of this group */
    /* this is a very ugly hack to shut off gcc warning about discarding a 
     * const pointer. Don't you like clean code ? :-) --cristiang */
    const char **p_gr_name = (const char **) &grent.gr_name;

    D(("called."));

    /* do the necessary checks */
    if (p == NULL) {
        return PWDB_ABORT;           /* were are we supposed to put pwdb ? */
    }
    if (!*p) /* we need some data to change ... */
        return PWDB_BAD_REQUEST;

    /* initialize */

    /* Try to make use of struct pwdb, if available */

    /* look for uid entry, we don't need to keep pwdb struct 
     * around after this lookup --cristiang */
    retval = pwdb_get_entry(*p, "gid", &pwe);
    if (retval == PWDB_SUCCESS) {
        pwdb_entry_gid = *(const int *)pwe->value;
        pwdb_entry_delete(&pwe);
    }

    /* try to get the groupname out of it */
    retval = pwdb_get_entry(*p, "group", &pwe);
    if (retval == PWDB_SUCCESS) {
        pwdb_entry_group = _pwdb_dup_string((const char *)pwe->value);
        if ((pwdb_entry_group == NULL) && (pwe->value != NULL)) {
            pwdb_entry_delete(&pwe);
            return PWDB_MALLOC;
        }
        pwdb_entry_delete(&pwe);
    }

    /* Figure out the group name */
    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_group == NULL)) {
        const struct __pwdb_group *temp_gr;
        gid_t use_gid;

        /* No name, try with gid */

        /* id should be the old GID, in case we change it */
        if (id != PWDB_ID_UNKNOWN)
            use_gid = id;
        else if (pwdb_entry_gid != PWDB_ID_UNKNOWN)
            use_gid = pwdb_entry_gid;
        else /* no name, no gid ... sad. */
            return PWDB_BAD_REQUEST;

        temp_gr = __pwdb_getgrgid(use_gid);
        if (!temp_gr)
            /* lookup failed */
            return PWDB_NOT_FOUND;
        pwdb_entry_group = _pwdb_dup_string(temp_gr->gr_name);
        name = pwdb_entry_group;
    }

    /* Open the database */
    /* try for PWDB_LOCK_TIME seconds to lock the passwd file */
    for (i = 0;i < 10;i++)
        if (__pwdb_gr_lock())
            break;
        else
            sleep (1);
    /* if we ran out of time ... */
    if (i == PWDB_LOCK_TIME) {
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_BLOCKED;
    }
                  
    /* now open the group file */
    if (!__pwdb_gr_open(O_RDWR)) {
        __pwdb_gr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_NOT_FOUND;
    }

    /* 
     * Database tests:
     *  - old_name exists in the database
     *  - if change_name, new_name does not exist
     */
    /* database open */
    gr = NULL;
    gr = __pwdb_gr_locate(name); /* this is the current group name */
    if (!gr) {
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_NOT_FOUND;
    }
    /* sanity check */
    if (
        ((id != PWDB_ID_UNKNOWN) && (gr->gr_gid != id)) ||
        ((id == PWDB_ID_UNKNOWN) && (pwdb_entry_gid != PWDB_ID_UNKNOWN) &&
         (gr->gr_gid != pwdb_entry_gid))) {
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            return PWDB_NOT_FOUND;
    }

    /* Now we have in gr the old entry. We need to modify it... */
   
    /*
     * NAME - if we already got the name from pwdb struct, skip over...
     */
    if (pwdb_entry_group == PWDB_NAME_UNKNOWN)
       *p_gr_name = name;
    else
        grent.gr_name = pwdb_entry_group;
        
    /*
     * UID
     */
    if (pwdb_entry_gid == PWDB_ID_UNKNOWN)   
        grent.gr_gid = gr->gr_gid;
    else
        grent.gr_gid = pwdb_entry_gid;

    /* See if the group passwd is about to change */
    retval = pwdb_get_entry(*p, "passwd", &pwe);
    if ((retval != PWDB_SUCCESS) || !pwe || !pwe->value)
        grent.gr_passwd = gr->gr_passwd; /* no change */
    else {
        pwdb_entry_passwd = _pwdb_dup_string((char *) pwe->value);
        grent.gr_passwd = pwdb_entry_passwd;
        pwdb_entry_delete(&pwe);
    }    

    /* Now update the users list if available in pwdb struct */
    retval = pwdb_get_entry(*p, "users", &pwe);
    if ((retval != PWDB_SUCCESS) || !pwe || !pwe->value)
        grent.gr_mem = gr->gr_mem; /* no change */
    else {
        int tnr=0;
        
        nr_users = 1;
        pwdb_entry_users = _pwdb_dup_string((char *) pwe->value);
        pwdb_entry_delete(&pwe);
        /* count the number of users */
        for (i=0; i < strlen(pwdb_entry_users); i++)
            if(pwdb_entry_users[i] == ',')
                nr_users++;
        /* now alloc (char *) pointer space */
        mem = (char **)malloc(sizeof(char *) * (nr_users + 1));
        if (!mem) {
            if (pwdb_entry_passwd)
                pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
            pwdb_entry_users = _pwdb_delete_string(pwdb_entry_users);
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            __pwdb_gr_close();
            __pwdb_gr_unlock();
            return PWDB_MALLOC;
        }
        /* fill the new group members struct */
        mem[0]=pwdb_entry_users;
        for (i=0; i < strlen(pwdb_entry_users); i++)
            if (pwdb_entry_users[i] == ',') {
                tnr++;
                pwdb_entry_users[i]='\0';
                mem[tnr]=pwdb_entry_users + i + 1;
            }
       mem[tnr + 1] = NULL;
       grent.gr_mem = mem;
    }    
    
    /* Do update */
    if (!__pwdb_gr_update(&grent)) {
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
        if (pwdb_entry_users)
            free(pwdb_entry_users); /* no longer a string... */
        __pwdb_gr_close();
        __pwdb_gr_unlock();
        return PWDB_ABORT;
    }

    /* See if we have changed the name */
    if ((pwdb_entry_group != NULL) && strcmp(name, pwdb_entry_group))
        if (!__pwdb_gr_remove(name)) {
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
            if (pwdb_entry_users)
                free(pwdb_entry_users); /* no longer a string... */
            __pwdb_gr_close();
            __pwdb_gr_unlock();
            return PWDB_ABORT;
        }
            
    if (!__pwdb_gr_close()) {
        __pwdb_gr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
        if (pwdb_entry_users)
            free(pwdb_entry_users); /* no longer a string... */
        return PWDB_ABORT;
    }
    __pwdb_gr_unlock();
    pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
    pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
    if (pwdb_entry_users)
        free(pwdb_entry_users); /* no longer a string... */
    return PWDB_SUCCESS;
}

static int _pwdb_unix_gdelete(const char *name,
                              const int id,
                              const struct pwdb **p)
{
    const struct pwdb_entry *pwe;
    int retval;
    int i;
    int tgid;
    char * pwdb_entry_group = NULL;
    
    D(("called."));

    tgid = id;
    if ((name == PWDB_NAME_UNKNOWN) && (id == PWDB_ID_UNKNOWN)) {
        /* Try to make use of struct pwdb, if available */
        if (!p || !*p)
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "group", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_group = _pwdb_dup_string((char *) pwe->value);
            if ((pwdb_entry_group == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);
            if (pwdb_entry_group == NULL)
                retval = PWDB_NOT_FOUND;
        }
        if (retval != PWDB_SUCCESS) {
            /* if group lookup did not succeed, try gid */
            retval = pwdb_get_entry(*p, "gid", &pwe);
            if (retval == PWDB_SUCCESS) {
                tgid = *(const int *)pwe->value;
                pwdb_entry_delete(&pwe);
            } else
                return PWDB_BAD_REQUEST;
        }
    }

    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_group != NULL))
        name = pwdb_entry_group;
            
    /* try for PWDB_LOCK_TIME seconds to lock the passwd file */
    for (i = 0;i < 10;i++)
        if (__pwdb_gr_lock())
            break;
        else
            sleep (1);
    /* if we ran out of time ... */
    if (i == PWDB_LOCK_TIME) {
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_BLOCKED;
    }
                  
    /* now open the group file */
    if (!__pwdb_gr_open(O_RDWR)) {
        __pwdb_gr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_NOT_FOUND;
    }
    
    if (name == PWDB_NAME_UNKNOWN) {
        /* find out the name if we have the gid ... */    
        const struct __pwdb_group *grp;
        
        grp = __pwdb_gr_locate_id(tgid);
        if (!grp) {
            __pwdb_gr_close();
            __pwdb_gr_unlock();
            return PWDB_NOT_FOUND;
        }                                 
        /* pwdb_entry_group is already NULL... */
        pwdb_entry_group = _pwdb_dup_string(grp->gr_name);
        name = pwdb_entry_group;
    };
    
    /* the file was read on gr_open call */
    if (!__pwdb_gr_remove(name)) {
        __pwdb_gr_close();
        __pwdb_gr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_ABORT;
    }

    /* close & rewrite the group file */
    if (! __pwdb_gr_close()) {
        __pwdb_gr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_ABORT;
    }
    __pwdb_gr_unlock();
    pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        
    return PWDB_SUCCESS;
}

static int _pwdb_unix_gsupport(const char *entry_name)
{
    int i = 0;

    static const char *supp_entry[] = {
    "group", "passwd",
    "gid", "users",                       /* these are from /etc/group */
    "groups", "gids",                /* these are serviced by requests */

        NULL
    };

    D(("called."));

    while (supp_entry[i] != NULL) {
        if (!strcmp(supp_entry[i], entry_name))
            return PWDB_SUCCESS;
        i++;
    }

    return PWDB_NOT_FOUND;
}
