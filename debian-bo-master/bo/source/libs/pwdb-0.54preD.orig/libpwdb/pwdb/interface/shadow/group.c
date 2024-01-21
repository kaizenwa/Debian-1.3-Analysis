/* This file contains the necessary interface functions to
 * the pwdb modules. They are declared as static and this file
 * should be included in the module interface file, because
 * the function addresses are used in the structures from
 * pwdb_module.c file, and I have no idea how to feed a pointer
 * to an external function in that struct...
 * CG
 */

/*************************************************
 * SHADOW GROUP INTERFACE
 *************************************************/

static int _pwdb_shadow_glocate(const char *name,
                                const int id,
                                const struct pwdb **p)
{
    struct __pwdb_sgrp *sgrp;
    const struct pwdb_entry *pwe;
    int retval;
    char *pwdb_entry_group = NULL;
    
    if (p == NULL) {
        return PWDB_ABORT;           /* were are we supposed to put pwdb ? */
    }

    /* id is unsused */
    if (name == PWDB_NAME_UNKNOWN) {
        /* Try to make use of struct pwdb, if available */
        if ( *p == NULL )
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "group", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_group = _pwdb_dup_string((char *)pwe->value);
            if ((pwdb_entry_group == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);
        } else {
            DO_DEBUG;
            return retval;
        }
    }
    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_group == NULL))
        return PWDB_BAD_REQUEST;
    else if (name == PWDB_NAME_UNKNOWN)
        name = pwdb_entry_group;

    sgrp = __pwdb_getsgnam(name);
    if (!sgrp) {
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_NOT_FOUND;
    }

    /* Now we have the user info in the sgrp pointer. Fill out the
     * pwdb_entry with this info */
    
    if (!*p) {
        retval = pwdb_new(p, TIME_TO_LIVE);
    } else {
        retval = pwdb_expire(*p, TIME_TO_LIVE);
    }
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return retval;
    }

    /* Start the show */
    retval = pwdb_set_entry(*p,"group", sgrp->sg_name
                            , 1+strlen(sgrp->sg_name)
                            , NULL, txtcpy, 1+strlen(sgrp->sg_name));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return retval;
    }

    retval = pwdb_set_entry(*p, "passwd", sgrp->sg_passwd,
                            1+strlen(sgrp->sg_passwd)
                            , NULL, txtcpy, 1+strlen(sgrp->sg_passwd));
    if (retval != PWDB_SUCCESS) {
        DO_DEBUG;
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return retval;
    }

    {
        char *mem;
        int glen,tlen;

        for (glen=tlen=0; sgrp->sg_mem[glen]; ++glen)
            tlen += 1+strlen(sgrp->sg_mem[glen]);

        /* reserve enough memory for user-list */

        mem = malloc(tlen);
        if (mem == NULL) {
            return PWDB_MALLOC;
        }

        /* copy usernames into mem separated by commas */

        for (glen=tlen=0; sgrp->sg_mem[glen]; ++glen) {
            strcpy(mem+tlen, sgrp->sg_mem[glen]);
            tlen += strlen(sgrp->sg_mem[glen]);
            mem[tlen++] = ',';
        }
        mem[--tlen] = '\0';   

        retval = pwdb_set_entry(*p, "users", mem, tlen, NULL, txtcpy, tlen + 1);
        if (retval != PWDB_SUCCESS) {
            DO_DEBUG;
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            return retval;
        }
        mem = _pwdb_delete_string(mem);
    }

    {
        char *mem;
        int glen,tlen;

        for (glen=tlen=0; sgrp->sg_adm[glen]; ++glen)
            tlen += 1+strlen(sgrp->sg_adm[glen]);

        /* reserve enough memory for user-list */

        mem = malloc(tlen);
        if (mem == NULL) {
            return PWDB_MALLOC;
        }

        /* copy usernames into mem separated by commas */

        for (glen=tlen=0; sgrp->sg_adm[glen]; ++glen) {
            strcpy(mem+tlen, sgrp->sg_adm[glen]);
            tlen += strlen(sgrp->sg_adm[glen]);
            mem[tlen++] = ',';
        }
        mem[--tlen] = '\0';   

        retval = pwdb_set_entry(*p, "admins", mem, tlen, NULL, txtcpy, tlen + 1);
        if (retval != PWDB_SUCCESS) {
            DO_DEBUG;
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            return retval;
        }
        mem=_pwdb_delete_string(mem);
    }

    pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);

    return PWDB_SUCCESS;
}

static int _pwdb_shadow_grequest(const char *entry_name,
                                 const struct pwdb **p)
{
    int retval = PWDB_BAD_REQUEST;

    if (p == NULL || *p == NULL)
        return PWDB_BAD_REQUEST;

    if (strcmp("groups", entry_name) == 0) {
        /* we need to build a list of groups that this user is a member of */
        const struct pwdb_entry *pwe;
        const struct __pwdb_sgrp *sgrp;
        char *groups=NULL;
        const char *user;
        int lgroups=0;

        /* which user are we interested in? */

        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval != PWDB_SUCCESS || pwe == NULL || pwe->value == NULL) {
            return PWDB_BAD_REQUEST;
        }
        user = (const char *) pwe->value;

        /* open the group file */
        __pwdb_setsgent();

        while ((sgrp = __pwdb_getsgent()) != NULL) {       /* read an entry */
            int i;
            /*
             * if the user is in this group add the group name to list
             */
            for (i=0; sgrp->sg_mem[i]; ++i) {
                if ( strcmp(sgrp->sg_mem[i], user) == 0 ) {
                    char *tgroups;
                    int nlgroups;

                    if (lgroups) {      /* already have one so need comma */
                        groups[lgroups++] = ',';
                    }
                    nlgroups = lgroups + strlen(sgrp->sg_name);
                    tgroups = realloc(groups, 1 + nlgroups);
                    if (tgroups == NULL) {
                        groups = _pwdb_delete_string(groups);
                        (void) pwdb_entry_delete(&pwe);
                        return PWDB_MALLOC;
                    }
                    groups = tgroups;

                    /* copy the group and gid */
                    strcpy(groups+lgroups, sgrp->sg_name);
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
        __pwdb_endsgent();
        
    } else {
        retval = PWDB_NOT_FOUND;
    }

    return retval;
}

static int _pwdb_shadow_greplace(const char *name,
                                 const int id,
                                 const struct pwdb **p)
{
    const struct pwdb_entry *pwe;
    const struct __pwdb_sgrp *sg;
    struct __pwdb_sgrp sgent;
    int retval;
    int i;
    char *pwdb_entry_group = NULL;
    char *pwdb_entry_passwd = NULL;
    char *pwdb_entry_users = NULL;
    char *pwdb_entry_admins = NULL;
    char **mem = NULL; /* temporary holdspace for member list */
    char **adm = NULL; /* temporary holdspace for admins list */
    int nr_users = 0; /* number of members of this group */
    int nr_admins = 0; /* number of admins in this group */
    /* this is a very ugly hack to shut off gcc warning about discarding a 
     * const pointer. Don't you like clean code ? :-) --cristiang */
    const char ** p_sg_name = (const char **) &sgent.sg_name;

    /* do the necessary checks */
    if (p == NULL) {
        return PWDB_ABORT;           /* were are we supposed to put pwdb ? */
    }
    if (!*p) /* we need some data to change ... */
        return PWDB_BAD_REQUEST;

    /* Try to make use of struct pwdb, if available */

    /* try to get the groupname out of it */
    retval = pwdb_get_entry(*p, "group", &pwe);
    if (retval == PWDB_SUCCESS) {
        pwdb_entry_group = _pwdb_dup_string((const char *)pwe->value);
        if ((pwdb_entry_group == NULL) && (pwe->value != NULL)) {
            pwdb_entry_delete(&pwe);
            return PWDB_MALLOC;
        }
        pwdb_entry_delete(&pwe);
        if (!strlen(pwdb_entry_group))
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
    }
    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_group == NULL))
        return PWDB_BAD_REQUEST;
    else if (name == PWDB_NAME_UNKNOWN)
        name = pwdb_entry_group;

    /* Open the database */
    /* try for PWDB_LOCK_TIME seconds to lock the passwd file */
    for (i = 0;i < 10;i++)
        if (__pwdb_sgr_lock())
            break;
        else
            sleep (1);
    /* if we ran out of time ... */
    if (i == PWDB_LOCK_TIME) {
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_BLOCKED;
    }
                  
    /* now open the group file */
    if (!__pwdb_sgr_open(O_RDWR)) {
        __pwdb_sgr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_NOT_FOUND;
    }

    /* 
     * Database tests:
     *  - old_name exists in the database
     *  - if change_name, new_name does not exist
     */
    /* database open */
    sg = NULL;
    sg = __pwdb_sgr_locate(name);
    if (!sg) {
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            return PWDB_NOT_FOUND;
    }

    /*
     * now try to extract some data from pwdb_entry structure
     */

    /*
     * NAME - if we already got the name from pwdb struct, skip over...
     */
    if (pwdb_entry_group == NULL)
        *p_sg_name = name;
    else
        sgent.sg_name = pwdb_entry_group;

    /* See if the group passwd is about to change */
    retval = pwdb_get_entry(*p, "passwd", &pwe);
    if ((retval != PWDB_SUCCESS) || !pwe || !pwe->value)
        sgent.sg_passwd = sg->sg_passwd; /* no change */
    else {
        pwdb_entry_passwd = _pwdb_dup_string((char *) pwe->value);
        sgent.sg_passwd = pwdb_entry_passwd;
        pwdb_entry_delete(&pwe);
    }    

    /* Now update the users list if available in pwdb struct */
    retval = pwdb_get_entry(*p, "users", &pwe);
    if ((retval != PWDB_SUCCESS) || !pwe || !pwe->value)
        sgent.sg_mem = sg->sg_mem; /* no change */
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
            pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
            pwdb_entry_users = _pwdb_delete_string(pwdb_entry_users);
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            __pwdb_sgr_close();
            __pwdb_sgr_unlock();
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
       sgent.sg_mem = mem;
    }    
    
    /* Now update the admins list if available in pwdb struct */
    retval = pwdb_get_entry(*p, "admins", &pwe);
    if ((retval != PWDB_SUCCESS) || !pwe || !pwe->value)
        sgent.sg_adm = sg->sg_adm; /* no change */
    else {
        int tnr=0;
        
        nr_admins = 1;
        pwdb_entry_admins = _pwdb_dup_string((char *) pwe->value);
        pwdb_entry_delete(&pwe);
        /* count the number of admins */
        for (i=0; i < strlen(pwdb_entry_admins); i++)
            if(pwdb_entry_admins[i] == ',')
                nr_admins++;
        /* now alloc (char *) pointer space */
        adm = (char **)malloc(sizeof(char *) * (nr_admins + 1));
        if (!mem) {
            pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
            pwdb_entry_users = _pwdb_delete_string(pwdb_entry_users);
            pwdb_entry_admins = _pwdb_delete_string(pwdb_entry_admins);
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            __pwdb_sgr_close();
            __pwdb_sgr_unlock();
            return PWDB_MALLOC;
        }
        /* fill the new group admins struct */
        adm[0]=pwdb_entry_admins;
        for (i=0; i < strlen(pwdb_entry_admins); i++)
            if (pwdb_entry_admins[i] == ',') {
                tnr++;
                pwdb_entry_admins[i]='\0';
                adm[tnr]=pwdb_entry_admins + i + 1;
            }
       adm[tnr + 1] = NULL;
       sgent.sg_adm = adm;
    }    

    /* Do update */
    if (!__pwdb_sgr_update(&sgent)) {
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
        if (pwdb_entry_users)
            free(pwdb_entry_users); /* no longer a string... */
        if (pwdb_entry_admins)
            free(pwdb_entry_admins); /* no longer a string... */
        __pwdb_sgr_close();
        __pwdb_sgr_unlock();
        return PWDB_ABORT;
    }

    /* See if we have changed the name */
    if ((pwdb_entry_group != NULL) && strcmp(name, pwdb_entry_group))
        if (!__pwdb_sgr_remove(name)) {
            pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
            pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
            if (pwdb_entry_users)
                free(pwdb_entry_admins); /* no longer a string... */
            if (pwdb_entry_admins)
                free(pwdb_entry_admins); /* no longer a string... */
            __pwdb_sgr_close();
            __pwdb_sgr_unlock();
            return PWDB_ABORT;
        }
            
    if (!__pwdb_sgr_close()) {
        __pwdb_sgr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
        if (pwdb_entry_users)
            free(pwdb_entry_users); /* no longer a string... */
        if (pwdb_entry_admins)
            free(pwdb_entry_admins); /* no longer a string... */
        return PWDB_ABORT;
    }
    __pwdb_sgr_unlock();
    pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
    pwdb_entry_passwd = _pwdb_delete_string(pwdb_entry_passwd);
    if (pwdb_entry_users)
        free(pwdb_entry_users); /* no longer a string... */
    if (pwdb_entry_admins)
        free(pwdb_entry_admins); /* no longer a string... */
    return PWDB_SUCCESS;
}

static int _pwdb_shadow_gdelete(const char *name,
                                const int id,
                                const struct pwdb **p)
{
    const struct pwdb_entry *pwe;
    int retval;
    int i;
    int tgid;
    char * pwdb_entry_group = NULL;
    
    tgid = id;
    if (name == PWDB_NAME_UNKNOWN) {
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
        } else {
            DO_DEBUG;
            return retval;
        }
    }

    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_group != NULL))
        name = pwdb_entry_group;
    if (name == PWDB_NAME_UNKNOWN)
        return PWDB_BAD_REQUEST;
            
    /* try for PWDB_LOCK_TIME seconds to lock the passwd file */
    for (i = 0;i < 10;i++)
        if (__pwdb_sgr_lock())
            break;
        else
            sleep (1);
    /* if we ran out of time ... */
    if (i == PWDB_LOCK_TIME) {
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_BLOCKED;
    }
                  
    /* now open the group file */
    if (!__pwdb_sgr_open(O_RDWR)) {
        __pwdb_sgr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_NOT_FOUND;
    }
    
    /* the file was read on sg_open call */
    if (!__pwdb_sgr_remove(name)) {
        __pwdb_sgr_close();
        __pwdb_sgr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_ABORT;
    }

    /* close & rewrite the group file */
    if (! __pwdb_sgr_close()) {
        __pwdb_sgr_unlock();
        pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        return PWDB_ABORT;
    }
    __pwdb_sgr_unlock();
    pwdb_entry_group = _pwdb_delete_string(pwdb_entry_group);
        
    return PWDB_SUCCESS;
}


static int _pwdb_shadow_gsupport(const char *entry_name)
{
    int i = 0;

    static const char *supp_entry[] = {
    "group", "passwd",
    "users", "admins"       /* these are from /etc/group */
    "groups",               /* these are serviced by requests */
    NULL
    };

    while (supp_entry[i] != NULL) {
        if (!strcmp(supp_entry[i], entry_name))
            return PWDB_SUCCESS;
        i++;
    }

    return PWDB_NOT_FOUND;
}

