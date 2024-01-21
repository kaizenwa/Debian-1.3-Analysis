/*
 * $Id: help_env.c,v 1.2 1997/01/04 20:19:20 morgan Exp $
 *
 * This file was written by Andrew G. Morgan <morgan@parc.power.net>
 *
 * $Log: help_env.c,v $
 * Revision 1.2  1997/01/04 20:19:20  morgan
 * added a prototype (no warning) and fixed paste function
 *
 * Revision 1.1  1996/12/01 03:25:37  morgan
 * Initial revision
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <security/pam_misc.h>

/*
 * This is a useful function for dumping the Linux-PAM environment
 * into some local memory, prior to it all getting lost when pam_end()
 * is called.
 */

char **pam_misc_copy_env(pam_handle_t *pamh)
{
    char **dump;
    char * const * env;
    int i;

    env = (char * const *) pam_getenvlist(pamh);
    if (env == NULL) {
	D(("no environment found"));
	return NULL;
    }
    
    /* reckon size of environment */
    for (i=0; env[i++]; ) {
	D(("env[%d] = %s", i-1,env[i-1]));
    }
    D(("now get some memory for dump"));

    /* allocate some memory for this (plus the null tail-pointer) */
    dump = calloc(i--, sizeof(char *));
    D(("dump = %p", dump));
    if (dump == NULL) {
	return NULL;
    }

    /* now run through entries and copy the variables over */
    dump[i] = NULL;
    while (i-- > 0) {
	D(("env[%d]=`%s'", i,env[i]));
	dump[i] = xstrdup(env[i]);
	D(("->dump[%d]=`%s'", i,dump[i]));
	if (dump[i] == NULL) {
	    /* out of memory */

	    while (dump[++i]) {
		_pam_overwrite(dump[i]);
		_pam_drop(dump[i]);
	    }
	    return NULL;

	}
    }

    env = NULL;                             /* forget now */

    /* transcribed environment */
    return dump;
}

/*
 * This function should be used to carefully dispose of the copied
 * environment.
 *
 *     usage:     env = pam_misc_drop_env(env);
 */

char **pam_misc_drop_env(char **dump)
{
    int i;

    for (i=0; dump[i] != NULL; ++i) {
	D(("dump[%d]=`%s'", i, dump[i]));
	_pam_overwrite(dump[i]);
	_pam_drop(dump[i]);
    }
    _pam_drop(dump);

    return NULL;
}

/*
 *  This function takes the supplied environment and uploads it to be
 *  the PAM one.
 */

int pam_misc_paste_env(pam_handle_t *pamh, const char * const * user_env)
{
    for (; user_env && *user_env; ++user_env) {
	int retval;

	D(("uploading: %s", *user_env));
	retval = pam_putenv(pamh, *user_env);
	if (retval != PAM_SUCCESS) {
	    D(("error setting %s: %s", *user_env, pam_strerror(retval)));
	    return retval;
	}
    }
    D(("done."));
    return PAM_SUCCESS;
}

/*
 * This is a wrapper to make pam behave in the way that setenv() does.
 */

int pam_misc_setenv(pam_handle_t *pamh, const char *name
		    , const char *value, int readonly)
{
    char *tmp;
    int retval;

    if (readonly) {
	const char *etmp;

	/* we check if the variable is there already */
	etmp = pam_getenv(pamh, name);
	if (etmp != NULL) {
	    D(("failed to set readonly variable: %s", name));
	    return PAM_PERM_DENIED;          /* not allowed to overwrite */
	}
    }
    tmp = malloc(2+strlen(name)+strlen(value));
    if (tmp != NULL) {
	sprintf(tmp,"%s=%s",name,value);
	D(("pam_putt()ing: %s", tmp));
	retval = pam_putenv(pamh, tmp);
	_pam_overwrite(tmp);                 /* purge */
	_pam_drop(tmp);                      /* forget */
    } else {
	D(("malloc failure"));
	retval = PAM_BUF_ERR;
    }

    return retval;
}
