/* pam_start.c */

/* Creator Marc Ewing
 *
 * $Id: pam_start.c,v 1.9 1997/02/15 16:02:31 morgan Exp $
 *
 * $Log: pam_start.c,v $
 * Revision 1.9  1997/02/15 16:02:31  morgan
 * lower case service name forced
 *
 * Revision 1.8  1997/01/04 20:10:26  morgan
 * initialize modules on pam_start call (pam_start can now fail)
 *
 * Revision 1.7  1996/12/01 03:14:13  morgan
 * use _pam_macros.h
 *
 * Revision 1.6  1996/11/10 20:08:45  morgan
 * name convention _pam_ and also modification for pam_get_user change
 *
 * Revision 1.5  1996/07/07 23:58:23  morgan
 * added support for pam_fail_delay function
 *
 * Revision 1.4  1996/05/21 04:39:21  morgan
 * added debugging code
 *
 *
 */

#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <syslog.h>

#include "pam_private.h"

int pam_start (
    const char *service_name,
    const char *user,
    const struct pam_conv *pam_conversation,
    pam_handle_t **pamh)
{
    D(("called pam_start: [%s] [%s] [%p] [%p]"
		,service_name,user,pam_conversation,pamh));

    if ((*pamh = malloc(sizeof(**pamh))) == NULL) {
	_pam_log_error("pam_start: malloc failed for *pamh");
	return (PAM_BUF_ERR);
    }

    if (service_name) {
	char *tmp;

	if (((*pamh)->service_name = _pam_strdup(service_name)) == NULL) {
	    _pam_log_error("pam_start: _pam_strdup failed for service name");
	    _pam_drop(*pamh);
	    return (PAM_BUF_ERR);
	}
	for (tmp=(*pamh)->service_name; *tmp; ++tmp)
	    *tmp = tolower(*tmp);                   /* require lower case */
    } else
       	(*pamh)->service_name = NULL;

    if (user) {
	if (((*pamh)->user = _pam_strdup(user)) == NULL) {
	    _pam_log_error("pam_start: _pam_strdup failed for user");
	    _pam_drop((*pamh)->service_name);
	    _pam_drop(*pamh);
	    return (PAM_BUF_ERR);
	}
    } else
	(*pamh)->user = NULL;

    (*pamh)->tty = NULL;
    (*pamh)->prompt = NULL;              /* prompt for pam_get_user() */
    (*pamh)->ruser = NULL;
    (*pamh)->rhost = NULL;
    (*pamh)->authtok = NULL;
    (*pamh)->oldauthtok = NULL;

    if (pam_conversation == NULL
	|| ((*pamh)->pam_conversation = (struct pam_conv *)
	    malloc(sizeof(struct pam_conv))) == NULL) {
	_pam_log_error("pam_start: malloc failed for pam_conv");
	_pam_drop((*pamh)->service_name);
	_pam_drop((*pamh)->user);
	_pam_drop(*pamh);
	return (PAM_BUF_ERR);
    } else {
	memcpy((*pamh)->pam_conversation
	       ,pam_conversation
	       ,sizeof(struct pam_conv));
    }

    (*pamh)->data = NULL;
    if ( _pam_make_env(*pamh) != PAM_SUCCESS ) {
	_pam_log_error("pam_start: failed to initialize environment");
	_pam_drop((*pamh)->service_name);
	_pam_drop((*pamh)->user);
	_pam_drop(*pamh);
	return PAM_ABORT;
    }

#ifdef PAM_FAIL_DELAY_ON
    _pam_reset_timer(*pamh);         /* initialize timer support */
#endif /* PAM_FAIL_DELAY_ON */

    _pam_start_handlers(*pamh);                   /* cannot fail */

    /* According to the SunOS man pages, loading modules and resolving
     * symbols happens on the first call from the application. */

    /*
     * XXX - should we call _pam_init_handlers() here ? The following
     * is new as of Linux-PAM 0.55
     */

    if ( _pam_init_handlers(*pamh) != PAM_SUCCESS ) {
	_pam_log_error("pam_start: failed to initialize handlers");
	_pam_drop_env(*pamh);                 /* purge the environment */
	_pam_drop((*pamh)->service_name);
	_pam_drop((*pamh)->user);
	_pam_drop(*pamh);
	return PAM_ABORT;
    }

    D(("exiting pam_start successfully"));

    return PAM_SUCCESS;
}
