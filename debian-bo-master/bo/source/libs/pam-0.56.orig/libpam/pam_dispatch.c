/* pam_dispatch.c - handles module function dispatch */

/*
 * $Id: pam_dispatch.c,v 1.8 1997/01/04 20:04:09 morgan Exp $
 *
 * last modified by AGM
 */

#include <stdlib.h>
#include <stdio.h>

#include "pam_private.h"

/*
 * this is the return code we return when a function pointer is NULL
 * or, the handler structure indicates a broken module config line
 */
#define PAM_MUST_FAIL_CODE        PAM_PERM_DENIED

static int _pam_dispatch_aux(pam_handle_t *pamh, int flags, struct handler *h)
{
    int ret, returned;
    int impression = PAM_PERM_DENIED;    /* carries the general impression */
    int result = PAM_PERM_DENIED;        /* this is what is to be returned */
    int got_one = 0;

    IF_NO_PAMH("_pam_dispatch_aux", pamh, PAM_SYSTEM_ERR);

    if (h == NULL) {
	const char *service=NULL;

	(void) pam_get_item(pamh, PAM_SERVICE, (const void **)&service);
	_pam_log_error("no modules loaded for `%s' service"
		       , service ? service:"<unknown>" );

	D(("no loaded modules for `%s' service"
	   , service ? service:"<unknown>"));
	service = NULL;
	return PAM_PERM_DENIED;
    }

    /* loop through all modules */
    while (h) {

#ifdef DEBUG
	{
	    int i;
	    D(("argc=%d\nargv=",h->argc));
	    for (i=0; i<h->argc; ++i) {
		D(("\t%s",h->argv[i]));
	    }
	}
#endif
	D(("user is %s", pamh->user));
	if (h->func == NULL) {
	    D(("module function is not defined, indicating fail"));
	    returned = PAM_MUST_FAIL_CODE;   /* mapped to other error above */
	} else {
	    D(("passing control to module..."));
	    returned = h->func(pamh, flags, h->argc, h->argv);
	    D(("module returned: %s", pam_strerror(returned)));
	    if (h->must_fail) {
		D(("module poorly listed in pam.conf; forcing failure"));
		returned = PAM_MUST_FAIL_CODE;  /* mapped to other error ^^ */
	    }
	}

	/*
	 * some returns can be treated like others: ret is less specific
	 * than returned, and indicates success as PAM_SUCCESS.
	 */

	switch (returned) {
	case PAM_AUTHTOKEN_REQD:         /* equivalent to success */
	    ret = PAM_SUCCESS;
	    D(("treating `%s' as PAM_SUCCESS", pam_strerror(returned)));
	    break;
	default:
	    ret = returned;              /* no mapping so take raw value */
	}

	if (ret != PAM_IGNORE) {
	    /* we should not ignore this result */

	    if (ret == PAM_SUCCESS) {

		if (!got_one) {              /* this is our first success */
		    impression = ret;        /* keep impression of success */
		    result = returned;       /* not PERM_DENIED now */
		    got_one = 1;
		}

		if (impression == PAM_SUCCESS      /* not failed a required */
		    && h->control_flag == PAM_F_SUFFICIENT /* is sufficient */
		    && ret == PAM_SUCCESS ) {      /* and it was successful */
		    D(("sufficient for success"));
		    break;      /* success! */
		}

	    } else {
		/* this is a failure... */

#ifdef PAM_FAIL_NOW_ON
		if (ret == PAM_ABORT) {
		    /*
		     * This is the result of an exceptional condition.  The
		     * module wants to stop talking to the user immediately.
		     */
		    got_one = 1;
		    impression = result = PAM_PERM_DENIED;
		    D(("pam failing now"));
		    break;
		}
#endif /* PAM_FAIL_NOW_ON */

		if ( h->control_flag == PAM_F_REQUIRED
		     || h->control_flag == PAM_F_CRITICAL ) {
		    D(("significant failure"));
		    if (!got_one || impression == PAM_SUCCESS) {
			D(("the recorded failure"));
			impression = ret;    /* set to fail */
			result = returned;   /* this takes the value of
						the first fail on a
						PAM_REQUIRED module */
		    }
		    got_one = 1;        /* only care about significant fails */
		    if ( h->control_flag == PAM_F_CRITICAL ) {
			D(("critical module has failed..."));
			break;
		    }
		} else {
		    D(("ignoring failure"));
		}
	    }
	}

	h = h->next;
    }

    if (got_one) {
	D(("returning %s", pam_strerror(result)));
	return result;
    }

/* this is if there are only PAM_OPTIONAL entries (or none at all)
   and they all fail! Is there a better error to return than this?  */

    D(("falling through to default failure"));
    return PAM_PERM_DENIED;
}


int _pam_dispatch(pam_handle_t *pamh, int flags, int choice)
{
    struct handler *h = NULL;
    int res;

    IF_NO_PAMH("_pam_dispatch",pamh,PAM_SYSTEM_ERR);

    /* Load all modules, resolve all symbols */

    if ((res = _pam_init_handlers(pamh)) != PAM_SUCCESS) {
	_pam_log_error("unable to dispatch function");
	return res;
    }

    switch (choice) {
    case PAM_AUTHENTICATE:
	h = pamh->handlers.conf.authenticate;
	break;
    case PAM_SETCRED:
	h = pamh->handlers.conf.setcred;
	break;
    case PAM_ACCOUNT:
	h = pamh->handlers.conf.acct_mgmt;
	break;
    case PAM_OPEN_SESSION:
	h = pamh->handlers.conf.open_session;
	break;
    case PAM_CLOSE_SESSION:
	h = pamh->handlers.conf.close_session;
	break;
    case PAM_CHAUTHTOK:
	h = pamh->handlers.conf.chauthtok;
	break;
    default:
	_pam_log_error("undefined fn choice; %d",choice);
	return PAM_ABORT;
    }

    if (h == NULL) {     /* there was no handlers.conf... entry; will use
			  * handlers.other... */
	switch (choice) {
	case PAM_AUTHENTICATE:
	    h = pamh->handlers.other.authenticate;
	    break;
	case PAM_SETCRED:
	    h = pamh->handlers.other.setcred;
	    break;
	case PAM_ACCOUNT:
	    h = pamh->handlers.other.acct_mgmt;
	    break;
	case PAM_OPEN_SESSION:
	    h = pamh->handlers.other.open_session;
	    break;
	case PAM_CLOSE_SESSION:
	    h = pamh->handlers.other.close_session;
	    break;
	case PAM_CHAUTHTOK:
	    h = pamh->handlers.other.chauthtok;
	    break;
	}
    }

    /* call the list of module functions */
    
    return _pam_dispatch_aux(pamh, flags, h);
}

/*
 * $Log: pam_dispatch.c,v $
 * Revision 1.8  1997/01/04 20:04:09  morgan
 * added "must fail" code to perform "garbled" syntax in pam.conf
 *
 * Revision 1.7  1996/12/18 04:16:56  morgan
 * stable code prior to new bad pam.conf handling...
 *
 */
