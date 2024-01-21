/* pam_password.c - PAM Password Management */

/*
 * $Id: pam_password.c,v 1.6 1996/12/01 03:14:13 morgan Exp $
 *
 * $Log: pam_password.c,v $
 * Revision 1.6  1996/12/01 03:14:13  morgan
 * use _pam_macros.h
 *
 * Revision 1.5  1996/09/05 06:16:41  morgan
 * changed delays and slight reformatting
 *
 * Revision 1.4  1996/05/11 08:13:07  morgan
 * clear the AUTHTOK items before returning to the application
 *
 * Revision 1.3  1996/03/16 21:58:12  morgan
 * appended PAM_UPDATE_AUTHTOK to flags of second call to _pam_dispatch
 * dropped authentication token free()'ing. Not appropriate here.
 *
 * Revision 1.2  1996/03/10 02:18:13  morgan
 * Embarrassing this. It was not being compiled so I missed some faulty
 * definitions.
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include "pam_private.h"

int pam_chauthtok(pam_handle_t *pamh, int flags)
{
    int retval;

    D(("called."));

    IF_NO_PAMH("pam_chauthtok",pamh,PAM_SYSTEM_ERR);

#ifdef PAM_FAIL_DELAY_ON
    _pam_start_timer(pamh);    /* we try to make the time for a failure
				  independent of the time it takes to
				  fail */
#endif /* PAM_FAIL_DELAY_ON */

    /* first loop through to check if there will be a problem */

    if ((retval = _pam_dispatch(pamh, flags|PAM_PRELIM_CHECK
				, PAM_CHAUTHTOK)) == PAM_SUCCESS) {
	retval = _pam_dispatch(pamh, flags|PAM_UPDATE_AUTHTOK
			       , PAM_CHAUTHTOK);

#ifdef PAM_FAIL_DELAY_ON
    } else {
	_pam_await_timer(pamh, retval);   /* if unsuccessful then wait now */
#endif /* PAM_FAIL_DELAY_ON */

    }

    /*
     * this is for security. We reset the auth-tokens here.
     */
    pam_set_item(pamh,PAM_AUTHTOK,NULL);
    pam_set_item(pamh,PAM_OLDAUTHTOK,NULL);

    return retval;
}

