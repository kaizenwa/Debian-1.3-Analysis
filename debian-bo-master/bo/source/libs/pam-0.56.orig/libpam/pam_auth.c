/* pam_auth.c -- PAM authentication */

#include <stdio.h>
#include <stdlib.h>

#include "pam_private.h"

int pam_authenticate(pam_handle_t *pamh, int flags)
{
    int retval;

    D(("pam_authenticate called"));

    IF_NO_PAMH("pam_authenticate",pamh,PAM_SYSTEM_ERR);
    retval = _pam_dispatch(pamh, flags, PAM_AUTHENTICATE);

#ifdef PAM_FAIL_DELAY_ON
    _pam_start_timer(pamh);    /* we try to make the time for a failure
				  independent of the time it takes to
				  fail */
#endif /* PAM_FAIL_DELAY_ON */

    /*
     * this is for security. We reset the auth-tokens here.
     */
    pam_set_item(pamh,PAM_AUTHTOK,NULL);
    pam_set_item(pamh,PAM_OLDAUTHTOK,NULL);

#ifdef PAM_FAIL_DELAY_ON
    _pam_await_timer(pamh, retval);   /* if unsuccessful then wait now */
#endif /* PAM_FAIL_DELAY_ON */

    D(("pam_authenticate exit"));

    return retval;
}

int pam_setcred(pam_handle_t *pamh, int flags)
{
    int retval;

    IF_NO_PAMH("pam_setcred",pamh,PAM_SYSTEM_ERR);

    D(("pam_setcred called"));

    if (! flags) {
	flags = PAM_CRED_ESTABLISH;
    }

    retval = _pam_dispatch(pamh, flags, PAM_SETCRED);

    D(("pam_setcred exit"));

    return retval;
}
