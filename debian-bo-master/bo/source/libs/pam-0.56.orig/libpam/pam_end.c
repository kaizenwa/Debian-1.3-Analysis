/* pam_end.c */

/*
 * $Id: pam_end.c,v 1.5 1996/12/01 03:14:13 morgan Exp $
 *
 * $Log: pam_end.c,v $
 * Revision 1.5  1996/12/01 03:14:13  morgan
 * use _pam_macros.h
 *
 * Revision 1.4  1996/11/10 20:01:44  morgan
 * changes to account for pam_get_user reimplementation
 *
 * Revision 1.3  1996/07/07 23:56:01  morgan
 * added some debugging info
 *
 * Revision 1.2  1996/03/16 21:47:12  morgan
 * added RCS info and also overwriting of passwords before liberating
 * memory.
 *
 *
 */

#include <stdlib.h>

#include "pam_private.h"

int pam_end(pam_handle_t *pamh, int pam_status)
{
    int ret;

    IF_NO_PAMH("pam_end",pamh,PAM_SYSTEM_ERR);

    D(("entering pam_end()"));

    /* first liberate the modules (it is not inconcevible that the
       modules may need to use the service_name etc. to clean up) */

    _pam_free_data(pamh, pam_status);

    /* now drop all modules */

    if ((ret = _pam_free_handlers(pamh)) != PAM_SUCCESS) {
	return ret;                 /* error occurred */
    }

    /* from this point we cannot call the modules any more. Free the remaining
       memory used by the Linux-PAM interface */

    _pam_drop_env(pamh);                      /* purge the environment */
    _pam_drop(pamh->service_name);
    _pam_drop(pamh->user);
    _pam_drop(pamh->prompt);                  /* prompt for pam_get_user() */
    _pam_drop(pamh->pam_conversation);
    _pam_drop(pamh->tty);
    _pam_drop(pamh->rhost);
    _pam_drop(pamh->ruser);
    _pam_overwrite(pamh->authtok);             /* blank out old token */
    _pam_drop(pamh->authtok);
    _pam_overwrite(pamh->oldauthtok);          /* blank out old token */
    _pam_drop(pamh->oldauthtok);

    /* and finally liberate the memory for the pam_handle structure */

    _pam_drop(pamh);

    D(("exiting pam_end() successfully"));

    return PAM_SUCCESS;
}
