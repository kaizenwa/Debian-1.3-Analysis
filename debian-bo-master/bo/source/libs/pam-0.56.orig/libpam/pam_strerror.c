/* pam_strerror.c */

/* $Id: pam_strerror.c,v 1.6 1997/01/04 20:12:02 morgan Exp $
 *
 * $Log: pam_strerror.c,v $
 * Revision 1.6  1997/01/04 20:12:02  morgan
 * replaced conditional FAIL_NOW with ABORT
 *
 * Revision 1.5  1996/07/07 23:58:56  morgan
 * corrected "... " to "..."
 *
 * Revision 1.4  1996/06/02 08:03:29  morgan
 * spelling correction
 *
 * Revision 1.3  1996/03/16 23:08:54  morgan
 * PAM --> Linux-PAM ;)
 *
 */

#include "pam_private.h"

const char *pam_strerror(int errnum)
{
    switch (errnum) {
    case PAM_SUCCESS:
	return "Success";
    case PAM_ABORT:
	return "Critical error - immediate abort";
    case PAM_OPEN_ERR:
	return "dlopen() failure";
    case PAM_SYMBOL_ERR:
	return "Symbol not found";
    case PAM_SERVICE_ERR:
	return "Error in service module";
    case PAM_SYSTEM_ERR:
	return "System error";
    case PAM_BUF_ERR:
	return "Memory buffer error";
    case PAM_PERM_DENIED:
	return "Permission denied";
    case PAM_AUTH_ERR:
	return "Authentication failure";
    case PAM_CRED_INSUFFICIENT:
	return "Insufficient credentials to access authentication data";
    case PAM_AUTHINFO_UNAVAIL:
	return "Authentication service cannot retrieve authentication info.";
    case PAM_USER_UNKNOWN:
	return "User not known to the underlying authentication module";
    case PAM_MAXTRIES:
	return "Have exhasted maximum number of retries for service.";
    case PAM_AUTHTOKEN_REQD:
	return "Authentication token is no longer valid; new one required.";
    case PAM_ACCT_EXPIRED:
	return "User account has expired";
    case PAM_SESSION_ERR:
	return "Cannot make/remove an entry for the specified session";
    case PAM_CRED_UNAVAIL:
	return "Authentication service cannot retrieve user credentials";
    case PAM_CRED_EXPIRED:
	return "User credentials expired";
    case PAM_CRED_ERR:
	return "Failure setting user credentials";
    case PAM_NO_MODULE_DATA:
	return "No module specific data is present";
    case PAM_BAD_ITEM:
	return "Bad item passed to pam_*_item()";
    case PAM_CONV_ERR:
	return "Conversation error";
    case PAM_AUTHTOK_ERR:
	return "Authentication token manipulation error";
    case PAM_AUTHTOK_RECOVER_ERR:
	return "Authentication information cannot be recovered";
    case PAM_AUTHTOK_LOCK_BUSY:
	return "Authentication token lock busy";
    case PAM_AUTHTOK_DISABLE_AGING:
	return "Authentication token aging disabled";
    case PAM_TRY_AGAIN:
	return "Failed preliminary check by password service";
    case PAM_IGNORE:
	return "Please ignore underlying account module";
    }

    return "Unknown Linux-PAM error";
}
