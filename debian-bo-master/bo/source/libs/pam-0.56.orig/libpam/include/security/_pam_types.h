/*
 * <security/_pam_types.h>
 *
 * $Id: _pam_types.h,v 1.9 1997/01/04 20:13:40 morgan Exp $
 *
 * This file defines all of the types common to the Linux-PAM library
 * applications and modules.
 *
 * Note, the copyright information is at end of file.
 *
 * Created: 1996/3/5 by AGM
 *
 * $Log: _pam_types.h,v $
 * Revision 1.9  1997/01/04 20:13:40  morgan
 * remove PAM_FAIL_NOW, moved PAM_DATA_xx to the modules file where
 * it belongs
 *
 * Revision 1.8  1996/12/01 03:24:00  morgan
 * added environment functions
 *
 * Revision 1.7  1996/11/10 19:54:58  morgan
 * added some optional memory debugging.
 * added some non-Linux-PAM compatability code.. [header file inclusion]
 *
 * Revision 1.6  1996/07/07 23:54:56  morgan
 * added support for pam_fail_delay
 *
 */

#ifndef _SECURITY__PAM_TYPES_H
#define _SECURITY__PAM_TYPES_H

/*
 * include local definition for POSIX - NULL
 */

#include <locale.h>

/* This is a blind structure; users aren't allowed to see inside a
 * pam_handle_t, so we don't define struct _pam_handle here.  This is
 * defined in a file private to the PAM library.  (i.e., it's private
 * to PAM service modules, too!)  */

typedef struct _pam_handle pam_handle_t;

/* ------------ The Linux-PAM conversation structures ------------ */

/* Used to pass prompting text, error messages, or other informatory
 * text to the user.  This structure is allocated and freed by the PAM
 * library (or loaded module).  */

struct pam_message {
    int msg_style;
    const char *msg;
};

/* Used to return the user's response to the PAM library.  This
 * structure is allocated by the application program, and free()'d by
 * the Linux-PAM library (or calling module).  */

struct pam_response {
    char *resp;
    int	resp_retcode;	/* currently un-used, zero expected */
};

/* The actual conversation structure itself */

struct pam_conv {
    int (*conv)(int num_msg, const struct pam_message **msg,
		struct pam_response **resp, void *appdata_ptr);
    void *appdata_ptr;
};

/* Message styles */

#define PAM_PROMPT_ECHO_OFF	1
#define PAM_PROMPT_ECHO_ON	2
#define PAM_ERROR_MSG		3
#define PAM_TEXT_INFO		4

/* maximum size of messages */

#define PAM_MAX_MSG_SIZE      200

/* ---------------------- The Linux-PAM flags -------------------- */

/* Authentication service should not generate any messages */
#define PAM_SILENT			0x8000

/* The authentication service should return PAM_AUTH_ERROR if the
 * user has a null authentication token */
#define PAM_DISALLOW_NULL_AUTHTOK	0x0001

/* Note: these flags are used for pam_setcred() */

/* Set user credentials for an authentication service */
#define PAM_CRED_ESTABLISH		0x0002

/* Delete user credentials associated with an authentication service */
#define PAM_CRED_DELETE			0x0004

/* Reinitialize user credentials */
#define PAM_CRED_REINITIALIZE		0x0008

/* Extend lifetime of user credentials */
#define PAM_CRED_REFRESH		0x0010

/* Note: these flags are used by pam_chauthtok */

/* The password service should only update those passwords that have
 * aged.  If this flag is not passed, the password service should
 * update all passwords. */
#define PAM_CHANGE_EXPIRED_AUTHTOK	0x0020

/* ----------------- The Linux-PAM return values ------------------ */

#define PAM_SUCCESS 0		/* Successful function return */
#define PAM_OPEN_ERR 1		/* dlopen() failure when dynamically */
				/* loading a service module */
#define PAM_SYMBOL_ERR 2	/* Symbol not found */
#define PAM_SERVICE_ERR 3	/* Error in service module */
#define PAM_SYSTEM_ERR 4	/* System error */
#define PAM_BUF_ERR 5		/* Memory buffer error */
#define PAM_PERM_DENIED 6	/* Permission denied */
#define PAM_AUTH_ERR 7		/* Authentication failure */
#define PAM_CRED_INSUFFICIENT 8	/* Can not access authentication data */
				/* due to insufficient credentials */
#define PAM_AUTHINFO_UNAVAIL 9	/* Underlying authentication service */
				/* can not retrieve authenticaiton */
				/* information  */
#define PAM_USER_UNKNOWN 10	/* User not known to the underlying */
				/* authenticaiton module */
#define PAM_MAXTRIES 11		/* An authentication service has */
				/* maintained a retry count which has */
				/* been reached.  No further retries */
				/* should be attempted */
#define PAM_AUTHTOKEN_REQD 12	/* New authentication token required. */
				/* This is normally returned if the */
				/* machine security policies require */
				/* that the password should be changed */
				/* beccause the password is NULL or it */
				/* has aged */
#define PAM_ACCT_EXPIRED 13	/* User account has expired */
#define PAM_SESSION_ERR 14	/* Can not make/remove an entry for */
				/* the specified session */
#define PAM_CRED_UNAVAIL 15	/* Underlying authentication service */
				/* can not retrieve user credentials */
                                /* unavailable */
#define PAM_CRED_EXPIRED 16	/* User credentials expired */
#define PAM_CRED_ERR 17		/* Failure setting user credentials */
#define PAM_NO_MODULE_DATA 18	/* No module specific data is present */
#define PAM_CONV_ERR 19		/* Conversation error */
#define PAM_AUTHTOK_ERR 20	/* Authentication token manipulation error */
#define PAM_AUTHTOK_RECOVER_ERR 21 /* Authentication information */
				   /* cannot be recovered */
#define PAM_AUTHTOK_LOCK_BUSY 22   /* Authentication token lock busy */
#define PAM_AUTHTOK_DISABLE_AGING 23 /* Authentication token aging disabled */
#define PAM_TRY_AGAIN 24	/* Preliminary check by password service */
#define PAM_IGNORE 25		/* Ingore underlying account module */
				/* regardless of whether the control */
				/* flag is required, optional, or sufficient */
#define PAM_ABORT 26            /* Critical error (?module fail now request) */

#define PAM_BAD_ITEM       28   /* Bad item passed to pam_*_item() */

/* these defines are used by pam_set_item() and pam_get_item() */

#define PAM_SERVICE	1	/* The service name */
#define PAM_USER        2	/* The user name */
#define PAM_TTY         3	/* The tty name */
#define PAM_RHOST       4	/* The remote host name */
#define PAM_CONV        5	/* The pam_conv structure */

/* missing entries found in <security/pam_modules.h> for modules only! */

#define PAM_RUSER       8	/* The remote user name */
#define PAM_USER_PROMPT 9       /* the prompt for getting a username */

/* ---------- Common Linux-PAM application/module PI ----------- */

extern int pam_set_item(pam_handle_t *pamh, int item_type, const void *item);
extern int pam_get_item(const pam_handle_t *pamh, int item_type
			, const void **item);
extern const char *pam_strerror(int errnum);

extern int pam_putenv(pam_handle_t *pamh, const char *name_value);
extern const char * pam_getenv(pam_handle_t *pamh, const char *name);
extern const char * const *pam_getenvlist(pam_handle_t *pamh);

/* ---------- Common Linux-PAM application/module PI ----------- */

/*
 * here are some proposed error status definitions for the
 * 'error_status' argument used by the cleanup function associated
 * with data items they should be logically OR'd with the error_status
 * of the latest return from libpam -- new with .52 and positive
 * impression from Sun although not official as of 1996/9/4
 * [generally the other flags are to be found in pam_modules.h]
 */

#define PAM_DATA_SILENT    0x40000000     /* used to suppress messages... */

#ifdef PAM_FAIL_DELAY_ON

/*
 * here we define an externally (by apps or modules) callable function
 * that primes the libpam library to delay when a stacked set of
 * modules results in a failure. In the case of PAM_SUCCESS this delay
 * is ignored.
 */

extern int pam_fail_delay(pam_handle_t *pamh, unsigned int musec_delay);

#endif /* PAM_FAIL_DELAY_ON */

#ifdef MEMORY_DEBUG
/*
 * this defines some macros that keep track of what memory has been
 * allocated and indicates leakage etc... It should not be included in
 * production application/modules.
 */
#include <security/pam_malloc.h>
#endif

#ifndef LINUX_PAM
/*
 * the following few lines represent a hack.  They are there to make
 * the Linux-PAM headers more compatible with the Sun ones, which have a
 * less strictly separated notion of module specific and application
 * specific definitions.
 */
#include <security/pam_appl.h>
#include <security/pam_modules.h>
#endif

/* ... adapted from the pam_appl.h file created by Theodore Ts'o and
 *
 * Copyright Theodore Ts'o, 1996.  All rights reserved.
 * Copyright (c) Andrew G. Morgan <morgan@parc.power.net>, 1996
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, and the entire permission notice in its entirety,
 *    including the disclaimer of warranties.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote
 *    products derived from this software without specific prior
 *    written permission.
 * 
 * ALTERNATIVELY, this product may be distributed under the terms of
 * the GNU Public License, in which case the provisions of the GPL are
 * required INSTEAD OF the above restrictions.  (This clause is
 * necessary due to a potential bad interaction between the GPL and
 * the restrictions contained in a BSD-style copyright.)
 * 
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.  */

#endif /* _SECURITY__PAM_TYPES_H */

