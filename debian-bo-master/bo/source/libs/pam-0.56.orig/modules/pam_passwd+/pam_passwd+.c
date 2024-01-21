/*
 * Copyright Al Longyear (longyear@netcom.com), 1996.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
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
 * the GNU General Public License, in which case the provisions of the
 * GNU GPL are required INSTEAD OF the above restrictions.  (This
 * clause is necessary due to a potential bad interaction between the
 * GNU GPL and the restrictions contained in a BSD-style copyright.)
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

static const char rcsid[] =
"$Id: pam_passwd+.c,v 1.4 1996/11/10 20:14:08 morgan Exp $";

#define PAM_SM_PASSWORD

#include "passwd.h"

static int converse(struct _options *opt, int nargs,
		    struct pam_message **message,
		    struct pam_response **response);
int pam_get_passwd (struct _options *opt, char **password, const char *prompt);
int pam_get_passwdold (struct _options *opt, char **passwdold, const char *prompt);
static void process_option (struct _options *opt, const char *name, char *param);
static void set_option (struct _options *opt, const char *arg);
static void set_parameters (struct _options *opt, int argc, const char **argv);
void plus_cleanup (struct _options *opt);
int _pam_passwd_chauthtok (pam_handle_t *pamh,
			   int flags,
			   int argc,
			   const char **argv);


/*------------- get the proposed new password value -----------------*/

static int converse(struct _options *opt, int nargs,
		    struct pam_message **message,
		    struct pam_response **response)
  {
    int retval;
    struct pam_conv *conv;

    if (opt->flags & PAM_SILENT)
	retval = PAM_CONV_ERR;
    else {
	retval = pam_get_item (opt->pamh, PAM_CONV, (const void **)&conv);
	if (retval == PAM_SUCCESS)
	    retval = conv->conv (nargs,
				 (const struct pam_message **) message,
				 response,
				 conv->appdata_ptr);
    }
    return retval;
}

/*------------- local routine to write a message to the user ----------- */

void do_converse (struct _options *opt, int is_error, const char *str)
{
    struct pam_message msg, *pmsg;
    struct pam_response *resp;
  
    pmsg          = &msg;
    msg.msg_style = is_error ? PAM_ERROR_MSG : PAM_TEXT_INFO;
    msg.msg       = str;
    resp          = NULL;
    
    (void) converse (opt, 1, &pmsg, &resp);

    if (resp) {
        memset (resp->resp, 0, strlen (resp->resp));
	free (resp->resp);
	free (resp);
    }
}

/*------------- get the proposed new password value -----------------*/

int pam_get_passwd (struct _options *opt, char **password, const char *prompt)
{
    const char *use_prompt;
    int retval;
    struct pam_message msg, *pmsg;
    struct pam_response *resp;
    char   *current;

    retval = pam_get_item (opt->pamh, PAM_AUTHTOK, (const void **) &current);
    if (retval != PAM_SUCCESS)
        return retval;

    if (current == NULL) {
        use_prompt = prompt;
	if (use_prompt == NULL) {
	    use_prompt = "New password: ";
	}

	/* converse with application -- prompt user for a password */
	
	pmsg          = &msg;
	msg.msg_style = PAM_PROMPT_ECHO_OFF;
	msg.msg       = use_prompt;
	resp          = NULL;

	retval = converse (opt, 1, &pmsg, &resp);

	if (retval == PAM_SUCCESS) {
	    current = resp->resp;
	    free (resp);
	    if (*current == '\0') {
	        free (current);
		current = (char *) 0;
	    }
	}
    }
    *password = current;

    return retval;        /* pass on any error from conversation */
}

/*------------- get the old password value -----------------*/

int pam_get_passwdold (struct _options *opt, char **passwdold, const char *prompt)
{
    const char *use_prompt;
    int retval;
    struct pam_message msg, *pmsg;
    struct pam_response *resp;
    char   *current;

    retval = pam_get_item (opt->pamh, PAM_OLDAUTHTOK, (const void **) &current);
    if (retval != PAM_SUCCESS)
        return retval;

    if (current == NULL) {
        retval = pam_get_item (opt->pamh, PAM_AUTHTOK, (const void **) &current);
	if (retval != PAM_SUCCESS)
	    return retval;

	if (current != NULL) {
	    char *new_current = (char *) 0;
	    (void) pam_set_item (opt->pamh, PAM_AUTHTOK, &new_current);
	}
    }

    if (current == NULL) {
        use_prompt = prompt;
	if (use_prompt == NULL) {
	    use_prompt = "Password: ";
	}

	/* converse with application -- prompt user for the old password */
	
	pmsg          = &msg;
	msg.msg_style = PAM_PROMPT_ECHO_OFF;
	msg.msg       = use_prompt;
	resp          = NULL;

	retval = converse (opt, 1, &pmsg, &resp);

	if (retval == PAM_SUCCESS) {
	    current = resp->resp;
	    free (resp);
	    if (*current == '\0') {
	        free (current);
		current = (char *) 0;
	    }
	}
    }
    *passwdold = current;

    return retval;        /* pass on any error from conversation */
}

/*------------- set options from the argument string -----------------*/

static void process_option (struct _options *opt, const char *name, char *param)
  {
    do {
        if (strcmp (name, "file") == 0) {
	    if (opt->pwtest != (char *) 0) {
	        free (opt->pwtest);
		opt->pwtest = (char *) 0;
	    }
	    if (param != (char *) 0)
	        opt->pwtest = strdup (param);
	    break;
	}

        if (!strcmp(name,"debug")) {
	    opt->ctrl |= PAM_ST_DEBUG;
	    break;
	}

	if (!strcmp(name,"no_warn")) {
	    opt->ctrl |= PAM_ST_NO_WARN;
	    break;
	}

	if (!strcmp(name,"use_first_pass")) {
	    opt->ctrl |= PAM_ST_USE_PASS1;
	    break;
	}

	if (!strcmp(name,"try_first_pass")) {
	    opt->ctrl |= PAM_ST_TRY_PASS1;
	    break;
	}
    } while (0);
}

/*------------- set options from the argument string -----------------*/

static void set_option (struct _options *opt, const char *arg)
{
    /* Look for a parameter first. If found, make a temp copy. */
    if (strchr (arg, '=') != (char *) 0) {
        char *temp_arg = strdup (arg);
	char *ptr      = strchr (temp_arg, '=');

	/* If there is a parameter then process it. */
	if (ptr != (char *) 0) {
	    *ptr++ = '\0';
	    process_option (opt, temp_arg, ptr);
	    free (temp_arg);
	    return;
	}

	free (temp_arg);
    }

    /* There is no argument on this option. */
    process_option (opt, arg, (char *) 0);
}

/*------------- set options from the argument strings -----------------*/

static void set_parameters (struct _options *opt, int argc, const char **argv)
{
    opt->pwtest = (char *) 0;

    while (argc-- > 0) {
	set_option (opt, *argv++);
    }

    if (opt->pwtest == (char *) 0) {
        opt->pwtest = strdup (PWTESTFILE);
    }
}

/*------------- cleanup from the initial processing -----------------*/

void plus_cleanup (struct _options *opt)
{
    if (opt->pwtest != (char *) 0) {
        free (opt->pwtest);
	opt->pwtest = (char *) 0;
    }
}

/*------------------ password validation -------------------------*/

int _pam_passwd_chauthtok (pam_handle_t *pamh,
			   int flags,
			   int argc,
			   const char **argv)
{
    int           retval;
    struct _options opt;
/*
 * Ignore the preliminary tests
 */
    memset (&opt, 0, sizeof (opt));
    opt.pamh  = pamh;
    opt.flags = flags;

    set_parameters (&opt, argc, argv);

    if (flags & PAM_PRELIM_CHECK) {           /* first call */
        if (opt.ctrl & PAM_ST_PRELIM)
	    return PAM_TRY_AGAIN;

	return PAM_SUCCESS;
    }

    if (!(flags & PAM_UPDATE_AUTHTOK)) {  /* second call */
        _pam_log_error ("pam_sm_chauthtok: this must be a Linux-PAM error");
	return PAM_SYSTEM_ERR;
    }
/*
 * Fetch all of the parameters
 */
    retval = pam_get_user (opt.pamh, &opt.user, NULL);

    if (retval == PAM_SUCCESS) {
        retval = pam_get_passwdold (&opt, &opt.password_old, NULL);
	if (retval == PAM_SUCCESS && opt.password_old == (char *) 0)
	    retval = PAM_AUTHTOK_RECOVER_ERR;
    }

    if (retval == PAM_SUCCESS)
        retval = pam_set_item (opt.pamh, PAM_OLDAUTHTOK,
			       (void *) opt.password_old);

    retval = pam_get_passwd (&opt, &opt.password, NULL);
    if (retval == PAM_SUCCESS && opt.password == (char *) 0)
        retval = PAM_AUTHTOK_ERR;
    
    if (retval == PAM_SUCCESS)
        retval = pam_set_item (opt.pamh, PAM_AUTHTOK, (void *) opt.password);
/*
 * Check the password at this time.
 */
    if (retval == PAM_SUCCESS) {
        opt.pwd = getpwnam(opt.user);
	if (opt.pwd == NULL)
	    retval = PAM_USER_UNKNOWN;
    }

    if (retval == PAM_SUCCESS) {
        if (verify_password (&opt) == 0) {
	    retval = PAM_AUTHTOK_ERR;
	}
    }

    plus_cleanup(&opt);
    return retval;
}

/*------------------ linkage routine -------------------------*/

PAM_EXTERN
int pam_sm_chauthtok(	pam_handle_t *pamh, 
			int flags,
			int argc, 
			const char **argv)
{
	return (_pam_passwd_chauthtok (pamh, flags, argc, (const char **) argv));
}


/* static module data */
#ifdef PAM_STATIC
struct pam_module _pam_passwdPLUS_modstruct = {
    "pam_passwdPLUS",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    pam_sm_chauthtok
};
#endif
