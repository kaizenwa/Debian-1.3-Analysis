/* pam_cracklib module */

/*
 * 0.8. tidied output and improved D(()) usage for debugging.
 * 0.7. added support for more obscure checks for new passwd.
 * 0.6. root can reset user passwd to any values (it's only warned)
 * 0.5. supports retries - 'retry=N' argument
 * 0.4. added argument 'type=XXX' for 'New XXX password' prompt
 * 0.3. Added argument 'debug'
 * 0.2. new password is feeded to cracklib for verify after typed once
 * 0.1. First release
 */
/*
 * Written by Cristian Gafton <gafton@sorosis.ro> 1996/09/10
 * See the end of the file for Copyright Information
 */

#include <stdio.h>
#define __USE_BSD
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>

extern char *FascistCheck(char *pw, const char *dictpath);

#ifndef CRACKLIB_DICTPATH
#define CRACKLIB_DICTPATH "/usr/lib/cracklib_dict"
#endif

#define PROMPT1 "New %s password: "
#define PROMPT2 "Retype new %s password: "
#define MISTYPED_PASS "Sorry, passwords do not match"

/*
 * here, we make a definition for the externally accessible function
 * in this file (this definition is required for static a module
 * but strongly encouraged generally) it is used to instruct the
 * modules include file to define the function prototypes.
 */

#define PAM_SM_PASSWORD

#include <security/pam_modules.h>
#include <security/_pam_macros.h>

#ifndef LINUX_PAM 
#include <security/pam_appl.h>
#endif  /* LINUX_PAM */

/* some syslogging */

static void _pam_log(int err, const char *format, ...)
{
    va_list args;

    va_start(args, format);
    openlog("PAM-Cracklib", LOG_CONS|LOG_PID, LOG_AUTH);
    vsyslog(err, format, args);
    va_end(args);
    closelog();
}

/* argument parsing */
#define PAM_DEBUG_ARG       0x0001

/* module data */
static int retry_times = 0;
static char prompt_type[BUFSIZ];

static int _pam_parse(int argc, const char **argv)
{
     int ctrl=0;

     /* step through arguments */
     for (ctrl=0; argc-- > 0; ++argv) {

          /* generic options */

          if (!strcmp(*argv,"debug"))
               ctrl |= PAM_DEBUG_ARG;
          else if (!strncmp(*argv,"type=",5))
               strcpy(prompt_type, *argv+5);
          else if (!strncmp(*argv,"retry=",6)) {
               char *ep = NULL;
               retry_times = strtol(*argv+6,&ep,10);
               if (!ep || (retry_times < 1))
                    retry_times = 1;
          } else {
               _pam_log(LOG_ERR,"pam_parse: unknown option; %s",*argv);
          }
     }

     return ctrl;
}

/* Helper functions */

/* this is a front-end for module-application conversations */
static int converse(pam_handle_t *pamh, int ctrl, int nargs,
                    struct pam_message **message,
                    struct pam_response **response)
{
    int retval;
    struct pam_conv *conv;

    retval = pam_get_item(pamh, PAM_CONV, (const void **) &conv); 

    if ( retval == PAM_SUCCESS ) {
        retval = conv->conv(nargs, (const struct pam_message **)message,
			                response, conv->appdata_ptr);
        if (retval != PAM_SUCCESS && (ctrl && PAM_DEBUG_ARG)) {
            _pam_log(LOG_DEBUG, "conversation failure [%s]",
                                 pam_strerror(retval));
        }
    } else {
        _pam_log(LOG_ERR, "couldn't obtain coversation function [%s]",
                pam_strerror(retval));
    }

    return retval;                  /* propagate error status */
}

static int make_remark(pam_handle_t *pamh, unsigned int ctrl,
                       int type, const char *text)
{
    struct pam_message *pmsg[1], msg[1];
    struct pam_response *resp;
	  
    pmsg[0] = &msg[0];
    msg[0].msg = text;
    msg[0].msg_style = type;
    resp = NULL;

    return converse(pamh, ctrl, 1, pmsg, &resp);
    return PAM_SUCCESS;
}

/* use this to free strings. ESPECIALLY password strings */
static char *_pam_delete(register char *xx)
{
    _pam_overwrite(xx);
    free(xx);
    return NULL;
}

static void _cleanup(pam_handle_t *pamh, void *x, int error_status)
{
    x = _pam_delete( (char *) x );
}

/*
 * Safe duplication of character strings. "Paranoid"; don't leave
 * evidence of old token around for later stack analysis.
 */
static char *xstrdup(const char *x)
{
    register char *new=NULL;

    if (x != NULL) {
        register int i;

        for (i=0; x[i]; ++i);                       /* length of string */
        if ((new = malloc(++i)) == NULL) {
            i = 0;
            _pam_log(LOG_CRIT, "out of memory in xstrdup");
        } else {
            while (i-- > 0) {
                new[i] = x[i];
            }
        }
        x = NULL;
    }
    return new;                 /* return the duplicate or NULL on error */
}

/*
 * can't be a palindrome - like `R A D A R' or `M A D A M'
 */
static int palindrome(const char *old, const char *new)
{
    int	i, j;

	i = strlen (new);

	for (j = 0;j < i;j++)
		if (new[i - j - 1] != new[j])
			return 0;

	return 1;
}

/*
 * more than half of the characters are different ones.
 */
static int similiar(const char *old, const char *new)
{
	int	i, j;

	for (i = j = 0;new[i] && old[i];i++)
		if (strchr (new, old[i]))
			j++;

	if (i >= j * 2)
		return 0;

	return 1;
}

/*
 * a nice mix of characters.
 */
static int simple(const char *old, const char *new)
{
	int	digits = 0;
	int	uppers = 0;
	int	lowers = 0;
	int	others = 0;
	int	size;
	int	i;

	for (i = 0;new[i];i++) {
		if (isdigit (new[i]))
			digits++;
		else if (isupper (new[i]))
			uppers++;
		else if (islower (new[i]))
			lowers++;
		else
			others++;
	}

	/*
	 * The scam is this - a password of only one character type
	 * must be 8 letters long.  Two types, 7, and so on.
	 */

	size = 9;
	if (digits) size--;
	if (uppers) size--;
	if (lowers) size--;
	if (others) size--;

	if (size <= i)
		return 0;

	return 1;
}

static char * str_lower(char *string)
{
	char *cp;

	for (cp = string; *cp; cp++)
		*cp = tolower(*cp);
	return string;
}

static const char * password_check(const char *old, const char *new)
{
	const char *msg = NULL;
	char *oldmono, *newmono, *wrapped;

	if (strcmp(new, old) == 0) {
        msg = "is the same as the old one";
        return msg;
    }

	newmono = str_lower(xstrdup(new));
	oldmono = str_lower(xstrdup(old));
	wrapped = malloc(strlen(oldmono) * 2 + 1);
	strcpy (wrapped, oldmono);
	strcat (wrapped, oldmono);

	if (palindrome(oldmono, newmono))
		msg = "is a palindrome";

	if (!msg && strcmp(oldmono, newmono) == 0)
		msg = "case changes only";

	if (!msg && similiar(oldmono, newmono))
		msg = "is too similiar with the old one";

	if (!msg && simple(old, new))
		msg = "is too simple";

	if (!msg && strstr(wrapped, newmono))
		msg = "is rotated";

	bzero(newmono, strlen(newmono));
	bzero(oldmono, strlen(oldmono));
	bzero(wrapped, strlen(wrapped));
	free(newmono);
	free(oldmono);
	free(wrapped);

	return msg;
}


static int _pam_unix_approve_pass(pam_handle_t *pamh,
                                  unsigned int ctrl,
                                  const char *pass_old,
                                  const char *pass_new)
{
    const char *msg = NULL;
    
    if (pass_new == NULL || (pass_old && !strcmp(pass_old,pass_new))) {
        if (ctrl && PAM_DEBUG_ARG)
            _pam_log(LOG_DEBUG, "bad authentication token");
        make_remark(pamh, ctrl, PAM_ERROR_MSG,
                    pass_new == NULL ?
                    "No password supplied":"Password unchanged" );
        return PAM_AUTHTOK_ERR;
    }

    /*
     * if one wanted to hardwire authentication token strength
     * checking this would be the place
     */
    msg = password_check(pass_old,pass_new);
    if (msg) {
        char remark[BUFSIZ];
        
        bzero(remark,sizeof(remark));
        sprintf(remark,"BAD PASSWORD: %s",msg);
        if (ctrl && PAM_DEBUG_ARG)
            _pam_log(LOG_NOTICE, "new passwd fails strength check: %s",
                                  msg);
        make_remark(pamh, ctrl, PAM_ERROR_MSG, remark);
        return PAM_AUTHTOK_ERR;
    };   
    return PAM_SUCCESS;
    
}

/* The Main Thing (by Cristian Gafton, CEO at this module :-) 
 * (stolen from http://home.netscape.com)
 */
PAM_EXTERN int pam_sm_chauthtok(pam_handle_t *pamh, int flags,
				int argc, const char **argv)
{
    unsigned int ctrl;
    
    retry_times = 1;
    bzero(prompt_type,sizeof(prompt_type));
    ctrl = _pam_parse(argc, argv);

    D(("called."));
    if (!prompt_type[0])
        strcpy(prompt_type,"UNIX");

    if (flags & PAM_PRELIM_CHECK) {
        /* Check for passwd dictionary */       
        struct stat st;
        char buf[sizeof(CRACKLIB_DICTPATH)+10];

	D(("prelim check"));

        bzero(buf,sizeof(buf)); /* zero the buffer */
        sprintf(buf,"%s.pwd",CRACKLIB_DICTPATH);

        if (!stat(buf,&st) && st.st_size)
            return PAM_SUCCESS;
        else {
            if (ctrl & PAM_DEBUG_ARG)
                _pam_log(LOG_NOTICE,"dict path '%s'[.pwd] is invalid",
                                     CRACKLIB_DICTPATH);
            return PAM_ABORT;
        }
        
        /* Not reached */
        return PAM_SERVICE_ERR;

    } else if (flags & PAM_UPDATE_AUTHTOK) {
        int retval;
        char *token1, *token2, *oldtoken;
        const char *item;
        struct pam_message msg[1],*pmsg[1];
        struct pam_response *resp;
        const char *cracklib_dictpath = CRACKLIB_DICTPATH;
        char prompt[BUFSIZ];

	D(("do update"));
        retval = pam_get_item(pamh, PAM_OLDAUTHTOK,
                              (const void **)&oldtoken);
        if (retval != PAM_SUCCESS) {
            if (ctrl & PAM_DEBUG_ARG)
                _pam_log(LOG_ERR,"Can not get old passwd");
            oldtoken=NULL;
            retval = PAM_SUCCESS;
        }

        do {        
        /*
         * make sure nothing inappropriate gets returned
         */
        token1 = token2 = NULL;
        
        if (!retry_times) {
	    D(("returning %s because maxtries reached",
	       pam_strerror(retval)));
            return retval;
	}

        /* Planned modus operandi:
         * Get a passwd.
         * Verify it against cracklib.
         * If okay get it a second time. 
         * Check to be the same with the first one.
         * set PAM_AUTHTOK and return
         */

        /* Prepare to ask the user for the first time */
        bzero(prompt,sizeof(prompt));
        sprintf(prompt,PROMPT1,prompt_type);
        pmsg[0] = &msg[0];
        msg[0].msg_style = PAM_PROMPT_ECHO_OFF;
        msg[0].msg = prompt;

        resp = NULL;
        retval = converse(pamh, ctrl, 1, pmsg, &resp);
        if (resp != NULL) {
            /* interpret the response */
            if (retval == PAM_SUCCESS) {     /* a good conversation */
                token1 = xstrdup(resp[0].resp);
                if (token1 == NULL) {
                    _pam_log(LOG_NOTICE,
                             "could not recover authentication token 1");
                    retval = PAM_AUTHTOK_RECOVER_ERR;
                }
            }
            /*
             * tidy up the conversation (resp_retcode) is ignored
             */
            resp[0].resp = _pam_delete(resp[0].resp);
            free(resp);
        } else {
            retval = (retval == PAM_SUCCESS) ?
                     PAM_AUTHTOK_RECOVER_ERR:retval ;
        }

        if (retval != PAM_SUCCESS) {
            if (ctrl && PAM_DEBUG_ARG)
                _pam_log(LOG_DEBUG,"unable to obtain a password");
            continue;
        }

	D(("testing password, retval = %s", pam_strerror(retval)));
        /* now test this passwd against cracklib */
        {
            char *crack_msg;
            char remark[BUFSIZ];
            
            bzero(remark,sizeof(remark));
	    D(("against cracklib"));
            if ((crack_msg = FascistCheck(token1, cracklib_dictpath))) {
                if (ctrl && PAM_DEBUG_ARG)
                    _pam_log(LOG_DEBUG,"bad password: %s",crack_msg);
                sprintf(remark,"BAD PASSWORD: %s", crack_msg);
                make_remark(pamh, ctrl, PAM_ERROR_MSG, remark);
                if (getuid() || (flags & PAM_CHANGE_EXPIRED_AUTHTOK))
                    retval = PAM_AUTHTOK_ERR;
                else
                    retval = PAM_SUCCESS;
            } else {
                /* check it for strength too... */
		D(("for strength"));
                if (oldtoken) {
                    retval = _pam_unix_approve_pass(pamh,ctrl,
                                               oldtoken,token1);
                    if (retval != PAM_SUCCESS)
                        if (getuid() || (flags & PAM_CHANGE_EXPIRED_AUTHTOK))
			    retval = PAM_AUTHTOK_ERR;
			else
			    retval = PAM_SUCCESS;
                }
            }
        }

	D(("after testing: retval = %s", pam_strerror(retval)));
        /* if cracklib/strength check said it is a bad passwd... */
        if ((retval != PAM_SUCCESS) && (retval != PAM_IGNORE)) {
	    int temp_unused;

	    temp_unused = pam_set_item(pamh, PAM_AUTHTOK, NULL);
            token1 = _pam_delete(token1);
            continue;
        }

        /* Now we have a good passwd. Ask for it once again */
        
        bzero(prompt,sizeof(prompt));
        sprintf(prompt,PROMPT2,prompt_type);
        pmsg[0] = &msg[0];
        msg[0].msg_style = PAM_PROMPT_ECHO_OFF;
        msg[0].msg = prompt;

        resp = NULL;
        retval = converse(pamh, ctrl, 1, pmsg, &resp);
        if (resp != NULL) {
            /* interpret the response */
            if (retval == PAM_SUCCESS) {     /* a good conversation */
                token2 = xstrdup(resp[0].resp);
                if (token2 == NULL) {
                    _pam_log(LOG_NOTICE,
                             "could not recover authentication token 2");
                    retval = PAM_AUTHTOK_RECOVER_ERR;
                }
            }
            /*
             * tidy up the conversation (resp_retcode) is ignored
             */
            resp[0].resp = _pam_delete(resp[0].resp);
            free(resp);
        } else {
            retval = (retval == PAM_SUCCESS) ?
                     PAM_AUTHTOK_RECOVER_ERR:retval ;
        }

        if (retval != PAM_SUCCESS) {
            if (ctrl && PAM_DEBUG_ARG)
                _pam_log(LOG_DEBUG
			 ,"unable to obtain the password a second time");
            continue;
        }

        /* Hopefully now token1 and token2 the same password ... */
        if (strcmp(token1,token2) != 0) {
            /* tell the user */
            make_remark(pamh, ctrl, PAM_ERROR_MSG, MISTYPED_PASS);
            token1 = _pam_delete(token1);
            token2 = _pam_delete(token2);
            pam_set_item(pamh, PAM_AUTHTOK, NULL);
            if (ctrl & PAM_DEBUG_ARG)
                _pam_log(LOG_NOTICE,"Password mistyped");
            retval = PAM_AUTHTOK_RECOVER_ERR;
            continue;
        }
        
        /* Yes, the password was typed correct twice
         * we store this password as an item
         */

        retval = pam_set_item(pamh, PAM_AUTHTOK, token1);
        /* clean it up */
        token1 = _pam_delete(token1);
        token2 = _pam_delete(token2);
        if (
            (retval != PAM_SUCCESS) ||
            (
             (
                retval = pam_get_item(pamh, PAM_AUTHTOK, (const void **)&item)
             ) != PAM_SUCCESS
            )
           ) {
                _pam_log(LOG_CRIT, "error manipulating password");
                continue;
        }
        item = NULL;                 /* break link to password */
        return PAM_SUCCESS;
        
        } while (retry_times--);

    } else {
        if (ctrl & PAM_DEBUG_ARG)
            _pam_log(LOG_NOTICE, "UNKNOWN flags setting %02X",flags);
        return PAM_SERVICE_ERR;
    }

    /* Not reached */
    return PAM_SERVICE_ERR;                            
}



#ifdef PAM_STATIC
/* static module data */
struct pam_module _pam_cracklib_modstruct = {
     "pam_cracklib",
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     pam_sm_chauthtok
};
#endif

/*
 * Copyright (c) Cristian Gafton <gafton@sorosis.ro>, 1996.
 *                                              All rights reserved
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
 * THIS SOFTWARE IS PROVIDED `AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
