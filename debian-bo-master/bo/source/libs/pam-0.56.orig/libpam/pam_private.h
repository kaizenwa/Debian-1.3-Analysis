/*
 * pam_private.h
 *
 * $Id: pam_private.h,v 1.11 1997/02/15 16:01:21 morgan Exp $
 *
 * This is the Linux-PAM Library Private Header. It contains things
 * internal to the Linux-PAM library. Things not needed by either an
 * application or module.
 *
 * Please see end of file for copyright.
 *
 * Creator: Marc Ewing.
 * 
 * $Log: pam_private.h,v $
 * Revision 1.11  1997/02/15 16:01:21  morgan
 * update version and #defines for /etc/pam.d/ code
 *
 * Revision 1.10  1997/01/04 20:09:27  morgan
 * update for .55
 * added "faulty module" token
 *
 * Revision 1.9  1996/12/18 04:24:04  morgan
 * logging prior to bad pam.conf entry handling
 *
 * Revision 1.8  1996/12/01 03:14:13  morgan
 * use _pam_macros.h
 */

#ifndef _PAM_PRIVATE_H
#define _PAM_PRIVATE_H

/* this is not used at the moment --- AGM */
#define LIBPAM_VERSION 56

#include <security/pam_appl.h>
#include <security/pam_modules.h>

/* the Linux-PAM configuration file */

#define PAM_CONFIG    "/etc/pam.conf"
#define PAM_CONFIG_D  "/etc/pam.d"
#define PAM_CONFIG_DF "/etc/pam.d/%s"

#define PAM_DEFAULT_SERVICE        "other"     /* lower case */
#define PAM_DEFAULT_SERVICE_FILE   PAM_CONFIG_D "/" PAM_DEFAULT_SERVICE

#ifdef PAM_LOCKING
/*
 * the Linux-PAM lock file. If it exists Linux-PAM will abort. Use it
 * to block access to libpam
 */
#define PAM_LOCK_FILE "/var/lock/subsys/PAM"
#endif

/* components of the pam_handle structure */

struct handler {
    int must_fail;
    int (*func)(pam_handle_t *pamh, int flags, int argc, char **argv);
    int control_flag;
    int argc;
    char **argv;
    struct handler *next;
};

struct loaded_module {
    char *name;
    int type; /* PAM_STATIC_MOD or PAM_DYNAMIC_MOD */
    void *dl_handle;
};

#define PAM_MT_DYNAMIC_MOD 0
#define PAM_MT_STATIC_MOD  1
#define PAM_MT_FAULTY_MOD 2

struct handlers {
    struct handler *authenticate;
    struct handler *setcred;
    struct handler *acct_mgmt;
    struct handler *open_session;
    struct handler *close_session;
    struct handler *chauthtok;
};

struct service {
    struct loaded_module *module; /* Only used for dynamic loading */
    int modules_allocated;
    int modules_used;
    int handlers_loaded;

    struct handlers conf;        /* the configured handlers -- AGM */
    struct handlers other;       /* the default handlers    -- AGM */
};

/*
 * Environment helper functions
 */

#define PAM_ENV_CHUNK         10 /* chunks of memory calloc()'d      *
				  * at once                          */

struct pam_environ {
    int entries;                 /* the number of pointers available */
    int requested;               /* the number of pointers used:     *
				  *     1 <= requested <= entries    */
    char **list;                 /* the environment storage (a list  *
				  * of pointers to malloc() memory)  */
};

#ifdef PAM_FAIL_DELAY_ON

/* warning this code is likely to change. It was added for .50 and is
   a rather crude attempt to provide some time delay support */

#include <sys/time.h>

typedef enum { PAM_FALSE, PAM_TRUE } _pam_boolean;

struct _pam_fail_delay {
     _pam_boolean set;
     unsigned int delay;
     time_t begin;
};

#endif /* PAM_FAIL_DELAY_ON */

struct _pam_handle {
    char *service_name;
    char *user;
    char *prompt;                /* for use by pam_get_user() -- AGM */
    char *tty;
    char *rhost;
    char *ruser;
    char *authtok;
    char *oldauthtok;
    struct pam_conv *pam_conversation;
    struct pam_data *data;
    struct pam_environ *env;      /* structure to maintain environment list */
#ifdef PAM_FAIL_DELAY_ON
    struct _pam_fail_delay fail_delay;   /* helper function for easy delays */
#endif /* PAM_FAIL_DELAY_ON */
    struct service handlers;
};

/* Values for select arg to _pam_dispatch() */
#define PAM_AUTHENTICATE  0
#define PAM_SETCRED       1
#define PAM_ACCOUNT       2
#define PAM_OPEN_SESSION  3
#define PAM_CLOSE_SESSION 4
#define PAM_CHAUTHTOK     5

/* Values for control_flag in struct handler */
#define PAM_F_REQUIRED    0
#define PAM_F_SUFFICIENT  1
#define PAM_F_OPTIONAL    2
#define PAM_F_CRITICAL    3

/*
 * internally defined functions --- these should not be directly
 * called by applications or modules
 */
int _pam_dispatch(pam_handle_t *pamh, int flags, int choice);

/* Free various allocated structures and dlclose() the libs */
int _pam_free_handlers(pam_handle_t *pamh);

/* Parse config file, allocate handler structures, dlopen() */
int _pam_init_handlers(pam_handle_t *pamh);

/* Set all hander stuff to 0/NULL - called once from pam_start() */
void _pam_start_handlers(pam_handle_t *pamh);

/* environment helper functions */

/* create the environment structure */
int _pam_make_env(pam_handle_t *pamh);

/* delete the environment structure */
void _pam_drop_env(pam_handle_t *pamh);

#ifdef LINUX_PAM

/* these functions deal with failure delays as required by the
   authentication modules and application. Their *interface* is likely
   to remain the same although their function is hopefully going to
   improve */

/* reset the timer to no-delay */
void _pam_reset_timer(pam_handle_t *pamh);

/* this sets the clock ticking */
void _pam_start_timer(pam_handle_t *pamh);

/* this waits for the clock to stop ticking if status != PAM_SUCCESS */
void _pam_await_timer(pam_handle_t *pamh, int status);


#endif /* LINUX_PAM */

#ifdef PAM_STATIC

/* The next two in ../modules/_pam_static/pam_static.c */

/* Return pointer to data structure used to define a static module */
struct pam_module * _pam_open_static_handler(char *path);

/* Return pointer to function requested from static module */

typedef void (*voidfunc(void))(void);
voidfunc *_pam_get_static_sym(struct pam_module *mod, const char *symname);

#endif

/* For now we just use a stack and linear search for module data. */
/* If it becomes apparent that there is a lot of data, it should  */
/* changed to either a sorted list or a hash table.               */

struct pam_data {
     char *name;
     void *data;
     void (*cleanup)(pam_handle_t *pamh, void *data, int error_status);
     struct pam_data *next;
};

void _pam_free_data(pam_handle_t *pamh, int status);

int _pam_strCMP(const char *s, const char *t);
char *_pam_StrTok(char *from, const char *format, char **next);

char *_pam_strdup(const char *s);

int _pam_mkargv(char *s, char ***argv, int *argc);

void _pam_log_error(const char *format, ...);

/*
 * XXX - Take care with this. It could confuse the logic of a trailing
 *       else
 */

#define IF_NO_PAMH(X,pamh,ERR) \
if ((pamh) == NULL) { \
    _pam_log_error(X ": NULL pam handle passed"); \
    return ERR; \
}

/* Definition for the default username prompt used by pam_get_user() */

#define PAM_DEFAULT_PROMPT "Please enter username: "

/*
 * include some helpful macros
 */

#include <security/_pam_macros.h>

/*
 * Copyright (C) 1995 by Red Hat Software, Marc Ewing
 * Copyright (c) 1996, Andrew G. Morgan <morgan@parc.power.net>
 *
 * All rights reserved
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
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#endif /* _PAM_PRIVATE_H_ */
