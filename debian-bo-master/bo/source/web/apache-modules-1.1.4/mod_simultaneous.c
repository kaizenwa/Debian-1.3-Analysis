/*-
 * Copyright (c) 1995 The Apache Group. All rights reserved.
 * 
 *
 * Apache httpd license
 * ====================
 * 
 *
 * This is the license for the Apache Server. It covers all the
 * files which come in this distribution, and should never be removed.
 * 
 * The "Apache Group" has based this server, called "Apache", on
 * public domain code distributed under the name "NCSA httpd 1.3".
 * 
 * NCSA httpd 1.3 was placed in the public domain by the National Center 
 * for Supercomputing Applications at the University of Illinois 
 * at Urbana-Champaign.
 * 
 * As requested by NCSA we acknowledge,
 * 
 *  "Portions developed at the National Center for Supercomputing
 *   Applications at the University of Illinois at Urbana-Champaign."
 *
 * Copyright on the sections of code added by the "Apache Group" belong
 * to the "Apache Group" and/or the original authors. The "Apache Group" and
 * authors hereby grant permission for their code, along with the
 * public domain NCSA code, to be distributed under the "Apache" name.
 * 
 * Reuse of "Apache Group" code outside of the Apache distribution should
 * be acknowledged with the following quoted text, to be included with any new
 * work;
 * 
 * "Portions developed by the "Apache Group", taken with permission 
 *  from the Apache Server   http://www.apache.org/apache/   "
 *
 *
 * Permission is hereby granted to anyone to redistribute Apache under
 * the "Apache" name. We do not grant permission for the resale of Apache, but
 * we do grant permission for vendors to bundle Apache free with other software,
 * or to charge a reasonable price for redistribution, provided it is made
 * clear that Apache is free. Permission is also granted for vendors to 
 * sell support for Apache. We explicitly forbid the redistribution of 
 * Apache under any other name.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 */


/*
 * mod_simultaneous.c
 * 
 * Michael Davon 10/95 (davon@web-depot.com)
 * 
 * Limit the number of simultaneous accesses to files in a
 * particular directory.  Allow servers to wait for access.
 *
 * Using this module will increase the load on the server, since
 * it will do a fair amount of file access to manage the locks.
 * However, this module is quite effective at limiting access.
 *
 * This is particularly useful if you only have so much bandwidth
 * on your network connection, and don't want it all sucked up
 * by people looking at pictures.
 *
 * Note that when servers are waiting for access, they wake up
 * once per second, and try again for access.  There is no queue, so
 * whichever waiting process wakes up first (when access is available)
 * gets the access.  Queuing would be nice, but is hard to implement.
 *
 * An effective strategy to limit bandwidth consumption may be 
 * as follows:
 *
 *  SimultaneousAccesses 	1
 *  SimultaneousWait		4000
 *  SimultaneousValidate	10
 *
 * This will have the effect of allowing only 1 access to a directory at a
 * time, however, it will keep the server waiting until it finally does
 * get access (up to 4000 seconds), and it will validate the locks every
 * 10 seconds, just to make sure the locking process is still around.
 *
 * Setting "SimultaneousAccesses 0" allows unchecked access.
 *
 * Setting "SimultaneousWait 0" will not wait, and will timeout
 * immediately if there is no slot available.
 *
 * Setting "SimultaneousValidate 0" will never validate the slots.
 *
 * Setting "SimultaneousValidate 1" will validate the slots every second.
 */

#include "httpd.h"
#include "http_config.h"
#include "http_core.h"
#include "http_log.h"
#include "http_protocol.h"

#define SIMUL_MAX	256	     	/* max simultaneous accesses */
#define SIMUL_NAME	".simultaneous"	/* name of lock file */


/* Defaults for SimultaneousWait, SimultaneousValidate */
#define SIMUL_DEF_WAIT		30
#define SIMUL_DEF_VALIDATE	15


/* Error code to return on access timeout */
#define SIMUL_TIMEOUT	SERVICE_UNAVAILABLE

/* declare this module */
module simultaneous_module;

typedef struct simultaneous_struct {
    int 	simul_max;
    int		simul_wait;
    int		simul_validate;
} simultaneous_rec;

static void 		*create_simultaneous(pool *p, char *d)
{
    simultaneous_rec *sr =
	(simultaneous_rec *)pcalloc (p, sizeof(simultaneous_rec));

    sr->simul_max = 0;
    sr->simul_wait = SIMUL_DEF_WAIT;
    sr->simul_validate = SIMUL_DEF_VALIDATE;  

    return (void *)sr;
}

static char 	       	*set_accesses(cmd_parms *cmd,
				      void *srp,
				      char *arg)
{
    simultaneous_rec *sr = (simultaneous_rec *)srp;

    if(arg && *arg)
	sr->simul_max = atoi (arg);
    
    if(sr->simul_max < 0)
	sr->simul_max = 0;

    return NULL;
}

static char 		*set_wait(cmd_parms *cmd,
				  void *srp,
				  char *arg)
{
    simultaneous_rec *sr = (simultaneous_rec *)srp;

    if(arg && *arg)
	sr->simul_wait = atoi (arg);

    if(sr->simul_wait < 0)
	sr->simul_wait = 0;

    return NULL;
}

static char 		*set_validate(cmd_parms *cmd,
				      void *srp,
				      char *arg)
{
    simultaneous_rec *sr = (simultaneous_rec *)srp;

    if(arg && *arg)
	sr->simul_validate = atoi (arg);

    if(sr->simul_validate < 0)
	sr->simul_validate = 0;

    return NULL;
}

static command_rec simul_cmds[] = {
{ "SimultaneousAccesses", set_accesses, NULL, OR_AUTHCFG, TAKE1, 
      "max simultaneous accesses to this directory" },
{ "SimultaneousWait", set_wait, NULL, OR_AUTHCFG, TAKE1, 
      "time (seconds) to wait for access" },
{ "SimultaneousValidate", set_validate, NULL, OR_AUTHCFG, TAKE1, 
      "seconds between periodic validation of used slots" },
{ NULL }
};

static void 	    	mutex_on(int lockf)
{
    static struct flock lock_it = { F_WRLCK, 0, 0, 0 };

    int ret;
    
    while ((ret = fcntl(lockf,F_SETLKW, &lock_it)) < 0
	   && errno == EINTR)
	continue;

    if (ret < 0) {
	fprintf(stderr, "Unknown failure grabbing accept lock.  Exiting!");
	exit(-1);
    }
}

static void 		mutex_off(int lockf)
{
    static struct flock unlock_it = { F_UNLCK, 0, 0, 0 };

    fcntl(lockf, F_SETLKW, &unlock_it);
}


static int		handle_locking(int lockf, 	/* fd to use */
				       int max,		/* num of locks */
				       int release,	/* release the lock? */
				       int validate)	/* validate pids? */
/*
 * return 0 if successful
 */
{
    int 		pid = getpid();
    int			pid_array[SIMUL_MAX];
    int			did_it = 0;
    int			status;
    int			i;

    if(max > (sizeof(pid_array)/sizeof(pid_array[0])))
	max = (sizeof(pid_array)/sizeof(pid_array[0]));

    memset(pid_array, '\0', sizeof(pid_array));

    /* Rewind File */
    lseek(lockf, 0L, 0);

    /* get exclusive access to the file */
    mutex_on(lockf);

    /* Read the PIDs from the file */
    for(i = 0; i < max; i++) {
	status = read(lockf, &pid_array[i], sizeof(pid_array[0]));
	if(status == sizeof(pid_array[0])) { /* Got one */
	    continue;
	}
	if(status == 0) /* EOF */
	    break;
	mutex_off(lockf);
	perror("error reading lock file");
	return (-1);
    }

    if(validate) {
	/* Validate all slots */
	for(i = 0; i < max; i++) {
	    if(pid_array[i] && kill(pid_array[i], 0)) { /* No such process */
		pid_array[i] = 0;
	    }
	}
    }

    if(release) {
	/* Find this PID and release it */
	for(i = 0; i < max; i++) {
	    if(pid_array[i] == pid) {
		pid_array[i] = 0;
		did_it = 1;
		goto record_and_exit;
	    }
	}
    }

    if(!release) {

	/* Check for this process already listed */
	for(i = 0; i < max; i++) {
	    if(pid_array[i] == pid) {
		did_it = 1;
		goto record_and_exit;
	    }
	}

	/* Find empty slot and fill it */
	for(i = 0; i < max; i++) {
	    if(!pid_array[i]) {
		pid_array[i] = pid;
		did_it = 1;
		goto record_and_exit;
	    }
	}
    }

 record_and_exit:

    /* Rewind File */
    lseek(lockf, 0L, 0);
	
    /* Write the PIDs */
    status = write(lockf, pid_array, sizeof(pid_array[0]) * max);

    /* release the lock on the file */
    mutex_off(lockf);

    if(did_it)
	return 0;
    else
	return 1;
}


static int 		get_lockfile_for_request(request_rec *r, char **namep)
{
    int			lock_file;
    static char		lock_file_name[256];
    char		*cp;

    if(namep)
	*namep = lock_file_name;

    if(r->filename && *r->filename) {
#if DEBUG
	fprintf(stderr, "file to lock '%s'\n", r->filename);
#endif
    }
    else {
	return (-1);
    }

    strcpy(lock_file_name, r->filename);

    if (S_ISDIR(r->finfo.st_mode)) {
	strcat(lock_file_name, "/");
    }
    else {
	if(!(cp = strrchr(lock_file_name, '/')))
	    return (-1);

	cp[1] = 0;
    }

    strcat(lock_file_name, SIMUL_NAME);

#if DEBUG
    fprintf(stderr, "lockfile is '%s'\n", lock_file_name);
#endif

    if((lock_file = open(lock_file_name, O_RDWR|O_CREAT, 0660)) < 0) {
	fprintf(stderr, "can't open datafile %s\n", lock_file_name);
	perror("error");
	return (-1);
    }

    return lock_file;
}


static int 		get_access (request_rec *r)
/*
 * This function always returns DECLINED so that other handlers
 * will be run, except when it fails to get access.  If it fails
 * to get access, it will return SIMUL_TIMEOUT.
 */
{
    int			lock_file;
    char		*lock_file_name;
    char 		ebuff[256];
    int			i;
    simultaneous_rec 	*srec =
	(simultaneous_rec *)get_module_config(r->per_dir_config,
					      &simultaneous_module);

    if(!srec || !srec->simul_max)
	return DECLINED;
	
    lock_file = get_lockfile_for_request(r, &lock_file_name);
    if(lock_file < 0) {
	sprintf(ebuff, "could not get lockfile for %s", lock_file_name);
	log_error(ebuff, r->server);
	return DECLINED;
    }

    for (i = 0; i <= srec->simul_wait; i++) {

        int validate = 0;

	if(srec->simul_validate) {
	    if ((i + 1) % srec->simul_validate ==  0)
		validate = 1;
	}

	if(handle_locking(lock_file, srec->simul_max, 0, validate) == 0) {
	    /* We got a slot */
	    close(lock_file);
	    return DECLINED;
	}

	if(i >= srec->simul_wait)
	    break;
#if DEBUG
	sprintf(ebuff, "waiting for access to %s", lock_file_name);
	log_error(ebuff, r->server);
#endif
	sleep(1);
    }

    close(lock_file);
    log_reason("too many simultaneous users", r->filename, r);

    return SIMUL_TIMEOUT;
}


static int 		release_access (request_rec *r)
/*
 * This function always returns DECLINED so that other handlers
 * will be run.
 */
{
    int			lock_file;
    simultaneous_rec *srec =
      (simultaneous_rec *)get_module_config (r->per_dir_config,
					     &simultaneous_module);

    if(!srec || !srec->simul_max)
	return DECLINED;

    lock_file = get_lockfile_for_request(r, NULL);
    if(lock_file < 0)
	return DECLINED;

    (void) handle_locking(lock_file, srec->simul_max, 1, 0);
    close(lock_file);

    return DECLINED;
}
    

module simultaneous_module = {
   STANDARD_MODULE_STUFF,
   NULL,			/* initializer */
   create_simultaneous,		/* dir config creater */
   NULL,			/* dir merger --- default is to override */
   NULL,			/* server config */
   NULL,			/* merge server config */
   simul_cmds,			/* command table */
   NULL,			/* handlers */
   NULL,			/* filename translation */
   NULL,		        /* check_user_id */
   NULL,			/* check auth */
   get_access,			/* check access */
   NULL,			/* type_checker */
   NULL,			/* fixups */
   release_access,		/* logger */
};
