/*  VER 056   TAB P   $Id: lock.c,v 1.18 1996/11/22 12:31:52 src Exp $
 *
 *  lock file handling 
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 *
 *  assuming "spool" is the name of the outgoing spool,
 *  the following conventions apply:
 *
 *  for Cnews:
 *	outgoing lock:	NEWSCTL/LOCKbatch
 *	incoming lock:	NEWSCTL/LOCKinput
 *	spool file:	NEWSSPOOL/out.going/spool/togo
 *	spool lock:	NEWSSPOOL/out.going/spool/LOCKb
 *
 *  for Inn:
 *	outgoing lock:	LOCKS/LOCK.sendbatch
 *	spool file:	NEWSSPOOL/out.going/spool
 *	spool lock:	LOCKS/LOCK.spool
 *
 *	LOCKS is usually the same as NEWSCTL
 *	BUG: We are ignoring LOCK_STYLE
 *	BUG: What about: shlock -p $$ -f lockfile
 *
 *  we don't need the general lock
 */

#include "common.h"
#include "proto.h"

#include <sys/stat.h>
#include <signal.h>

/*
 *  local
 */
static int force_unlock(char *name,int sig);

/*
 *  locking state
 */
static char name_temp[PATH_MAX];
static char name_lock[PATH_MAX];

static int lock_active = 0;
#define LOCK_TEMP 1
#define LOCK_LOCK 2

/*
 *  perform a lock
 */
void lock(
    char *dir_name, /* empty if in main lock */
    char *lock_name)
{
    long t = 0L;
    long locktime = timeout ? timeout : LOCKTIME;
    int pid;
    char pid_buf[100];
    int sig = SIGUSR1; /* start as gentle as possible */
    FILE *f;

    pid = getpid();
    sprintf(pid_buf,"%d",pid);

    /* just in case we've left any locks ourselves */
    unlock();

    /* BUG: .. umask $NEWSUMASK */

    /* make the filenames */
    if (dir_name && dir_name[0]) {
	/* path specified */
	build_filename(name_lock,dir_name,"/",lock_name,NULL);
	build_filename(name_temp,name_lock,".",pid_buf,NULL);
    } else {
	/* common lock directory */
	build_filename(name_lock,NEWSCTL,"/",lock_name,NULL);
	/* BUG: the Cnews convention is used for the temp_name */
	build_filename(name_temp,NEWSCTL,"/L.",pid_buf,NULL);
    }

    /* build our own little file */
    if (!(f = fopen(name_temp,"w"))) {
	log_msg(L_ERRno,"cannot create lock '%s'", name_temp);
	unlock_exit(5);
    }
    lock_active |= LOCK_TEMP;
    fprintf(f,"%s\n",pid_buf);
    if (fclose(f) == EOF) {
	log_msg(L_ERRno,"problems creating lock '%s'", name_temp);
	unlock_exit(5);
    }
    /* perform the locking action */
    for (;;) {
	/* BUG: check if name_temp has disappeared... */
	if (link(name_temp,name_lock) >= 0) {
	    /* locked successfully */
	    lock_active |= LOCK_LOCK;
	    log_msg(L_DEBUG3,"created lock: %s",name_lock);
	    break;
	} 

	/* failed, tried force? */
	if (sig == SIGKILL) {
	    log_msg(L_ERR,"can't lock '%s' after forced unlock", name_lock);
	    unlock_exit(5);
	}

	if (t==0L) {
	    log_msg(L_INFO,"awaiting lockfile to disappear (max %ld sec)", 
								locktime);
	}

	/* don't wait forever for access */
	if (sig == SIGUSR1 || (t += LOCKDELTA) > locktime) {
	    if (sig == SIGUSR1)
		log_msg(L_DEBUG,"send USR1 to see if process exists");
	    else 
		log_msg(L_ERR,"file lock '%s' timed out", name_lock);

	    /* file remains locked, what shall we do? */
	    if (force_unlock(name_lock,sig)) {
		/* we've probably managed to remove the lock by force */
		sig = (sig==SIGUSR1) ? SIGTERM : SIGKILL;
	    } else {
		/* give up */
		unlock_exit(5);
	    }
	}
	sleep(LOCKDELTA);
    }
}

/*
 *  lock failed, try to force a unlock
 *  return true on success
 *  BUG: we really should have checked that it is the 
 *	 same process that has been locking all the time
 */
static int force_unlock(char *name,int sig)
{
#ifdef NO_FORCE
    log_msg(L_INFO,"unlock by force permanently disabled");
    return 0;
#else
    char name_other[PATH_MAX];
    char *p;
    char pid_buf[100];
    int other_pid = -1;
    struct stat st;
    FILE *f;

    if (noforce_flag) {
	log_msg(L_INFO,"no attempt at unlocking by force");
	return 0;
    }

    /* read the file to find the pid */
    if (!(f = fopen(name,"r"))) {
	log_msg(L_ERRno,"lockfile '%s' unreadable", name);
	/* assume file has disappeared */
	return 1;
    }
    pid_buf[0] = '\0';
    fgets(pid_buf,sizeof(pid_buf),f);
    fclose(f);

    for (p=pid_buf; isspace(*p); ++p)
	;
    if (!isdigit(*p)) {
	/* 
	 *  whatever shall we do? give up?
	 *  we assume the risk, and continue
	 */ 
	log_msg(L_ERRno,"lockfile '%s' has wrong format",
			     name);
	return 0;
    } 
    if ((other_pid = atoi(p)) < 0) {
	/* 
	 *  cannot allow this, so again, we continue
	 */ 
	log_msg(L_ERRno,"lockfile '%s' has negative pid",
			     name);

	return 0;
    } 

    /* contents is plausible */
    if (other_pid == getpid()) {
	/* ourselves? shouldn't happen */
	log_msg(L_ERRno,"file '%s' locked by ourselves?", name);
	other_pid = -1;
    } else {
	/*     
	 *  OK, we know who it is
	 *  we try to execute a pretty sure kill
	 */
	log_msg(L_INFO,"sending process %d signal %d", other_pid, sig);

	if (kill(other_pid,sig) == -1) {
	    if (errno == ESRCH) {
		/* pretty likely scenario */
		log_msg(L_INFO,"process %d does not exist", other_pid);
	    } else {
		/* we probably don't have permission for this */
		log_msg(L_ERRno,"kill stale process %s failed", pid_buf);
		return 0;
	    }
	} else {
	    if (sig==SIGUSR1) {
		log_msg(L_INFO,"process %d exists, wait...", other_pid);
		return 1;
	    }
	}
	/* give the process time to clean up */
	sleep(LOCKDELTA);
    }

    /* 
     *	try to remove the stale lockfiles
     *	(if the kill already hasn't done that for us)
     */
    if (unlink(name) == -1) {
	if (stat(name,&st) != -1) {
	    /* file still exists */
	    log_msg(L_ERRno,"cannot remove lockfile '%s'", name);
	}
    }
    
    /* close the other processes private lockfile too */
    if (other_pid != -1) {
	build_filename(name_other,name,".",pid_buf,NULL);
	if (unlink(name_other) == -1) {
	    if (stat(name_other,&st) != -1) {
		/* file still exists */
		log_msg(L_ERRno,"cannot remove file '%s'", name_other);
	    }
	}
    }
    return 1;
#endif
}

/*
 *  remove any locks
 */
void unlock(void)
{
    if (lock_active & LOCK_TEMP) {
	unlink(name_temp);
	lock_active &= ~LOCK_TEMP; 
    }
    if (lock_active & LOCK_LOCK) {
	unlink(name_lock);
	lock_active &= ~LOCK_LOCK; 
	log_msg(L_DEBUG3,"unlocked: %s", name_lock);
    }
}     

/*
 *  build a file name, max length PATH_MAX
 *  argument is NULL-terminated list 
 */
void build_filename(char *where, char *arg1, ...)      
{
    int len = 0;
    int n;
    char **argp;

    for (argp = &arg1; *argp; ++argp) {
	if ((len += (n=strlen(*argp))) >= PATH_MAX) {
	    log_msg(L_ERR,"file name longer than %d chars", PATH_MAX-1);
	    unlock_exit(1);
	}
	strcpy(where+len-n,*argp);
    }
}

/*
 *  perform exit(), but unlock() first
 */
void unlock_exit(int n)
{
    progtitle("exit");
    history_done();
    unlock();
    exit(n);
}

