/*
**	uufuncs.c
**
**	functions needed by uugetty
**
*/

#define UUFUNCS
#define UUGETTY

#include "main.h"
#include "uufuncs.h"
#include "debug.h"

/* forward declarations
*/

sig_t cshangup();


/*
** waitlocks
**
** if there are UUCP style lockfiles around, sleep until
** they disappear
*/

void waitlocks()
{
	debug(D_RUN, "checking for lockfiles..."); 

	if(checklock(lock)) {
		while(checklock(lock)) (void) sleep(30);
		exit(0);
	}
	if((altlock) && checklock(altlock)) {
		while(checklock(altlock)) (void) sleep(30);
		exit(0);
	}
} 


/*
** lockline
**
** lock the main (lock) and alternate (altlock) lines
** set to remove locks on a signal
*/

void lockline()
{
	debug(D_RUN, "locking the line");

	if(makelock(lock) == FAIL) exit(0);
	if((altlock) && (makelock(altlock) == FAIL)) exit(0);

	(void) signal(SIGHUP, rmlocks);
	(void) signal(SIGINT, rmlocks);
	(void) signal(SIGQUIT, rmlocks);
	(void) signal(SIGTERM, rmlocks);
}


/*
** makelock
**
** attempt to make a lockfile
** Returns FAIL if lock could not be made (line in use).
*/

int
makelock(name)
char *name;
{
	int fd, pid;
	char *temp, buf[MAXLINE+1];
#ifdef	ASCIIPID
	char apid[16];
#endif	/* ASCIIPID */
	int getpid();
	char *mktemp();

	debug(D_LOCK, "makelock(%s) called", name);

	/* first make a temp file
	 */
	(void) sprintf(buf, LOCK, "TM.XXXXXX");
	if ((fd = creat((temp=mktemp(buf)), 0444)) == FAIL) {
		logerr("create failed on temp lockfile \"%s\": %s",
		       temp, strerror(errno));
		return(FAIL);
	}
	debug(D_LOCK, "temp = (%s)", temp);

	/* put my pid in it
	 */
#ifdef	ASCIIPID
	(void) sprintf(apid, "%09d", getpid());
	(void) write(fd, apid, strlen(apid));
#else
	pid = getpid();
	(void) write(fd, (char *)&pid, sizeof(pid));
#endif	/* ASCIIPID */
	(void) close(fd);

	/* link it to the lock file
	 */
	while (link(temp, name) == FAIL) {
		debug(D_LOCK, "link(temp,name) failed, errno=%d", errno);
		if (errno == EEXIST) {		/* lock file already there */
			if ((pid = readlock(name)) == FAIL)
				continue;
			if ((kill(pid, 0) == FAIL) && errno == ESRCH) {
				/* pid that created lockfile is gone */
				(void) unlink(name);
				continue;
			}
		}
		debug(D_LOCK, "lock NOT made");
		(void) unlink(temp);
		return(FAIL);
	}
	debug(D_LOCK, "lock made");
	(void) unlink(temp);
	return(SUCCESS);
}

/*
** checklock
**
** test for presense of valid lock file
** Returns TRUE if lockfile found, FALSE if not.
*/

boolean
checklock(name)
char *name;
{
	int pid;
	struct stat st;

	debug(D_LOCK, "checklock(%s) called", name);

	if ((stat(name, &st) == FAIL) && errno == ENOENT) {
		debug(D_LOCK, "no lockfile found");
		return(FALSE);
	}

	if ((pid = readlock(name)) == FAIL) {
		debug(D_LOCK, "couldn't read lockfile");
		return(FALSE);
	}

	if (pid == getpid()) {
		debug(D_LOCK, 
		  "lockfile belongs to me... damn race conditions");
		return(FALSE);
	}

	if ((kill(pid, 0) == FAIL) && errno == ESRCH) {
		debug(D_LOCK, "no active process has lock, will remove");
		(void) unlink(name);
		return(FALSE);
	}

	debug(D_LOCK, "active process has lock, return(TRUE)");
	return(TRUE);
}

/*
** readlock
**
** read contents of lockfile
** Returns pid read or FAIL on error.
*/

int
readlock(name)
char *name;
{
	int fd, pid, n=0;
#ifdef	ASCIIPID
	char apid[16];
#endif	/* ASCIIPID */

	if ((fd = open(name, O_RDONLY)) == FAIL)
		return(FAIL);

#ifdef	ASCIIPID
	(void) read(fd, apid, sizeof(apid));
	n = sscanf(apid, "%d", &pid);
#else
	(void) read(fd, (char *)&pid, sizeof(pid));
#endif	/* ASCIIPID */

#ifdef  BOTHPID
	if (n != 1){
		(void) close(fd);
		fd = open(name, O_RDONLY);
		(void) read(fd, (char *)&pid, sizeof(pid));
		}
#endif

	(void) close(fd);
	debug(D_LOCK, "read %d from the lockfile", pid);
	return(pid);
}

/*
** rmlocks
**
** remove lockfile(s)
*/

sig_t
rmlocks()
{
	if (altlock != (char *) NULL)
		(void) unlink(altlock);

	(void) unlink(lock);
}

