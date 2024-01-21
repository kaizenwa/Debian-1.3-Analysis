#include <config.h>

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#ifdef HAVE_DIRENT_H
# include <dirent.h>
#else
# define dirent direct
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#include <string.h>
#include <signal.h>
#include <ctype.h>

#include "suck.h"
#include "suck_config.h"
#include "both.h"
#include "suckutils.h"
#include "phrases.h"

/*------------------------------------------------------------------------*/
/* check if directory exists, if not, try to create it.			  */
/* return TRUE if made/exists and can write to it			  */
/* -----------------------------------------------------------------------*/
int checkdir(const char *dirname) {
	
	struct stat buf;
	int retval = TRUE;

	if(stat(dirname, &buf)== 0) {
		/* it exists */
		if(!S_ISDIR(buf.st_mode)) {
			error_log(ERRLOG_REPORT, sucku_phrases[0], dirname, NULL);
			retval = FALSE;
		}
		else if(access(dirname, W_OK) != 0) {
			error_log(ERRLOG_REPORT, sucku_phrases[1], dirname, NULL);
			retval = FALSE;
		}
	}
	else if(errno == ENOENT) {
		/* doesn't exist, make it */
		if(mkdir(dirname, (S_IRUSR | S_IWUSR | S_IXUSR)) != 0) {
			MyPerror(dirname);
			retval = FALSE;
		}
	}
	else {
		/* huh? */
		MyPerror(dirname);
		retval = FALSE;
	} 
	return retval;	
}
/*--------------------------------------------------------------*/
/* if which = FP_SET, store directory in array 			*/
/* 	    = FP_GET, retrieve directory and add file name 	*/
/* retval     FP_SET, if valid dir, the dir, else NULL		*/
/*	      FP_GET, full path					*/
/*--------------------------------------------------------------*/
const char *full_path(int which, int dir, const char *fname) {

	static char dirs[FP_NR_DIRS][PATH_MAX+1] = { N_TMPDIR, N_DATADIR, N_MSGDIR };
	static char path[PATH_MAX+1];	/* static so area valid after return */
	static char postfix[PATH_MAX+1] = { "\0" };

	char *retptr;
	int ok, i;

	retptr = NULL;

	switch(which) { 
	  case FP_GET:
	  case FP_GET_NOPOSTFIX:
		/* put the right directory as the first part of the path */
		strcpy(path, dirs[dir]);
		/* if it contains a dit, then change to full path */
		if(path[0] == '.') {
			path[0] = '\0';
			if(getcwd(path, PATH_MAX) == NULL) {
					MyPerror(path);
					/* restore current path and pray */
					/* it will work later */
					strcpy(path, dirs[dir]);
			}
			else {
				/* tack on remainder of path */
				/* the 1 so skip . */
				strcat(path, &dirs[dir][1]);
			}		
		}
		if(fname != NULL && fname[0] != '\0') {
			i = strlen(path);
			/* if nothing to put on don't do any of this */
			/* put on trailing slant bar if needed */
			if(path[i-1] != '/') {
				path[i++] = '/';
				path[i] = '\0';	/* null terminate it */
			}
			/* finally, put on filename */
			strcpy(&path[i], fname);

			if(which != FP_GET_NOPOSTFIX && postfix[0] != '\0') {
				strcat(path, postfix);
			}
		}
		retptr = path;
		break;
	  case FP_SET_POSTFIX:
		ok = TRUE;
		strncpy(postfix, fname, sizeof(postfix));
		break;
	  case FP_SET:
		ok = TRUE;
		switch(dir) {
		  case FP_TMPDIR:
			if(access(fname, W_OK) == -1) {
				MyPerror(fname);
				ok = FALSE;
			}
			break;
		  case FP_DATADIR:
			if(access(fname, R_OK) == -1) {
				MyPerror(fname);
				ok = FALSE;
			}
			break;
		  case FP_MSGDIR: /* do_nothing, this is checked elsewhere */
			break;
		}
		if(ok == FALSE) {
			retptr = NULL;
		}
		else {
			strncpy(dirs[dir], fname, sizeof(dirs[0]));
			retptr = dirs[dir];
		}
		break;
	}
	return retptr;
}
#ifdef LOCKFILE
/*--------------------------------------------------------------*/
int do_lockfile(PMaster master) {

	int retval;
	FILE *f_lock;
	const char *lockfile;
	pid_t pid;

	retval = RETVAL_OK;

	lockfile = full_path(FP_GET, FP_TMPDIR, N_LOCKFILE);
	if((f_lock = fopen(lockfile, "r")) != NULL) {
		/* okay, let's try and see if this sucker is truly alive */
		fscanf(f_lock, "%ld", (long *) &pid);
		fclose(f_lock);
		if(pid <= 0) {
			error_log(ERRLOG_REPORT,  sucku_phrases[2], lockfile, NULL);
			retval = RETVAL_ERROR;
		}
		/* this next technique borrowed from pppd, sys-linux.c (ppp2.2.0c) */
		/* if the pid doesn't exist (can't send any signal to it), then try */
		/* to remove the stale PID file so can recreate it. */
		else if(kill(pid, 0) == -1 && errno == ESRCH) {
			/* no pid found */
			if(unlink(lockfile) == 0) {
				/* this isn't error so don't put in error log */
				fprintf(master->msgs, sucku_phrases[3], lockfile, NULL);
			}
			else {
				error_log(ERRLOG_REPORT, sucku_phrases[4], lockfile, NULL);
				retval = RETVAL_ERROR;
			}
		}
		else {
			error_log(ERRLOG_REPORT, sucku_phrases[5], lockfile,  NULL);
			retval = RETVAL_ERROR;
		}
	}
	if(retval == RETVAL_OK) {
		if((f_lock = fopen(lockfile, "w")) != NULL) { 
			fprintf(f_lock, "%ld", (long) getpid());
			fclose(f_lock);
		}
		else {
			error_log(ERRLOG_REPORT, sucku_phrases[6],  lockfile, NULL);
			retval = RETVAL_ERROR;
		}
	}
	return retval;
}
#endif
/*------------------------------------------------------------------------------*/
int cmp_msgid(const char *nr1, const char *nr2) {
	/* cmp two article id numbers */
	/* which is <xxxxxxxxxxxxxxxxx@place.org */
	/* use case sensitive compares up to the @ */
	/* then use case insensitive compares on the organization */
	/* this is to comform to rfc822 */
	int retval = FALSE;

	if(nr1 != NULL && nr2 != NULL) { 
		
		while ( *nr1 == *nr2 && *nr1 != '\0' && *nr1 != '@') {
			nr1++;
			nr2++;			
		}
		if(*nr1 == '@') {
			while( tolower(*nr1) == tolower(*nr2) && *nr1 != '\0') {
				nr1++;
				nr2++;
			}
		}
		if(*nr1 == *nr2 && *nr1 == '\0') {
			retval = TRUE;
		}
	}
	return retval;

}
