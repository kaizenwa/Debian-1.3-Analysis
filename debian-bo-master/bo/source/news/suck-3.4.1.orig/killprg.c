#include <config.h>

#include <stdio.h>
#include <stdlib.h>

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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >>8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) ((stat_val) & 255) == 0)
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

#include "suck_config.h"
#include "both.h"
#include "suck.h"
#include "suckutils.h"
#include "killfile.h"
#include "phrases.h"

#ifdef MYSIGNAL
#include <signal.h>
#endif


/* KILLPRG.C -------------------------------------------------------------*/
/* this file contains all subroutines pertaining to forking a killfile    */
/* program, calling the program for each message and checking the results.*/
/* It also must handle gracefully the child dieing, etc.		  */
/*------------------------------------------------------------------------*/

/* function prototypes */
int find_in_path(char *);

#ifdef MYSIGNAL
RETSIGTYPE pipe_sighandler(int);
#endif

int killprg_forkit(PKillStruct mkill, char *args) {
/*------------------------------------------------------------------------ */
/* take the char string args, parse it to get the program to check to see  */
/* if it exists, then try to fork, execl, and set up pipe to it.  If       */
/* successful, return TRUE, else return FALSE.				   */
/*-------------------------------------------------------------------------*/
	int i, newstdin[2], newstdout[2], retval = FALSE;
	char *prg[KILLPRG_MAXARGS+1], *myargs = NULL, *ptr, *nptr;

	/* initialize the master struct */
	mkill->child.Stdin = mkill->child.Stdout = -1;
	mkill->child.Pid = -1;

	/* first parse args into array */
	i = strlen(args);
	/* do this so can muck around with string with messing up original */
	if((myargs = malloc(i+1)) == NULL) {
		error_log(ERRLOG_REPORT, killp_phrases[0], NULL);
	}
	else {
		strcpy(myargs, args);
		/* strip off nl */
		if(myargs[i-1] == '\n') {
			myargs[i-1] = '\0';
		}
		
		/* build array of pointers to string, by finding spaces and */
		/* replacing them with \0 */ 
		i = 0;
		ptr = myargs;
		do {
			prg[i++] = ptr;
			nptr = strchr(ptr, ' ');
			if(nptr != NULL) {
				*nptr = '\0';
				nptr++;
			}
			ptr = nptr;				
		}
		while(ptr != NULL && i < KILLPRG_MAXARGS);
		prg[i] = NULL;		/* must do this for execv */
	}

#ifdef DEBUG2
	do_debug("Prg = %s, %d args\n", prg[0], i);
#endif
	if(prg[0] != NULL && find_in_path(prg[0]) == TRUE ) { 
		/* okay it exists, fork, execl, etc */
		/* first set up our pipes */
		if(pipe(newstdin) == 0) {
			if(pipe(newstdout) == 0) {
				retval = TRUE;
			}
			else {
				close(newstdin[0]);
				close(newstdin[1]);
			}
		}
		if(retval == FALSE) {
			MyPerror(killp_phrases[1]);
		}
		else {
#ifdef MYSIGNAL
			signal(SIGPIPE, pipe_sighandler);	/* set up signal handler if pipe breaks */
			signal_block(MYSIGNAL_ADDPIPE);		/* set up sgetline() to block signal */
#endif
			mkill->child.Pid=fork();
			if(mkill->child.Pid == 0) {
				/* in child */
				close(newstdin[1]); 	/* will only be reading from this */
				close(newstdout[0]);	/* will only be writing to this */
				close(0);
				close(1);		/* just to be on safe side */
				dup2(newstdin[0], 0);	/* copy new read to stdin */
				dup2(newstdout[1], 1);	/* copy new write to stdout */
				execvp(prg[0], prg);
			}
			else if( mkill->child.Pid == -1) {
			  	/* whoops */
				MyPerror(killp_phrases[3]);
				retval = FALSE;
				/* close down all the pipes */
				close(newstdin[0]);
				close(newstdin[1]);
				close(newstdout[0]);
				close(newstdout[1]);
			}
			else {
				/* parent */
				close(newstdin[0]);	/* will only be writing to new stdin */
				close(newstdout[1]);	/* will only be reading from new stdout */
				/* so subroutine can read/write to child */
				mkill->child.Stdin = newstdin[1];
				mkill->child.Stdout = newstdout[0];
				mkill->killfunc = chk_msg_kill_fork;	/* point to our subroutine */
			}
		}
	
	}
	if(myargs != NULL) {
		free(myargs);
	}
	return retval;

}
/*-----------------------------------------------------------------------*/
int find_in_path(char *prg) {
	/* parse the path and search thru it for the program to see if it exists */
	
	int retval = FALSE, len;
	char fullpath[PATH_MAX+1], *ptr, *mypath, *nptr;
	struct stat buf;

	/* if prg has a slant bar in it, its an absolute/relative path, no search done */
	if(strchr(prg, '/') == NULL) {
		ptr = getenv("PATH");
		if(ptr != NULL) {
			len = strlen(ptr)+1;
			/* now have to copy the environment, since I can't touch the ptr */
			if((mypath = malloc(len)) == NULL) { 
				error_log(ERRLOG_REPORT, "%s %s", killp_phrases[2], NULL);
			}
			else {
				strcpy(mypath, ptr);
				ptr = mypath;	/* start us off */
				do {
					nptr = strchr(ptr, PATH_SEPARATOR);
					if(nptr != NULL) {
						*nptr = '\0';		/* null terminate the current one */
						nptr++;			/* move past it */
					}
					/* build the fullpath name */
					strcpy(fullpath, ptr);
					if(ptr[strlen(ptr)-1] != '/') {
						strcat(fullpath, "/");
					}
					strcat(fullpath, prg);
					/* okay now have path, check to see if it exists */
					if(stat(fullpath, &buf) == 0) {
						if(S_ISREG(buf.st_mode)) {
							retval = TRUE;
						}
					}
					/* go onto next one */
					ptr = nptr;
				}
				while(ptr != NULL && retval == FALSE);
				free(mypath);
			}
		}	
	}
	/* last ditch try, in case of a relative path or current directory */
	if (retval == FALSE) {
		if(stat(full_path(FP_GET_NOPOSTFIX, FP_NONE, prg), &buf) == 0) {
			if(S_ISREG(buf.st_mode)) {
				retval = TRUE;
			}
		}
	}
	if(retval == FALSE) {
		error_log(ERRLOG_REPORT, killp_phrases[3], prg, NULL);
	}
	return retval;
}
/*----------------------------------------------------------------------------*/
int chk_msg_kill_fork(PKillStruct pkill, char *header) {
	/* write the header to ChildStdin, read from ChildStdout */
	/* read 0 if get whole message, 1 if skip */
	/* return TRUE if kill article, FALSE if keep  */

	int retval = FALSE, y, status;
	char keepyn, keep[4], len[KILLPRG_LENGTHLEN+1];
	pid_t waitstatus;	

	/* first make sure our child is alive WNOHANG so if its alive, immed return */
	keepyn = -1;	/* our error status */
	waitstatus = waitpid(pkill->child.Pid, &status, WNOHANG);
	if(waitstatus == 0) { /* child alive */
		/* first write the length down */
		y = strlen(header);
#ifdef DEBUG2
		do_debug("\nTrying to write len %d to child:\n%s", y,header);
#endif
		sprintf(len, "%-*d\n", KILLPRG_LENGTHLEN-1, y);
		if(write(pkill->child.Stdin, len, KILLPRG_LENGTHLEN) <= 0) {
			error_log(ERRLOG_REPORT,  killp_phrases[4], NULL);		
		}
		/* then write the actual header */
		else if(write(pkill->child.Stdin, header,y) <= 0) {
			error_log(ERRLOG_REPORT,  killp_phrases[4], NULL);
		}
		/* read the result back */
		else if(read(pkill->child.Stdout, &keep, 2) <= 0) {
			error_log(ERRLOG_REPORT,  killp_phrases[5], NULL);
		}
		else {
			keepyn = keep[0] - '0';	/* convert ascii to 0/1 */ 
#ifdef DEBUG2
			do_debug("\nRead from child: %s :\n", keep);
#endif
		}
	}
	else if(waitstatus == -1) { /* huh? */
		MyPerror( killp_phrases[6]);
	}
	else { /* child died */
		error_log(ERRLOG_REPORT,  killp_phrases[7]);
	} 		

	switch (keepyn) {
	  case 0:
		retval = FALSE;
		break;
	  case 1:
		retval = TRUE;
		break;
	  default: /* error in child don't use anymore */
		retval = FALSE;
		pkill->killfunc = chk_msg_kill; /* back to normal method */
	}

	return retval;
}
/*----------------------------------------------------------------------*/
void killprg_closeit(PKillStruct master) {
	/* tell it to close down by writing a length of zero */
	/* then wait for the pid to close down*/
	char len[KILLPRG_LENGTHLEN+1];

	sprintf(len, "%-*d\n", KILLPRG_LENGTHLEN-1, 0);

	write(master->child.Stdin, len, KILLPRG_LENGTHLEN);
	waitpid(master->child.Pid, NULL, 0);

}
/*---------------------------------------------------------------------*/
#ifdef MYSIGNAL
RETSIGTYPE pipe_sighandler(int what) {

	/* we don't have to do anything else, since the routine above will detect a dead child */
	/* and handle it appropriately*/
	
	int status;
	error_log(ERRLOG_REPORT, killp_phrases[8], NULL);
	wait(&status);
	if(WIFEXITED(status) != 0) {
		error_log(ERRLOG_REPORT, killp_phrases[9], str_int(WEXITSTATUS(status)), NULL);
	}
	else if(WIFSIGNALED(status) != 0) {
		error_log(ERRLOG_REPORT, killp_phrases[10], str_int(WTERMSIG(status)), NULL);
	}
	else {
		error_log(ERRLOG_REPORT, killp_phrases[11], NULL);
	}
}
#endif
