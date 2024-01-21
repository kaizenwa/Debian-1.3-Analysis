/*
 * Encryption utilites
 * Bradley Williams	
 * {allegra,ihnp4,uiucdcs,ctvax}!convex!williams
 * $Revision: 6.1 $
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>

#include <unistd.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/wait.h>

#if defined(BSD42) || defined(BSD43)
#include <sys/file.h>
#else
#include <fcntl.h>
#endif

#include "sc.h"
#include "crypt.h"
#include "cmds.h"
#include "interp.h"
#include "lex.h"


int         Crypt = 0;

void creadfile (char *save, int eraseflg)
{
    register FILE *f;
    int pipefd[2];
    int fildes;
    int pid;

    if (eraseflg && strcmp(save, curfile) && modcheck(" first")) return;

    fildes = open (save, O_RDONLY, 0);
    if (fildes < 0)
    {
	slsc_error ("Can't read file \"%s\"", save);
	return;
    }

    if (eraseflg) erasedb ();

    if (pipe(pipefd) < 0) {
	slsc_error("Can't make pipe to child");
	return;
    }

    sc_reset_display ();
    if ((pid=fork()) == 0)			  /* if child  */
    {
	(void) close (0);		  /* close stdin */
	(void) close (1);		  /* close stdout */
	(void) close (pipefd[0]);	  /* close pipe input */
	(void) dup (fildes);		  /* standard in from file */
	(void) dup (pipefd[1]);		  /* connect to pipe */
	(void) fprintf (stderr, " ");
	(void) execl ("/bin/sh", "sh", "-c", "crypt", 0);
	exit (-127);
    }
    else				  /* else parent */
    {
	(void) close (fildes);
	(void) close (pipefd[1]);	  /* close pipe output */
	f = fdopen (pipefd[0], "r");
	if (f == 0)
	{
	    (void) kill (pid, -9);
	    slsc_error ("Can't fdopen file \"%s\"", save);
	    (void) close (pipefd[0]);
	    return;
	}
    }

    loading++;
    while (fgets(line,sizeof line,f)) {
	linelim = 0;
	if (line[0] != '#') (void) yyparse ();
    }
    --loading;
    (void) fclose (f);
    (void) close (pipefd[0]);
    while (pid != wait(&fildes)) /**/;
    sc_init_display ();
    linelim = -1;
    modflg++;
    if (eraseflg) {
	(void) strcpy (curfile, save);
	modflg = 0;
    }
    sc_eval_all ();
}

int cwritefile (char *fname, int r0, int c0, int rn, int cn)
{
   register FILE *f;
   int pipefd[2];
   int fildes;
   int pid;
    char save[1024];
   char *fn;


    if (*fname == 0) fname = &curfile[0];

    fn = fname;
    while (*fn && (*fn == ' '))  /* Skip leading blanks */
	fn++;

    if ( *fn == '|' ) {
	slsc_error ("Can't have encrypted pipe");
	return(-1);
	}

    (void) strcpy(save,fname);

    fildes = open (save, O_WRONLY|O_CREAT, 0600);
    if (fildes < 0)
    {
	slsc_error ("Can't create file \"%s\"", save);
	return(-1);
    }

    if (pipe (pipefd) < 0) {
	slsc_error ("Can't make pipe to child\n");
	return(-1);
    }

    sc_reset_display ();
    if ((pid=fork()) == 0)			  /* if child  */
    {
	(void) close (0);			  /* close stdin */
	(void) close (1);			  /* close stdout */
	(void) close (pipefd[1]);		  /* close pipe output */
	(void) dup (pipefd[0]);			  /* connect to pipe input */
	(void) dup (fildes);			  /* standard out to file */
	(void) fprintf (stderr, " ");
	(void) execl ("/bin/sh", "sh", "-c", "crypt", 0);
	exit (-127);
    }
    else				  /* else parent */
    {
	(void) close (fildes);
	(void) close (pipefd[0]);		  /* close pipe input */
	f = fdopen (pipefd[1], "w");
	if (f == 0)
	{
	    (void) kill (pid, -9);
	    slsc_error ("Can't fdopen file \"%s\"", save);
	    (void) close (pipefd[1]);
	    return(-1);
	}
    }

    write_fd(f, r0, c0, rn, cn);

    (void) fclose (f);
    (void) close (pipefd[1]);
    while (pid != wait(&fildes)) /**/;
    (void) strcpy(curfile,save);

    modflg = 0;
    slsc_error ("File \"%s\" written", curfile);
    sc_init_display ();
    return(0);
}

