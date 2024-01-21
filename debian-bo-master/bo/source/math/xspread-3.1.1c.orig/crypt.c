/*
 * Encryption utilites
 * Bradley Williams	
 * {allegra,ihnp4,uiucdcs,ctvax}!convex!williams
 * $Revision: 1.1 $
 */
#include <config.h>

#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)

#include <curses.h>

#ifdef HAVE_X11_X_H
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "scXstuff.h"
#endif /* HAVE_X11_X_H */

#if defined(BSD42) || defined(BSD43) && !defined(SYSV)
#include <sys/types.h>
#include <sys/file.h>
#else
#include <fcntl.h>
#endif

#include "sc.h"
#include <signal.h>

char        *getpass();

#ifdef SYSV3
void exit();
#endif

int         Crypt = 0;		/* Set the default not to use crypt? */

void
creadfile (save, eraseflg)
char *save;
int  eraseflg;
{
    register FILE *f = NULL;
    int pipefd[2];
    int fildes;
    int pid;
    char cryptcommand[30];
#ifdef HAVE_X11_X_H
    char *key, prompt[20];
#endif

#ifndef DES
    strcpy(cryptcommand, "crypt ");
#else
    strcpy(cryptcommand, "des -b -d -k ");
#endif

    if (eraseflg && strcmp(save, curfile) && modcheck(" first")) 
	return;

    if ((fildes = open(findhome(save), O_RDONLY, 0)) < 0)
    {
	sprintf(stringbuf, "Can't read file \"%s\"", save);
	scerror(stringbuf);
	return;
    }

    if (eraseflg) 
	erasedb();

    if (pipe(pipefd) < 0) {
	scerror("Can't make pipe to child");
	return;
    }

#ifdef HAVE_X11_X_H
    if (using_X)
    {
	/*  Get a non-null key value
	 * 
	 */
	strcpy(prompt, "Enter key:");
	while(strlen(key = get_str(prompt, 9)) == 0)
		strcpy(prompt, "Enter key:");

	/*  Append the key to the crypt/des command
	 *
	 */
	strcat(cryptcommand, key);
    } else
#endif /* HAVE_X11_X_H */
    {	deraw();
	(void) strcat(cryptcommand, getpass("Enter key:"));
	goraw();
    } /* HAVE_X11_X_H, end curses */

    if ((pid=fork()) == 0)			  /* if child  */
    {
	(void) close (0);		  /* close stdin */
	(void) close (1);		  /* close stdout */
	(void) close (pipefd[0]);	  /* close pipe input */
	(void) dup (fildes);		  /* standard in from file */
	(void) dup (pipefd[1]);		  /* connect to pipe */
	(void) fprintf (stderr, " ");
	(void) execl ("/bin/sh", "sh", "-c", cryptcommand, 0);
	(void) fprintf (stderr, "execl failed in creadfile() running %s \n",
			cryptcommand);
	exit (-127);
    }
    else				  /* else parent */
    {
	(void) close (fildes);
	(void) close (pipefd[1]);	  /* close pipe output */
	if ((f = fdopen (pipefd[0], "r")) == (FILE *)0)
	{
	    (void) kill (pid, -9);
	    sprintf(stringbuf, "Can't fdopen file \"%s\"", save);
	    scerror(stringbuf);
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
    linelim = -1;
    modflg++;
    if (eraseflg) {
	(void) strcpy (curfile, save);
	modflg = 0;
    }
    EvalAll();
}

int
cwritefile (fname, r0, c0, rn, cn)
char *fname;
int r0, c0, rn, cn;
{
    register FILE *f;
    int pipefd[2];
    int fildes;
    int pid;
    char save[PATHLEN];
    char *fn;
    char *busave;
    char cryptcommand[30];
#ifdef HAVE_X11_X_H
    char key1[10], key2[10], *key, prompt[20];
#endif

#ifndef DES
    strcpy(cryptcommand, "crypt ");
#else
    strcpy(cryptcommand, "des -b -e -k ");
#endif

    if (*fname == '\0') 
	fname = &curfile[0];

    fn = fname;
    while (*fn && (*fn == ' '))  /* Skip leading blanks */
	fn++;

    if ( *fn == '|' ) {
	scerror("Can't have encrypted pipe");
	return(-1);
    }

    (void) strcpy(save, fname);

    busave = findhome(save);
#ifdef DOBACKUPS
    if (!backup_file(busave) &&
	(yn_ask("Could not create backup copy, Save anyhow?: (y,n)") != 1))
		return(0);
#endif
    if ((fildes = open (busave, O_TRUNC|O_WRONLY|O_CREAT, 0600)) < 0)
    {
	sprintf(stringbuf, "Can't create file \"%s\"", save);
	scerror(stringbuf);
	return(-1);
    }

    if (pipe (pipefd) < 0) {
	scerror("Can't make pipe to child\n");
	return(-1);
    }

#ifdef HAVE_X11_X_H
     if (using_X)
     {
	/* added [jclark:19920930.1259CST]
	 *
	 * Neither `crypt' nor `des' double-prompt for the encryption key.
	 * If you mistype the key, and aren't sure what you've typed, you're
	 * screwed.  This code prompts for a key, requires that it be confirmed,
	 * and then passes the key to the encryption program.
	 *
	 */
 
	/* get a non-null key value
	 *
	 */
	strcpy(prompt, "Enter Key:");
	while (strlen(key = get_str(prompt,9)) == 0) 
		strcpy(prompt, "Enter Key:");

	strcpy(key1, key);
 
	/* require the key to be re-typed
	 *
	 */
	strcpy(prompt, "Confirm Key:");
	while (strlen(key = get_str(prompt,9)) == 0) 
		strcpy(prompt, "Confirm Key:");

	strcpy(key2, key);

	/* if the keys don't match, Quit!
	 *
	 */
	if (strcmp(key1, key2) != 0) {
		scerror("Keys do not match!");
		return(-1);
	}
 
	/* append the key to the crypt/des command */
	strcat(cryptcommand, key1);
     } else
#endif /* HAVE_X11_X_H */
     {	deraw();
	(void) strcat(cryptcommand, getpass("Enter key:"));
	goraw();
     } /* HAVE_X11_X_H, end curses */

    if ((pid=fork()) == 0)			  /* if child  */
    {
	(void) close (0);			  /* close stdin */
	(void) close (1);			  /* close stdout */
	(void) close (pipefd[1]);		  /* close pipe output */
	(void) dup (pipefd[0]);			  /* connect to pipe input */
	(void) dup (fildes);			  /* standard out to file */
	(void) fprintf (stderr, " ");
	(void) execl ("/bin/sh", "sh", "-c", cryptcommand, 0);
	(void) fprintf (stderr, "execl failure in cwritefile doing %s \n",
			cryptcommand);
	exit (-127);
    }
		/*  Else if parent */
    (void) close (fildes);
    (void) close (pipefd[0]);		  /* close pipe input */
    if (NULL == ( f = fdopen (pipefd[1], "w")))
    {
	(void) kill (pid, -9);
	sprintf(stringbuf, "Can't fdopen file \"%s\"", save);
	scerror(stringbuf);
	(void) close (pipefd[1]);
	return(-1);
    }
    else
    {
	write_fd(f, r0, c0, rn, cn);
	(void) fclose (f);
	(void)  close (pipefd[1]);
	while (pid != wait(&fildes)) /**/;
	(void) strcpy(curfile, save);
	modflg = 0;
	sprintf(stringbuf, "File \"%s\" written (encrypted).", curfile);
	scerror(stringbuf);
    }
    return(0);
}

#endif /* CRYPT */
