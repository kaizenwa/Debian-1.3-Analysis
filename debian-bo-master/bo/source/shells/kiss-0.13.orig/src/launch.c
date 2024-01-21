#include "kiss.h"

void launch (Stringstack org, int infile, int outfile)
{
    register int
	i;
    Stringstack
	s;
    
    /* signals to default */
    signal (SIGINT, SIG_DFL);
    signal (SIGQUIT, SIG_DFL);
    signal (SIGTSTP, SIG_DFL);
    signal (SIGTTIN, SIG_DFL);
    signal (SIGTTOU, SIG_DFL);
    signal (SIGCHLD, SIG_DFL);

    /* see if we're redirected */
    if (infile != STDIN_FILENO)
    {
	dup2 (infile, STDIN_FILENO);
	close (infile);
    }
    if (outfile != STDOUT_FILENO)
    {
	dup2 (outfile, STDOUT_FILENO);
	close (outfile);
    }
    
    /* re-expand wildcards and environment variables */
    s = reexpand (org);

    /* launch internal command if that is the name */
    /* note: this is a child process, we can alter `progname' */
    progname = s.str [0];
    if ( (i = isinternal (s.str [0])) != -1 )
	exit (cmdtable [i].cmd (s));

    /* launch external program */
    /* append last NULL for exec */
    s.str = xrealloc (s.str, (s.nstr + 1) * sizeof (char *));
    s.str [s.nstr] = NULL;

    execvp (s.str [0], s.str);
    warning ("command not found", s.str [0]);
    exit (1);
}
