#include "kiss.h"

void sighandler (int sig)
{
    register char
	*cp;
    
    if (sig == SIGINT)
	error ("ok, ^C.");
    else if (sig == SIGUSR1 || sig == SIGUSR2)
    {
	warning ("caught usersignal %d, reloading..", sig);
	execv (orgargv [0], orgargv);
	error ("bummer, can't reload \"%s\"", orgargv [0]);
    }
    else if (sig == SIGSEGV)
    {
	signal (SIGSEGV, SIG_DFL);
	warning ("oops, segment violation");
	if ( (cp = getenv ("SHLVL")) && atoi (cp) == 1 && inputparsed)
	if (inputparsed)
	{
	    warning ("trying to reload");
	    execv (orgargv [0], orgargv);
	}
	error ("bummer.. it's not a perfect world");
    }
    
    error ("caught signal %d, terminating", sig);
}
