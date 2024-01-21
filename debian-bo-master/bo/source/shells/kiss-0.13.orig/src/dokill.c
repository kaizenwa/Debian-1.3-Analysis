#include "kiss.h"

int dokill (Stringstack s)
{
    register int
	i,
	ret = 0;
    int
	sig,
	pid;
    
    if (s.nstr < 3)
	error ("Bad commandline.\n"
	       "Usage: %s -SIG processnr [processnr..]\n"
	       "Where:\n"
	       "    -SIG: signal to send to process\n"
	       "    processnr: process to signal\n"
	       , progname);

    if (! sscanf (s.str [1], "%d", &sig))
	error ("bad signal \"%s\"", s.str [1]);
    sig = -sig;
    
    for (i = 2; i < s.nstr; i++)
    {
	if (! sscanf (s.str [i], "%d", &pid))
	    ret += warning ("bad processnr \"%s\"", s.str [i]);
	else if (kill (pid, sig))
	    ret += warning ("problem signalling process \"%d\"", pid);
    }
    
    return (ret);
}
