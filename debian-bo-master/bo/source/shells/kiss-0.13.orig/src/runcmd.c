#include "kiss.h"

/* define DEBUG_ONE_CMD to get one shot at gdb'ing the child process */
/* then make a -g version of the command to see and set a trace in the code */

/*
#define DEBUG_ONE_CMD
*/

void runcmd (Stringstack s)
{
    register int
	pid,
	background = 0;
    Stringstack
	tmp1,
	tmp2,
	tmp3;

#   ifdef DEBUG_ONE_CMD
    runchild (s);
#   endif

    if (! s.nstr)
	return;

    dumpstack ("parser result:", s);
    
    /* cmd goes into history */
    addtohistory (s);

    /* expand s to whatever it may mean, unless it's an alias definition  */
    if (strcmp (s.str [0], "alias"))
    {
	tmp1 = expandalias (s);		/* expand aliases */
	dumpstack ("after alias expansion:" , tmp1);
    
	tmp2 = reexpand (tmp1);		/* expand wildcards and vars */
	clearstack (&tmp1);
	dumpstack ("after wordexp expansion:", tmp2);
    
	expandtilde (tmp2);		/* expand ~/ to homedir */
	dumpstack ("after ~/ expansion:" , tmp2);

	tmp3 = expandbackquotes (tmp2);
	clearstack (&tmp2);
	dumpstack ("after backquote expansion:", tmp3);
    }
    else
	tmp3 = copystringstack (s, 0, s.nstr - 1);

    /* reset getopt variables */
    opterr =  optopt = optind = 0;
    optarg = NULL;

    /* reset last status */
    laststatus = 0;

    dumpstack ("running part:", tmp3);

    if (! runinternal (tmp3))			/* try to run as internal */
    {						/* otherwise: via fork */
	if (! strcmp (tmp3.str [tmp3.nstr - 1], "&"))
	{
	    background++;
	    free (tmp3.str [tmp3.nstr - 1]);
	    tmp3.nstr--;
	}

	if ( (pid = fork ()) == -1 )	    /* try forking */
	{
	    warning ("fork failure");
	    return;
	}
	else if (! pid)			    /* child: run that cmd */
	{
	    setshlvl ();
	    runchild (tmp3, background);
	    exit (0);
	}
	else				    /* parent: wait for child */
	    waitforchild (tmp3.str [0], pid, background);
    }
    
    if (! flags.supressstat && laststatus && ! background)
	printf ("[%s: exited with %d]\n", tmp3.str [0], laststatus);
    clearstack (&tmp3);
}
