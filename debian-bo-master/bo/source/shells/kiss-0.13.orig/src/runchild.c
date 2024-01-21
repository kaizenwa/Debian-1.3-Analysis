#include "kiss.h"

/* for debugging purposes: */
#define DUMPCMD(str,stack) {flags.debug = 1; dumpstack (str,stack);}

void runchild (Stringstack s, int background)
{
    register int
	i,
	j,
	childpid;
    Stringstack
	one;
    int
	pipefound = 1,
	p [2],
	infile = STDIN_FILENO,
	outfile = STDOUT_FILENO,
	allow_inpipe = 1;

    /* set group id if necessary */
    if (background && ! flags.controlkids)
	setpgid (getpid (), getpid ());

    /* parse from left to right, splitting into piped parts */
    while (pipefound)
    {
	pipefound = 0;			    /* assume no pipe present */
	for (i = 1; i < s.nstr - 1; i++)
	    if (! strcmp (s.str [i], "|"))
	    {
		pipefound = 1;
		/* found pipe: copy that part to 'one'*/
		one.str = xmalloc ( i * sizeof (char *) );
		one.nstr = i;
		for (j = 0; j < i; j++)
		    one.str [j] = s.str [j];
		/* remove that part from 's' */
		s.nstr -= i + 1;
		s.str  += i + 1;

		if (pipe (p) < 0)
		{
		    warning ("pipe failure");
		    return;
		}
		outfile = p [P_BLOW];

		if ( (childpid = fork ()) < 0 )	    /* fork failure? */
		{
		    warning ("fork failure");
		    return;
		}
		else if (! childpid)		    /* child process */
		{
		    setshlvl ();
		    onechild (one, infile, outfile, allow_inpipe, 0);
		}
		else				    /* parent */
		{
		    allow_inpipe = 0;		    /* next may not pipe in */
		
		    if (infile != STDIN_FILENO)	    /* clean up pipes */
			close (infile);
		    if (outfile != STDOUT_FILENO)
			close (outfile);
		
		    infile = p [P_SUCK];	    /* infile for next one */
		}
		break;				    /* terminate for loop, */
	    }					    /* fall thru to while */
    }

    /* here's the last one */
    onechild (s, infile, STDOUT_FILENO, allow_inpipe, 1);
}
