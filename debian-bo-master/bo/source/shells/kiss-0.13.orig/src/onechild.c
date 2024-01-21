#include "kiss.h"

void onechild (Stringstack org, int indes, int outdes,
	       int allow_from, int allow_to)
{
    char
	fname [LINELEN];
    Redirect
	red;
    FILE
	*outf,
	*inf;
    Stringstack
	s;

    /* expand any aliased commands */
    s = expandalias (org);
    
    /* see if we're redirecting */
    if ( (red = redirected (&s, fname)) )
    {
	/* try to open the file, see if we may */
	switch (red)
	{
	    case writeto:
		if (! allow_to)
		{
		    warning ("%s: not allowed to redirect to file",
			     s.str [0]);
		    return;
		}
		if (! (outf = fopen (fname, "w")) )
		{
		    warning ("cannot open redirect file \"%s\" for writing",
			     fname);
		    return;
		}
		outdes = fileno (outf);
		break;
	    case appendto:
		if (! allow_to)
		{
		    warning ("%s: not allowed to redirect to file",
			     s.str [0]);
		    return;
		}
		if (! (outf = fopen (fname, "a")) &&
		    ! (outf = fopen (fname, "w")) )
		{
		    warning ("cannot open redirect file \"%s\" for appending",
			     fname);
		    return;
		}
		outdes = fileno (outf);
		break;
	    case readfrom:
		if (! allow_from)
		{
		    warning ("%s: not allowed to redirect from file",
			     s.str [0]);
		    return;
		}
		if (! (inf = fopen (fname, "r")))
		{
		    warning ("cannot open redirect file \"%s\" for reading",
			     fname);
		    return;
		}
		indes = fileno (inf);
		break;
	    default:
		warning ("internal error [1] in onechild");
	}
    }

    /* launch program */
    launch (s, indes, outdes);
}
