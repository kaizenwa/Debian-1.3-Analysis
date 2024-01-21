#include "kiss.h"

static int buildmask (char *s, int *owner, int *setordel, int *readmode,
		      int *writemode, int *execmode)
{
    register int
	state = 0;

    *owner = 0;
    *readmode = 0;
    *writemode = 0;
    *execmode = 0;
    
    while (*s)
    {
	if (! state)			/* expecting owner info */
	{
	    if (*s == 'u')
		*owner |= S_IRWXU;
	    else if (*s == 'g')
		*owner |= S_IRWXG;
	    else if (*s == 'o')
		*owner |= S_IRWXO;
	    else if (*s == 'a')
		*owner |= (S_IRWXU | S_IRWXG | S_IRWXO);
	    else if (*s == '-' || *s == '+')
	    {
		*owner |= S_IRWXU;
		s--;
	    }
	    else
		return (0);
	    state++;
	}
	else if (state == 1)		/* expecting set or remove info */
	{
	    if (*s == '-')
		*setordel = -1;
	    else if (*s == '+')
		*setordel = 1;
	    else
		return (0);
	    state++;
	}
	else if (state == 2)		/* expecting modebits info */
	{
	    if (*s == 'r')
		*readmode = 1;
	    else if (*s == 'w')
		*writemode = 1;
	    else if (*s == 'x')
		*execmode = 1;
	    else
		return (0);
	}
	s++;
    }

    return (1);				/* successful parsing */
}

int dochmod (Stringstack s)
{
    int
	owner,
	setordel,
	readmode,
	writemode,
	execmode,
	newmode;
    register int
	ret = 0,
	octalmode = 0,
	i;
    struct stat
	statbuf;
    
    if (s.nstr < 3)
	error ("Bad commandline.\n"
	       "Usage: %s asciimode | octalmode  file(s)\n"
	       "Where:\n"
	       "    asciimode: the mode to set, use [ugoa][-+][rwx] to:\n"
	       "        apply to user(u), group(g), other(o) or all(a)"
							    " [default:u]\n"
	       "        remove(-) or set(+) bits [required],\n"
	       "        apply to the read(r), write(w), exec(x) bits\n"
	       "    octalmode: mode as an octal number\n"
	       "    file(s): the files to process\n"
	       , progname);

    if (s.str [1][0] >= '0' && s.str [1][0] <= '7')
    {
	if (! sscanf (s.str [1], "%o", &newmode))
	    return (warning ("bad octal mode \"%s\"", s.str [1]));
	octalmode++;
    }
    else if (! buildmask (s.str [1], &owner, &setordel, &readmode,
		     &writemode, &execmode))
	return (warning ("bad ascii mode \"%s\"", s.str [1]));


    for (i = 2; i < s.nstr; i++)
    {
	if (stat (s.str [i], &statbuf))
	{
	    ret += warning ("cannot stat \"%s\"", s.str [i]);
	    continue;
	}

	if (octalmode)
	{
	    if (chmod (s.str [i], newmode))
		ret += warning ("problems setting octal mode "
				"%o for file \"%s\"",
				newmode, s.str [i]);
	    continue;
	}
	
	newmode = statbuf.st_mode;
	
	if (owner & S_IRWXU)
	{
	    if (setordel == 1)
	    {
		if (readmode)
		    newmode |= S_IRUSR;
		if (writemode)
		    newmode |= S_IWUSR;
		if (execmode)
		    newmode |= S_IXUSR;
	    }
	    else
	    {
		if (readmode)
		    newmode &= ~S_IRUSR;
		if (writemode)
		    newmode &= ~S_IWUSR;
		if (execmode)
		    newmode &= ~S_IXUSR;
	    }
	}
	
	if (owner & S_IRWXG)
	{
	    if (setordel == 1)
	    {
		if (readmode)
		    newmode |= S_IRGRP;
		if (writemode)
		    newmode |= S_IWGRP;
		if (execmode)
		    newmode |= S_IXGRP;
	    }
	    else
	    {
		if (readmode)
		    newmode &= ~S_IRGRP;
		if (writemode)
		    newmode &= ~S_IWGRP;
		if (execmode)
		    newmode &= ~S_IXGRP;
	    }
	}
	if (owner & S_IRWXO)
	{
	    if (setordel == 1)
	    {
		if (readmode)
		    newmode |= S_IROTH;
		if (writemode)
		    newmode |= S_IWOTH;
		if (execmode)
		    newmode |= S_IXOTH;
	    }
	    else
	    {
		if (readmode)
		    newmode &= ~S_IROTH;
		if (writemode)
		    newmode &= ~S_IWOTH;
		if (execmode)
		    newmode &= ~S_IXOTH;
	    }
	}

	if (chmod (s.str [i], newmode))
	    ret += warning ("problems setting octal mode %o for file \"%s\"",
			    newmode, s.str [i]);
    }

    return (0);
}
