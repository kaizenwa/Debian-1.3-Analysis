#include "kiss.h"

char *getprompt ()
{
    register char
	*prompt,
	*cwd;
    static char
	buf [LINELEN];
    char
	twobuf [2] = { 0, 0 };

    cwd = getcwd (NULL, 0);
    if (! flags.noenviron)
	addtoenv ("PWD", cwd);

    if (! (prompt = getenv ("PROMPT")) )
	prompt = "[%u] %p > ";

    buf [0] = '\0';

    while (*prompt)
    {
	if (*prompt == '%' && *(prompt + 1))
	{
	    prompt++;
	    switch (*prompt)
	    {
		case 'n':
		    strcat (buf, "\n");
		    break;
		case 'u':
		    strcat (buf, username);
		    break;
		case 'p':
		    strcat (buf, cwd);
		    break;
		default:
		    twobuf [0] = *prompt;
		    strcat (buf, twobuf);
	    }
	}
	else
	{
	    twobuf [0] = *prompt;
	    strcat (buf, twobuf);
	}
	prompt++;
    }
    
    free (cwd);

    return (buf);
}
