#include "kiss.h"

void addtohistory (Stringstack cmd)
{
    register int
	i;
    
    if (nhislist < MAXHIST - 1)
    {
	hislist = xrealloc (hislist, (nhislist + 1) * sizeof (Stringstack));
	hislist [nhislist++] = copystringstack (cmd, 0, cmd.nstr - 1);
    }
    else
    {
	for (i = 0; i < MAXHIST - 1; i++)
	    hislist [i] = hislist [i + 1];
	clearstack (&hislist [MAXHIST - 1]);
	hislist [MAXHIST - 1] = copystringstack (cmd, 0, cmd.nstr - 1);
    }
}
