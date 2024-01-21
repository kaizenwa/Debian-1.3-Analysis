#include "kiss.h"

static void deletelasttwo (Stringstack *cmd)
{
    free (cmd->str [cmd->nstr - 2]);
    free (cmd->str [cmd->nstr - 1]);
    cmd->nstr -= 2;
}

Redirect redirected (Stringstack *cmd, char *fname)
{
    register char
	*token,
	*name;
    
    if (cmd->nstr >= 3)
    {
	token = cmd->str [cmd->nstr - 2];
	name  = cmd->str [cmd->nstr - 1];
	
	if (! strcmp (token, ">"))
	{
	    strcpy (fname, name);
	    deletelasttwo (cmd);
	    return (writeto);
	}
	if (! strcmp (token, ">>"))
	{
	    strcpy (fname, name);
	    deletelasttwo (cmd);
	    return (appendto);
	}
	if (! strcmp (token, "<"))
	{
	    strcpy (fname, name);
	    deletelasttwo (cmd);
	    return (readfrom);
	}
    }
    return (none);
}
