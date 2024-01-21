/////////////////////////////////////////////////////////////////
//
// $Id: McWildMatch.cpp,v 3.0 1997/02/04 17:49:00 bzfzoeck Exp $
//
/////////////////////////////////////////////////////////////////

int mcWildMatch(const char* str, const char* pat)
{
    register int i = 0, j = 0;
    int	star = 0, k = 0;

    while (pat[i] != '\0'&& str[j] != '\0')
    {
	if (pat[i] == '*')
	{
	    star = 1;
	    while (pat[i] == '*') i++;
	    k = i;
	}
	else
	{
	    if (pat[i] == '?' || pat[i] == str[j])
	    {
	        j++; i++;
	    }
	    else
	    {
		if (star == 0)
		{
		    return 0;
		}
	        i = k;
		j++;
	    }
	}
    }

    while (pat[i] == '*') i++;

    if (pat[i] == str[j])
    {
	return 1;
    }
    else if (pat[i] == '\0' && (pat[i-1] == '*' || pat[i-1] == '?'))
    {
	return 1;
    }
    else
    {
	return 0;
    }
}
