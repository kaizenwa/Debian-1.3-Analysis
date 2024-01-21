#include <stdio.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif
/*****************************************************************************
 *	Does `string' match `pattern'? '*' in pattern matches any sub-string
 *	(including the null string) '?' matches any single char. For use
 *	by filenameforall. Note that '*' matches across directory boundaries
 *
 *      This code donated by  Paul Hudson <paulh@harlequin.co.uk>    
 *      It is public domain, no strings attached. No guarantees either.
 *		Modified by Emanuele Caratti <wiz@iol.it>
 *
 *****************************************************************************/

int 
matchWildcards(char *pattern, char *string)
{
	if (pattern == NULL)
		return TRUE;
	else if (*pattern == '*' && !*(pattern+1))
		return TRUE;

	if (string == NULL)
		return FALSE;

	while (*string && *pattern) {
		if (*pattern == '?') {
			/* match any character */
			pattern++;
			string++;
		} else if (*pattern == '*') {
			/* see if the rest of the pattern matches any trailing substring
			 * of the string. */
			pattern++;
			if (*pattern == 0) {
				return TRUE;	/* trailing * must match rest */
			}
			while (1){
				while(*string && (*string != *pattern))
					string++;
				if(!*string)
					return FALSE;
				if( matchWildcards(pattern,string))
					return TRUE;
				string++;
			}
		
		} else {
			if (*pattern == '\\')
				pattern++;		/* has strange, but harmless effects if the last
								 * character is a '\\' */
			if (*pattern++ != *string++) {
				return FALSE;
			}
		}
	}
	if ((*pattern == 0) && (*string == 0))
		return TRUE;
	if ((*string == 0) && (strcmp(pattern, "*") == 0))
		return TRUE;
	return FALSE;
}
