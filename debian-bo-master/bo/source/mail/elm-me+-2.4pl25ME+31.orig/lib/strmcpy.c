#include "defs.h"
#include "elmlib.h"

char * strmcpy (dest, src)
     char *dest, *src;
{
	int len = strlen (src) + 1;

	if (dest)
		dest = (char *) safe_realloc (dest, len);
	else
		dest = (char *) safe_malloc (len);
	strfcpy (dest, src, len);
	return dest;
}

