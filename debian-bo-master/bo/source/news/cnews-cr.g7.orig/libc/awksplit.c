/*
 * awksplit - awk-like split(3) that handles memory allocation automatically
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int				/* number of fields, including overflow */
awksplit(string, fieldsp, nfields, sep)
char *string;
register char ***fieldsp;	/* list is not NULL-terminated */
register int nfields;		/* number of entries available in fields[] */
char *sep;			/* "" white, "c" single char, "ab" [ab]+ */
{
	register int nf;

	nf = split(string, *fieldsp, nfields, sep);
	if (nf > nfields) {	/* too many fields to fieldsp? */
		register char **array =
			(char **)malloc((unsigned)(nf * sizeof(char *)));
		register int i;

		if (array == NULL)
			*fieldsp = NULL;	/* you lose */
		else {
			for (i = 0; i < nfields; i++)
				array[i] = (*fieldsp)[i];
			*fieldsp = array;
			(void) split(array[nfields-1], &array[nfields],
				nf - nfields, sep);
		}
	}
	return nf;
}
