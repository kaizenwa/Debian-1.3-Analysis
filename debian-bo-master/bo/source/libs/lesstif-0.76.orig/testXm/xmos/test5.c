/*
 *
 * test for _XmOSBuildFileList
 *
 */

#include <Xm/Xm.h>
#include <Xm/XmosP.h>
#include <stdio.h>

int
main(int argc,
     char **argv)
{
	char dir[256], pat[256];
	String quald, qualpat, *entries;
	unsigned int numents, numalloc, i;

	entries = NULL;
	numents = numalloc = 0;
	for (;;) {
		printf("Enter dir: ");
		gets(dir);
		if (feof(stdin))
			break;
		printf("Enter pat: ");
		gets(pat);
		if (feof(stdin))
			break;
		_XmOSQualifyFileSpec(dir, pat, &quald, &qualpat);
		printf("\n  Dir: '%s' Pat: '%s' quald: '%s' qualpat: '%s'\n",
			dir, pat, quald, qualpat);
		_XmOSBuildFileList(quald, qualpat, XmFILE_ANY_TYPE,
				   &entries, &numents, &numalloc);
		for (i = 0; i < numents; i++)
			printf("Entry %d: '%s'\n", i, entries[i]);
	}
	exit(0);
}
