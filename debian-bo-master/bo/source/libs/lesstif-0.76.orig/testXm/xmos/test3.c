/*
 *
 * test for _XmOSQualifyFileSpec
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
	String quald, qualpat;

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
	}
	exit(0);
}
