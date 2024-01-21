/*
 *
 * test for _XmOSFindPatternPart
 *
 */

#include <Xm/Xm.h>
#include <Xm/XmosP.h>
#include <stdio.h>

int
main(int argc,
     char **argv)
{
	char buf[256];
	String ret;

	for (;;) {
		gets(buf);
		if (feof(stdin))
			break;
		ret = _XmOSFindPatternPart(buf);
		printf("buf: %p FindPatternPart: %p: string '%s'\n", buf, ret, (ret) ? ret : "");
	}
	exit(0);
}
