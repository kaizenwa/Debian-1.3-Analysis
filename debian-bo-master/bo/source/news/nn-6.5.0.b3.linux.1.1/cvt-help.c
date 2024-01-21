/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 *	Convert help-files to binary.
 *	;:X	->	^X (control X)
 */

#include <stdio.h>

main()
{
    register int c;

    while ((c = getchar()) != EOF) {
	if (c == ';') {
	    c = getchar();
	    if (c == ':') {
		c = getchar();
		putchar(c & 0xf);
		continue;
	    }
	    putchar(';');
	    putchar(c);
	    continue;
	}
	if (c >= 1 && c <= 7) {
	    putchar(';');
	    putchar(':');
	    putchar(c | 0x40);
	    continue;
	}
	putchar(c);
    }

    exit(0);
}
