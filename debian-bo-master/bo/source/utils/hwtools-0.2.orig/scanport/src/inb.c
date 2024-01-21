/*
        inb.c

        Read i/o port and display result in hex and binary.

        This little utility saves you having to run DOS debug
        just to check an i/o port value. It is also safer than
        trying to use "dd" on "/dev/port", as some "dd" bins
        actually read over regions that are "skip=" protected.

        This code is ugly, and is hence GPL.     :-P

        Compile with: gcc -s -Wall -O2 inb.c -o inb

                                                Paul Gortmaker 09/96

*/

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <asm/io.h>

void main(int argc, char **argv) {

int i, val, bit;

if (argc != 2) {
	fprintf(stderr,"Usage: inb 0x<port_addr>\n");
	exit(-EINVAL);
}
	
i=strtol(argv[1], NULL, 16);

if (i<0x100) {
	fprintf(stderr,"port address must be greater than 0x100\n");
	exit(-EINVAL);
}

/* fprintf(stderr,"Reading i/o port 0x%3x\n",i); */

if (iopl(3)) {
	perror("iopl");
	exit(errno);
}

val=inb_p(i);
fprintf(stderr,"Port 0x%3x has value 0x%x (",i, val);
for (bit=128;bit>0;bit/=2) 
        fprintf(stderr, "%s", (val & bit) ? "1" : "0");
fprintf(stderr,").\n");

}
