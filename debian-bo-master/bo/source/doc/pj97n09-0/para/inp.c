/* 
 * inp.c -- read all the ports specified in hex on the command line
 *
 * Tested with 2.0.x (intel and alpha) and 1.2.13 (intel)
 * Can't run on the Sparc, which has no concept of I/O space.
 */

#ifdef __sparc__
#  error "This program can't compile or run on the Sparc platform"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <asm/io.h>
#ifdef __alpha__
#include <sys/io.h> /* ioperm. The header doesn't exist on the PC */
#endif

int main(int argc, char **argv)
{
    unsigned int i,n;

    setuid(0); /* if we're setuid, do it really */
    for (i=1;i<argc;i++) {
        sscanf(argv[i],"%x",&n);
        if (ioperm(n,1,1)) {perror("ioperm()"); exit(1);}
        printf("%03x: %02x\n",n,inb(n));
    }
    return 0;
}
