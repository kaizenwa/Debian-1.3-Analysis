/* 
 * outp.c -- write the port/value pairs specified in hex on the command line
 *
 * Tested with 2.0.x (intel and alpha) and 1.2.13 (intel)
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <asm/io.h>

#ifdef __sparc__
#  error "This program can't compile or run on the Sparc platform"
#endif

#ifdef __alpha__
#include <sys/io.h> /* ioperm */
#endif

int main(int argc, char **argv)
{
    unsigned int i,n,v;

    setuid(0); /* if we're setuid, do it really */
    for (i=1;i<argc-1;i++) {
        sscanf(argv[i],"%x",&n);
        sscanf(argv[++i],"%x",&v);
        if (ioperm(n,1,1)) {perror("ioperm()"); exit(1);}
        outb(v,n);
    }
    return 0;
}
