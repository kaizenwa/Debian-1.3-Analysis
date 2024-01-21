/*
 * $Id: csum.c,v 1.3 1995/02/26 19:21:02 begemot Exp $
 * $Log: csum.c,v $
 * Revision 1.3  1995/02/26  19:21:02  begemot
 * Nothing important
 *
 * Revision 1.2  1995/02/26  18:59:25  begemot
 * Added RCS Id & Log entries into the all source files.
 *
 */

/* Borrowed from linux/net/inet/ip.c */
unsigned short ip_fast_csum(unsigned char *  buff, int wlen)
{
	unsigned long sum = 0;

    	if (wlen) 
    	{
    	unsigned long bogus;
	 __asm__("clc\n"
		"1:\t"
		"lodsl\n\t"
		"adcl %3, %0\n\t"
		"decl %2\n\t"
		"jne 1b\n\t"
		"adcl $0, %0\n\t"
		"movl %0, %3\n\t"
		"shrl $16, %3\n\t"
		"addw %w3, %w0\n\t"
		"adcw $0, %w0"
	    : "=r" (sum), "=S" (buff), "=r" (wlen), "=a" (bogus)
	    : "0"  (sum),  "1" (buff),  "2" (wlen));
    	}
	return (~sum) & 0xffff;
}
