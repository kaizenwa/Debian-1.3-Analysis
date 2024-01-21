
/*
 * coswitch
 * For the IBM PCRT running under ACIS Berkeley 4.2 Unix on Andrew/Vice
 * Initial version, Mark Sherman, July 10, 1986
 *
 */
coswitch(old_cs /* r2 */, new_cs /* r3 */, first)
int *old_cs, *new_cs;
int first;
{
   /* *old_cs = 22; */
   /* *new_cs = 23; */
   /* runerr(401, NULL); */

    asm("  put r1,0(r2)		# Save the stack pointer in special place");
    asm("  stm r6,4(r2)		# Save state registers ");

    /* Note: we can't munge the stack pointer until after testing for first==0
*/

    if (first == 0) { /* this is the first activation */
	asm("    get r1,0(r3)	# Get the new stack pointer ");
	/* asm("    get r14,$0		# Zero out the data save address") */
	new_context(0,0);
	syserr("new_context() returned in coswitch");
	}
    else {
	asm("   get r1,0(r3)	# get the new stack pointer");
	asm("    lm r6,4(r3)	# restore all of the old registers");
	}
   
}
