/* coswitch.c for AT&T unixpc rel. 3.0 */

coswitch(oldce, newce, first)
int	*oldce, *newce;
int	first;
{
	asm("	mov.l	8(%fp), %a0");		/* a0 <- &old */
	asm("	mov.l	12(%fp), %a1");		/* a1 <- &new */
	asm("	mov.l	%sp, (%a0)");		/* save sp in old[0] */
	asm("	movm.l	&0x7cfc, 4(%a0)");	/* save a6-a2, d7-d2 at old+1 */
	if (first == 0) {	/* This is the first activation. */
		asm("	mov.l	(%a1), %sp");
		asm("	mov.l	&0, %fp");
		new_context(0, 0);
		syserr("new_context() returned in coswitch().");
	}
	else {
		asm("	mov.l	(%a1), %sp");	/* Re-load regs from new[] */
		asm("	movm.l	4(%a1), &0x7cfc"); 
	}
}

