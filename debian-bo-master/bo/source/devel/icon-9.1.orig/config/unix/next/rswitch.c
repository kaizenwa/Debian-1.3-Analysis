/*
 * This is the co-expression context switch for the NeXT. It is a verbatim
 * copy of the co-expresion switch for the Sun-3 Workstation.
 */

/*
 * coswitch
 */
coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
#ifdef m68k
   asm("  movl a6@(8),a0");		/* a0 = old */
   asm("  movl a6@(12),a1");		/* a1 = new */
   asm("  movl a7,a0@");		/* save sp in cstate[0] */
   asm("  movl a6,a0@(4)");		/* save a6 (fp) in cstate[0] */
   asm("  moveml #0x3cfc,a0@(8)");	/* store d2-d7, a2-a6 in old->cstate */
   if (first == 0) {	/* this is first activation */
      asm("  movl a1@,a7");
      asm("  movl #0,a6");
      new_context(0, 0);
      syserr("new_context() returned in coswitch");
      }
   else {
      asm(" movl a1@,a7");		/* restore sp */
      asm(" movl a1@(4),a6");		/* restore fp */
      asm(" moveml a1@(8),#0x3cfc");	/* restore d2-d7, a2-a6 */
      }
#endif
#ifdef i386
   asm("  movl 8(%ebp),%eax");
   asm("  movl %esp,0(%eax)");
   asm("  movl %ebp,4(%eax)");
   asm("  movl 12(%ebp),%eax");
   if (first == 0) {            /* this is the first activation */
      asm("  movl 0(%eax),%esp");
      asm("  movl $0,%ebp");
      new_context(0, 0);
      syserr("interp() returned in coswitch");
      }
   else {
      asm("  movl 0(%eax),%esp");
      asm("  movl 4(%eax),%ebp");
      }
#endif
}
