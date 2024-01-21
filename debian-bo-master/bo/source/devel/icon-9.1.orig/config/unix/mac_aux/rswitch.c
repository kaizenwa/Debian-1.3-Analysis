/*
 * This is the co-expression context switch for the Macintosh II (etc.)
 * operating under A/UX 2.0. (??? KLUDGE ???)
 */

/*
 * coswitch
 */
coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
   asm("  mov.l 8(%a6),%a0");		/* a0 = old */
   asm("  mov.l 12(%a6),%a1");		/* a1 = new */

   asm("  mov.l %a7,(%a0)");		/* save sp in cstate[0] */
   asm("  mov.l %a6,4(%a0)");		/* save a6 (fp) in cstate[0] */
   asm("  movm.l &0x3cfc,8(%a0)");	/* store d2-d7, a2-a6 in old->cstate */
   if (first == 0) {	/* this is first activation */
      asm("  mov.l (%a1),%a7");
      asm("  mov.l &0,%a6");
      new_context(0, 0);
      syserr("new_context() returned in coswitch");
      }
   else {
      asm(" mov.l (%a1),%a7");		/* restore sp */
      asm(" mov.l 4(%a1),%a6");		/* restore fp */
      asm(" movm.l 8(%a1),&0x3cfc");	/* restore d2-d7, a2-a6 */
      }
}
