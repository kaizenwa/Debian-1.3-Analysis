/*
 * This is the co-expression context switch for the Unisys 7000/40
 * under Berkeley 4.3bsd.  The 7000/40 is really a CCI Power 6/32
 * and its codename is tahoe.
 */

/*
 * coswitch
 */

/* >coswitch */
coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
   {
   asm("  movl 4(fp),r0");
   asm("  movl 8(fp),r1");
   asm("  moval 0(sp),0(r0)");	/* The sp can't be a direct source address. */
   asm("  movl fp,4(r0)");
   asm("  movl r12,8(r0)");
   asm("  movl r11,16(r0)");
   asm("  movl r10,20(r0)");
   asm("  movl r9,24(r0)");
   asm("  movl r8,28(r0)");
   asm("  movl r7,32(r0)");
   asm("  movl r6,36(r0)");
   asm("  movl r5,40(r0)");
   asm("  movl r4,44(r0)");		/*  It may not be necessary to      */
   asm("  movl r3,48(r0)");		/*     save all these registers.    */
   asm("  movl r2,52(r0)");
   if (first == 0) {		/* this is the first activation */
      asm("  movl 0(r1),sp");
      asm("  clrl fp");
      new_context(0, 0);
      syserr("new_context() returned in coswitch");
      }
   else {
      asm(" movl 0(r1),sp");
      asm(" movl 4(r1),fp");
      asm(" movl 8(r1),r12");
      asm(" movl 16(r1),r11");
      asm(" movl 20(r1),r10");
      asm(" movl 24(r1),r9");
      asm(" movl 28(r1),r8");
      asm(" movl 32(r1),r7");
      asm(" movl 36(r1),r6");
      asm(" movl 40(r1),r5");
      asm(" movl 44(r1),r4");
      asm(" movl 48(r1),r3");
      asm(" movl 52(r1),r2");
      }
   }
/* <coswitch */
