coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
  asm("movd 8(fp), r0");
  asm("movd 12(fp), r1");

  asm("sprd sp, 0(r0)");
  asm("sprd fp, 4(r0)");
  asm("movd r7, 8(r0)");
  asm("movd r6, 12(r0)");
  asm("movd r5, 16(r0)");
  asm("movd r4, 20(r0)");
  asm("movd r3, 24(r0)");
  asm("movf f7, 28(r0)");
  asm("movf f6, 32(r0)");
  asm("movf f5, 36(r0)");
  asm("movf f4, 40(r0)");

  if (first == 0) {		/* i.e. this is the first activation */

    asm("lprd sp, 0(r1)");
    asm("lprd fp, 0");
    new_context(0,0);
    syserr("new_context() returned in coswitch");

  } else {

    asm("lprd sp, 0(r1)");
    asm("lprd fp, 4(r1)");
    asm("movd 8(r1), r7");
    asm("movd 12(r1), r6");
    asm("movd 16(r1), r5");
    asm("movd 20(r1), r4");
    asm("movd 24(r1), r3");
    asm("movf 28(r1), f7");
    asm("movf 32(r1), f6");
    asm("movf 36(r1), f5");
    asm("movf 40(r1), f4");

  }
}
