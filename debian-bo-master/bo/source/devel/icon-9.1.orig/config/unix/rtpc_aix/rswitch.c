/*
 * For the IBM RT PC AIX
 */
coswitch(old_cs /* r2 */, new_cs /* r3 */, first)
int *old_cs, *new_cs;
int first;
{

        /*  cstate must have 19 words: sp, GPR's 6-15, FPR's 2-5 */

    asm("  st    1,0(2)         # Save sp");
    asm("  stm   6,4(2)         # Save GPR's");
    /*  following assumes C code doesn't care that I trash GPR's 11,12,13*/
    asm("  cal       12,44(2)   # FPR save area address");
    asm("  cal       11,4080    # FPR bit mask");
    asm("  cal       13,0x800(0)# fpfp address");
    asm("  l         13,284(13) # _FPsmr address");
    asm("  callr 13,0                       ");

    if (first == 0) { /* this is the first activation */

        asm("  l 1,0(3) # Get the new stack pointer");
        new_context(0,0);
        syserr("new_context() returned in coswitch");

    } else {

        asm("  l     1,0(3)         # get sp");
        asm("  cal       12,44(3)   # FPR save area address");
        asm("  cal       11,4080    # FPR bit mask");
        asm("  cal       13,0x800(0)# fpfp address");
        asm("  l         13,280(13) # _FPlmr address");
        asm("  callr 13,0                       ");
        asm("  lm    6,4(3)         # restore GPR's");
    }

}
