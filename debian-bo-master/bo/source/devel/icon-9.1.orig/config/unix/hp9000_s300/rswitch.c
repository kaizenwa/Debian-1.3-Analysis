/*
 * coswitch
 */
coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
  /* save sp & other registers in old_cs */
  asm(" mov.l 8(%a6),%a0");
  asm(" mov.l %sp,(%a0)");
  asm(" movm.l &0x7cfc,4(%a0)");
  if (first == 0) {
    /* load sp, clear other registers */
    asm(" mov.l 12(%a6),%a1");
    asm(" mov.l (%a1),%sp");
    new_context(0,0);
    syserr("new_context() returned in coswitch");
  } else {
    /* load sp & other registers from new_cs */
    asm(" mov.l 12(%a6),%a1");
    asm(" mov.l (%a1),%sp");
    asm(" movm.l 4(%a1),&0x7cfc");
  }
  /* runerr(401, 0); */
}
