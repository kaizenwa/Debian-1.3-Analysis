#include <stdio.h>
/*
 * coswitch
 */
coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
   asm("  l  2,64(11)");
   asm("  l  3,68(11)");
   asm("  st 13,0(2)");
   asm("  st 12,4(2)");
   asm("  st 11,8(2)");
   asm("  st 10,12(2)");
   asm("  st 9,16(2)");
   asm("  st 14,20(2)");
   asm("  st 15,24(2)");
   asm("  st 4,28(2)");
   asm("  st 5,32(2)");
   asm("  st 6,36(2)");
   asm("  st 7,40(2)");
   asm("  st 8,44(2)");
   if (first == 0) { /* this is first activation */
      asm("  l  3,68(11)");
      asm("  l 13,0(3)");
      new_context(0, 0);
      syserr("new_context() returned in coswitch");
      }
   else {
   asm("  l  3,68(11)");
   asm("  l 13,0(3)");
   asm("  l 12,4(3)");
   asm("  l 11,8(3)");
   asm("  l 10,12(3)");
   asm("  l 9,16(3)");
   asm("  l 14,20(3)");
   asm("  l 15,24(3)");
   asm("  l 4,28(3)");
   asm("  l 5,32(3)");
   asm("  l 6,36(3)");
   asm("  l 7,40(3)");
   asm("  l 8,44(3)");
      }
}
   
