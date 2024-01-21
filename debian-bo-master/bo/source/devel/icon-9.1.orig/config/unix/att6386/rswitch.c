#include "../h/config.h"
#include "../h/rt.h"

/*
 * coswitch
 */
coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
   asm("  movl 8(%ebp),%eax");
   asm("  movl %ebp,8(%eax)");
   asm("  movl %esp,12(%eax)");
   asm("  movl %esi,16(%eax)");
   asm("  movl %edi,20(%eax)");
   asm("  movl %ebx,24(%eax)");
   if (first == 0) { /* this is first activation */
      asm("  movl 12(%ebp),%edx");
      asm("  movl (%edx),%esp");
      asm("  xor %eax,%eax");
      asm("  movl %eax,%ebp");
      new_context(0, 0);
      syserr("new_context() returned in coswitch");
      }
   else {
      asm("  movl 12(%ebp),%edx");
      asm("  movl 8(%edx),%ebp");
      asm("  movl 12(%edx),%esp");
      asm("  movl 16(%edx),%esi");
      asm("  movl 20(%edx),%edi");
      asm("  movl 24(%edx),%ebx");
      }
}
