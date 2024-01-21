/*
 * Context switch for Dynix 3.0 on the Symmetry.
 *
 * the contents of the save areas are as follows:
 *
 *     word   byte    contents
 *     ----   ----    --------
 *       0       0     sp
 *       1       4     ebp (frame pointer)
 *       2       8     ebx
 *       3      12     esi
 *       4      16     edi
 *  
 * compilers do not expect eax, ecx, and edx to survive across
 * function calls, so they are not saved
 */

/*
 * coswitch
 */

static int *ecx_save;

int coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{

   asm("movl  8(%ebp), %eax");	/* eax gets old_cs */
   asm("movl 12(%ebp), %ecx");	/* ecx gets new_cs */

   asm("movl %esp,  0(%eax)"); 	/* save  sp in cstate[0] */
   asm("movl %ebp,  4(%eax)");	/* save  fp in cstate[1] */
   asm("movl %ebx,  8(%eax)");  /* save ebx in cstate[2] */
   asm("movl %esi, 12(%eax)");  /* save esi in cstate[3] */
   asm("movl %edi, 16(%eax)");  /* save edi in cstate[4] */
   asm("movl %ecx, _ecx_save"); /* scratch register could get hosed by
				 * if test */


   if (first == 0) {		/* this is the first activation */

      asm("movl _ecx_save, %ecx");
      asm("movl  0(%ecx), %esp");	/* load  sp from cstate[0] */
      asm("movl $0,	  %ebp");	/* zero  fp */

      new_context(0,0);
      syserr("new_context() returned in coswitch");

   } else {

      asm("movl _ecx_save, %ecx");
      asm("movl  0(%ecx), %esp");	/* load  sp from cstate[0] */
      asm("movl  4(%ecx), %ebp");	/* load ebp from cstate[1] */
      asm("movl  8(%ecx), %ebx");	/* load ebx from cstate[2] */
      asm("movl 12(%ecx), %esi");	/* load esi from cstate[3] */
      asm("movl 16(%ecx), %edi");	/* load edi from cstate[4] */

   }

}
