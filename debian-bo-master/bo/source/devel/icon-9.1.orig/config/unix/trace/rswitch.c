#include <stdio.h>
#include <setjmp.h>

/**
*** This code depends on the value for sp being saved by _setjmp in
*** the first word of the jump buffer.  Should this change revisions
*** will be needed.
***
*** Original Trace code by James Cowie.  Mods by JMO.
**/

coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
    jmp_buf JB; 
       
    if (!_setjmp(JB)) 
       *old_cs = *((int *) JB);

    if (first==0) {                    /* first invocation of coexpression */
       if (!_setjmp(JB)) {
           *((int *) JB) = *new_cs;     /* install new SP from *new_cs      */
           _longjmp(JB);
       }
       
       new_context(0,0);                    /* begin execution of coexpression  */
       fprintf(stderr,
                "\n\n*** ERROR *** new_context() returned in coswitch\n\n");  
    }
    else if (!_setjmp(JB)) {           /* resumption of coexpression */
       *((int *) JB) = *new_cs;         /* install new SP from *new_cs */
       _longjmp(JB);
    }
}
