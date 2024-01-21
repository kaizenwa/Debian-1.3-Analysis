#include "../h/rt.h"

/*
 * coswitch for the Convex
 */
coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
    asm ("ld.w 0(ap),a1");
    asm ("ld.w 4(ap),a2");
    asm ("st.w sp,0(a1)");
    asm ("st.w fp,4(a1)");

    if (!first) {
	asm ("ld.w 0(a2),sp");
	asm ("ld.w #0,fp");
	new_context (0, 0);
	syserr ("new_context() returned in coswitch");}
    else {
        asm ("ld.w 0(a2),sp");
	asm ("ld.w 4(a2),fp");}
}
