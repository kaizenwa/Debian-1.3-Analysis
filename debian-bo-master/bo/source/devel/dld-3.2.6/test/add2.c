#include <ansidecl.h>
#include <gnu-stabs.h>


extern int x;

void __add2() {
    x+=2;
}

function_alias(add2, __add2, void, (),
	       DEFUN(add2, (),
		     void))
