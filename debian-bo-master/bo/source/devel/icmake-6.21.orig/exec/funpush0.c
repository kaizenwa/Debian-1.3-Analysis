/*
\funcref{fun\_push\_0}{void fun\_push\_0 ()}
    {}
    {}
    {push()}
    {fun\_push\_1()}
    {funpush0.c}
    {
        This function serves the evaluation of logical expressions. It is
        executed when an {\em op\_push\_0} opcode is found in the binary
        makefile. A variable of type {\em e\_int} is pushed, while its {\em
        vu.intval} field is set to zero.
    }
*/

#include "icm-exec.h"

void fun_push_0 ()
{
    VAR_
        tmp;

    tmp.type = e_int;
    tmp.vu.intval = 0;

    push (tmp);
}
