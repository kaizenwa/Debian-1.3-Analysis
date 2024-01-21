/*
\funcref{fun\_gets}{void fun\_gets ()}
    {}
    {}
    {}
    {fun\_getch()}
    {fungets.c}
    {

        This function reads in a string and returns it in the {\em reg} return
        register as an {\em e\_str} value.

    }
*/

#include "icm-exec.h"

void fun_gets ()
{
    char
        buffer [255];

    fflush (stdout);
    reg = newvar (e_str);
    reg.vu.i->ls.str = xstrdup (gets (buffer));
}
