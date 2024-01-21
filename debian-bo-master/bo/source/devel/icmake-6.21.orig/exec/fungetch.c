/*
\funcref{fun\_getch}{void fun\_getch ()}
    {}
    {}
    {}
    {fun\_gets()}
    {fungetch.c}
    {

        This function reads in one key and returns it in the {\em reg} return
        register as an {\em e\_str} value.

    }
*/

#include "icm-exec.h"

void fun_getch ()
{
    static char
        buf [200];
#ifdef MSDOS
    buf [0] = (char) getch ();
#else
    gets (buf);
    buf [1] = '\0';
#endif

    reg = newvar (e_str);
    reg.vu.i->ls.str = xstrdup (buf);
}
