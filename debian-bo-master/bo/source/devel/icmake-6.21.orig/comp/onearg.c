/*
                                O N E A R G . C
*/

#include "iccomp.h"

ESTRUC_ *onearg(type, arg)
    E_TYPE_
        type;
    ESTRUC_
        *arg;
{
    register int
        ok;

    etoc(arg);                               /* arg to stack */

    switch (type)
    {
        case f_ascii_int:
            if ( (ok = test_type(arg, e_str | e_int)) )
            {
                if (test_type(arg, e_int))  /* int received    */
                    type = f_ascii_str;     /* string returned */
            }

        break;

        case f_sizeoflist:
            ok = test_type(arg, e_list);
        break;

        case f_echo:
            ok = test_type(arg, e_int);
        break;

        default:
        /*
            case f_putenv:
            case f_exists:
            case f_cmd_head:
            case f_cmd_tail:
            case f_arg_head:
            case f_arg_tail:
            case f_g_path:
            case f_g_base:
            case f_g_ext:
            case f_strlen:
        */
            ok = test_type(arg, e_str);
    }

    if (ok)
    {
        if (type == f_strlen)
            callhidden(he_strlen, arg);
        else
            callrss(arg, type);
    }
    else
        semantic(type_conflict, funstring[type]);

    return (arg);
}
