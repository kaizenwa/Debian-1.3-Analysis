/*
                                C A L L R S S . C
*/

#include "iccomp.h"

void callrss(ESTRUC_ *e, FUNNR_ funnr, ...)
{
    register E_TYPE_
        type = e_reg | e_int;               /* default return type: intreg */
    register unsigned
        args = 1;                           /* most f()s having 1 argument */
    va_list
        marker;

    va_start(marker, funnr);

    gencode (e, op_call_rss, funnr);        /* call the function */

    switch (funnr)
    {
                                            /* 0 args, returning string */
        case f_getch:
        case f_gets:
            args--;
            type = e_str | e_reg;
        break;
                                            /* 0 args, returning int */
        case f_getpid:
            args--;
            type = e_int | e_reg;
        break;
                                            /* 1 arg, returning string */
        case f_ascii_str:
        case f_g_base:
        case f_g_ext:
        case f_g_path:
            type = e_str | e_reg;
        break;
                                            /* 2 arguments, returning int */
        case f_system:
            args++;
        break;
                                            /* 2 args, returning string */
        case f_chdir:
        case f_c_base:
        case f_c_ext:
        case f_c_path:
        case f_str_el:
        case f_element:
            args++;                         /* two arguments */
            type = e_str | e_reg;           /* returning string */
        break;

                                            /* 2 args, returning list */
        case f_stat:
        case f_fgets:                       /* list fgets(string, int) */
        case f_fields:                      /* list fields(string, string) */
        case f_makelist:                    /* list makelist(int, string)  */
            args++;
            type = e_list | e_reg;
        break;
                                            /* # args passed as argument */
        case f_execute:
        case f_exec:
        case f_fprintf:
        case f_printf:
            args = va_arg(marker, unsigned);
        break;

        /*
            default: functions having one arg returning an int

            case f_ascii_int:
            case f_putenv:
            case f_arg_head:
            case f_arg_tail:
            case f_cmd_head:
            case f_cmd_tail:
            case f_echo:
            case f_exists:
            case f_sizeoflist:
        */
    }
    if (args)
        gencode (e, op_asp, args);          /* add stack pointer */
    set_type(e, type);                      /* type of resulting expression */
}
