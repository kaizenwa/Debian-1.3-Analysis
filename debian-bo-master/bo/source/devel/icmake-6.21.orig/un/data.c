#include "icmun.h"

BIN_HEADER_
    *headerp;

char
    *funname [] =
    {
        "arghead",
        "argtail",
        "ascii_str2int",
        "ascii_int2str",

        "change_base",
        "chdir",
        "change_ext",
        "cmdhead",
        "cmdtail",
        "change_path",

        "echo",
        "list_element",
        "exec",
        "execute",
        "exists",

        "strtok",
        "fgets",
        "fprintf",

        "get_base",
        "getch",
        "gets",
        "get_ext",
        "get_path",

        "makelist",

        "printf",
        "putenv",

        "sizeoflist",
        "stat",
        "string_element",
        NULL,                               /* occupied by strlwr */
        NULL,                               /* occupied by strupr */
        "system",
        NULL,				    /* occupied by strlen */
        NULL,				    /* occupied by substr */
        "getpid",
    };

FILE
    *infile;

INT8
    *local_types;

UNS16
    curoffs,
    nvar;

VAR_
    *var;

void
    (*procfun[]) ARG ((void)) =
    {
        fun_jmp,
        fun_jmp_false,
        fun_jmp_true,
        fun_push_1_jmp_end,
        fun_push_0,
        fun_push_imm,
        fun_push_strconst,
        fun_push_var,
        fun_push_reg,
        fun_pop_var,
        fun_umin,
        fun_atoi,
        fun_itoa,
        fun_atol,
        fun_mul,
        fun_div,
        fun_mod,
        fun_add,
        fun_sub,
        fun_eq,
        fun_neq,
        fun_sm,
        fun_gr,
        fun_younger,
        fun_older,
        fun_smeq,
        fun_greq,
        fun_call_rss,
        fun_asp,
        fun_exit,
        fun_copy_var,
        fun_inc,
        fun_dec,
        fun_call,
        fun_frame,
        fun_ret,
        fun_pop_reg,
        fun_band,
        fun_bor,
        fun_bnot,
        fun_xor,
        fun_shl,
        fun_shr,
        /* fun_hlt : bogus value... op_hlt does not really exist */
    };
