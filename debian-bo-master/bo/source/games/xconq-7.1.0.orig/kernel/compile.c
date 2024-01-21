/* Compiler for game modules.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "config.h"

#ifdef COMPILER

#include "misc.h"

#undef SPECIAL

#include "game.h"

void compile PARAMS ((void));

void
compile()
{
#ifdef __GNUC__
    /* (Decide what is to be precompiled?) */
/*    printf("#define SPECIAL\n\n");  */
    /* The numbers of types are identical to their max limits. */
    printf("#undef  MAXUTYPES\n");
    printf("#define MAXUTYPES (%d)\n", max(1, numutypes));
    printf("#define numutypes (%d)\n", numutypes);
/*    printf("const short numutypes = %d;\n", numutypes);  */
    printf("#undef  MAXMTYPES\n");
    printf("#define MAXMTYPES (%d)\n", max(1, nummtypes));
    printf("#define nummtypes (%d)\n", nummtypes);
/*    printf("const short nummtypes = %d;\n", nummtypes);  */
    printf("#undef  MAXTTYPES\n");
    printf("#define MAXTTYPES (%d)\n", max(1, numttypes));
    printf("#define numttypes (%d)\n", numttypes);
/*    printf("const short numttypes = %d;\n", numttypes);  */
    /* Now dump out definitions for Property accessors. */

#undef  DEF_VAR_I
#define DEF_VAR_I(str,FNAME,SETFNAME,doc,VAR,lo,DFLT,hi)  \
    printf("#define %s() (%d)\n", # FNAME, DFLT);  \
    printf("#define %s(v)  run_warning(\"can't set\")\n", # SETFNAME);
#undef  DEF_VAR_S
#define DEF_VAR_S(str,FNAME,SETFNAME,doc,VAR,DFLT)  \
    printf("#define %s() (\"%s\")\n", # FNAME, DFLT);  \
    printf("#define %s(v)  run_warning(\"can't set\")\n", # SETFNAME);
#undef  DEF_VAR_L
#define DEF_VAR_L(str,FNAME,SETFNAME,doc,VAR,DFLT)  \
    printf("#define %s() (DFLT != NULL ? lispnil : 0)\n", # FNAME);  \
    printf("#define %s(v)  run_warning(\"can't set\")\n", # SETFNAME);

#include "gvar.def"

/* Define functions for all unit type properties. */

#undef  DEF_UPROP_I
#define DEF_UPROP_I(name,fname,doc,slot,lo,DFLT,hi)  \
    printf("#define %s(u) (%d)\n", # fname, DFLT);
#undef  DEF_UPROP_S
#define DEF_UPROP_S(name,fname,doc,slot,DFLT)  \
    printf("#define %s(u) (\"%s\")\n", # fname, DFLT);
#undef  DEF_UPROP_L
#define DEF_UPROP_L(name,fname,doc,slot)  \
    printf("#define %s(u) (lispnil)\n", # fname);

#include "utype.def"

/* Define functions for all material type properties. */

#undef  DEF_MPROP_I
#define DEF_MPROP_I(name,fname,doc,slot,lo,DFLT,hi)  \
    printf("#define %s(u) (%d)\n", # fname, DFLT);
#undef  DEF_MPROP_S
#define DEF_MPROP_S(name,fname,doc,slot,DFLT)  \
    printf("#define %s(u) (\"%s\")\n", # fname, DFLT);
#undef  DEF_MPROP_L
#define DEF_MPROP_L(name,fname,doc,slot)  \
    printf("#define %s(u) (lispnil)\n", # fname);

#include "mtype.def"

/* Define functions for all terrain type properties. */

#undef  DEF_TPROP_I
#define DEF_TPROP_I(name,fname,doc,slot,lo,DFLT,hi)  \
    printf("#define %s(u) (%d)\n", # fname, DFLT);
#undef  DEF_TPROP_S
#define DEF_TPROP_S(name,fname,doc,slot,DFLT)  \
    printf("#define %s(u) (\"%s\")\n", # fname, DFLT);
#undef  DEF_TPROP_L
#define DEF_TPROP_L(name,fname,doc,slot)  \
    printf("#define %s(u) (lispnil)\n", # fname);

#include "ttype.def"

/* Define functions for all the tables. */

#undef  DEF_UU_TABLE
#define DEF_UU_TABLE(name,FNAME,doc,table,slot,lo,DFLT,hi)  \
    printf("#define %s(u1,u2) (%d)\n", # FNAME, DFLT);
#undef  DEF_UM_TABLE
#define DEF_UM_TABLE(name,FNAME,doc,table,slot,lo,DFLT,hi)  \
    printf("#define %s(u1,u2) (%d)\n", # FNAME, DFLT);
#undef  DEF_UT_TABLE
#define DEF_UT_TABLE(name,FNAME,doc,table,slot,lo,DFLT,hi)  \
    printf("#define %s(u1,u2) (%d)\n", # FNAME, DFLT);
#undef  DEF_TM_TABLE
#define DEF_TM_TABLE(name,FNAME,doc,table,slot,lo,DFLT,hi)  \
    printf("#define %s(t1,t2) (%d)\n", # FNAME, DFLT);
#undef  DEF_TT_TABLE
#define DEF_TT_TABLE(name,FNAME,doc,table,slot,lo,DFLT,hi)  \
    printf("#define %s(t1,t2) (%d)\n", # FNAME, DFLT);
#undef  DEF_MM_TABLE
#define DEF_MM_TABLE(name,FNAME,doc,table,slot,lo,DFLT,hi)  \
    printf("#define %s(t1,t2) (%d)\n", # FNAME, DFLT);

#include "table.def"

#endif /* __GNUC__ */
}

/* Compiler should disable unused init methods? */

#endif /* COMPILER */

