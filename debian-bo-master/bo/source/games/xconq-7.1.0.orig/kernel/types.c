/* Data structures defining the properties of types.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "config.h"
#include "misc.h"
#include "lisp.h"
#include "game.h"

#ifndef SPECIAL

#ifdef offsetof
#define OFFS(TYPE,FLD) (offsetof(TYPE, FLD))
#else
#define OFFS(TYPE,FLD)  \
  ((unsigned int) (((char *) (&(((TYPE *) NULL)->FLD))) - ((char *) NULL)))
#endif

PropertyDefn utypedefns[] = {

#undef  DEF_UPROP_I
#define DEF_UPROP_I(NAME,FNAME,DOC,SLOT,LO,DFLT,HI)  \
    { NAME, FNAME, NULL, NULL, OFFS(Utype, SLOT), DOC, DFLT, NULL, NULL, LO, HI },
#undef  DEF_UPROP_S
#define DEF_UPROP_S(NAME,FNAME,DOC,SLOT,DFLT)  \
    { NAME, NULL, FNAME, NULL, OFFS(Utype, SLOT), DOC,    0, DFLT, NULL,  0,  0 },
#undef  DEF_UPROP_L
#define DEF_UPROP_L(NAME,FNAME,DOC,SLOT)  \
    { NAME, NULL, NULL, FNAME, OFFS(Utype, SLOT), DOC,    0, NULL, NULL,  0,  0 },

#include "utype.def"

    { NULL }
};

PropertyDefn mtypedefns[] = {

#undef  DEF_MPROP_I
#define DEF_MPROP_I(NAME,FNAME,DOC,SLOT,LO,DFLT,HI)  \
    { NAME, FNAME, NULL, NULL, OFFS(Mtype, SLOT), DOC, DFLT, NULL, NULL, LO, HI },
#undef  DEF_MPROP_S
#define DEF_MPROP_S(NAME,FNAME,DOC,SLOT,DFLT)  \
    { NAME, NULL, FNAME, NULL, OFFS(Mtype, SLOT), DOC,    0, DFLT, NULL,  0,  0 },
#undef  DEF_MPROP_L
#define DEF_MPROP_L(NAME,FNAME,DOC,SLOT)  \
    { NAME, NULL, NULL, FNAME, OFFS(Mtype, SLOT), DOC,    0, NULL, NULL,  0,  0 },

#include "mtype.def"

    { NULL }
};

PropertyDefn ttypedefns[] = {

#undef  DEF_TPROP_I
#define DEF_TPROP_I(NAME,FNAME,DOC,SLOT,LO,DFLT,HI)  \
    { NAME, FNAME, NULL, NULL, OFFS(Ttype, SLOT), DOC, DFLT, NULL, NULL, LO, HI },
#undef  DEF_TPROP_S
#define DEF_TPROP_S(NAME,FNAME,DOC,SLOT,DFLT)  \
    { NAME, NULL, FNAME, NULL, OFFS(Ttype, SLOT), DOC,    0, DFLT, NULL,  0,  0 },
#undef  DEF_TPROP_L
#define DEF_TPROP_L(NAME,FNAME,DOC,SLOT)  \
    { NAME, NULL, NULL, FNAME, OFFS(Ttype, SLOT), DOC,    0, NULL, NULL,  0,  0 },

#include "ttype.def"

    { NULL }
};

#undef  DEF_UPROP_I
#define DEF_UPROP_I(name,FNAME,doc,SLOT,lo,dflt,hi)  \
  int FNAME(u) int u; { checku(u); return utypes[u].SLOT; }
#undef  DEF_UPROP_S
#define DEF_UPROP_S(name,FNAME,doc,SLOT,dflt)  \
  char *FNAME(u) int u; { checku(u); return utypes[u].SLOT; }
#undef  DEF_UPROP_L
#define DEF_UPROP_L(name,FNAME,doc,SLOT)  \
  Obj *FNAME(u) int u; { checku(u); return utypes[u].SLOT; }

#include "utype.def"

/* Define functions for all material type properties. */

#undef  DEF_MPROP_I
#define DEF_MPROP_I(name,FNAME,doc,SLOT,lo,dflt,hi)  \
  int FNAME(m) int m; { checkm(m); return mtypes[m].SLOT; }
#undef  DEF_MPROP_S
#define DEF_MPROP_S(name,FNAME,doc,SLOT,dflt)  \
  char *FNAME(m) int m; { checkm(m);  return mtypes[m].SLOT; }
#undef  DEF_MPROP_L
#define DEF_MPROP_L(name,FNAME,doc,SLOT)  \
  Obj *FNAME(m) int m; { checkm(m);  return mtypes[m].SLOT; }

#include "mtype.def"

/* Define functions for all terrain type properties. */

#undef  DEF_TPROP_I
#define DEF_TPROP_I(name,FNAME,doc,SLOT,lo,dflt,hi)  \
  int FNAME(t) int t; { checkt(t); return ttypes[t].SLOT; }
#undef  DEF_TPROP_S
#define DEF_TPROP_S(name,FNAME,doc,SLOT,dflt)  \
  char *FNAME(t) int t; { checkt(t); return ttypes[t].SLOT; }
#undef  DEF_TPROP_L
#define DEF_TPROP_L(name,FNAME,doc,SLOT)  \
  Obj *FNAME(t) int t; { checkt(t); return ttypes[t].SLOT; }

#include "ttype.def"

#endif

/* Acquire a unit type extension property by name. */

/* (At any given time, there may be no parts of Xconq that use extension
   properties, since releases will usually integrate all properties into
   the normal set of property definitions ([umt]type.def).  However, these
   extension accessors should be kept around, so that they're available
   when needed.) */

Obj *
get_u_extension(u, name, dflt)
int u;
char *name;
Obj *dflt;
{
    Obj *ext = u_extensions(u), *rest, *bdg;

    for_all_list(ext, rest) {
	bdg = car(rest);
	if (consp(bdg)
	    && symbolp(car(bdg))
	    && strcmp(c_string(car(bdg)), name) == 0) {
	    return cadr(bdg);
	}
    }
    return dflt;
}

/* Acquire a material type extension property by name. */

Obj *
get_m_extension(m, name, dflt)
int m;
char *name;
Obj *dflt;
{
    Obj *ext = m_extensions(m), *rest, *bdg;

    for_all_list(ext, rest) {
	bdg = car(rest);
	if (consp(bdg)
	    && symbolp(car(bdg))
	    && strcmp(c_string(car(bdg)), name) == 0) {
	    return cadr(bdg);
	}
    }
    return dflt;
}

/* Acquire a terrain type extension property by name. */

Obj *
get_t_extension(t, name, dflt)
int t;
char *name;
Obj *dflt;
{
    Obj *ext = t_extensions(t), *rest, *bdg;

    for_all_list(ext, rest) {
	bdg = car(rest);
	if (consp(bdg)
	    && symbolp(car(bdg))
	    && strcmp(c_string(car(bdg)), name) == 0) {
	    return cadr(bdg);
	}
    }
    return dflt;
}
