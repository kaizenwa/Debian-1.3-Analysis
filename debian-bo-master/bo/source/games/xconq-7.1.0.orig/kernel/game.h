/* Interface between game parameters and the rest of Xconq.
   Copyright (C) 1992, 1993, 1994 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This file defines the structures that are filled in with type info,
   one for each type, plus the declarations for all functions and variables. */

/* Numbers guaranteed to be invalid types in each category.  Should be
   careful that these don't overflow anything. */

#define NONUTYPE (MAXUTYPES)
#define NONMTYPE (MAXMTYPES)
#define NONTTYPE (MAXTTYPES)

/* Indices for each category of types. */

typedef enum {
  UTYP = 0,
  MTYP = 1,
  TTYP = 2
} Typetype;

/* The four roles for terrain. */

enum terrain_subtype {
    cellsubtype = 0,
    bordersubtype = 1,
    connectionsubtype = 2,
    coatingsubtype = 3
};

/* Ultimate limits on values in properties. */

#define PROPLO -32768
#define PROPHI 32767

/* Ultimate limits on values in tables. */

#define TABLO -32768
#define TABHI 32767

/* Ultimate limits on values of globals. */

#define VARLO -2000000000
#define VARHI 2000000000

/* If a specialized Xconq is being compiled, use the specialized .h
   instead of the general one.  All of the general game parameter
   machinery should go away and be replaced by either constant
   defaults or precalculated tables/formulas. */

#ifdef SPECIAL

#include "special.h"

#else

#include "lisp.h"

/* This is the structure representing info about a property
   of a type, such as a unit type's maximum speed. */

typedef struct propertydefn {
    char *name;
    int (*intgetter) PARAMS ((int));
    char *(*strgetter) PARAMS ((int));
    Obj *(*objgetter) PARAMS ((int));
    short offset;
    char *doc;
    short dflt;
    char *dfltstr;
    void (*dlftfn) PARAMS ((void));
    short lo, hi;
} PropertyDefn;

/* This is the structure with info about a table. */

typedef struct tabledefn {
    char *name;			/* name of the table */
    int (*getter) PARAMS ((int, int));  /* accessor function */
    char *doc;			/* documentation string */
    short **table;		/* pointer to table itself */
    short *cnst;		/* pointer to constant value, if table is constant */
    short dflt;			/* default value of entries */
    short lo, hi;		/* bounds of table value */
    char index1, index2;	/* type of row and column indices */
} TableDefn;

/* This is the structure with info about a global variable. */

typedef struct vardefn {
    char *name;			/* name of the global */
    int   (*intgetter) PARAMS ((void));  /* accessor if integer type */
    char *(*strgetter) PARAMS ((void));  /* accessor if string type */
    Obj  *(*objgetter) PARAMS ((void));  /* accessor if object type */
    void (*intsetter) PARAMS ((int));  /* setter if integer type */
    void (*strsetter) PARAMS ((char *));  /* setter if string type */
    void (*objsetter) PARAMS ((Obj *));  /* setter if object type */
    char *doc;			/* documentation string */
    int dflt;			/* default value if integer type */
    char *dfltstr;		/* default value if string type */
    void (*dfltfn) PARAMS ((void));  /* default value if object type */
    int lo, hi;			/* bounds of integer value */
} VarDefn;

extern short numutypes;
extern short nummtypes;
extern short numttypes;

typedef struct utype {

#undef  DEF_UPROP_I
#define DEF_UPROP_I(name,fname,doc,SLOT,lo,dflt,hi)  \
    short SLOT;
#undef  DEF_UPROP_S
#define DEF_UPROP_S(name,fname,doc,SLOT,dflt)  \
    char *SLOT;
#undef  DEF_UPROP_L
#define DEF_UPROP_L(name,fname,doc,SLOT)  \
    Obj *SLOT;

#include "utype.def"

} Utype;

/* Definition of material types. */

typedef struct mtype {

#undef  DEF_MPROP_I
#define DEF_MPROP_I(name,fname,doc,SLOT,lo,dflt,hi)  \
    short SLOT;
#undef  DEF_MPROP_S
#define DEF_MPROP_S(name,fname,doc,SLOT,dflt)  \
    char *SLOT;
#undef  DEF_MPROP_L
#define DEF_MPROP_L(name,fname,doc,SLOT)  \
    Obj *SLOT;

#include "mtype.def"

} Mtype;

/* Definition of terrain types. */

typedef struct ttype {

#undef  DEF_TPROP_I
#define DEF_TPROP_I(name,fname,doc,SLOT,lo,dflt,hi)  \
    short SLOT;
#undef  DEF_TPROP_S
#define DEF_TPROP_S(name,fname,doc,SLOT,dflt)  \
    char *SLOT;
#undef  DEF_TPROP_L
#define DEF_TPROP_L(name,fname,doc,SLOT)  \
    Obj *SLOT;

#include "ttype.def"

} Ttype;

/* The global data. */

typedef struct a_globals {

#undef  DEF_VAR_I
#define DEF_VAR_I(name,fname,setfname,doc,VAR,lo,dflt,hi)  \
    int VAR;
#undef  DEF_VAR_S
#define DEF_VAR_S(name,fname,setfname,doc,VAR,dflt)  \
    char *VAR;
#undef  DEF_VAR_L
#define DEF_VAR_L(name,fname,setfname,doc,VAR,dflt)  \
    Obj *VAR;

#include "gvar.def"

} Globals;

/* Declarations of the functions accessing and setting type properties. */

#undef  DEF_UPROP_I
#define DEF_UPROP_I(name,FNAME,doc,slot,lo,dflt,hi)  int FNAME PARAMS ((int u));
#undef  DEF_UPROP_S
#define DEF_UPROP_S(name,FNAME,doc,slot,dflt)  char *FNAME PARAMS ((int u));
#undef  DEF_UPROP_L
#define DEF_UPROP_L(name,FNAME,doc,slot)  Obj *FNAME PARAMS ((int u));

#include "utype.def"

#undef  DEF_MPROP_I
#define DEF_MPROP_I(name,FNAME,doc,slot,lo,dflt,hi)  int FNAME PARAMS ((int m));
#undef  DEF_MPROP_S
#define DEF_MPROP_S(name,FNAME,doc,slot,dflt)  char *FNAME PARAMS ((int m));
#undef  DEF_MPROP_L
#define DEF_MPROP_L(name,FNAME,doc,slot)  Obj *FNAME PARAMS ((int m));

#include "mtype.def"

#undef  DEF_TPROP_I
#define DEF_TPROP_I(name,FNAME,doc,slot,lo,dflt,hi)  int FNAME PARAMS ((int t));
#undef  DEF_TPROP_S
#define DEF_TPROP_S(name,FNAME,doc,slot,dflt)  char *FNAME PARAMS ((int t));
#undef  DEF_TPROP_L
#define DEF_TPROP_L(name,FNAME,doc,slot)  Obj *FNAME PARAMS ((int t));

#include "ttype.def"

#undef  DEF_VAR_I
#define DEF_VAR_I(str,FNAME,SETFNAME,doc,var,lo,dflt,hi)  \
  int FNAME PARAMS ((void));  \
  void SETFNAME PARAMS ((int val));
#undef  DEF_VAR_S
#define DEF_VAR_S(str,FNAME,SETFNAME,doc,var,dflt)  \
  char *FNAME PARAMS ((void));  \
  void SETFNAME PARAMS ((char *val));
#undef  DEF_VAR_L
#define DEF_VAR_L(str,FNAME,SETFNAME,doc,var,dflt)  \
  Obj *FNAME PARAMS ((void));  \
  void SETFNAME PARAMS ((Obj *val));

#include "gvar.def"

/* Declarations of table accessor functions and the globals
   for constant and filled-in tables. */

#undef  DEF_UU_TABLE
#define DEF_UU_TABLE(name,FNAME,doc,TABLE,CNST,lo,dflt,hi)  \
  int FNAME PARAMS ((int u1, int u2));  \
  extern short *TABLE, CNST;
#undef  DEF_UM_TABLE
#define DEF_UM_TABLE(name,FNAME,doc,TABLE,CNST,lo,dflt,hi)  \
  int FNAME PARAMS ((int u, int m));  \
  extern short *TABLE, CNST;
#undef  DEF_UT_TABLE
#define DEF_UT_TABLE(name,FNAME,doc,TABLE,CNST,lo,dflt,hi)  \
  int FNAME PARAMS ((int u, int t));  \
  extern short *TABLE, CNST;
#undef  DEF_TM_TABLE
#define DEF_TM_TABLE(name,FNAME,doc,TABLE,CNST,lo,dflt,hi)  \
  int FNAME PARAMS ((int t, int m));  \
  extern short *TABLE, CNST;
#undef  DEF_TT_TABLE
#define DEF_TT_TABLE(name,FNAME,doc,TABLE,CNST,lo,dflt,hi)  \
  int FNAME PARAMS ((int t1, int t2));  \
  extern short *TABLE, CNST;
#undef  DEF_MM_TABLE
#define DEF_MM_TABLE(name,FNAME,doc,TABLE,CNST,lo,dflt,hi)  \
  int FNAME PARAMS ((int m1, int m2));  \
  extern short *TABLE, CNST;

#include "table.def"

/* Declarations of the globals description structures. */

extern Globals globals;

extern Utype *utypes;

extern Mtype *mtypes;

extern Ttype *ttypes;

extern PropertyDefn utypedefns[];

extern PropertyDefn mtypedefns[];

extern PropertyDefn ttypedefns[];

extern TableDefn tabledefns[];

extern VarDefn vardefns[];

#endif /* SPECIAL */

/* The following definitions are valid for both general and specialized
   games. */

/* Macros for iterating over types. */

#define for_all_unit_types(v)      for (v = 0; v < numutypes; ++v)

#define for_all_material_types(v)  for (v = 0; v < nummtypes; ++v)

#define for_all_terrain_types(v)   for (v = 0; v < numttypes; ++v)

#define for_all_possible_unit_types(v)      for (v = 0; v < MAXUTYPES; ++v)

#define for_all_possible_material_types(v)  for (v = 0; v < MAXMTYPES; ++v)

#define for_all_possible_terrain_types(v)   for (v = 0; v < MAXTTYPES; ++v)

/* Macros encapsulating things about units. */

#define checku(x) if ((x) < 0 || (x) >= numutypes) utype_error(x);

#define checkm(x) if ((x) < 0 || (x) >= nummtypes) mtype_error(x);

#define checkt(x) if ((x) < 0 || (x) >= numttypes) ttype_error(x);

/* Fix eventually. */

/* (should say u_... or ..._type ?) */

#define actor(u) (u_acp(u) > 0)

#define mobile(u) (u_speed(u) > 0)

#define u_hp(u) (u_hp_max(u))

#define could_be_on(u,t)  \
  ((ut_capacity_x(u, t) > 0 || ut_size(u, t) <= t_capacity(t)))

#define could_live_on(u,t)  \
   (could_be_on(u, t) && !ut_vanishes_on(u, t) && !ut_wrecks_on(u, t))

#define could_carry(u1,u2)  \
  (uu_capacity_x(u1, u2) > 0 || uu_size(u2, u1) <= u_capacity(u1))

#define could_create(u1,u2) (uu_acp_to_create(u1, u2) > 0)

#define could_repair(u1, u2) (uu_repair(u1, u2) > 0)

#define could_hit(u1,u2) (uu_hit(u1, u2) > 0 || uu_fire_hit(u1, u2) > 0)

/* These need actual units rather than types. */

#define impassable(u, x, y) (!could_be_on((u)->type, terrain_at((x), (y))))

#define isbase(u) (u_is_base((u)->type))

#define base_builder(u) (u_is_base_builder((u)->type))

#define istransport(u) (u_is_transport((u)->type))

#define t_is_cell(t) (t_subtype(t) == cellsubtype)

#define t_is_border(t) (t_subtype(t) == bordersubtype)

#define t_is_connection(t) (t_subtype(t) == connectionsubtype)

#define t_is_coating(t) (t_subtype(t) == coatingsubtype)

#define is_unit_type(u) ((u) >= 0 && (u) < numutypes)

#define is_material_type(m) ((m) >= 0 && (m) < nummtypes)

#define is_terrain_type(t) ((t) >= 0 && (t) < numttypes)

extern short canaddutype;
extern short canaddmtype;
extern short canaddttype;

extern short tmputype;
extern short tmpmtype;
extern short tmpttype;

extern void utype_error  PARAMS ((int u));
extern void mtype_error  PARAMS ((int m));
extern void ttype_error  PARAMS ((int t));
extern void init_types PARAMS ((void));
extern void init_globals PARAMS ((void));
extern void default_unit_type PARAMS ((int x));
extern void default_material_type PARAMS ((int x));
extern void default_terrain_type PARAMS ((int x));
extern char *index_type_name();
extern void allocate_table PARAMS ((int tbl, int reset));
extern int numtypes_from_index_type PARAMS ((int x));
extern char *index_type_name PARAMS ((int x));

extern void set_g_synth_methods_default PARAMS ((void));
extern void set_g_side_lib_default PARAMS ((void));
extern Obj *get_u_extension PARAMS ((int u, char *name, Obj *dflt));
extern Obj *get_m_extension PARAMS ((int m, char *name, Obj *dflt));
extern Obj *get_t_extension PARAMS ((int t, char *name, Obj *dflt));
