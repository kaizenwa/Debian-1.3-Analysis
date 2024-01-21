/* Definitions for Lisp objects in Xconq.
   Copyright (C) 1989, 1991, 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#ifndef LISP_H
#define LISP_H

/* Lisp objects in Xconq are pretty basic, since they are used only for
   game definition and (usually) not the core computations. */

/* The different types of Lisp objects. */

enum lisptype {
  NIL,
  CONS,
  NUMBER,
  STRING,
  SYMBOL,
  UTYPE,
  MTYPE,
  TTYPE,
  POINTER,
  EOFOBJ
  };

/* Declaration of a cons cell. */

struct a_cons {
    struct a_obj *car;
    struct a_obj *cdr;
};

/* A symbol includes its index and a pointer to its binding. */

struct a_symbol {
    struct a_symentry *symentry;
    struct a_obj *value;
};

/* A pointer is an address with associated name.  Interpretation
   and usage is up to the context. */

struct a_pointer {
    struct a_obj *sym;
    char *data;
};

/* The basic Lisp object.  This should be small. */

typedef struct a_obj {
    enum lisptype type;          /* type of the object */
    union {
	int num;       /* numeric value */
	char *str;     /* string value */
	struct a_symbol sym;
	struct a_cons cons;
	struct a_pointer ptr;
    } v;               /* the "content" of the object */
} Obj;

/* The symbol table is the way to map names into symbols. */

typedef struct a_symentry {
    char *name;
    struct a_obj *symbol;
    char constantp;
    struct a_symentry *next;
} Symentry;

/* A stream is just a union of string pointer and file pointer. */

enum strmtype { stringstrm, filestrm };

typedef struct a_strm {
    enum strmtype type;
    union {
	char *sp;
	FILE *fp;
	} ptr;
} Strm;

/* Enum of all the random keywords. */

enum keywords {

#undef  DEF_KWD
#define DEF_KWD(name,CODE)  CODE,

#include "keyword.def"

    LAST_KEYWORD
};

#define match_keyword(ob,key) \
  (symbolp(ob) && strcmp(c_string(ob), keyword_name(key)) == 0)

#define for_all_list(lis,rest)  \
  for (rest = (lis); rest != lispnil; rest = cdr(rest))

/* All the Lisp interface declarations. */

extern Obj *lispnil;
extern Obj *lispeof;

extern void init_lisp PARAMS ((void));
extern int strmgetc PARAMS ((Strm *strm));
extern void strmungetc PARAMS ((int ch, Strm *strm));
extern Obj *read_form PARAMS ((FILE *fp, int *p1, int *p2));
extern Obj *read_form_from_string PARAMS ((char *str, int *p1, int *p2));
extern void sprintf_context PARAMS ((char *buf, int n, int *start, int *end, char *context));
extern Obj *read_form_aux PARAMS ((Strm *strm));
extern Obj *read_list PARAMS ((Strm *strm));
extern int read_delimited_text PARAMS ((Strm *strm, char *delim, int spacedelimits, int eofdelimits));
extern int length PARAMS ((Obj *list));
extern Obj *new_string PARAMS ((char *str));
extern Obj *new_number PARAMS ((int num));
extern Obj *new_utype PARAMS ((int u));
extern Obj *new_mtype PARAMS ((int r));
extern Obj *new_ttype PARAMS ((int t));
extern Obj *new_pointer PARAMS ((Obj *sym, char *ptr));
extern Obj *cons PARAMS ((Obj *x, Obj *y));
extern void type_warning PARAMS ((char *funname, Obj *x, char *typename, Obj *subst));
extern Obj *car PARAMS ((Obj *x));
extern Obj *cdr PARAMS ((Obj *x));
extern Obj *cadr PARAMS ((Obj *x));
extern Obj *cddr PARAMS ((Obj *x));
extern Obj *caddr PARAMS ((Obj *x));
extern void set_cdr PARAMS ((Obj *x, Obj *v));
extern char *c_string PARAMS ((Obj *x));
extern int c_number PARAMS ((Obj *x));
extern Obj *intern_symbol PARAMS ((char *str));
extern Obj *symbol_value PARAMS ((Obj *sym));
extern Obj *setq PARAMS ((Obj *sym, Obj *x));
extern void makunbound PARAMS ((Obj *sym));
extern void flag_as_constant PARAMS ((Obj *sym));
extern int constantp PARAMS ((Obj *sym));
extern int numberp PARAMS ((Obj *x));
extern int stringp PARAMS ((Obj *x));
extern int symbolp PARAMS ((Obj *x));
extern int consp PARAMS ((Obj *x));
extern int utypep PARAMS ((Obj *x));
extern int mtypep PARAMS ((Obj *x));
extern int ttypep PARAMS ((Obj *x));
extern int pointerp PARAMS ((Obj *x));
extern int boundp PARAMS ((Obj *sym));
extern int numberishp PARAMS ((Obj *x));
extern int listp PARAMS ((Obj *x));
extern int equal PARAMS ((Obj *x, Obj *y));
extern int member PARAMS ((Obj *x, Obj *lis));
extern Obj *elt PARAMS ((Obj *lis, int n));
extern Obj *reverse PARAMS ((Obj *lis));
extern Obj *find_at_key PARAMS ((Obj *lis, char *key));
extern Obj *replace_at_key PARAMS ((Obj *lis, char *key, Obj *newval));
extern void fprintlisp PARAMS ((FILE *fp, Obj *obj));
extern void fprint_list PARAMS ((FILE *fp, Obj *obj));
extern void sprintlisp PARAMS ((char *buf, Obj *obj));
extern void sprint_list PARAMS ((char *buf, Obj *obj));
extern void dlisp PARAMS ((Obj *x));
extern void print_form_and_value PARAMS ((FILE *fp, Obj *form));
extern Obj *append_two_lists PARAMS ((Obj *x1, Obj *x2));
extern Obj *append_lists PARAMS ((Obj *lis));
extern Obj *remove_from_list PARAMS ((Obj *elt, Obj *lis));
extern void push_binding PARAMS ((Obj **lis, Obj *key, Obj *val));
extern void push_cdr_binding PARAMS ((Obj **lis, Obj *key, Obj *val));
extern void push_int_binding PARAMS ((Obj **lis, Obj *key, int val));
extern void push_key_binding PARAMS ((Obj **lis, int key, Obj *val));
extern void push_key_cdr_binding PARAMS ((Obj **lis, int key, Obj *val));
extern void push_key_int_binding PARAMS ((Obj **lis, int key, int val));
extern Obj *eval PARAMS ((Obj *x));
extern Obj *eval_symbol PARAMS ((Obj *x));
extern Obj *eval_list PARAMS ((Obj *x));
extern int eval_boolean_expression PARAMS ((Obj *expr, int (*fn)(Obj *), int dflt));
extern int interpolate_in_list PARAMS ((int val, Obj *lis, int *rslt));
extern int interpolate_in_list_ext PARAMS ((int val, Obj *lis,
			int mindo, int minval, int minrslt,
			int maxdo, int maxval, int maxrslt,
			int *rslt));

/* Functions that the Lisp code needs to have defined. */

extern void init_warning PARAMS ((char *str, ...));
extern void low_init_warning PARAMS ((char *str));
extern void init_error PARAMS ((char *str, ...));
extern void low_init_error PARAMS ((char *str));
extern void run_warning PARAMS ((char *str, ...));
extern void low_run_warning PARAMS ((char *str));
extern void run_error PARAMS ((char *str, ...));
extern void low_run_error PARAMS ((char *str));
extern void announce_read_progress PARAMS ((void));
extern int keyword_code PARAMS ((char *str));
extern char *keyword_name PARAMS ((enum keywords k));
extern int lazy_bind PARAMS ((Obj *sym));
extern void init_predefined_symbols PARAMS ((void));

#endif /* LISP_H */
