/* Definitions for GNU Rez.
   Copyright (C) 1995 Free Software Foundation, Inc.

This file is part of GNU Rez.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"

#include "ansidecl.h"

#include "libiberty.h"

#ifdef MPW
#include <stdlib.h>
#endif

#ifdef MPW
#include <Types.h>
#include <Files.h>
#include <Memory.h>
#include <Resources.h>
#include <Errors.h>
#include <CursorCtl.h>
#else
#include "mactypes.h"
#include <unistd.h>
#endif

#include <stdio.h>
#include <setjmp.h>

struct resource_header {
  long data_offset;
  long map_offset;
  long data_size;
  long map_size;
  struct resource_map *map;
};

struct resource_map {
  char header_copy[16];
  char **nextmaphandle;
  short refnum;
  short fork_attrs;
  short type_list_offset;
  short name_list_offset;
  short numtypes;
  struct resource_type_list *types;
  struct resource *refs;
  struct resource *resources;
  struct resource_map *next;
};

struct resource_type_list {
  long type;
  short numresources;
  struct resource *resources;
  struct resource_type_list *next;
  long offset;
  short resource_count;
  long reflist_offset;
};

struct resource {
  long type;
  short id;
  char *name;
  char attrs;
  int size;
  int allocated;
  char *data;
  long data_offset;
  long ref_offset;
  long name_offset;
  char **handle;
  struct resource *tnext;
  struct resource *next;
};

extern struct resource_header resource_fork_header;

extern struct resource *current_resource;

enum expr_code {
  e_num,
  e_str,
  e_sym,
  e_neg,
  e_not,
  e_cmp,
  e_add,
  e_sub,
  e_mul,
  e_div,
  e_mod,
  e_lsh,
  e_rsh,
  e_lt,
  e_gt,
  e_leq,
  e_geq,
  e_eq,
  e_ne,
  e_and,
  e_or,
  e_xor,
  e_aan,
  e_oor
#ifdef MPW_C
  , expr_code_intifier = 1000000
#endif
};

struct expr {
  enum expr_code code;
  int numfields;
  union {
    long num;
    char *str;
    struct expr *exp;
  } fields[4];
};

struct rspec {
  long Type;
  char type[4];
  struct expr *type_expr;
  short id;
  int id_given;
  struct expr *id_expr;
  short id2;
  char name[256];
  struct expr *name_expr;
  char attrs;
  int attrs_given;
  struct expr *attrs_expr;
};

enum rtype_code {
  rt_nothing,
  rt_type,
  rtf_boolean,
  rtf_numeric,
  rtf_char,
  rtf_string,
  rtf_point,
  rtf_rect,
  rtf_array,
  rtf_switch,
  rtf_fill,
  rtf_align,
  rts_label,
  rts_symbolic,
  rt_last
#ifdef MPW_C
  , rtype_code_intifier = 1000000
#endif
};

struct rtype {
  long type;
  struct rspec *rspec;
};

struct rtype_node {
  enum rtype_code code;
  int subcode;
  struct rspec *rspec;
  int size;
  struct expr *length_expr;
  struct expr *const_expr;
  struct expr *symbol;
  struct rtype_node *symbolics;
  struct rtype_node *sub;
  struct rtype_node *next;
};

extern int debug;

extern char *version_string;

extern struct rtype_node *current_type;

extern struct rtype_node *field_stack[];

extern int field_stack_top;

extern struct resource_map *create_new_resource_map ();

extern struct resource *create_resource ();

extern struct rtype_node *find_type_definition ();

