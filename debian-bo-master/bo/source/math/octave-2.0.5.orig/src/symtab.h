/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_symtab_h)
#define octave_symtab_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

#include "SLStack.h"

#include "str-vec.h"

#include "variables.h"

// Must be multiple of 2.
#define HASH_TABLE_SIZE 1024
#define HASH_MASK (HASH_TABLE_SIZE - 1)

class tree;
class tree_fvc;
class tree_builtin;
class tree_constant;
class tree_function;

class string_vector;

class symbol_def;
class symbol_record;
class symbol_record_info;
class symbol_table;

// Variables or functions.

class symbol_def
{
  friend class symbol_record;
  friend class symbol_record_info;

public:

  symbol_def (void);
  symbol_def (tree_constant *t);
  symbol_def (tree_builtin *t, unsigned fcn_type = 0);
  symbol_def (tree_function *t, unsigned fcn_type = 0);

  ~symbol_def (void);

  int is_variable (void) const;
  int is_function (void) const;
  int is_text_function (void) const;
  int is_mapper_function (void) const;
  int is_user_variable (void) const;
  int is_user_function (void) const;
  int is_builtin_variable (void) const;
  int is_builtin_function (void) const;
  int is_map_element (const string& elts) const;

  void define (tree_constant *t);
  void define (tree_builtin *t, unsigned fcn_type = 0);
  void define (tree_function *t, unsigned fcn_type = 0);

  void protect (void);
  void unprotect (void);
  void make_eternal (void);

  tree_fvc *def (void) const;
  string help (void) const;
  void document (const string& h);

  enum TYPE
    {
      UNKNOWN = 0,
      USER_FUNCTION = 1,
      USER_VARIABLE = 2,
      BUILTIN_FUNCTION = 4,
      TEXT_FUNCTION = 8,
      MAPPER_FUNCTION = 16,
      BUILTIN_VARIABLE = 32
    };

  friend maybe_delete (symbol_def *def);

private:

  unsigned type : 6;
  unsigned eternal : 1;
  unsigned read_only : 1;

  string help_string;
  tree_fvc *definition;
  symbol_def *next_elem;
  int count;

  void init_state (void);

  symbol_def (const symbol_def& sd);
  symbol_def& operator = (const symbol_def& sd);
};

// Individual records in a symbol table.

class
symbol_record
{
  friend class symbol_record_info;

public:
  symbol_record (void);
  symbol_record (const string& n, symbol_record *nxt = 0);

  ~symbol_record (void) { }

  string name (void) const;
  string help (void) const; 
  tree_fvc *def (void) const;

  void rename (const string& new_name);

  int is_function (void) const;
  int is_user_function (void) const;
  int is_text_function (void) const;
  int is_mapper_function (void) const;
  int is_builtin_function (void) const;
  int is_variable (void) const;
  int is_user_variable (void) const;
  int is_builtin_variable (void) const;
  int is_map_element (const string& elts) const;

  unsigned type (void) const;

  int is_defined (void) const;
  int is_read_only (void) const;
  int is_eternal (void) const;

  void protect (void);
  void unprotect (void);
  void make_eternal (void);

  void set_sv_function (sv_Function f);

  int define (tree_constant *t);
  int define (const octave_value& v);
  int define (tree_builtin *t, int text_fcn = 0);
  int define (tree_function *t, int text_fcn = 0);
  int define_as_fcn (const octave_value& v);
  int define_builtin_var (const octave_value& v);

  void document (const string& h);

  int clear (void);

  void alias (symbol_record *s, int force = 0);

  void mark_as_formal_parameter (void);
  int is_formal_parameter (void) const;

  void mark_as_linked_to_global (void);
  int is_linked_to_global (void) const;

  octave_value variable_value (void) const;
  octave_value& variable_reference (void);

  symbol_record *next (void) const;

  void chain (symbol_record *s);

  void push_context (void);
  void pop_context (void);

private:

  unsigned formal_param : 1;
  unsigned linked_to_global : 1;

  string nm;
  sv_Function sv_fcn;
  symbol_def *definition;
  symbol_record *next_elem;

// This should maybe be one stack with a structure containing all the
// items we need to save for recursive calls...
  SLStack <symbol_def *> context;
  SLStack <unsigned> global_link_context;

  void init_state (void);

  int read_only_error (void);

  void push_def (symbol_def *sd);
  symbol_def *pop_def (void);

  symbol_record& operator = (const symbol_record& s);
};

// A structure for handling verbose information about a symbol_record.

class
symbol_record_info
{
public:

  symbol_record_info (void);
  symbol_record_info (const symbol_record& s);

  symbol_record_info (const symbol_record_info& s);

  ~symbol_record_info (void) { }

  symbol_record_info& operator = (const symbol_record_info& s);

  int is_defined (void) const;
  int is_read_only (void) const;
  int is_eternal (void) const;
  int hides_fcn (void) const;
  int hides_builtin (void) const;
  string type_name (void) const;
  int is_function (void) const;
  int rows (void) const;
  int columns (void) const;
  string name (void) const;

  enum HIDES
    {
      SR_INFO_NONE = 0,
      SR_INFO_USER_FUNCTION = 1,
      SR_INFO_BUILTIN_FUNCTION = 2
    };

private:

  int initialized;
  int nr;
  int nc;
  unsigned type : 6;
  unsigned hides : 2;
  unsigned eternal : 1;
  unsigned read_only : 1;
  string nm;
  string const_type;
};

// A symbol table.

#define SYMTAB_LOCAL_SCOPE 1
#define SYMTAB_GLOBAL_SCOPE 2

#define SYMTAB_ALL_SCOPES (SYMTAB_LOCAL_SCOPE | SYMTAB_GLOBAL_SCOPE)

#define SYMTAB_ALL_TYPES (symbol_def::USER_FUNCTION \
			  | symbol_def::USER_VARIABLE \
			  | symbol_def::BUILTIN_FUNCTION \
			  | symbol_def::TEXT_FUNCTION \
			  | symbol_def::MAPPER_FUNCTION \
			  | symbol_def::BUILTIN_VARIABLE)

#define SYMTAB_VARIABLES (symbol_def::USER_VARIABLE \
			  | symbol_def::BUILTIN_VARIABLE)

class
symbol_table
{
public:

  symbol_table (void);

  symbol_record *lookup (const string& nm, int insert = 0, int warn = 0);

  void rename (const string& old_name, const string& new_name);

  void clear (int clear_user_functions = 1);
  int clear (const string& nm, int clear_user_functions = 1);

  int size (void) const;

  symbol_record_info *
  long_list (int& count, const string_vector& pats = string_vector (),
	     int npats = 0, int sort = 0, unsigned type = SYMTAB_ALL_TYPES,
	     unsigned scope = SYMTAB_ALL_SCOPES) const;

  string_vector
  list (int& count, const string_vector& pats = string_vector (),
	int npats = 0, int sort = 0, unsigned type = SYMTAB_ALL_TYPES,
	unsigned scope = SYMTAB_ALL_SCOPES) const;

  symbol_record **glob (int& count, const string& pat = string ("*"),
			unsigned type = SYMTAB_ALL_TYPES,
			unsigned scope = SYMTAB_ALL_SCOPES) const;

  void push_context (void);
  void pop_context (void);

private:

  unsigned int hash (const string& s);

  symbol_record table[HASH_TABLE_SIZE];
};

extern int valid_identifier (const char *s);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
