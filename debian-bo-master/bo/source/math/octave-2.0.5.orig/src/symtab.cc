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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>

#include "oct-glob.h"
#include "str-vec.h"

#include "error.h"
#include "pt-const.h"
#include "pt-fcn.h"
#include "pt-fvc.h"
#include "symtab.h"
#include "utils.h"
#include "variables.h"

// Variables and functions.

symbol_def::symbol_def (void)
{
  init_state ();
}

symbol_def::symbol_def (tree_constant *t)
{
  init_state ();
  definition = t;
  type = USER_VARIABLE;
}

symbol_def::symbol_def (tree_builtin *t, unsigned fcn_type)
{
  init_state ();
  definition = t;
  type = BUILTIN_FUNCTION | fcn_type;
}

symbol_def::symbol_def (tree_function *t, unsigned fcn_type)
{
  init_state ();
  definition = t;
  type = USER_FUNCTION | fcn_type;
}

void
symbol_def::init_state (void)
{
  type = UNKNOWN;
  eternal = 0;
  read_only = 0;

  definition = 0;
  next_elem = 0;
  count = 0;
}

symbol_def::~symbol_def (void)
{
  delete definition;
}

int
symbol_def::is_variable (void) const
{
  return (type & USER_VARIABLE || type & BUILTIN_VARIABLE);
}

int
symbol_def::is_function (void) const
{
  return (type & USER_FUNCTION || type & BUILTIN_FUNCTION);
}

int
symbol_def::is_user_variable (void) const
{
  return (type & USER_VARIABLE);
}

int
symbol_def::is_text_function (void) const
{
  return (type & TEXT_FUNCTION);
}

int
symbol_def::is_mapper_function (void) const
{
  return (type & MAPPER_FUNCTION);
}

int
symbol_def::is_user_function (void) const
{
  return (type & USER_FUNCTION);
}

int
symbol_def::is_builtin_variable (void) const
{
  return (type & BUILTIN_VARIABLE);
}

int
symbol_def::is_builtin_function (void) const
{
  return (type & BUILTIN_FUNCTION);
}

// XXX FIXME XXX
int
symbol_def::is_map_element (const string& /* elts */) const
{
  return 0;
}

void
symbol_def::define (tree_constant *t)
{
  if (t)
    t->maybe_mutate ();

  definition = t;

  if (! is_builtin_variable ())
    type = USER_VARIABLE;
}

void
symbol_def::define (tree_builtin *t, unsigned fcn_type)
{
  definition = t;
  type = BUILTIN_FUNCTION | fcn_type;
}

void
symbol_def::define (tree_function *t, unsigned fcn_type)
{
  definition = t;
  type = USER_FUNCTION | fcn_type;
}

void
symbol_def::protect (void)
{
  read_only = 1;
}

void
symbol_def::unprotect (void)
{
  read_only = 0;

}

void
symbol_def::make_eternal (void)
{
  eternal = 1;
}

tree_fvc *
symbol_def::def (void) const
{
  return definition;
}

string
symbol_def::help (void) const
{
  return help_string;
}

void
symbol_def::document (const string& h)
{
  help_string = h;
}

int
maybe_delete (symbol_def *def)
{
  int count = 0;
  if (def && def->count > 0)
    {
      def->count--;
      count = def->count;
      if (def->count == 0)
	delete def;
    }
  return count;
}

// Individual records in a symbol table.

symbol_record::symbol_record (void)
{
  init_state ();
}

symbol_record::symbol_record (const string& n, symbol_record *nxt)
{
  init_state ();
  nm = n;
  next_elem = nxt;
}

void
symbol_record::init_state (void)
{
  formal_param = 0;
  linked_to_global = 0;
  sv_fcn = 0;
  definition = 0;
  next_elem = 0;
}

string
symbol_record::name (void) const
{
  return nm;
}

string
symbol_record::help (void) const
{
  string retval;
  if (definition)
    retval = definition->help ();
  return retval;
}

tree_fvc *
symbol_record::def (void) const
{
  return definition ? definition->def () : 0;
}

void
symbol_record::rename (const string& new_name)
{
  nm = new_name;
}

int
symbol_record::is_function (void) const
{
  return definition ? definition->is_function () : 0;
}

int
symbol_record::is_text_function (void) const
{
  return definition ? definition->is_text_function () : 0;
}

int
symbol_record::is_mapper_function (void) const
{
  return definition ? definition->is_mapper_function () : 0;
}

int
symbol_record::is_user_function (void) const
{
  return definition ? definition->is_user_function () : 0;
}

int
symbol_record::is_builtin_function (void) const
{
  return definition ? definition->is_builtin_function () : 0;
}

int
symbol_record::is_variable (void) const
{
  return definition ? definition->is_variable () : 0;
}

int
symbol_record::is_user_variable (void) const
{
  return definition ? definition->is_user_variable () : 0;
}

int
symbol_record::is_builtin_variable (void) const
{
  return definition ? definition->is_builtin_variable () : 0;
}

int
symbol_record::is_map_element (const string& elts) const
{
  return definition ? definition->is_map_element (elts) : 0;
}

unsigned
symbol_record::type (void) const
{
  return definition ? definition->type : 0;
}

int
symbol_record::is_defined (void) const
{
  return definition ? (definition->def () != 0) : 0;
}

int
symbol_record::is_read_only (void) const
{
  return definition ? definition->read_only : 0;
}

int
symbol_record::is_eternal (void) const
{
  return definition ? definition->eternal : 0;
}

void
symbol_record::protect (void)
{
  if (definition)
    {
      definition->protect ();

      if (! is_defined ())
	warning ("protecting undefined variable `%s'", nm.c_str ());
    }
}

void
symbol_record::unprotect (void)
{
  if (definition)
    definition->unprotect ();
}

void
symbol_record::make_eternal (void)
{
  if (definition)
    {
      definition->make_eternal ();

      if (! is_defined ())
	warning ("giving eternal life to undefined variable `%s'",
		 nm.c_str ());
    }
}

void
symbol_record::set_sv_function (sv_Function f)
{
  sv_fcn = f;
}

int
symbol_record::define (tree_constant *t)
{
  if (is_variable () && read_only_error ())
    return 0;

  tree_fvc *saved_def = 0;
  if (! definition)
    {
      definition = new symbol_def ();
      definition->count = 1;
    }
  else if (is_function ())
    {
      symbol_def *new_def = new symbol_def ();
      push_def (new_def);
      definition->count = 1;
    }
  else if (is_variable ())
    {
      saved_def = definition->def ();
    }

  definition->define (t);

  if (sv_fcn && sv_fcn () < 0)
    {
      // Would be nice to be able to avoid this cast.  XXX FIXME XXX

      definition->define ((tree_constant *) saved_def);
      return 0;
    }

  delete saved_def;

  return 1;
}

int
symbol_record::define (const octave_value& v)
{
  tree_constant *t = new tree_constant (v);
  return define (t);
}

int
symbol_record::define (tree_builtin *t, int text_fcn)
{
  if (read_only_error ())
    return 0;

  if (is_variable ())
    {
      symbol_def *old_def = pop_def ();
      maybe_delete (old_def);
    }

  if (is_function ())
    {
      symbol_def *old_def = pop_def ();
      maybe_delete (old_def);
    }

  unsigned fcn_type = text_fcn ? symbol_def::TEXT_FUNCTION
    : ((t && t->is_mapper_function ()) ? symbol_def::MAPPER_FUNCTION
       : symbol_def::UNKNOWN);

  symbol_def *new_def = new symbol_def (t, fcn_type);
  push_def (new_def);
  definition->count = 1;

  return 1;
}

int
symbol_record::define (tree_function *t, int text_fcn)
{
  if (read_only_error ())
    return 0;

  if (is_variable ())
    {
      symbol_def *old_def = pop_def ();
      maybe_delete (old_def);
    }

  if (is_function ())
    {
      symbol_def *old_def = pop_def ();
      maybe_delete (old_def);
    }

  unsigned fcn_type = text_fcn ? symbol_def::TEXT_FUNCTION
    : symbol_def::UNKNOWN;

  symbol_def *new_def = new symbol_def (t, fcn_type);
  push_def (new_def);
  definition->count = 1;

  return 1;
}

int
symbol_record::define_as_fcn (const octave_value& v)
{
  if (is_variable () && read_only_error ())
    return 0;

  if (is_variable ())
    {
      symbol_def *old_def = pop_def ();
      maybe_delete (old_def);
    }

  if (is_function ())
    {
      symbol_def *old_def = pop_def ();
      maybe_delete (old_def);
    }

  tree_constant *t = new tree_constant (v);
  symbol_def *new_def = new symbol_def (t);
  push_def (new_def);
  definition->count = 1;
  definition->type = symbol_def::BUILTIN_FUNCTION;

  return 1;
}

int
symbol_record::define_builtin_var (const octave_value& v)
{
  tree_constant *t = new tree_constant (v);
  define (t);
  if (is_variable ())
    definition->type = symbol_def::BUILTIN_VARIABLE;
  return 1;
}

void
symbol_record::document (const string& h)
{
  if (definition)
    {
      definition->document (h);

      if (! is_defined ())
	warning ("documenting undefined variable `%s'", nm.c_str ());
    }
}

int
symbol_record::clear (void)
{
  int count = 0;
  if (linked_to_global)
    {
      count = maybe_delete (definition);
      definition = 0;
      linked_to_global = 0;
    }
  else
    {
      symbol_def *old_def = pop_def ();
      count = maybe_delete (old_def);
    }
  return count;
}

void
symbol_record::alias (symbol_record *s, int force)
{
  sv_fcn = s->sv_fcn;

  if (force && ! s->definition)
    {
      s->definition = new symbol_def ();
      definition = s->definition;
      definition->count = 2; // Yes, this is correct.
    }
  else if (s->definition)
    {
      definition = s->definition;
      definition->count++;
    }
}

void
symbol_record::mark_as_formal_parameter (void)
{
  formal_param = 1;
}

int
symbol_record::is_formal_parameter (void) const
{
  return formal_param;
}

void
symbol_record::mark_as_linked_to_global (void)
{
  linked_to_global = 1;
}

int
symbol_record::is_linked_to_global (void) const
{
  return linked_to_global;
}

octave_value
symbol_record::variable_value (void) const
{
  octave_value retval;

  if (is_variable ())
    {
      tree_constant *tmp = (tree_constant *) def ();
      retval = tmp->value ();
    }

  return retval;
}

octave_value&
symbol_record::variable_reference (void)
{
  if (is_function ())
    clear ();

  if (! is_defined ())
    {
      if (! (is_formal_parameter () || is_linked_to_global ()))
	link_to_builtin_variable (this);

      if (! is_defined ())
	{
	  tree_constant *tmp = new tree_constant ();
	  define (tmp);
	}
    }

  tree_constant *tmp = (tree_constant *) def ();

  return tmp->reference ();
}

symbol_record *
symbol_record::next (void) const
{
  return next_elem;
}

void
symbol_record::chain (symbol_record *s)
{
  next_elem = s;
}

void
symbol_record::push_context (void)
{
  context.push (definition);
  definition = 0;

  global_link_context.push ((unsigned) linked_to_global);
  linked_to_global = 0;
}

void
symbol_record::pop_context (void)
{
  // It is possible for context to be empty if new symbols have been
  // inserted in the symbol table during recursive calls.  This can
  // happen as a result of calls to eval() and feval().

  if (! context.empty ())
    {
      if (is_variable ())
	{
	  symbol_def *old_def = pop_def ();
	  maybe_delete (old_def);
	}

      if (is_function ())
	{
	  symbol_def *old_def = pop_def ();
	  maybe_delete (old_def);
	}

      definition = context.pop ();
      linked_to_global = global_link_context.pop ();
    }
}

int
symbol_record::read_only_error (void)
{
  if (is_read_only ())
    {
      if (is_variable ())
	{
	  ::error ("can't redefine read-only constant `%s'", nm.c_str ());
	}
      else if (is_function ())
	{
	  ::error ("can't redefine read-only function `%s'", nm.c_str ());
	}
      else
	{
	  ::error ("can't redefine read-only symbol `%s'", nm.c_str ());
	}

      return 1;
    }
  else
    return 0;
}

void
symbol_record::push_def (symbol_def *sd)
{
  if (! sd)
    return;

  sd->next_elem = definition;
  definition = sd;
}

symbol_def *
symbol_record::pop_def (void)
{
  symbol_def *top = definition;
  if (definition)
    definition = definition->next_elem;
  return top;
}

// A structure for handling verbose information about a symbol_record.

symbol_record_info::symbol_record_info (void)
  : initialized (0), nr (-1), nc (-1), type (symbol_def::UNKNOWN),
    hides (SR_INFO_NONE), eternal (0), read_only (0), nm (),
    const_type () { }

symbol_record_info::symbol_record_info (const symbol_record& sr)
  : initialized (0), nr (-1), nc (-1), type (sr.type ()),
    hides (SR_INFO_NONE), eternal (0), read_only (0), nm (),
    const_type ()
{
  if (sr.is_variable () && sr.is_defined ())
    {
      // Would be nice to avoid this cast.  XXX FIXME XXX

      tree_constant *tmp = (tree_constant *) sr.def ();

      const_type = tmp->type_name ();

      nr = tmp->rows ();
      nc = tmp->columns ();

      symbol_def *sr_def = sr.definition;
      symbol_def *hidden_def = sr_def->next_elem;

      if (hidden_def)
	{
	  if (hidden_def->is_user_function ())
	    hides = SR_INFO_USER_FUNCTION;
	  else if (hidden_def->is_builtin_function ())
	    hides = SR_INFO_BUILTIN_FUNCTION;
	}
    }

  eternal = sr.is_eternal ();
  read_only = sr.is_read_only ();

  nm = sr.name ();

  initialized = 1;
}

symbol_record_info::symbol_record_info (const symbol_record_info& s)
  : initialized (s.initialized), nr (s.nr), nc (s.nc), type (s.type),
    hides (s.hides), eternal (s.eternal), read_only (s.read_only),
    nm (s.nm), const_type (s.const_type) { }

symbol_record_info&
symbol_record_info::operator = (const symbol_record_info& s)
{
  if (this != &s)
    {
      initialized = s.initialized;
      nr = s.nr;
      nc = s.nc;
      type = s.type;
      hides = s.hides;
      eternal = s.eternal;
      read_only = s.read_only;
      nm = s.nm;
      const_type = s.const_type;
    }
  return *this;
}

int
symbol_record_info::is_defined (void) const
{
  return initialized;
}

int
symbol_record_info::is_read_only (void) const
{
  return read_only;
}

int
symbol_record_info::is_eternal (void) const
{
  return eternal;
}

int
symbol_record_info::hides_fcn (void) const
{
  return (hides & SR_INFO_USER_FUNCTION);
}

int
symbol_record_info::hides_builtin (void) const
{
  return (hides & SR_INFO_BUILTIN_FUNCTION);
}

string
symbol_record_info::type_name (void) const
{
  string retval;

  if (type == symbol_def::USER_FUNCTION)
    retval = "user function";
  else if (type & symbol_def::BUILTIN_FUNCTION)
    {
      if (type & symbol_def::TEXT_FUNCTION)
	retval = "text function";
      else if (type & symbol_def::MAPPER_FUNCTION)
	retval = "mapper function";
      else
	retval = "builtin function";
    }
  else
    retval = const_type;

  return retval;
}

int
symbol_record_info::is_function (void) const
{
  return (type == symbol_def::USER_FUNCTION
	  || type == symbol_def::BUILTIN_FUNCTION
	  || symbol_def::TEXT_FUNCTION
	  || symbol_def::MAPPER_FUNCTION);
}

int
symbol_record_info::rows (void) const
{
  return nr;
}

int
symbol_record_info::columns (void) const
{
  return nc;
}

string
symbol_record_info::name (void) const
{
  return nm;
}

// A symbol table.

symbol_table::symbol_table (void)
{
}

symbol_record *
symbol_table::lookup (const string& nm, int insert, int warn)
{
  int index = hash (nm) & HASH_MASK;

  symbol_record *ptr = table[index].next ();

  while (ptr)
    {
      if (ptr->name () == nm)
	return ptr;
      ptr = ptr->next ();
    }

  if (insert)
    {
      symbol_record *new_sym;
      new_sym = new symbol_record (nm, table[index].next ());
      table[index].chain (new_sym);
      return new_sym;
    }
  else if (warn)
    warning ("lookup: symbol`%s' not found", nm.c_str ());

  return 0;
}

void
symbol_table::rename (const string& old_name, const string& new_name)
{
  int index = hash (old_name) & HASH_MASK;

  symbol_record *prev = &table[index];
  symbol_record *ptr = prev->next ();

  while (ptr)
    {
      if (ptr->name () == old_name)
	{
	  prev->chain (ptr->next ());

	  index = hash (new_name) & HASH_MASK;
	  table[index].chain (ptr);
	  ptr->rename (new_name);

	  return;
	}

      prev = ptr;
      ptr = ptr->next ();
    }

  error ("unable to rename `%s' to `%s'", old_name.c_str (),
	 new_name.c_str ());
}

void
symbol_table::clear (int clear_user_functions)
{
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  if (ptr->is_user_variable ()
	      || (clear_user_functions && ptr->is_user_function ()))
	    {
	      ptr->clear ();
	    }

	  ptr = ptr->next ();
	}
    }
}

int
symbol_table::clear (const string& nm, int clear_user_functions)
{
  int index = hash (nm) & HASH_MASK;

  symbol_record *ptr = table[index].next ();

  while (ptr)
    {
      if (ptr->name () == nm
	  && (ptr->is_user_variable ()
	      || (clear_user_functions && ptr->is_user_function ())))
	{
	  ptr->clear ();
	  return 1;
	}
      ptr = ptr->next ();
    }

  return 0;
}

int
symbol_table::size (void) const
{
  int count = 0;
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();
      while (ptr)
	{
	  count++;
	  ptr = ptr->next ();
	}
    }
  return count;
}

static inline int
pstrcmp (char **a, char **b)
{
  return strcmp (*a, *b);
}

static inline int
symbol_record_info_cmp (symbol_record_info *a, symbol_record_info *b)
{
  return (a->name () == b->name ());
}

static int
matches_patterns (const string& name, const string_vector& pats, int npats)
{
  for (int i = 0; i < npats; i++)
    {
      glob_match pattern (pats[i]);
      if (pattern.match (name))
	return 1;
    }

  return 0;
}

// This function should probably share code with symbol_table::list.
// XXX FIXME XXX

symbol_record_info *
symbol_table::long_list (int& count, const string_vector& pats,
			 int npats, int sort, unsigned type,
			 unsigned scope) const 
{
  count = 0;
  int n = size ();
  if (n == 0)
    return 0;

  symbol_record_info *symbols = new symbol_record_info [n+1];
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();
      while (ptr)
	{
	  assert (count < n);

	  unsigned my_scope = ptr->is_linked_to_global () + 1; // Tricky...

	  unsigned my_type = ptr->type ();

	  string my_name = ptr->name ();

	  if ((type & my_type) && (scope & my_scope)
	      && (npats == 0 || matches_patterns (my_name, pats, npats)))
	    symbols[count++] = symbol_record_info (*ptr);

	  ptr = ptr->next ();
	}
    }
  symbols[count] = symbol_record_info ();

  if (sort && symbols)
    qsort ((void *) symbols, count, sizeof (symbol_record_info),
	   (int (*)(const void*, const void*)) symbol_record_info_cmp);

  return symbols;
}

string_vector
symbol_table::list (int& count, const string_vector& pats, int npats,
		    int sort, unsigned type, unsigned scope) const
{
  count = 0;
  int n = size ();
  if (n == 0)
    return 0;

  string_vector symbols (n);

  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();
      while (ptr)
	{
	  assert (count < n);

	  unsigned my_scope = ptr->is_linked_to_global () + 1; // Tricky...

	  unsigned my_type = ptr->type ();

	  string my_name = ptr->name ();

	  if ((type & my_type) && (scope & my_scope)
	      && (npats == 0 || matches_patterns (my_name, pats, npats)))
	    symbols[count++] = ptr->name ();

	  ptr = ptr->next ();
	}
    }

  symbols.resize (count);

  if (sort && ! symbols.empty ())
    symbols.qsort ();

  return symbols;
}

symbol_record **
symbol_table::glob (int& count, const string& pat, unsigned type,
		    unsigned scope) const
{
  count = 0;
  int n = size ();
  if (n == 0)
    return 0;

  symbol_record **symbols = new symbol_record * [n+1];
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();
      while (ptr)
	{
	  assert (count < n);

	  unsigned my_scope = ptr->is_linked_to_global () + 1; // Tricky...

	  unsigned my_type = ptr->type ();

	  glob_match pattern (pat);

	  if ((type & my_type) && (scope & my_scope)
	      && pattern.match (ptr->name ()))
	    {
	      symbols[count++] = ptr;
	    }

	  ptr = ptr->next ();
	}
    }
  symbols[count] = 0;

  return symbols;
}

void
symbol_table::push_context (void)
{
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  ptr->push_context ();
	  ptr = ptr->next ();
	}
    }
}

void
symbol_table::pop_context (void)
{
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  ptr->pop_context ();
	  ptr = ptr->next ();
	}
    }
}

// Chris Torek's fave hash function.

unsigned int
symbol_table::hash (const string& str)
{
  unsigned h = 0;
  for (unsigned i = 0; i < str.length (); i++)
    h = h * 33 + str[i];
  return h;
}

// Return nonzero if S is a valid identifier.

int
valid_identifier (const char *s)
{
  if (! s || ! (isalnum (*s) || *s == '_'))
     return 0;

  while (*++s != '\0')
    if (! (isalnum (*s) || *s == '_'))
      return 0;

  return 1;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
