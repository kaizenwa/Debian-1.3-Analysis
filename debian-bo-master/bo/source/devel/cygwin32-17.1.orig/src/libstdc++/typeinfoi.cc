// Methods for type_info for the -*- C++ -*- Run Time Type Identification.
// Copyright (C) 1994 Free Software Foundation

// This file is part of the GNU ANSI C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

// As a special exception, if you link this library with files
// compiled with a GNU compiler to produce an executable, this does not cause
// the resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.

// Written by Kung Hsu based upon the specification in the 20 September 1994
// C++ working paper, ANSI document X3J16/94-0158.
// Rewritten by Jason Merrill.

#ifdef __GNUG__
#pragma implementation "std/typeinfo.h"
#endif

#include <std/cstdlib.h>
#include <std/typeinfo.h>
#include <new>			// for placement new

extern "C" void* __throw_type_match_rtti (void *, void *, void *);

// service function for comparing types by name.

inline int fast_compare (const char *n1, const char *n2) {
  int c;
  if (n1 == n2) return 0;
  if (n1 == 0) return *n2;
  else if (n2 == 0) return *n1;

  c = (int)*n1++ - (int)*n2++;
  return c == 0 ? strcmp (n1, n2) : c;
};

bool
type_info::before (const type_info &arg)
{
  return fast_compare (name (), arg.name ()) < 0;
}

// type_info for a class with no base classes.

struct __user_type_info : public type_info {
  __user_type_info (const char *n) : type_info (n) {}

  // If our type can be converted to the desired type, 
  // return the pointer, adjusted accordingly; else return 0.
  virtual void* dcast (const type_info &, int, void *,
		       const type_info * = 0, void * = 0) const;
};

void * __user_type_info::
dcast (const type_info& to, int, void *addr, const type_info *, void *) const
{ return (*this == to) ? addr : 0; }

// type_info for a class with one public, nonvirtual base class.

class __si_type_info : public __user_type_info {
  const __user_type_info &base;

public:
  __si_type_info (const char *n, const __user_type_info &b)
    : __user_type_info (n), base (b) { }

  virtual void *dcast (const type_info &, int, void *,
		       const type_info * = 0, void * = 0) const;
};

void * __si_type_info::
dcast (const type_info& to, int require_public, void *addr,
       const type_info *sub, void *subptr) const
{
  if (*this == to)
    return addr;
  return base.dcast (to, require_public, addr, sub, subptr);
}

// type_info for a general class.

struct __class_type_info : public __user_type_info {
  enum access { PUBLIC = 1, PROTECTED = 2, PRIVATE = 3 };

  struct base_info {
    const __user_type_info *base;
    size_t offset: 29;
    bool is_virtual: 1;
    access access: 2;
  };

  const base_info *base_list;
  size_t n_bases;

  __class_type_info (const char *name, const base_info *bl, size_t bn)
    : __user_type_info (name), base_list (bl), n_bases (bn) {}

  // This is a little complex.
  virtual void* dcast (const type_info &, int, void *,
		       const type_info * = 0, void * = 0) const;
};

// Offset functions for the class type.

// 0 is returned if the cast is invalid, otherwise the converted
// object pointer that points to the sub-object that is matched is
// returned.

void* __class_type_info::
dcast (const type_info& desired, int is_public, void *objptr,
       const type_info *sub, void *subptr) const
{
  if (*this == desired)
    return objptr;

  void *match_found = 0;
  for (int i = 0; i < n_bases; i++)
    {
      if (is_public && base_list[i].access != PUBLIC)
	continue;

      void *p = (char *)objptr + base_list[i].offset;
      if (base_list[i].is_virtual)
	p = *(void **)p;
      p = base_list[i].base->dcast (desired, is_public, p, sub, subptr);
      if (p)
	{
	  if (match_found == 0)
	    match_found = p;
	  else if (match_found != p)
	    {
	      if (sub)
		{
		  // Perhaps we're downcasting from *sub to desired; see if
		  // subptr is a subobject of exactly one of {match_found,p}.

		  __user_type_info &d =
		    static_cast <__user_type_info &> (desired);

		  void *os = d.dcast (*sub, 1, match_found);
		  void *ns = d.dcast (*sub, 1, p);

		  if (os == ns)
		    /* ambiguous -- subptr is a virtual base */;
		  else if (os == subptr)
		    continue;
		  else if (ns == subptr)
		    {
		      match_found = p;
		      continue;
		    }
		}

	      // base found at two different pointers,
	      // conversion is not unique
	      return 0;
	    }
	}
    }

  return match_found;
}

// type info for pointer type.

struct __pointer_type_info : public type_info {
  const type_info& type;

  __pointer_type_info (const char *n, const type_info& ti)
    : type_info (n), type (ti) {}
};

// type info for attributes

struct __attr_type_info : public type_info {
  enum cv { NONE = 0, CONST = 1, VOLATILE = 2, CONSTVOL = 1 | 2 };

  const type_info& type;
  cv attr;

  __attr_type_info (const char *n, cv a, const type_info& t)
    : type_info (n), type (t), attr (a) {}
};

// type_info for builtin type

struct __builtin_type_info : public type_info {
  __builtin_type_info (const char *n): type_info (n) {}
};

// type info for function.

struct __func_type_info : public type_info {
  __func_type_info (const char *n) : type_info (n) {}
};

// type info for pointer to member function.

struct __ptmf_type_info : public type_info {
  __ptmf_type_info (const char *n) : type_info (n) {}
};

// type info for pointer to data member.

struct __ptmd_type_info : public type_info {
  __ptmd_type_info (const char *n): type_info (n) {}
};

// type info for array.

struct __array_type_info : public type_info {
  __array_type_info (const char *n): type_info (n) {}
};

// Entry points for the compiler.

/* Low level match routine used by compiler to match types of catch
   variables and thrown objects.  */

extern "C" void*
__throw_type_match_rtti (void *catch_type_r, void *throw_type_r, void *objptr)
{
  type_info &catch_type = *(type_info*)catch_type_r;
  type_info &throw_type = *(type_info*)throw_type_r;
  
  if (catch_type == throw_type)
    return objptr;
  
#if 0
  printf ("We want to match a %s against a %s!\n",
	  throw_type.name (), catch_type.name ());
#endif

  void *new_objptr = 0;

  if (__user_type_info *p = dynamic_cast <__user_type_info *> (&throw_type))
    {
      /* The 1 skips conversions to private bases. */
      new_objptr = p->dcast (catch_type, 1, objptr);
    }
  else if (__pointer_type_info *fr =
	   dynamic_cast <__pointer_type_info *> (&throw_type))
    {
      __pointer_type_info *to =
	   dynamic_cast <__pointer_type_info *> (&catch_type);

      if (! to)
	goto fail;

      const type_info *subfr = &fr->type, *subto = &to->type;
      __attr_type_info::cv cvfrom, cvto;

      if (const __attr_type_info *at
	  = dynamic_cast <const __attr_type_info *> (subfr))
	{
	  cvfrom = at->attr;
	  subfr = &at->type;
	}
      else
	cvfrom = __attr_type_info::NONE;
      
      if (const __attr_type_info *at
	  = dynamic_cast <const __attr_type_info *> (subto))
	{
	  cvto = at->attr;
	  subto = &at->type;
	}
      else
	cvto = __attr_type_info::NONE;

      if ((cvfrom & __attr_type_info::CONST
	   > cvto & __attr_type_info::CONST)
	  || (cvfrom & __attr_type_info::VOLATILE
	      > cvto & __attr_type_info::VOLATILE))
	goto fail;

      if (*subto == typeid (void)
	  && dynamic_cast <const __func_type_info *> (subfr) == 0)
	new_objptr = objptr;
      else if (const __user_type_info *p
	       = dynamic_cast <const __user_type_info *> (subfr))
	{
	  /* The 1 skips conversions to private bases. */
	  new_objptr = p->dcast (*subto, 1, objptr);
	}
      // still need to handle multi-level pointer qual conversions
    }
 fail:

#if 0
  if (new_objptr)
    printf ("It converts, delta is %d\n", new_objptr-objptr);
#endif
  return new_objptr;
}

extern "C" void
__rtti_class (void *addr, const char *name,
	      const __class_type_info::base_info *bl, size_t bn)
{ new (addr) __class_type_info (name, bl, bn); }

extern "C" void
__rtti_ptr (void *addr, const char *n, const type_info *ti)
{ new (addr) __pointer_type_info (n, *ti); }

extern "C" void
__rtti_si (void *addr, const char *n, const type_info *ti)
{
  new (addr) __si_type_info
    (n, static_cast <__user_type_info &> (*ti));
}

extern "C" void
__rtti_attr (void *addr, const char *n, int attrval, const type_info *ti)
{
  new (addr) __attr_type_info
    (n, static_cast <__attr_type_info::cv> (attrval), *ti);
}

extern "C" void
__rtti_func (void *addr, const char *name)
{ new (addr) __func_type_info (name); }

extern "C" void
__rtti_user (void *addr, const char *name)
{ new (addr) __user_type_info (name); }

extern "C" void
__rtti_ptmf (void *addr, const char *name)
{ new (addr) __ptmf_type_info (name); }

extern "C" void
__rtti_ptmd (void *addr, const char *name)
{ new (addr) __ptmd_type_info (name); }

extern "C" void
__rtti_array (void *addr, const char *name)
{ new (addr) __array_type_info (name); }

extern "C" void *
__dynamic_cast (const type_info& from, const type_info& to,
		int require_public, void *address, const type_info &sub,
		void *subptr)
{
  return static_cast <__user_type_info &> (from).dcast
    (to, require_public, address, &sub, subptr);
}

// type_info nodes and functions for the builtin types.  The mangling here
// must match the mangling in gcc/cp/rtti.c.

#define BUILTIN(mangled)					\
unsigned char __ti##mangled [sizeof (__builtin_type_info)]	\
  __attribute__ ((aligned (__alignof__ (void *))));;			\
extern "C" const type_info &__tf##mangled (void) {		\
  if ((*(void **) __ti##mangled) == 0)				\
    new (__ti##mangled) __builtin_type_info (#mangled);		\
  return *(type_info *)__ti##mangled;				\
}

BUILTIN (v); BUILTIN (x); BUILTIN (l); BUILTIN (i); BUILTIN (s); BUILTIN (b);
BUILTIN (c); BUILTIN (w); BUILTIN (r); BUILTIN (d); BUILTIN (f);
BUILTIN (Ui); BUILTIN (Ul); BUILTIN (Ux); BUILTIN (Us); BUILTIN (Uc);
