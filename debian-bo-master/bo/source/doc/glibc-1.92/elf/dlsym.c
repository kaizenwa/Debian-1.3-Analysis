/* dlsym -- Look up a symbol in a shared object loaded by `dlopen'.
Copyright (C) 1995, 1996 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <stddef.h>
#include <link.h>
#include <dlfcn.h>
#include <setjmp.h>


void *
dlsym (void *handle, const char *name)
{
  struct link_map *map = handle;
  ElfW(Addr) loadbase;
  const ElfW(Sym) *ref = NULL;
  void doit (void)
    {
      struct link_map *scope[2] = { map, NULL };
      loadbase = _dl_lookup_symbol (name, &ref, scope, map->l_name, 0, 0);
    }

  return _dlerror_run (doit) ? NULL : (void *) (loadbase + ref->st_value);
}
