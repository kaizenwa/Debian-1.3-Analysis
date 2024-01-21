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

/*

The classes in this file are derived from the old `genclass' versions
of Map and CHMap from libg++, originally:

  Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

and distributed under the terms of the GNU Library General Public
License as published by the Free Software Foundation.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>

#include "Map.h"

static unsigned int
hash (const string& str)
{
  unsigned h = 0;
  for (unsigned i = 0; i < str.length (); i++)
    h = h * 33 + str[i];
  return h;
}

template <class C>
Pix
Map<C>::seek (const string& item) const
{
  Pix i = 0;

  for (i = first (); i != 0 && key (i) != item; next (i))
    ; // Skip items until match found.

  return i;
}

template <class C>
int
Map<C>::owns (Pix idx) const
{
  if (idx == 0)
    return 0;

  for (Pix i = first (); i != 0; next (i))
    if (i == idx)
      return 1;

  return 0;
}

template <class C>
void
Map<C>::clear (void)
{
  Pix i = first ();
  while (i != 0)
    {
      del (key (i));
      i = first ();
    }
}

template <class C>
int
Map<C>::contains (const string& item) const
{
  return seek (item) != 0;
}

template <class C>
void
Map<C>::error (const string& msg) const
{
  cerr << "Map: " << msg << "\n";
}

// CHMap class.

// The nodes are linked together serially via a version of a trick
// used in some vtables: odd pointers are actually links to the next
// table entry.  Not terrible, but not wonderful either.

template <class C>
static int
goodCHptr (CHNode<C> *t)
{
  return ((((unsigned) t) & 1) == 0);
}

// This sucks, but avoids g++ 2.6.0 `type unification failed' errors.

static void *
index_to_CHptr (int i)
{
  return (void *) ((i << 1) + 1);
}

template <class C>
static unsigned int
CHptr_to_index (CHNode<C> *t)
{
  return ((unsigned) t) >> 1;
}

template <class C>
CHMap<C>::CHMap (const C& dflt, unsigned int sz) : Map<C> (dflt)
{
  tab = new CHNode<C>* [size = sz];
  for (unsigned int i = 0; i < size; ++i)
    tab[i] = (CHNode<C> *) index_to_CHptr (i+1);
  count = 0;
}

template <class C>
CHMap<C>::CHMap (const CHMap& a) : Map<C> (a.def)
{
  tab = new CHNode<C>* [size = a.size];
  for (unsigned int i = 0; i < size; ++i)
    tab[i] = (CHNode<C> *) index_to_CHptr (i+1);
  count = 0;
  for (Pix p = a.first (); p; a.next (p))
    (*this) [a.key (p)] = a.contents (p);
}

template <class C>
Pix
CHMap<C>::seek (const string& key) const
{
  unsigned int h = hash (key) % size;

  for (CHNode<C> *t = tab[h]; goodCHptr (t); t = t->tl)
    if (key == t->hd)
      return Pix (t);

  return 0;
}

template <class C>
C&
CHMap<C>::operator [] (const string& item)
{
  unsigned int h = hash (item) % size;

  CHNode<C> *t = 0;
  for (t = tab[h]; goodCHptr (t); t = t->tl)
    if (item == t->hd)
      return t->cont;

  t = new CHNode<C> (item, def, tab[h]);
  tab[h] = t;
  ++count;
  return t->cont;
}

template <class C>
void
CHMap<C>::del (const string& key)
{
  unsigned int h = hash (key) % size;

  CHNode<C> *t = tab[h];
  CHNode<C> *trail = t;
  while (goodCHptr (t))
    {
      if (key == t->hd)
	{
	  if (trail == t)
	    tab[h] = t->tl;
	  else
	    trail->tl = t->tl;
	  delete t;
	  --count;
	  return;
	}
      trail = t;
      t = t->tl;
    }
}

template <class C>
void
CHMap<C>::clear (void)
{
  for (unsigned int i = 0; i < size; ++i)
    {
      CHNode<C> *p = tab[i];
      tab[i] = (CHNode<C> *) index_to_CHptr (i+1);
      while (goodCHptr (p))
	{
	  CHNode<C> *nxt = p->tl;
	  delete p;
	  p = nxt;
	}
    }
  count = 0;
}

template <class C>
Pix
CHMap<C>::first (void) const
{
  for (unsigned int i = 0; i < size; ++i)
    if (goodCHptr (tab[i]))
      return Pix (tab[i]);
  return 0;
}

template <class C>
void
CHMap<C>::next (Pix& p) const
{
  CHNode<C> *t = ((CHNode<C> *) p)->tl;
  if (goodCHptr (t))
    p = Pix (t);
  else
    {
      for (unsigned int i = CHptr_to_index (t); i < size; ++i)
	{
	  if (goodCHptr (tab[i]))
	    {
	      p =  Pix (tab[i]);
	      return;
	    }
	}
      p = 0;
    }
}

template <class C>
int
CHMap<C>::OK (void) const
{
  int v = tab != 0;
  int n = 0;

  for (unsigned int i = 0; i < size; ++i)
    {
      CHNode<C> *p = 0;

      for (p = tab[i]; goodCHptr (p); p = p->tl)
	++n;

      v &= CHptr_to_index (p) == i + 1;
    }

  v &= count == n;

  if (! v)
    error ("invariant failure");

  return v;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
