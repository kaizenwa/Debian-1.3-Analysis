/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: vector.c,v 1.20 1996/02/10 04:48:30 janssen Exp $ */
/* Last edited by Mike Spreitzer May 23, 1995 3:01 pm PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"

#include "vector.h"

/*L1, L2, Main unconstrained*/

ilu_Vector _ilu_vector_new (ilu_cardinal capacity)
{
  ilu_Vector new = (ilu_Vector) ilu_malloc(sizeof(struct ilu_vector_s));
  new->ve_capacity = (capacity < 1) ? 1 : capacity;
  new->ve_size = 0;
  new->ve_elements = (ilu_refany *) ilu_malloc(capacity * (sizeof(ilu_refany)));
  return (new);
}

/*L1 >= {some mutex that protects the vector}*/

void _ilu_vector_destroy (ilu_Vector v, void (*f) (ilu_refany))
{
  register ilu_cardinal i;

  if (f != NULLFN && v->ve_elements != NIL && v->ve_size > 0)
    for (i = 0;  i < v->ve_size;  i += 1)
      (*f)(v->ve_elements[i]);
  if (v->ve_elements != NIL)
    ilu_free(v->ve_elements);
  ilu_free(v);
}

ilu_cardinal _ilu_vector_size (ilu_Vector v)
{
  if (v != NIL)
    return (v->ve_size);
  else
    return (0);
}

ilu_refany * _ilu_vector_elements(ilu_Vector v)
{
  if (v != NIL)
    return (v->ve_elements);
  else
    return (NIL);
}
      
void _ilu_vector_add (ilu_Vector v, ilu_refany e)
{
  if (v->ve_size >= v->ve_capacity)
    v->ve_elements = (ilu_refany *) ilu_realloc(v->ve_elements, ((v->ve_capacity *= 2) * sizeof(ilu_refany)));
  v->ve_elements[v->ve_size] = e;
  v->ve_size += 1;
}

void _ilu_vector_add_if_not_present (ilu_Vector v, ilu_refany e)
{
  register ilu_cardinal i;

  if (v == NIL)
    return;
  for (i = 0;  i < v->ve_size;  i++)
    if (v->ve_elements[i] == e)
      return;
  _ilu_vector_add (v, e);
}

void _ilu_vector_remove (ilu_Vector v, ilu_refany e)
{
  register ilu_cardinal i, j;

  for (i = 0;  i < v->ve_size;  i += 1)
    if (e == v->ve_elements[i])
      {
	for (j = i + 1;  j < v->ve_size;  j += 1)
	  v->ve_elements[j-1] = v->ve_elements[j];
	v->ve_size -= 1;
	i -= 1;
      }
}

ilu_Vector _ilu_vector_copy (ilu_Vector old)
{
  ilu_Vector new = _ilu_vector_new(old->ve_size);
  register ilu_cardinal i;
  for (i = 0; i < old->ve_size; i++)
      new->ve_elements[i] = old->ve_elements[i];
  return new;
}

void _ilu_vector_assign (ilu_Vector l, ilu_Vector r)
{
  register ilu_cardinal i, m = (l->ve_size < r->ve_size) ? l->ve_size : r->ve_size;
  for (i = 0; i < m; i++)
      l->ve_elements[i] = r->ve_elements[i];
  for (i = m; i < r->ve_size; i++)
      _ilu_vector_add(l, r->ve_elements[i]);
  return;
}
