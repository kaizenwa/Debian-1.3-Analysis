/* Copyright (C) 1991,1995 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Heap manipulation.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/*
 * Changes for gpc:
 *   _p_new:  used to be a function. Then a procedure.
 *           We used to pass a pointer to pointer to it
 *           Now it is again a function. :-)
 *   _p_mark: same changes as in _p_new
 */

#include "rts.h"

/* The code implements both MARK/RELEASE and
   DISPOSE. Either one may be used, but not both in the same
   program. Dispose should be preferred, since it's faster.
 */

#define GPC_FMARK	0x01
#define GPC_FDISPOSE	0x02

#define ValueOfDISPOSED 0xdeadbeef

typedef struct heap_object 
{
	struct heap_object	*link;
} heap_object;

static heap_object *last_object;
static int	    heap_mode;

/* To keep the heap aligned correctly for any data access, we need to
   keep track of how many bytes to add and subtract from the malloced
   space for the chain of allocated heap objects.
   (chain used by mark/release)
 */
static int heap_align;

void
_p_init_heap_alignment ()
{
  /* If you have longer natural datatypes add them here to
   * align the heap objects
   */
  union alignment {
    struct heap_object a;
    double b;
    void  *c;
    long   d;
  };

  heap_align = sizeof (union alignment);
}

char *
_p_malloc (size)
     int size;
{
  char *ptr = (char *) malloc ((unsigned int) size);
  if (! ptr)
    _p_error (ABORT, "Out of heap when allocating %d bytes", size);

  return ptr;
}

void
_p_mark(mk)
char **mk;
{
    if ((heap_mode |= GPC_FMARK) & GPC_FDISPOSE)
	_p_error(ABORT,"MARK after DISPOSE");
    *mk = _p_new(0);
}

void
_p_release(rel0)
char	*rel0;
{
    heap_object	*p,*pp,*rel;

    rel0 -= heap_align;
    rel = (heap_object*)rel0;
    if ((heap_mode |= GPC_FMARK) & GPC_FDISPOSE)
	_p_error(ABORT, "RELEASE after DISPOSE");
    for(p = last_object; p && p != rel; p = p->link);
    if (p == NULL) {
	_p_error(REPORT, "Cannot release object at address 0x%lx", rel0);

	return;
    }
    for(p = last_object; p && p != rel; p = pp) {
	pp = p->link;
	(void)free((char *)p);
    }
    last_object = rel->link;
    (void)free((char *)rel);
}

char *
_p_new(size)
int	size;
{
    char	*ret;
    heap_object	*p;

    size += heap_align;
    ret = _p_malloc (size);

    /* -JV- G_NPE points to the LAST byte of the last object,
       instead of FIRST byte of the last object.
       Heap limits are INCLUSIVE. */

    if (ret+(unsigned)size > GLOBAL_P(G_NPE))
	GLOBAL_P(G_NPE) = ret+(unsigned)size-1;
    p = (heap_object*)ret;
    p->link = last_object;
    last_object = p;
    return ret+heap_align;
}

void
_p_dispose(p, size)
char	*p;
int     size;   /* The compiler gives the size; not used currently */
{
    heap_object *zap;
	
    if ((heap_mode |= GPC_FDISPOSE) & GPC_FMARK)
	_p_error(ABORT, "DISPOSE after MARK");
    p -= heap_align;
    zap = (heap_object *)p;

    zap->link = (heap_object *)ValueOfDISPOSED;
      /* To make detection of re-use possible */
      /* However, it is not certain that we */
      /* Trap it, as the object may get */
      /* re-allocated -JV- */
      /* (but this should do no harm, either) */

    (void)free(p);
}
