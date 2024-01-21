/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

/* "setjump.h" memory and stack parameters.
   Author: Aubrey Jaffer */

/* CELL_UP and CELL_DN are used by init_heap_seg to find cell aligned inner
   bounds for allocated storage */

#ifdef PROT386
/*in 386 protected mode we must only adjust the offset */
#define CELL_UP(p) MK_FP(FP_SEG(p), ~7&(FP_OFF(p)+7))
#define CELL_DN(p) MK_FP(FP_SEG(p), ~7&FP_OFF(p))
#else
#ifdef _UNICOS
#define CELL_UP(p) (CELLPTR)(~1L & ((long)(p)+1L))
#define CELL_DN(p) (CELLPTR)(~1L & (long)(p))
#else
#define CELL_UP(p) (CELLPTR)(~(sizeof(cell)-1L) & ((long)(p)+sizeof(cell)-1L))
#define CELL_DN(p) (CELLPTR)(~(sizeof(cell)-1L) & (long)(p))
#endif				/* UNICOS */
#endif				/* PROT386 */

/* These are parameters for controlling memory allocation.  The heap
   is the area out of which cons and object headers is allocated.
   Each heap object is 8 bytes on a 32 bit machine and 16 bytes on a
   64 bit machine.  The units of the _SIZE parameters are bytes.

   INIT_HEAP_SIZE is the initial size of heap.  If this much heap is
   allocated initially the heap will grow by half its current size
   each subsequent time more heap is needed.

   If INIT_HEAP_SIZE heap cannot be allocated initially, HEAP_SEG_SIZE
   will be used, and the heap will grow by HEAP_SEG_SIZE when more
   heap is needed.  HEAP_SEG_SIZE must fit into type sizet.  This code
   is in init_storage() and alloc_some_heap() in sys.c

   If INIT_HEAP_SIZE can be allocated initially, the heap will grow by
   EXPHEAP(heap_size) when more heap is needed.

   MIN_HEAP_SEG_SIZE is minimum size of heap to accept when more heap
   is needed.

   INIT_MALLOC_LIMIT is the initial amount of malloc usage which will
   trigger a GC. */

#define INIT_HEAP_SIZE (25000L*sizeof(cell))
#define MIN_HEAP_SEG_SIZE (2000L*sizeof(cell))
#ifdef _QC
#define HEAP_SEG_SIZE 32400L
#else
#ifdef sequent
#define HEAP_SEG_SIZE (7000L*sizeof(cell))
#else
#define HEAP_SEG_SIZE (8100L*sizeof(cell))
#endif
#endif
#define EXPHEAP(heap_size) (heap_size*2)
#define INIT_MALLOC_LIMIT 100000

#ifdef IN_CONTINUE_C
# include "scm.h"
# define malloc(size) must_malloc((long)(size), s_cont)
# define free(obj) must_free((char *)(obj))
#endif

/* other.dynenv and other.parent get GCed just by being there.  */
struct scm_other {SCM dynenv;
		  SCM parent;
#ifdef CAUTIOUS
		  SCM stack_trace;
#endif
		};
#define CONTINUATION_OTHER struct scm_other
#define CONT(x) ((CONTINUATION *)CDR(x))
#define SETCONT SETCDR
void dowinds P((SCM to, long delta));

#include "continue.h"

/* See scm.h for definition of P */
void  mark_locations P((STACKITEM x [], sizet n ));
void	scm_dynthrow P((CONTINUATION *cont, SCM val));
#define s_cont (ISYMCHARS(IM_CONT)+20)
