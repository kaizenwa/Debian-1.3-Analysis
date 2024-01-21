/* Memory access checker sub functions: use brk to alloc the bitmaps
   Copyright 1994, 1995 Tristan Gingold
		  Written September 1993 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/

/* with Checker, the memory map is like this:
 +---------------------------------------------------------------------------+
 | code | data | bss |  data  | heap |  heap  | stack  | >>>>   <<<< | stack |
 |      |      |     | bitmap |      | bitmap | bitmap | >>>>   <<<< |       |
 +---------------------------------------------------------------------------+
 0    &etext       &end                              sbrk(0)        %esp
*/

#include <limits.h>
#include <stddef.h>
#include "malloc.h"
#include "chkrlib.h"

static PTR safe_sbrk (int incr);

#ifdef CHKR_HEAPBITMAP
PTR chkr_heap_begin;
static PTR heap_end;
static PTR real_heap_end;
unsigned char *chkr_heap_bitmap;
static int heap_bitmap_size;
#endif /* CHKR_HEAPBITMAP */

#ifdef CHKR_DATABITMAP
unsigned char *chkr_data_bitmap;
static int data_bitmap_size;
#endif /* CHKR_DATABITMAP */

#ifdef CHKR_STACKBITMAP
unsigned char *chkr_stack_bitmap;
static int stack_bitmap_size;
#endif /* CHKR_STACKBITMAP */

int bytes_per_bbm_round;			/* number of bytes per byte of bitmap -1 */
int bm_log_size;
static int initialized;

#ifdef CHKR_USE_BITMAP
void
init_morecore (void)
{
  void *result;

  if (initialized)
    return;			/* because, init_morecore can be called by __chkr_init_chkr */

  /* initialisation */
  if (bytes_per_state == 0)	/* Can't be used */
    {
      chkr_perror (E_I_BBS_MA_ET);
      bytes_per_state = 1;	/* default value */
    }
  while (bytes_per_state != 1)
    {
      bm_log_size++;
      bytes_per_state >>= 1;
    }
  bytes_per_state = 1 << bm_log_size;
  bm_log_size += 2;
  bytes_per_bbm_round = (1 << bm_log_size) - 1;
  init_mapinfos ();
#ifdef CHKR_DATABITMAP
  result = chkr_sbrk (0);
  data_bitmap_size = (result - (PTR) & etext + bytes_per_bbm_round) >> bm_log_size;
  chkr_data_bitmap = chkr_sbrk ((data_bitmap_size + 0x10) & (~0x0f));	/* alignment */
  if (chkr_data_bitmap == (PTR) - 1)
    chkr_data_bitmap = NULL;	/* Won't be checked */
  memset (chkr_data_bitmap, 0xff, data_bitmap_size);
  mapinfo[DATABM]->length = data_bitmap_size << bm_log_size;
  mapinfo[DATABM]->bmbase = chkr_data_bitmap;
#endif /* CHKR_DATABITMAP */
#ifdef CHKR_HEAPBITMAP
  chkr_heap_begin = heap_end = chkr_sbrk (0);	/* Can sbrk(0) return -1 ??? */
  chkr_heap_bitmap = heap_end;
  heap_bitmap_size = 0;
  mapinfo[HEAPBM]->base = chkr_heap_begin;
  mapinfo[HEAPBM]->length = heap_bitmap_size << bm_log_size;
  mapinfo[HEAPBM]->bmbase = chkr_heap_bitmap;
#endif /* CHKR_HEAPBITMAP */
#ifdef CHKR_STACKBITMAP
  known_stack_limit = stack_bitmapped = &result;
  stack_bitmap_size = (STACK_LIMIT - (int) (&result) + bytes_per_bbm_round) >> bm_log_size;
  chkr_stack_bitmap = chkr_sbrk (stack_bitmap_size + 1);
  if (chkr_stack_bitmap == (PTR) - 1)
    chkr_stack_bitmap = NULL;
  /* Rights are set to read/write */
  memset (chkr_stack_bitmap, 0xff, stack_bitmap_size);
  mapinfo[STACKBM]->base = (uchar *) STACK_LIMIT;
  mapinfo[STACKBM]->length = stack_bitmap_size << bm_log_size;
  mapinfo[STACKBM]->bmbase = chkr_stack_bitmap;
#endif
  real_heap_end = chkr_sbrk (0);
  initialized = 1;		/* don't forget this !!! */
}

#endif /* CHKR_USE_BITMAP */

/* sbrk() return the old break address. ie brk(0) == brk(100) */
/* Note that morecore has to take a signed argument so
   that negative values can return memory to the system. */
void *
morecore (int size)
{
  void *result;
#ifdef CHKR_HEAPBITMAP
  int size1;
#endif /* CHKR_HEAPBITMAP */

#ifdef CHKR_USE_BITMAP
  if (!initialized)
    init_morecore ();
#endif /* CHKR_USE_BITMAP */

#if !defined(CHKR_HEAPBITMAP)
  /* really stupid morecore */
  result = chkr_sbrk (size);
  if (result == (void *) -1)
    {
      chkr_perror (M_M_OOM_MC_ET);
      return NULL;
    }
  return result;
#else
  if (size == 0)
    {
      safe_sbrk (0);
      return heap_end;		/* morecore(0) cheats the caller... */
    }
  if (size > 0)
    {
      /* We must grow the bitmap */
      size1 = (size + bytes_per_bbm_round) >> bm_log_size;

      result = safe_sbrk (size + size1);
      if (result == NULL)
	{
	  chkr_perror (M_M_OOM_MC_ET);
	  return NULL;
	}
#ifdef CHKR_STACKBITMAP
      /* Move the stack bitmap */
      memmove (chkr_stack_bitmap + size + size1, chkr_stack_bitmap, stack_bitmap_size);
      chkr_stack_bitmap += size + size1;
      mapinfo[STACKBM]->bmbase = chkr_stack_bitmap;
#endif
      /* Move the bitmap */
      memmove (chkr_heap_bitmap + size, chkr_heap_bitmap, heap_bitmap_size);
      /* initialize the new bitmap */
      chkr_heap_bitmap += size;
      mapinfo[HEAPBM]->bmbase = chkr_heap_bitmap;
      memset (chkr_heap_bitmap + heap_bitmap_size, 0, size1);
      /* Adjust the bitmap info */
      heap_bitmap_size += size1;
      mapinfo[HEAPBM]->length = heap_bitmap_size << bm_log_size;
      /* Clean the new area */
      memset (heap_end, 0, size);
      heap_end += size;
      real_heap_end += size + size1;
      return heap_end - (size /*+ size1*/ );	/* return the old address */
    }
  else
    {
      /* We must reduce the bitmap */
      size1 = (abs (size) + bytes_per_bbm_round) >> bm_log_size;
      /* Move the heap bitmap */
      memmove (chkr_heap_bitmap + size, chkr_heap_bitmap, heap_bitmap_size - size1);
      chkr_heap_bitmap += size;
      mapinfo[HEAPBM]->bmbase = chkr_heap_bitmap;
#ifdef CHKR_STACKBITMAP
      /* Move the stack bitmap */
      memmove (chkr_stack_bitmap + size - size1, chkr_stack_bitmap, stack_bitmap_size);
      chkr_stack_bitmap += size - size1;
      mapinfo[STACKBM]->bmbase = chkr_stack_bitmap;
#endif
      /* return memory to the system */
      result = safe_sbrk (size - size1);
      if (result == NULL)	/* Is it possible ?? */
	return NULL;
      /* Adjust the bitmap info */
      heap_bitmap_size -= size1;
      mapinfo[HEAPBM]->length = heap_bitmap_size << bm_log_size;
      heap_end += size;
      real_heap_end += size - size1;
      return heap_end - (size /*- size1*/ );	/* return the old address */
    }
#endif
}

#ifdef CHKR_STACKBITMAP
void
adjust_stackbitmap (PTR ptr)
{
  int diff;
  int size;
  void *result;

  size = (STACK_LIMIT - (int) ptr + bytes_per_bbm_round) >> bm_log_size;
  if (s
