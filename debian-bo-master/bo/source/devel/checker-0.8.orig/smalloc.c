/* sys_malloc implementation (simple malloc, which doesn't use [s]brk)
   This file is part of Checker.
   Copyright 1994, 1995 Tristan Gingold
		  Written January 1994 by Tristan Gingold

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
/* This simple malloc is used by Checker.  Checker can't use the standard
   malloc, so I provided a simple one, based on mmap.  */
 
#include <sys/types.h>
#define NEED_MM
#include "checker.h"
#include "errlist.h"

/* This malloc uses a double-linked list of block.  Each block is preceded by
 * an header, defined by sys_malloc_header.
 */
struct sys_malloc_header;
struct sys_malloc_header
{
  struct sys_malloc_header *prev; /* the next malloc_header, or NULL_HEADER */
  struct sys_malloc_header *next; /* the previous malloc_header, or NULL_HEADER */
  size_t size;			/* the total size */
  char state;			/* state, see MD* */
  char pad[3];			/* currently unused */
};

/* Often used values. */
#define SYS_NULL_HEADER (struct sys_malloc_header*)0
#define SYS_HEADER_SIZE (sizeof(struct sys_malloc_header))

/* The smallest space that malloc can allocate. It is also the alignment
 * value.  Must be a power of 2.
 */
#define LITTLE_SPACE BIGGEST_ALIGNMENT

/* State possibilities.  */
#define MDFREE 1	/* block is free */
#define MDBUSY 2	/* block is busy */

/* The low level routine to allocate memory.  It is like sbrk. */
static PTR morecore (int size);

/* Pointer to the first block.  By construction, 
 * _sys_firstblock->prev = SYS_NULL_HEADER.
 */
struct sys_malloc_header *_sys_firstblock;

/* Pointer to the last block.  By construction, 
 * _sys_lastblock->next = SYS_NULL_HEADER.
 */
struct sys_malloc_header *_sys_lastblock;

/* sys_malloc: returns the address of a block of SIZE bytes.
 * sys_malloc(0) returns an address.
 */ 
PTR
sys_malloc (size_t size)
{
  struct sys_malloc_header *tmp;
  struct sys_malloc_header *tmp1;

  /* Round the size to align block.  */
  size = (size + LITTLE_SPACE - 1) & ~(LITTLE_SPACE - 1);
  
  if (_sys_firstblock == SYS_NULL_HEADER)
    {
      /* sys_malloc() was never called.  Initialize.  */
      tmp = (struct sys_malloc_header*) morecore (SYS_HEADER_SIZE + size);
      tmp->state = MDBUSY;
      tmp->prev = tmp->next = SYS_NULL_HEADER;
      _sys_firstblock = _sys_lastblock = tmp;
      tmp->size = size;
      return (PTR)tmp + SYS_HEADER_SIZE;
    }
    
  /* Search for a free block.  */
  for (tmp = _sys_firstblock; tmp != SYS_NULL_HEADER; tmp = tmp->next)
    {
      /* The block must be free.  */
      if (tmp->state != MDFREE || tmp->size < size)
        continue;
      
      /* If the block has exactly the same size, it is easy.  */
      if (tmp->size == size)
        {
          tmp->state = MDBUSY;
          return (PTR)tmp + SYS_HEADER_SIZE;
        }
      
      /* We are trying to split the block.
         However, it must be big enough to create another block.  */
      if (tmp->size - size < (SYS_HEADER_SIZE + LITTLE_SPACE))
        continue;
        
      /* Split this block.  */
      tmp1 = (struct sys_malloc_header*)((uint)tmp + SYS_HEADER_SIZE + size);
      tmp1->size = tmp->size - size - SYS_HEADER_SIZE;
      tmp->size = size;
      tmp1->next = tmp->next;
      tmp1->prev = tmp;
      tmp->next = tmp1;
      if (tmp1->next)
        tmp1->next->prev = tmp1;
      else
        _sys_lastblock = tmp1;
      tmp1->state = MDFREE;
      tmp->state = MDBUSY;
      return (PTR)tmp + SYS_HEADER_SIZE;
    }

#if 1
  /* Must never happen because it must have been released.  */
  if (_sys_lastblock->state == MDFREE)
    chkr_abort();
#endif
    
  /* Create a new block at the end.  */
  tmp = (struct sys_malloc_header*)morecore(size + SYS_HEADER_SIZE);
  tmp->prev = _sys_lastblock;
  tmp->next = SYS_NULL_HEADER;
  tmp->state = MDBUSY;
  _sys_lastblock->next = tmp;
  _sys_lastblock = tmp;
  tmp->size = size;
  return (PTR)tmp + SYS_HEADER_SIZE;
}

/* Free a block.  If it is the last, the memory is release to the system.  */
void
sys_free (PTR ptr)
{
 struct sys_malloc_header *tmp;
 struct sys_malloc_header *tmp1; 
 
 /* ANSI-C test.  */
 if (ptr == (PTR)0)
   return;
   
 /* Compute the address of the header from ptr.  */
 tmp = (struct sys_malloc_header*)(ptr - SYS_HEADER_SIZE);

 /* Try to coalise this block with its next one.  */
 if (tmp->next && tmp->next->state == MDFREE)
   {
     tmp1 = tmp->next;
     tmp->size += tmp1->size + SYS_HEADER_SIZE;
     tmp->next = tmp1->next;
     if (tmp->next)
       tmp->next->prev = tmp;
     if (_sys_lastblock == tmp1)
      _sys_lastblock = tmp;
   }
 
 /* Try to coalise this block with its predecessor.  */
 if (tmp->prev && tmp->prev->state == MDFREE)
   {
     tmp1 = tmp->prev;
     tmp1->size += tmp->size + SYS_HEADER_SIZE;
     tmp1->next = tmp->next;
     if (tmp->next)
       tmp->next->prev = tmp1;
     if (_sys_lastblock == tmp)
       _sys_lastblock = tmp1;
     tmp = tmp1;
   }

 /* Can we free memory ?  */
 if(_sys_lastblock == tmp)
   {
     /* Updates _lastblock.  */
     _sys_lastblock = tmp->prev;
     if (_sys_lastblock)
       _sys_lastblock->next = SYS_NULL_HEADER;
     /* Is there only one block ? */
     if (_sys_firstblock == tmp)
       _sys_firstblock = SYS_NULL_HEADER;
     /* Free memory.  */
     morecore(-(int)(tmp->size + SYS_HEADER_SIZE));
     /* That's all.  */
     return;
   }
 tmp->state = MDFREE;
 return;
}

/* Like realloc.  Very simple.  */
PTR
sys_realloc (PTR ptr, size_t size)
{
  struct sys_malloc_header *tmp;
  PTR result;
  
  if (ptr == (PTR)0)
    return sys_malloc(size);
    
  if (size == 0)
    {
      sys_free (ptr);
      return sys_malloc (0);
    }
    
  /* Compute the address of the header from ptr.  */
  tmp = (struct sys_malloc_header*)(ptr - SYS_HEADER_SIZE);
  
  /* Very simple realloc implementation 
     But easy to do and bugs free ( I hope !).  */
  result = sys_malloc (size);
  if (result != (PTR)0)
    {
      memcpy (result, ptr, (size > tmp->size) ? tmp->size : size);
      sys_free (ptr);
    }
  return result;
}

/* Duplicate a block: allocate a new block with the same size of PTR and copy
 *  its contents to the new allocated block.  */
PTR
sys_dupalloc (PTR ptr)
{
  struct sys_malloc_header *tmp;
  PTR result;
  
  if (ptr == (PTR)0)
    return (PTR)0;
    
  /* Compute the address of the header from ptr.  */
  tmp = (struct sys_malloc_header*)(ptr - SYS_HEADER_SIZE);
  
  result = sys_malloc (tmp->size);
  if (result != (PTR)0)
    memcpy (result, ptr, tmp->size);
  return result;
}

/* Current size in byte.  */
static uint current_size = 0;

/* Current size in page.  */
static uint current_pages = 0;

/* Allocate memory if SIZE > 0.  Release memory if SIZE < 0. */
static PTR
morecore (int size)
{
 /* Number of page needed for all the blocks.  This is rounded.  */
 int pages = (current_size + size + CHKR_PAGESIZE-1) / CHKR_PAGESIZE;
 
 if (pages > current_pages)
   {
     /* Allocate memory.  */
     PTR res;
     res = mmap ((char*)(MM_MEM + current_pages * CHKR_PAGESIZE),
     	  (pages - current_pages) * CHKR_PAGESIZE,
     	  MM_PROT, MM_FLAGS, MM_FILE, 0);
     if (res != (PTR)(MM_MEM + current_pages * CHKR_PAGESIZE))
       {
         chkr_perror (M_I_OOS_SM_ET);
         chkr_printf ("mmap at 0x%08x for %d pages\n", 
         	(MM_MEM + current_pages * CHKR_PAGESIZE),
     	  	pages - current_pages);
         chkr_abort ();
       }
     current_pages = pages;
   }
 else if (pages < current_pages)
   {
     /* Free memory.  */
     if (munmap ((char*)(MM_MEM + pages * CHKR_PAGESIZE),
            (current_pages - pages) * CHKR_PAGESIZE) != 0)
       {
         chkr_perror (M_I_OOS_SM_ET);
         chkr_abort ();
       }
     current_pages = pages;
   }
 current_size += size;
 return (PTR)(MM_MEM + current_size - size);
}

/* Dump the heap.  Useful if you want to debug it.  */
void
__chkr_dump_sbusy (void)
{
 struct sys_malloc_header *tmp;

 tmp = _sys_firstblock;
 if (tmp == SYS_NULL_HEADER)
     return;
 chkr_report (M_C_MES_CK_ET);
 
 while (tmp != NULL)
   {
     switch (tmp->state)
       {
         case MDBUSY: chkr_printf ("BUSY: ");
         	break;
         case MDFREE: chkr_printf ("FREE: ");
         	break;
       }
     chkr_printf ("block at 0x%p, size %db\n", tmp, (uint) tmp->size);
     tmp = tmp->next;
   }
}
