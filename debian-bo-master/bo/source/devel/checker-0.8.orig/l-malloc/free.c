/* free(3) implementation.
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written December 1993 by Tristan Gingold

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
#define _MALLOC_INTERNAL
#include "malloc.h"

#include "errlist.h"
#include "message.h"

#ifdef CHKR_PROFILE
/* Number of calls to free/mfree.  */
uint nbr_free_calls = 0;
#endif

/* Number of aged block kept in memory.  */
uint aged_queue_length = DEFAULT_BLOCK_AGE;

/* If set, emit a warning if free is called with a null argument.  */
extern int flag_warn_free_null;

/* Subroutine of free.  _internal_free is also called by malloc.
   It actually frees a block.  It handles neither age nor coallition nor
   mutex.  */
void
_internal_free (struct mdesc *mdp, struct malloc_header *block)
{
  struct malloc_header *tmp;
  size_t size;
  int l_size;

  size = block->size;

  /* Can we free memory ?  It must be the last block and should be big
     enough.  */
  if (mdp->_lastblock == block && size >= FINAL_FREE_SIZE)
    {
      /* Simple check.  */
#ifndef CHKR_IS_SAFE
      if (block->next)
        {
          chkr_report (M_C_MES_CK_ET);
	  chkr_printf (M_BAD_NLINK_4BLOCK, block);
	}
#endif /* CHKR_IS_SAFE */

      /* Updates mdp->_lastblock.  */
      mdp->_lastblock = block->prev;
      if (mdp->_lastblock)
	mdp->_lastblock->next = NULL_HEADER;
	
      /* Is there only one block ?  */
      if (mdp->_firstblock == block)
	mdp->_firstblock = NULL_HEADER;

      /* Free memory.  */
      mdp->morecore (mdp, -(int)(size + HEADER_SIZE));
      return;
    }

  /* The block can't really be freed, so we have to insert it a free list.  */

  /* Compute the log size.  */
  l_size = log_size (size);

  /* Now the block is considered as free.  */
  block->state = MDFREE;

#ifndef NO_HEAPINDEX
  /* Update _heapindex, the biggest size of free blocks available.  */
  if (l_size > mdp->_heapindex)
    mdp->_heapindex = l_size;
#endif /* NO_HEAPINDEX */

  tmp = mdp->_heapinfo[l_size];
  /* The list of free blocks must be sorted by size.
   * It is the smallest free block of the list ?
   * This can be the case if there was no block in the list.  */
  if (!tmp || tmp->size < size)
    {
      /* Put it at the begin of the list.  */
      block->info.free.next = tmp;
      block->info.free.prev = NULL_HEADER;

      /* Set the 'prev' link of the next block, if necessary.  */
      if (tmp)
	tmp->info.free.prev = block;
      mdp->_heapinfo[l_size] = block;
    }
  else
    {
      /* Find the good slot.  */
      while (tmp->info.free.next && tmp->info.free.next->size > size)
	tmp = tmp->info.free.next;

      /* Put it after tmp.  */
      block->info.free.prev = tmp;
      block->info.free.next = tmp->info.free.next;

      /* Set the 'prev' link of the next block.  */
      if (tmp->info.free.next)
	tmp->info.free.next->info.free.prev = block;
      tmp->info.free.next = block;
    }
}

/* The well-known free function.  */
void
free_1 (struct mdesc *mdp, PTR ptr)
{
  struct malloc_header *tmp;
  struct malloc_header *tmp1;
  int l_size;

  /* ANSI-C test.  */
  if (ptr == (PTR) 0)
    return;

  mdp = MD_TO_MDP (mdp);

  /* Compute the address of the header from ptr.  */
  tmp = (struct malloc_header *) (ptr - HEADER_SIZE - be_red_zone);

  /* Check if the address is good.  */
  if (tmp != find_header (mdp, ptr, 1))
    {
      chkr_perror (M_M_FBA_FR_ET);
      return;
    }

  /* A little check: the block must be busy, to be freed.  */
  if (tmp->state != MDBUSY)
    {
      chkr_perror (M_M_FFB_FR_ET);
      return;
    }

#ifdef CHKR_HEAPBITMAP
  /* Disable reading and writing right in the block.  */
  chkr_set_right ((PTR) tmp + HEADER_SIZE + be_red_zone,
		  tmp->info.busy.real_size, CHKR_UN);
#endif /* CHKR_HEAPBITMAP */

  /* We will handled internal data.  */
  if (mutex_atomic_swap (&mdp->lock, 1))
    MUTEX_FAIL;

#ifndef NO_AGE
  if (aged_queue_length)
    {
      /* Save the stack. */
#ifdef CHKR_SAVESTACK
      chkr_get_history (
		    (PTR *) ((u_int) tmp + HEADER_SIZE),
		    1,
		    (tmp->size - af_red_zone + be_red_zone) / sizeof (void *));	/* number of frames to save */
#endif /* CHKR_SAVESTACK */
      
      /* The block is now considered as an aged one.  */
      tmp->state = MDAGED;

      /* Put it in the aged list (at the beginning).  */
      tmp->info.aged.next = mdp->_youngerblock;
      tmp->info.aged.prev = NULL_HEADER;
      if (mdp->_youngerblock)
        mdp->_youngerblock->info.aged.prev = tmp;
      mdp->_youngerblock = tmp;
      if (mdp->_olderblock == NULL_HEADER)
        mdp->_olderblock = tmp;
      if (mdp->_agedblock >= aged_queue_length)
        {
          /* 'tmp' is now the older block, which must be freed.  */
          tmp = mdp->_olderblock;

          /* A simple check.  */
          if (tmp->state != MDAGED)
	    chkr_printf (M_FOUND_AGED_BLOCK, tmp);
          mdp->_olderblock = tmp->info.aged.prev;
          mdp->_olderblock->info.aged.next = NULL_HEADER;
        }
      else
        {
          /* No too many old blocks.  */
          mdp->_agedblock++;
          mutex_atomic_swap (&mdp->lock, 0);
          return;
        }
    }
#endif /* NO_AGE */

  /* Try to coalise this block with its next one.  */
  tmp1 = tmp->next;
  if (tmp1 && tmp->next->state == MDFREE)
    {
      l_size = log_size (tmp1->size);
      if (mdp->_heapinfo[l_size] == tmp1)
	mdp->_heapinfo[l_size] = tmp1->info.free.next;	/* BUG: _heapindex */
      if (tmp1->info.free.next)
	tmp1->info.free.next->info.free.prev = tmp1->info.free.prev;
      if (tmp1->info.free.prev)
	tmp1->info.free.prev->info.free.next = tmp1->info.free.next;
      tmp->size += tmp1->size + HEADER_SIZE;
      tmp->next = tmp1->next;
      if (tmp->next)
	tmp->next->prev = tmp;
      if (mdp->_lastblock == tmp1)
	mdp->_lastblock = tmp;
    }

  /* Try to coalise this block with its predecessor.  */
  tmp1 = tmp->prev;
  if (tmp1 && tmp->prev->state == MDFREE)
    {
      l_size = log_size (tmp1->size);
      if (mdp->_heapinfo[l_size] == tmp1)
	mdp->_heapinfo[l_size] = tmp1->info.free.next;	/* BUG: _heapindex */
      if (tmp1->info.free.next)
	tmp1->info.free.next->info.free.prev = tmp1->info.free.prev;
      if (tmp1->info.free.prev)
	tmp1->info.free.prev->info.free.next = tmp1->info.free.next;
      tmp1->size += tmp->size + HEADER_SIZE;
      tmp1->next = tmp->next;
      if (tmp->next)
	tmp->next->prev = tmp1;
      if (mdp->_lastblock == tmp)
	mdp->_lastblock = tmp1;
      tmp = tmp1;
    }

  _internal_free (mdp, tmp);
  mutex_atomic_swap (&mdp->lock, 0);
}

/* The well-known free function.  */
void
free (PTR ptr)
{
#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR)&ptr, sizeof (PTR), CHKR_RO);
#endif

  /* Imagine you call free before malloc.  */
  if (!__malloc_initialized)
    {
      chkr_perror (M_M_FBM_FR_ET);
      return;
    }

  /* Emit a warning if free(NULL).  */
  if (flag_warn_free_null && ptr == NULL)
    chkr_perror (M_M_FNL_FR_ET);

  /* Free is called.  */
#ifdef CHKR_PROFILE
  nbr_free_calls++;
#endif

  if (trace_malloc_flag)
    chkr_printf ("# free (%p);\n", ptr);

  free_1 (NULL_MDESC, ptr);
}

void
mfree (struct mdesc *mdp, PTR ptr)
{
#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR)&mdp, sizeof (mdp), CHKR_RO);
  chkr_check_addr ((PTR)&ptr, sizeof (PTR), CHKR_RO);
#endif

  /* Imagine you call free before malloc.  */
  if (!__malloc_initialized)
    {
      chkr_perror (M_M_FBM_FR_ET);
      return;
    }

  /* Emit a warning if free(NULL).  */
  if (flag_warn_free_null && ptr == NULL)
    chkr_perror (M_M_FNL_FR_ET);

#ifdef CHKR_PROFILE
  /* Free is called.  */
  nbr_free_calls++;
#endif

  if (trace_malloc_flag)
    chkr_printf ("# mfree (%p, %p);\n", mdp, ptr);

  free_1 (mdp, ptr);
}
