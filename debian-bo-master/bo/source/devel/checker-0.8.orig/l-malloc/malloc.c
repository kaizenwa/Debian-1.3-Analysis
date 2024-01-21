/* malloc(3) implementation
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

/* This is used to link each mdesc header.  */
struct mdesc *_firstmdesc;	/* Points to the first header.  */
struct mdesc *_lastmdesc;	/* Points to the last header.  */

/* This is a global lock, used where the links are handled.  */
uint mdesc_lock;

/* True is malloc was called.  */
int __malloc_initialized = 0;

#ifdef CHKR_PROFILE
/* Number of calls to malloc/mmalloc.  */
uint nbr_malloc_calls = 0;
#endif

PTR low_addr_heap = (PTR) 0;	/* Set in machine.c */
PTR high_addr_heap = (PTR) HIGH_ADDR_HEAP;

/* Initialize a mdesc.  */
static int
initialize (struct mdesc *mdp)
{
  memset (mdp->_heapinfo, 0, HASH_SIZE * sizeof (struct malloc_header*));
#ifndef NO_HEAPINDEX
  mdp->_heapindex = 0;
#endif
  mdp->_firstblock = NULL_HEADER;
  mdp->_lastblock = NULL_HEADER;
  mdp->_youngerblock = NULL_HEADER;
  mdp->_olderblock = NULL_HEADER;  
  mdp->flags |= MMALLOC_INITIALIZED;
  return 1;
}

/* malloc: return the address of a block of size bytes.
 * malloc(0) returns 0.  */
PTR
malloc_1 (struct mdesc *mdp, size_t real_size)
{
  size_t round_size;
  struct malloc_header *tmp;
  struct malloc_header *tmp1;
  int i;

  /* malloc(0) returns 0.  */
  if (real_size == 0)
    return (PTR) 0;
    
  mdp = MD_TO_MDP(mdp);
  
  /* We are going to access to internal data.  */
  if (mutex_atomic_swap (&mdp->lock, 1))
    MUTEX_FAIL;
   
  /* Initialize all, if necessary.  */
  if (__malloc_initialized == 0)
    {
      __malloc_initialized = 1;
      
      /* chkr_initialize call init_morecore.  */
      chkr_initialize ();
    }
  
  /* Initialize the block, if necessary.  */
  if (!(mdp->flags & MMALLOC_INITIALIZED))
    if (!initialize (mdp))
      {
        mutex_atomic_swap (&mdp->lock, 0);
        return NULL;
      }
      
  round_size = real_size + be_red_zone + af_red_zone;
  round_size = (round_size + (LITTLE_SPACE - 1)) & ~(LITTLE_SPACE - 1);

  /* Try to find a free block, in the lists of free blocks.  Begins with the
     smallest list.  */
  tmp1 = NULL_HEADER;
#ifdef NO_HEAPINDEX
  for (i = log_size (round_size); i < HASH_SIZE; i++)
#else /* !NO_HEAPINDEX */
  for (i = log_size (round_size); i <= mdp->_heapindex; i++)
#endif /* NO_HEAPINDEX */
    {
      /* Empty list.  */
      if (mdp->_heapinfo[i] == NULL_HEADER)
	continue;
	
      tmp = mdp->_heapinfo[i];
      for (tmp = mdp->_heapinfo[i]; tmp != NULL_HEADER; tmp = tmp->info.free.next)
	{
	  /* The block is too small.  */
	  if (tmp->size < round_size)
	    continue;
	  
	  /* This block will be used.  */
	  tmp->state = MDBUSY;
	  
	  /* The block is perhaps too big and must be split.  */
	  if (tmp->size > (round_size + HEADER_SIZE + LITTLE_SPACE))
	    {
	      tmp1 = (struct malloc_header *) ((char *) tmp + round_size + HEADER_SIZE);
	      tmp1->prev = tmp;
	      tmp1->next = tmp->next;
	      if (tmp->next)
		tmp->next->prev = tmp1;
	      tmp->next = tmp1;
	      tmp1->size = tmp->size - HEADER_SIZE - round_size;
	      tmp->size = round_size;
	      if (mdp->_lastblock == tmp)
		mdp->_lastblock = tmp1;
	    }
	    
	  /* Remove the block from the list.  */
	  if (tmp->info.free.next)
	    tmp->info.free.next->info.free.prev = tmp->info.free.prev;
	  if (tmp->info.free.prev)
	    tmp->info.free.prev->info.free.next = tmp->info.free.next;
	  if (mdp->_heapinfo[i] == tmp)
	    mdp->_heapinfo[i] = tmp->info.free.next;

#ifndef NO_HEAPINDEX
	  /* Compute _heapindex, if necessary.  */
	  if (mdp->_heapindex == i && mdp->_heapinfo[i] == NULL_HEADER)
	    while (_heapinfo[_heapindex] == NULL_HEADER)
	      {
		if (mdp->_heapindex)
		  mdp->_heapindex--;
		else
		  break;
	      }
#endif /* NO_HEAPINDEX */

	  /* Free the split block, if it exists.  */
	  if (tmp1)
	    {
	      /* Cheats _internal_free.  */
	      tmp1->state = MDBUSY;
	      
	      /* The new block can't be coalised.  So call 
	         _internal_free ().  */
	      _internal_free (mdp, tmp1);
	      tmp1 = NULL_HEADER;
	    }
	  goto done;
	}
    }
    
  /* No free block: must allocate memory.  */
  if (mdp->_lastblock && mdp->_lastblock->state == MDFREE)
    {
      /* If the last block is free, use its memory.  */
      tmp = mdp->_lastblock;
      if (mdp->morecore (mdp, round_size - tmp->size) == (PTR) 0)
        {
          mutex_atomic_swap (&mdp->lock, 0);
	  return (PTR) 0;
	}
      i = log_size (tmp->size);
      tmp->size = round_size;
      
      /* Remove the block from the list. */
      if (tmp->info.free.next)
	tmp->info.free.next->info.free.prev = tmp->info.free.prev;
      if (tmp->info.free.prev)
	tmp->info.free.prev->info.free.next = tmp->info.free.next;
      if (mdp->_heapinfo[i] == tmp)
	mdp->_heapinfo[i] = tmp->info.free.next;
    }
  else
    {
      /* Create a new block.  */
      tmp = (struct malloc_header *) mdp->morecore (mdp, round_size + HEADER_SIZE);
      if (tmp == NULL_HEADER)
        {
          mutex_atomic_swap (&mdp->lock, 0);
	  return (PTR) 0;
	}
      tmp->size = round_size;
      tmp->prev = mdp->_lastblock;
      if (mdp->_lastblock)
	mdp->_lastblock->next = tmp;
      tmp->next = NULL_HEADER;
      mdp->_lastblock = tmp;
      if (!mdp->_firstblock)
	mdp->_firstblock = tmp;
    }
done:
  /* All is right.  */
  tmp->state = MDBUSY;
  tmp->garbage_t = POINT_NOT;
  tmp->info.busy.real_size = real_size;
  tmp->s_diff = tmp->size - real_size - be_red_zone - af_red_zone;
  mutex_atomic_swap (&mdp->lock, 0);
#ifdef CHKR_SAVESTACK
  chkr_get_history (
	 (PTR *) ((u_int) tmp + HEADER_SIZE + round_size - af_red_zone),
	 1,
	 af_red_zone / sizeof (void *)); /* Number of frames to save.  */
#endif /* CHKR_SAVESTACK */
#ifdef CHKR_HEAPBITMAP
  chkr_set_right ((PTR) tmp + HEADER_SIZE + be_red_zone, real_size, CHKR_WO);
#endif /* CHKR_HEAPBITMAP */
  return (PTR) tmp + HEADER_SIZE + be_red_zone;
}

/* malloc: returns the address of a block of size bytes.  */
PTR
malloc (size_t real_size)
{
  PTR result;
  
#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &real_size, sizeof (size_t), CHKR_RO);
#endif

#ifdef CHKR_PROFILE
  /* Malloc is called.  */
  nbr_malloc_calls++;
#endif

  real_size = test_malloc0 (real_size, "malloc");
  result = malloc_1 (NULL_MDESC, real_size);
  
  if (trace_malloc_flag)
    chkr_printf ("# malloc (%d) = %p;\n", (uint) real_size, result);

  return result;
}

PTR
mmalloc (struct mdesc *mdp, size_t real_size)
{
  PTR result;
  
#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &mdp, sizeof (mdp), CHKR_RO);
  chkr_check_addr ((PTR) &real_size, sizeof (size_t), CHKR_RO);
#endif

#ifdef CHKR_PROFILE
  /* Malloc is called.  */
  nbr_malloc_calls++;
#endif

  real_size = test_malloc0 (real_size, "mmalloc");
  result = malloc_1 (mdp, real_size);
  
  if (trace_malloc_flag)
    chkr_printf ("# mmalloc (%p, %d) = %p;\n", mdp, (uint) real_size, result);
    
  return result;
}
