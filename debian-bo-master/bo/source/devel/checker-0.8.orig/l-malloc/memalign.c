/* memalign
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

#ifndef	_MALLOC_INTERNAL
#define _MALLOC_INTERNAL
#include "malloc.h"
#endif

#include "errlist.h"
#include "message.h"

/* If true, emit a warning if the alignment argument is not a power of 2.  */
extern int flag_warn_memalign;

/* Get real_size bytes, located on ALIGNMENT boundary.
 * ALIGNMENT is not necessary a power of 2.  */
static PTR
memalign_1 (struct mdesc *mdp, size_t alignment, size_t real_size)
{
  size_t round_size;
  struct malloc_header *tmp;
  struct malloc_header *tmp1;
  int i;
  uint lost;		/* Bytes lost due to alignment. */

  /* Too easy!  */
  if (real_size == 0)
    return (PTR) 0;
   
  mdp = MD_TO_MDP(mdp);
  
  /* Check for stupid alignment.  */
  if (alignment == 0)
    {
      alignment = 1;
      chkr_perror (M_M_MNA_AL_ET);
    }
  
  /* Emit a warning if alignment is not a power of 2.  */
  if (flag_warn_memalign)
    {
      i = alignment;
      while ((i & 1) == 0)
        i >>= 1;
      if (i != 1)
        chkr_perror (M_M_MBA_AL_ET);
    }
    
  /* Avoid boring problems: alignment must be at leat LITTLE_SPACE.  */
  while (alignment & (LITTLE_SPACE - 1))
    alignment <<= 1;

  round_size = real_size + af_red_zone + be_red_zone;
  round_size = (round_size + (LITTLE_SPACE - 1)) & ~(LITTLE_SPACE - 1);
  
  /* We are going to access to internal data.  */
  if (mutex_atomic_swap (&mdp->lock, 1))
    MUTEX_FAIL;

  /* Try to find a free block, in the lists of free blocks.  */
  tmp1 = NULL_HEADER;
  
  /* Begins with the smallest list.  */
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
	  /* Block is too small.  */
	  if (tmp->size < round_size)
	    continue;
	  
	  /* Number of bytes lost due to the alignment.  */
	  lost = alignment - (((int) tmp + HEADER_SIZE + be_red_zone) % alignment);
	  if (lost == alignment)
	    lost = 0;
	  
	  /* If bytes are lost, we should create a new block and as a consequence
	     have enough memory to insert at least an header.  */
	  if (lost != 0)
	    while (lost < (HEADER_SIZE + LITTLE_SPACE))
	      lost += alignment;
	      
	  /* If too mamy bytes are lost, we loose!  */
	  if (tmp->size - lost < round_size || tmp->size < lost)
	    continue;
	  
	  /* If we can't insert an header in our block, we loose!  */
	  if ((tmp->size - lost) - round_size != 0 &&
	      (tmp->size - lost) - round_size < (HEADER_SIZE + LITTLE_SPACE))
	    continue;
	    
	  /* This is now OK: the block tmp will be used.  */
	  /* Yes, this block will be used.  */
	  tmp->state = MDBUSY;
	  
	  /* Remove the block from the list.  */
	  if (tmp->info.free.next)
	    tmp->info.free.next->info.free.prev = tmp->info.free.prev;
	  if (tmp->info.free.prev)
	    tmp->info.free.prev->info.free.next = tmp->info.free.next;
	  if (mdp->_heapinfo[i] == tmp)
	    mdp->_heapinfo[i] = tmp->info.free.next;
	    
	  /* Must we create a block before ?  */
	  if (lost != 0)
	    {
	      /* Create a new block.  */
	      tmp1 = (struct malloc_header *) ((char *) tmp + lost);
	      tmp1->prev = tmp;
	      tmp1->next = tmp->next;
	      if (tmp->next)
		tmp->next->prev = tmp1;
	      tmp->next = tmp1;
	      tmp1->size = tmp->size - lost;	/* Not true at this time.  */
	      tmp->size = lost - HEADER_SIZE;
	      if (mdp->_lastblock == tmp)
		mdp->_lastblock = tmp1;
	      tmp1->state = MDBUSY;
	      _internal_free (mdp, tmp);
	      tmp = tmp1;
	    }
	  if (tmp->size > round_size)
	    {
	      /* split the block: tmp1 is a new block.  */
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
	      tmp1->state = MDBUSY;
	      _internal_free (mdp, tmp1);
	    }
	    
#ifndef NO_HEAPINDEX
	  /* Compute _heapindex, if necessary.  */
	  if (mdp->_heapindex == i && mdp->_heapinfo[i] == NULL_HEADER)
	    while (mdp->_heapinfo[mdp->_heapindex] == NULL_HEADER)
	      {
		if (mdp->_heapindex)
		  mdp->_heapindex--;
		else
		  break;
	      }
#endif /* NO_HEAPINDEX */
	  goto done;
	}
    }
    
  /* No free block: must allocate memory.  */
  tmp1 = NULL_HEADER;
  
  /* first, try the last block.  */
  if (mdp->_lastblock->state == MDFREE)
    {
      tmp = mdp->_lastblock;
      lost = alignment - (int) tmp % alignment;
      if (lost != (HEADER_SIZE + be_red_zone))
	while (lost < (2 * HEADER_SIZE + LITTLE_SPACE + be_red_zone))
	  lost += alignment;
      i = log_size (tmp->size);
      tmp->size = round_size;
      
      /* Remove the block from the list.  */
      if (tmp->info.free.next)
	tmp->info.free.next->info.free.prev = tmp->info.free.prev;
      if (tmp->info.free.prev)
	tmp->info.free.prev->info.free.next = tmp->info.free.next;
      if (mdp->_heapinfo[i] == tmp)
	mdp->_heapinfo[i] = tmp->info.free.next;
      if (lost != (HEADER_SIZE + be_red_zone))
	{
	  mdp->morecore (mdp, lost - 2 * HEADER_SIZE - LITTLE_SPACE - be_red_zone - tmp->size);
	  tmp->size = lost - 2 * HEADER_SIZE - LITTLE_SPACE - be_red_zone;
	  tmp1 = tmp;
	}
      else
	{
	  mdp->morecore (mdp, round_size - tmp->size);
	  tmp->size = round_size;
	  goto done;
	}
    }
  tmp = (struct malloc_header *) mdp->morecore (mdp, 0);
  lost = alignment - (int) tmp % alignment;
  if (lost != (HEADER_SIZE + be_red_zone))
    while (lost < (2 * HEADER_SIZE + LITTLE_SPACE + be_red_zone))
      lost += alignment;
  if (lost != (HEADER_SIZE + be_red_zone))
    {
      mdp->morecore (mdp, lost - HEADER_SIZE - be_red_zone);
      tmp->prev = mdp->_lastblock;
      tmp->next = NULL_HEADER;
      if (mdp->_lastblock)
	tmp->prev->next = tmp;
      else
	mdp->_firstblock = tmp;
      mdp->_lastblock = tmp;
      tmp->size = lost - 2 * HEADER_SIZE - be_red_zone;
      tmp->state = MDBUSY;
      tmp1 = tmp;		/* to free */
      tmp = (struct malloc_header *) mdp->morecore (mdp, round_size + HEADER_SIZE);
    }
  else
    mdp->morecore (mdp, round_size + HEADER_SIZE);
  tmp->size = round_size;
  tmp->prev = mdp->_lastblock;
  if (mdp->_lastblock)
    mdp->_lastblock->next = tmp;
  else
    tmp->next = NULL_HEADER;
  mdp->_lastblock = tmp;
  if (!mdp->_firstblock)
    mdp->_firstblock = tmp;
  if (tmp1 != NULL_HEADER)
    _internal_free (mdp, tmp1);
done:
  /* All is right.  */
  tmp->state = MDBUSY;
  tmp->garbage_t = POINT_NOT;
  tmp->info.busy.real_size = real_size;
  mutex_atomic_swap (&mdp->lock, 0);
#ifdef CHKR_SAVESTACK
  chkr_get_history (
	(PTR *) ((u_int) tmp + HEADER_SIZE + round_size - af_red_zone),
	1,
	af_red_zone / sizeof (void *));  /* Number of frames to save.  */
#endif /* CHKR_SAVESTACK */
#ifdef CHKR_HEAPBITMAP
  chkr_set_right ((PTR) tmp + HEADER_SIZE + be_red_zone, real_size, CHKR_WO);
#endif /* CHKR_HEAPBITMAP */

  return (PTR) tmp + HEADER_SIZE + be_red_zone;
}

/* Allocate on a page boundary.  */
PTR
valloc (size_t size)
{
  PTR result;
  
#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &size, sizeof (size_t), CHKR_RO);
#endif

  size = test_malloc0 (size, "valloc");
  result = memalign_1 (NULL_MDESC, CHKR_PAGESIZE, size);
  
  if (trace_malloc_flag)
    chkr_printf ("# valloc (%d) = %p;\n", (uint) size, result);

  return result;
}

/* The standard function.  */
PTR
memalign (size_t alignment, size_t size)
{
  PTR result;
  
#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &alignment, sizeof (size_t), CHKR_RO);
  chkr_check_addr ((PTR) &size, sizeof (size_t), CHKR_RO);
#endif

  size = test_malloc0 (size, "memalign");
  result = memalign_1 (NULL_MDESC, alignment, size);
  
  if (trace_malloc_flag)
    chkr_printf ("# memalign (%d, %d) = %p;\n", (uint) alignment,
    					        (uint) size, result);

  return result;
}

PTR
mvalloc (struct mdesc *mdp, size_t size)
{
  PTR result;
  
#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &mdp, sizeof (mdp), CHKR_RO);
  chkr_check_addr ((PTR) &size, sizeof (size_t), CHKR_RO);
#endif

  size = test_malloc0 (size, "mvalloc");
  result = memalign_1 (mdp, CHKR_PAGESIZE, size);
  
  if (trace_malloc_flag)
    chkr_printf ("# mvalloc (%p, %d) = %p;\n", mdp, (uint) size, result);
  
  return result;
}

PTR
mmemalign (struct mdesc *mdp, size_t alignment, size_t size)
{
  PTR result;
  
#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &mdp, sizeof (mdp), CHKR_RO);
  chkr_check_addr ((PTR) &alignment, sizeof (size_t), CHKR_RO);
  chkr_check_addr ((PTR) &size, sizeof (size_t), CHKR_RO);
#endif

  size = test_malloc0 (size, "mmemalign");
  result = memalign_1 (mdp, alignment, size);
  
  if (trace_malloc_flag)
    chkr_printf ("# mmemalign (%p, %d, %d) = %p;\n", mdp, (uint) alignment,
						     (uint) size, result);
  
  return result;
}
