/* Garbage detector of checker.
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written August 1993 by Tristan Gingold
		
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
#include <alloca.h>
#include <sys/time.h>
#include "malloc.h"
#include "message.h"
#include "errlist.h"

#ifdef CHKR_GARBAGE
/* Number of leaks detected (do not include potential leak).  */
static unsigned int nbr_leaks;	/* How many have been detected */
static unsigned int nbr_inuse;

/* Number of potentiel leaks detected.  */
static unsigned int nbr_pot_leaks;

/* Memory lost due to leaks.  */
static unsigned int memory_lost;

#ifdef CHKR_PROFILE
/* The total time used for garbage detector.  */
struct timeval tv_garbage;
#endif

/* Minimum size of a family leak to be individually displayed.  */
int leak_size_threshold;

/* Some prototypes ... */
static void disp_leaks (void);
static void search_in_block (struct malloc_header *block, int sure);

/* Check an address
 * Does a recursive descent if on heap.
 * If SURE is true, the address comes not from a POINT_MAYBE block.
 */
void
chkr_address (PTR ptr, int sure)
{
  struct malloc_header *block;

  block = find_header ((struct mdesc *)0, ptr, 0);
  
  if (block == NULL_HEADER)
    return;

  /* Don't forget that ptr can point to anywhere.  */
  if (block->state != MDBUSY)
    return;		/* Pointer on free (or aged) block.  */

  /* Can't change the status when the block is already pointed.
   * Progression (POINT_MAYBE -> POINT_SURE) if SURE is true.  */
  if ( ((block->garbage_t & POINT_MASK) != POINT_NOT)
       && !((block->garbage_t & POINT_MASK) == POINT_MAYBE && sure))
    return;
    
  /* if ptr == real_ptr + (...), ptr points on the begin of the block.  */
  if (sure && ptr == ((PTR) block + be_red_zone + HEADER_SIZE))
    block->garbage_t = POINT_SURE | (block->garbage_t & INUSE_MASK);
  else
    {
      /* If the block was already a potential leak, return immidiately.  */
      if ((block->garbage_t & POINT_MASK) == POINT_MAYBE)
        return;
      block->garbage_t = POINT_MAYBE | (block->garbage_t & INUSE_MASK);
    }
  search_in_block (block, sure);	/* recursive.  */
}

/* Set all garbage_t to POINT_NOT, so that all block are garbage.  */
static void
init_detector (void)
{
  struct mdesc *mdp;
  struct malloc_header *tmp;
  
  for (mdp = _firstmdesc; mdp != (struct mdesc*) 0; mdp = mdp->info.inmem.next_mdesc)
    for (tmp = mdp->_firstblock; tmp != NULL_HEADER; tmp = tmp->next)
      {
        tmp->garbage_t = POINT_NOT | (tmp->garbage_t & INUSE_MASK);
        tmp->g_flag = G_ALONE;
      }
}

/* POINT_LEAK becomes POINT_SEEN.
 * POINT_SEEN remainds POINT_SEEN.
 */
static void
init_evol_detector (void)
{
  struct mdesc *mdp;
  struct malloc_header *tmp;
  
  for (mdp = _firstmdesc; mdp != (struct mdesc*) 0; mdp = mdp->info.inmem.next_mdesc)
    for (tmp = mdp->_firstblock; tmp != NULL_HEADER; tmp = tmp->next)
      {
        if ((tmp->garbage_t & POINT_MASK) == POINT_LEAK 
            || (tmp->garbage_t & POINT_MASK) == POINT_SEEN)
	  tmp->garbage_t = POINT_SEEN | (tmp->garbage_t & INUSE_MASK);
        else
	  {
	    tmp->garbage_t = POINT_NOT | (tmp->garbage_t & INUSE_MASK);
	    tmp->g_flag = G_ALONE;
	  }
      }
}

/* Search pointer inside all static segments (ie data and bss).  */
static void
search_data (void)
{
  PTR *ptr;
  struct object *obj = objects;

  for (obj = objects; obj; obj = obj->next)
    if ((obj->rights & (OBJECT_DATA | OBJECT_READ | OBJECT_WRIT))
                    == (OBJECT_DATA | OBJECT_READ | OBJECT_WRIT))
      {
        ptr = (PTR*) ((obj->org + sizeof(int*) - 1) & ~(sizeof(int*) - 1));
        for (; ptr < (PTR*)(obj->end); ptr++)
          {
            /* Optimization: avoid the call if it doesn't point on an heap.  */
            if (*ptr >= low_addr_heap && *ptr < high_addr_heap)
              /* PTR can always be accessed: SURE is true.  */
	      chkr_address (*ptr, 1);
          }
      }
}

/* Search pointers inside a block of the heap.
   This is a recursive search: all good pointers are used for searching in
   other blocks.  */
static void
search_in_block (struct malloc_header *block, int sure)
{
  PTR *ptr;
  PTR *ptr1;

  /* Check each pointer in this block.  */
  ptr = (PTR *) ((char *) block + be_red_zone + HEADER_SIZE);
  ptr1 = (PTR *) ((char *) ptr + block->info.busy.real_size);
  for (; ptr < ptr1; ptr++)
    {
      if (*ptr >= low_addr_heap && *ptr < high_addr_heap)
	  chkr_address (*ptr, sure);
    }
}

/* Search for leaks in all segments.  */
static void
all_search (void)
{
#ifdef CHKR_PROFILE
  struct timeval tv_beg;
  struct timeval tv_end;
  struct timezone tz;
  int no_profile = 0;
#endif
  
  /* No blocks, so no leaks!  */
  if (_firstmdesc == NULL_MDESC)
    return;
    
#ifdef CHKR_PROFILE
  if (profile_flag)
    if (gettimeofday (&tv_beg, &tz) != 0)
      no_profile = 1;
#endif

  /* looking for pointers ... */
  search_data ();
  search_stack ();
  search_register ();
  
#ifdef CHKR_PROFILE
  if (profile_flag)
    {
      if (gettimeofday (&tv_end, &tz) != 0)
        no_profile = 1;
      if (!no_profile)
        {
          if (tv_beg.tv_usec > tv_end.tv_usec)
            {
              tv_end.tv_usec += 1000000; /* ie 1 sec */
              tv_end.tv_sec -= 1;
            }
          tv_end.tv_usec -= tv_beg.tv_usec;
          tv_end.tv_sec -= tv_beg.tv_sec;
          tv_garbage.tv_usec += tv_end.tv_usec;
          tv_garbage.tv_sec += tv_end.tv_sec;
          if (tv_garbage.tv_usec >= 1000000)
            {
              tv_garbage.tv_sec++;
              tv_garbage.tv_usec -= 1000000;
            }
          chkr_report (M_I_PRF_MA_ET);
          chkr_printf (M_GARBAGE_TIME, (int)tv_end.tv_sec, (int)tv_end.tv_usec);
        }
    }
#endif
}

/* Compare two groups of leaks.
   This function is used to reverse sort.  Called by chkr_qsort().  */
static int
comp_leak (const void *ptr1, const void *ptr2)
{
  return ((struct malloc_header *) *(PTR *) ptr2)->info.busy.g_link.g_number
   - ((struct malloc_header *) *(PTR *) ptr1)->info.busy.g_link.g_number;
}

/* Are the two zones allocated by the same function?  */
static int
comp_history (struct malloc_header *bl1, struct malloc_header *bl2)
{
  PTR *ptr1;
  PTR *ptr2;
  
  /* Do they have the same size ?  */
  if (bl1->info.busy.real_size != bl2->info.busy.real_size)
    return 0;
    
  /* Point to the tops of the history.  */
  ptr1 = (PTR *) ((char *) bl1 + HEADER_SIZE + bl1->size - af_red_zone);
  ptr2 = (PTR *) ((char *) bl2 + HEADER_SIZE + bl2->size - af_red_zone);
  while (*ptr1 && *ptr2 && *ptr1++ == *ptr2++) ;
  if (*ptr1 == *ptr2)
    return 1;
  else
    return 0;
}

/* Gather the brothers.
 * Once a leak has been found, regroup_stack_for_leaks calls
 *  regroup_brother_for_leaks.
 * Because a leak has often brother (other leaks which are allocated by the
 *  same functions), regroup_stack marks known those brothers and counts
 *  them. Regrouping underlines important leaks and avoid to have a very long
 *  list.
 */
static void
regroup_brother_for_leaks (struct malloc_header *zone)
{
  struct malloc_header *block;

  /* Look for brother of zone in all blocks.  */
  for (block = zone; block != NULL_HEADER; block = block->next)
    {
      /* Only busy block can be a leak.  */
      if (block->state != MDBUSY)
	continue;
	
      if ((block->garbage_t & POINT_MASK) == POINT_NOT 
          && block->g_flag == G_ALONE
	  && comp_history (zone, block))
	{
	  /* 'block' is a brother of 'zone'.  It is marked as known.  */
	  block->g_flag = G_CHILD;
	  block->garbage_t = POINT_LEAK | (block->garbage_t & INUSE_MASK);
	  block->info.busy.g_link.g_parent = zone;
	  zone->info.busy.g_link.g_number++;
	}
    }
}

/* Regroup the leaks.
 * regroup_stack_for_leaks looks for unregistred leaks.
 * Each time it finds a leak, it calls regroup_brother_for_leaks which 
 * search brothers.
 */
static void
regroup_stack_for_leaks (void)
{
  struct mdesc *mdp;
  struct malloc_header *block;
  
  /* Search all block.  */
  for (mdp = _firstmdesc; mdp != (struct mdesc*)0; mdp = mdp->info.inmem.next_mdesc)
    for (block = mdp->_firstblock; block != NULL_HEADER; block = block->next)
      {
        /* Only busy blocks can be leaks.  */
        if (block->state != MDBUSY)
	  continue;
        if (((block->garbage_t & POINT_MASK) == POINT_NOT 
            || (block->garbage_t & POINT_MASK) == POINT_MAYBE )
	    && block->g_flag == G_ALONE)
	  {
	    /* A block with no brothers has been found.  It becomes the parent 
	     * of the family.  */
	    block->g_flag = G_PARENT;
	    block->garbage_t = POINT_LEAK | (block->garbage_t & INUSE_MASK);
	    block->info.busy.g_link.g_number = 1;
	    nbr_leaks++;
	    if ((block->garbage_t & POINT_MASK) == POINT_MAYBE)
	      nbr_pot_leaks++;
	    /* Look for other brothers of this block.  */
	    regroup_brother_for_leaks (block);
	  }
      }
}

/* Function used to display all leaks...
   In fact, it displays all the groups of leaks. Two leaks belong to the same
   group if their allocators are the same.  */
static void
disp_leaks (void)
{
  uint i;
  uint total_mem;
  struct mdesc *mdp;
  struct malloc_header **leaks;
  struct malloc_header **pleaks;
  struct malloc_header *block;

  nbr_leaks = 0;
  nbr_pot_leaks = 0;
  memory_lost = 0;

  /* Regroup the brothers together.  */
  regroup_stack_for_leaks ();

  chkr_report (M_I_GAR_MA_ET);
  if (nbr_leaks == 0)
    {
      chkr_printf (M_NO_LEAK);
      return;	/* no leaks, so nothing to do ! */
    }
  else if (nbr_leaks == 1)
    chkr_printf (M_ONE_LEAK, nbr_pot_leaks);
  else
    chkr_printf (M_NBR_LEAKS, nbr_leaks, nbr_pot_leaks);

  /* Store the pointers to the leaks.  */
  pleaks = leaks = (struct malloc_header **) alloca (nbr_leaks * sizeof (void *));
  
  /* Check all block.  */
  for (mdp = _firstmdesc; mdp != (struct mdesc*)0; mdp = mdp->info.inmem.next_mdesc)
    for (block = mdp->_firstblock; block != NULL_HEADER; block = block->next)
      {
        if (block->state != MDBUSY)
	  continue;
        if ((block->garbage_t & POINT_MASK) == POINT_LEAK 
            && block->g_flag == G_PARENT)
	  {
	    *pleaks++ = block;
	    memory_lost += block->info.busy.g_link.g_number * block->info.busy.real_size;
	  }
      }
  pleaks = leaks;
  total_mem = get_total_mem ();
  chkr_printf (M_LEAKS_USE_N_BYTE, memory_lost,	memory_lost / 1024, total_mem / 1024);
  i = (memory_lost * 100) / (total_mem / 100);	/* mutiply by 10000 */
  chkr_printf (M_P_MEM_IS_LOST, i / 100, i % 100);
  chkr_qsort (leaks, nbr_leaks, sizeof (void *), comp_leak);

#if CHKR_SAVESTACK
  chkr_load_symtab ();
#endif
  
  /* Will now contains size used by undisplayed leaks.  */
  memory_lost = 0;
  
  for (i = 0; i < nbr_leaks; i++, pleaks++)
    {
      if ((*pleaks)->info.busy.g_link.g_number 
          * (*pleaks)->info.busy.real_size >= leak_size_threshold)
        {
          chkr_printf (M_FOUND_N2_BYTES,
		   (*pleaks)->info.busy.g_link.g_number,
		   (*pleaks)->info.busy.real_size);
          chkr_printf (M_LOST_IS_PTR,
		   (*pleaks) + be_red_zone + HEADER_SIZE);
#if CHKR_SAVESTACK
          disp_block_history((PTR)(*pleaks) + HEADER_SIZE + (*pleaks)->size - af_red_zone);
#endif
	}
      else
        memory_lost += (*pleaks)->info.busy.g_link.g_number
                    * (*pleaks)->info.busy.real_size;
    }
  if (memory_lost)
    chkr_printf (M_BYTES_NOT_DISPLAYED, memory_lost);
    
}

/* Garbage detector.  Display leaks.  All leaks become new leaks.  */
void
__chkr_garbage_detector (void)
{
  if (!__malloc_initialized)
    return;
  init_detector ();		/* Initialisation */
  all_search ();		/* Searching */
  disp_leaks ();		/* Display leaks */
}

/* Garbage detector which shows only the new leaks. */
void
__chkr_evol_detector (void)
{
  if (!__malloc_initialized)
    return;
  init_evol_detector ();	/* Initialisation */
  all_search ();		/* Searching */
  disp_leaks ();		/* Display leaks */
}

/* Garbage detector, but don't show the leaks so that the next 
   __chkr_evol_detector will not report them.  */
void
__chkr_clear_leaks (void)
{
  if (!__malloc_initialized)
    return;
  init_evol_detector ();
  all_search ();
}

/* Gather the brothers.
 * Once a block has been found, regroup_stack_for_inuse calls
 *  regroup_brother_for_inuse.
 * Because a block has often brother (other leaks which are allocated by the
 *  same functions), regroup_stack marks known those brothers and counts
 *  them. Regrouping underlines important leaks and avoid to have a very long
 *  list.
 */
static void
regroup_brother_for_inuse (struct malloc_header *zone)
{
  struct malloc_header *block;

  /* Look for brother of zone in all blocks.  */
  for (block = zone; block != NULL_HEADER; block = block->next)
    {
      if (block->state == MDBUSY
          && (block->garbage_t & INUSE_MASK) == INUSE_NOT
          && block->g_flag == G_ALONE
	  && comp_history (zone, block))
	{
	  /* 'block' is a brother of 'zone'.  It is marked as known.  */
	  block->g_flag = G_CHILD;
	  block->garbage_t = INUSE_SEEN | (block->garbage_t & POINT_MASK);
	  block->info.busy.g_link.g_parent = zone;
	  zone->info.busy.g_link.g_number++;
	}
    }
}

/* Regroup the blocks
 * regroup_stack_for_inuse looks for unregistred blocks.
 * Each time it finds a block, it calls regroup_brother_for_inuse which 
 * search brothers.
 */
static void
regroup_stack_for_inuse (void)
{
  struct mdesc *mdp;
  struct malloc_header *block;
  
  /* Search all block.  */
  for (mdp = _firstmdesc; mdp != (struct mdesc*)0; mdp = mdp->info.inmem.next_mdesc)
    for (block = mdp->_firstblock; block != NULL_HEADER; block = block->next)
      {
        /* Only busy blocks can be leaks.  */
        if (block->state != MDBUSY)
	  continue;
        if ((block->garbage_t & INUSE_MASK) == INUSE_NOT 
	    && block->g_flag == G_ALONE)
	  {
	    /* A block with no brothers has been found.  It becomes the parent
	     * of the family.  */
	    block->g_flag = G_PARENT;
	    block->garbage_t = INUSE_SEEN | (block->garbage_t & POINT_MASK);
	    block->info.busy.g_link.g_number = 1;
	    nbr_inuse++;
	    /* Look for other brothers of this block.  */
	    regroup_brother_for_inuse (block);
	  }
      }
}

/* Function used to display all blocks...
 * In fact, it displays all the groups of blocks. Two blocks belong to the same
 * group if their allocators are the same.
 */
static void
disp_inuse (void)
{
  uint i;
  uint total_mem;
  struct mdesc *mdp;
  struct malloc_header **inuse;
  struct malloc_header **pinuse;
  struct malloc_header *block;
  int memory_used;

  nbr_inuse = 0;
  memory_used = 0;
  
  /* Regroup the brothers together.  */
  regroup_stack_for_inuse ();

  chkr_report (M_I_GAR_MA_ET);
/*  chkr_printf (M_INUSE_RESULTS); */
  if (nbr_inuse == 0)
    {
      chkr_printf (M_NO_INUSE);
      return;	/* no blocks, so nothing to do !  */
    }
  else if (nbr_inuse == 1)
    chkr_printf (M_ONE_INUSE);
  else
    chkr_printf (M_NBR_INUSE, nbr_inuse);

  /* Store the pointers to the leaks.  */
  pinuse = inuse = (struct malloc_header **) alloca (nbr_inuse * sizeof (PTR));
  
  /* Check all block.  */
  for (mdp = _firstmdesc; mdp != (struct mdesc*)0; mdp = mdp->info.inmem.next_mdesc)
    for (block = mdp->_firstblock; block != NULL_HEADER; block = block->next)
      {
        if (block->state != MDBUSY)
	  continue;
        if ((block->garbage_t & INUSE_MASK) == INUSE_SEEN
            && block->g_flag == G_PARENT)
	  {
	    *inuse++ = block;
	    memory_used += block->info.busy.g_link.g_number * block->info.busy.real_size;
	  }
      }
  inuse = pinuse;
  total_mem = get_total_mem ();
  chkr_printf (M_INUSE_USE_N_BYTE, memory_used, memory_used / 1024, total_mem / 1024);
  chkr_qsort (inuse, nbr_inuse, sizeof (PTR), comp_leak);

#if CHKR_SAVESTACK
  chkr_load_symtab ();
#endif

  for (i = 0; i < nbr_inuse; i++, pinuse++)
    {
      chkr_printf (M_FOUND_N2_BYTES,
		   (*pinuse)->info.busy.g_link.g_number,
		   (*pinuse)->info.busy.real_size);
      chkr_printf (M_LOST_IS_PTR,
		   (*pinuse) + be_red_zone + HEADER_SIZE);
#if CHKR_SAVESTACK
      disp_block_history ((PTR*) (*pinuse) + HEADER_SIZE + (*pinuse)->size - af_red_zone);
#endif
    }
}

/* Inuse.  Display blocks used.  */
void
__chkr_disp_inuse (void)
{
  struct mdesc *mdp;
  struct malloc_header *tmp;
  
  if (!__malloc_initialized)
    return;
  
  for (mdp = _firstmdesc; mdp != (struct mdesc*)0; mdp = mdp->info.inmem.next_mdesc)
    for (tmp = mdp->_firstblock; tmp != NULL_HEADER; tmp = tmp->next)
      {
        tmp->garbage_t &= POINT_MASK;
        tmp->g_flag = G_ALONE;
      }
  disp_inuse ();		/* Display leaks.  */
}

/* Inuse which shows only the new blocks used.  */
void
__chkr_evol_inuse (void)
{
  struct mdesc *mdp;
  struct malloc_header *tmp;
  
  if (!__malloc_initialized)
    return;
  
  for (mdp = _firstmdesc; mdp != (struct mdesc*)0; mdp = mdp->info.inmem.next_mdesc)
    for (tmp = mdp->_firstblock; tmp != NULL_HEADER; tmp = tmp->next)
      tmp->g_flag = G_ALONE;
  disp_inuse ();		/* Display leaks */
}

/* Inuse, but don't show the blocks so that the next 
   __chkr_evol_inuse will not report them.  */
void
__chkr_clear_inuse (void)
{
  struct mdesc *mdp;
  struct malloc_header *tmp;
  
  if (!__malloc_initialized)
    return;
  
  for (mdp = _firstmdesc; mdp != (struct mdesc*)0; mdp = mdp->info.inmem.next_mdesc)
    for (tmp = mdp->_firstblock; tmp != NULL_HEADER; tmp = tmp->next)
      tmp->garbage_t = (tmp->garbage_t & POINT_MASK) | INUSE_SEEN;
}

#endif /* CHKR_GARBAGE */
