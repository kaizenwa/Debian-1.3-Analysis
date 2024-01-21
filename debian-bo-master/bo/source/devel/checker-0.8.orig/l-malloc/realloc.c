/* Change the size of a block allocated by `malloc'.
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

#define _MALLOC_INTERNAL
#include "malloc.h"

#ifdef CHECKER
#include "errlist.h"
#endif

#ifdef CHKR_PROFILE
/* Number of calls to realloc/mrealloc.  */
uint nbr_realloc_calls = 0;
#endif

/* Resize the given region to the new size, returning a pointer
   to the (possibly moved) region.  */
static PTR
realloc_1 (struct mdesc *mdp, PTR ptr, size_t size)
{
  PTR result;
  struct malloc_header *block;
  size_t old_size;

  mdp = MD_TO_MDP(mdp);

  if (ptr == NULL)
    return malloc_1 (mdp, size);

  /* Imagine you call realloc before malloc.  */
  if (!__malloc_initialized)	
    {
      chkr_perror (M_M_FBM_FR_ET);
      return NULL;
    }

  /* Compute the address of the header from ptr.  */
  block = (struct malloc_header *) (ptr - HEADER_SIZE - be_red_zone);
  
  /* Check if the address is good.  */
  if (block != find_header (mdp, ptr, 1))
    {
      chkr_perror (M_M_FBA_FR_ET);
      return NULL;
    }
    
  /* A little check: to be freed, the block must be busy.  */
  if (block->state != MDBUSY)
    {
      chkr_perror (M_M_FFB_FR_ET);
      return NULL;
    }

  if (size == 0)
    {
      free_1 (mdp, ptr);
      return malloc_1 (mdp, 0);
    }

  /* Very simple realloc implementation
      But easy to do and bugs free ( I hope !)
      Anyway, realloc is very rarely used ...
      memory management in C++ implement only malloc (new) and free (delete).
   */
  result = malloc_1 (mdp, size);
  if (result != NULL)
    {
      old_size = block->info.busy.real_size;
      memcpy (result, ptr, old_size < size ? old_size : size);
#ifdef CHKR_HEAPBITMAP

      /* This was the first bug !!!  */
      chkr_copy_bitmap (result, ptr, old_size > size ? size : old_size);
      
      /* Disable reading and writing right in the block.  */
      chkr_set_right ((PTR) block + HEADER_SIZE + be_red_zone,
		      block->info.busy.real_size, CHKR_UN);
#endif /* CHKR_HEAPBITMAP */
      free_1 (mdp, ptr);
    }
  return result;
}

PTR
realloc (PTR ptr, size_t size)
{
  PTR result;

#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &ptr, sizeof (PTR), CHKR_RO);
  chkr_check_addr ((PTR) &size, sizeof (size_t), CHKR_RO);
#endif

#ifdef CHKR_PROFILE
  /* Realloc is called.  */
  nbr_realloc_calls++;
#endif

  size = test_malloc0 (size, "realloc");
  result = realloc_1 (NULL_MDESC, ptr, size);

  if (trace_malloc_flag)
    chkr_printf ("# realloc (%p, %d) = %p;\n", ptr, (uint) size, result);

  return result;
}

PTR
mrealloc (struct mdesc *mdp, PTR ptr, size_t size)
{
  PTR result;

#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &mdp, sizeof (mdp), CHKR_RO);
  chkr_check_addr ((PTR) &ptr, sizeof (PTR), CHKR_RO);
  chkr_check_addr ((PTR) &size, sizeof (size_t), CHKR_RO);
#endif

#ifdef CHKR_PROFILE
  /* relloc is called.  */
  nbr_realloc_calls++;
#endif  

  size = test_malloc0 (size, "mrealloc");
  result = realloc_1 (mdp, ptr, size);
  
  if (trace_malloc_flag)
    chkr_printf ("# mrealloc (%p, %p, %d) = %p;\n", mdp, ptr, (uint) size, result);

  return result;
}
