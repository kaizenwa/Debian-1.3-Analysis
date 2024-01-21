/* Calloc implementation.
   Copyright 1993, 1994, 1995, 1996 Tristan Gingold
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

#ifndef	_MALLOC_INTERNAL
#define	_MALLOC_INTERNAL
#include "malloc.h"
#endif

/* Allocate an array of NMEMB elements each SIZE bytes long.
   The entire array is initialized to zeros.  */
PTR
calloc (size_t nmemb, size_t size)
{
  struct mdesc *mdp = MD_TO_MDP (NULL_MDESC);
  register PTR result;
  register size_t total_size;

#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &nmemb, sizeof (nmemb), CHKR_RO);
  chkr_check_addr ((PTR) &size, sizeof (size_t), CHKR_RO);
#endif

  total_size = test_malloc0 (nmemb * size, "calloc");
  result = malloc_1 (mdp, total_size);
  if (result != NULL)
    {
      (void) memset (result, 0, total_size);
#ifdef CHKR_HEAPBITMAP
      chkr_set_right ((PTR) result, total_size, CHKR_RW);
#endif
    }
  
  if (trace_malloc_flag)
    chkr_printf ("# calloc (%d, %d) = %p;\n", (uint) nmemb, (uint) size, result);
    
  return result;
}

/* Allocate an array of NMEMB elements each SIZE bytes long.
   The entire array is initialized to zeros.  */
PTR
mcalloc (struct mdesc *mdp, size_t nmemb, size_t size)
{
  register PTR result;
  register size_t total_size;

#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &nmemb, sizeof (nmemb), CHKR_RO);
  chkr_check_addr ((PTR) &size, sizeof (size_t), CHKR_RO);
  chkr_check_addr ((PTR) &mdp, sizeof (mdp), CHKR_RW);
#endif

  mdp = MD_TO_MDP (mdp);
  total_size = test_malloc0 (nmemb * size, "mcalloc");
  result = malloc_1 (mdp, total_size);
  if (result != NULL)
    {
      (void) memset (result, 0, total_size);
#ifdef CHKR_HEAPBITMAP
      chkr_set_right (result, total_size, CHKR_RW);
#endif
    }
    
  if (trace_malloc_flag)
    chkr_printf ("# mcalloc (%p, %d, %d) = %p;\n", mdp, (uint) nmemb,
    						   (uint) size, result);
    
  return result;
}
