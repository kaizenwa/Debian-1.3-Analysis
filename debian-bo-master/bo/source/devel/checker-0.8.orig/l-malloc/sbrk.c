/* Sbrk simulator for a malloc implementation.
   Copyright 1993, 1994, 1995 Tristan Gingold
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

#define _MALLOC_INTERNAL
#include "malloc.h"
#include "errlist.h"

/* True if a warning should be emitted.  */
extern int flag_warn_sbrk;

#ifndef DONT_DEFINE_SBRK
/* Emulate sbrk.  */
PTR
sbrk (int incr)
{
  struct mdesc *mdp;
  int round_size;

  /* If Checker was not initialized, call the real sbrk.  This allow crt0.o
     to call sbrk.  */ 
  if (!chkr_is_init)
    return chkr_sbrk (incr);

  /* Emit a warning.  */
  if (flag_warn_sbrk > 1 || (incr != 0 && flag_warn_sbrk))
    {
      chkr_report (M_I_BKC_MA_ET);
      chkr_printf ("sbrk (%d)\n", incr);
      chkr_disp_call_chain ();
    }

#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &incr, sizeof (int), CHKR_RO);
#endif

/* Initialize all, if necessary.  */
  if (__malloc_initialized == 0)
    {
      __malloc_initialized = 1;
      chkr_initialize ();
    }

  mdp = MD_TO_MDP (NULL_MDESC);
  round_size = (incr + LITTLE_SPACE-1) & ~(LITTLE_SPACE-1);

  if (mutex_atomic_swap (&mdp->lock, 1))
    MUTEX_FAIL;

  if (incr >= 0)
    {
      /* Get more memory.  */
      if (mdp->_lastblock && mdp->_lastblock->state == MDBRK)
	{
	  /* sbrk was already called and the last block is MDBRK.  Just alloc
	     more memory.  */
	  PTR res;
	  res = mdp->morecore (mdp, incr);
	  if (res == (PTR) 0)
	    {
	      mutex_atomic_swap (&mdp->lock, 0);
	      errno = ENOMEM;
	      return (PTR) -1;
	    }
	  mdp->_lastblock->info.brk.real_size += incr;
	  mutex_atomic_swap (&mdp->lock, 0);
#ifdef CHKR_HEAPBITMAP
          if (incr != 0)
	    chkr_set_right (res, incr, CHKR_WO);
#endif
	  memset (res, 0, incr);
	  return res;
	}
      else if (mdp->_lastblock && mdp->_lastblock->state == MDFREE)
	{
	  /* The last block is a FREE block.  Adjust its size.  */
	  if (mdp->morecore (mdp, round_size - mdp->_lastblock->size) == (PTR) 0)
	    {
	      mutex_atomic_swap(&mdp->lock, 0);
	      errno = ENOMEM;
	      return (PTR) -1;
	    }
	  mdp->_lastblock->state = MDBRK;
	  mdp->_lastblock->size = round_size;
	  mdp->_lastblock->info.brk.real_size = incr;
	  mutex_atomic_swap (&mdp->lock, 0);
	  return (PTR) mdp->_lastblock + HEADER_SIZE;
	}
      else
	{
	  /* Alloc a new block for SBRK.  */
	  struct malloc_header *res;

	  res = (struct malloc_header *) mdp->morecore (mdp, round_size + HEADER_SIZE);
	  if (res == NULL_HEADER)
	    {
	      mutex_atomic_swap (&mdp->lock, 0);
	      errno = ENOMEM;
	      return (PTR) -1;
	    }
	  res->size = round_size;
	  res->prev = mdp->_lastblock;
	  if (mdp->_lastblock)
	    mdp->_lastblock->next = res;
	  res->next = NULL_HEADER;
	  mdp->_lastblock = res;
	  if (!mdp->_firstblock)
	    mdp->_firstblock = res;
	  res->state = MDBRK;
	  res->s_diff = res->size - incr;
	  res->info.brk.real_size = incr;
	  mutex_atomic_swap (&mdp->lock, 0);
#ifdef CHKR_HEAPBITMAP
          if (incr != 0)
	    chkr_set_right ((PTR) res + HEADER_SIZE, incr, CHKR_WO);
#endif
	  memset ((PTR) res + HEADER_SIZE, 0, incr);
	  return (PTR) res + HEADER_SIZE;
	}
    }
  else
    {
      /* Realease memory.  Be sure the last block is BRK.  */
      PTR res;
      if (!mdp->_lastblock || mdp->_lastblock->state != MDBRK)
	{
	  mutex_atomic_swap (&mdp->lock, 0);
	  /* The last block is not BRK...  */
	  chkr_perror (M_M_SBA_BR_ET);
	  errno = ENOMEM;
	  return (PTR) -1;
	}
      res = (PTR) mdp->_lastblock + HEADER_SIZE;
      if (mdp->_lastblock->info.brk.real_size + incr < 0)
	{
	  /* INCR is too big.  */
	  chkr_perror (M_M_SBA_BR_ET);
	  incr = -mdp->_lastblock->info.brk.real_size;
	}
      if (mdp->_lastblock->info.brk.real_size + incr == 0)
	{
	  if (mdp->_firstblock == mdp->_lastblock)
	    mdp->_firstblock = NULL_HEADER;
	  mdp->_lastblock = mdp->_lastblock->prev;
	  if (mdp->_lastblock)
	    mdp->_lastblock->next = NULL_HEADER;
	  mdp->morecore (mdp, incr - HEADER_SIZE);
	  mutex_atomic_swap (&mdp->lock, 0);
	  return res;
	}
      else
	{
	  mdp->morecore (mdp, round_size);
	  mdp->_lastblock->size += round_size;
	  res += mdp->_lastblock->info.brk.real_size;
	  mdp->_lastblock->info.brk.real_size += incr;
	  mutex_atomic_swap (&mdp->lock, 0);
	  return res;
	}
    }
}
#endif /* DONT_DEFINE_SBRK */
