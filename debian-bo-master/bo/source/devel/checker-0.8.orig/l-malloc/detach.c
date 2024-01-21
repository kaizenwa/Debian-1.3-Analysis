/* detach for mmalloc.  Heavily modified by Tristan Gingold.  */
/* Finish access to a mmap'd malloc managed region.
   Copyright 1992 Free Software Foundation, Inc.

   Contributed by Fred Fish at Cygnus Support.   fnf@cygnus.com

This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <unistd.h>
#include <fcntl.h>
#define _MALLOC_INTERNAL
#define NEED_MM
#include "malloc.h"
#include "errlist.h"

/* Terminate access to a mmalloc managed region by unmapping all memory pages
   associated with the region, and closing the file descriptor if it is one
   that we opened.

   Returns NULL on success.

   Returns the malloc descriptor on failure, which can subsequently be used
   for further action, such as obtaining more information about the nature of
   the failure by examining the preserved errno value.

   Note that the malloc descriptor that we are using is currently located in
   region we are about to unmap, so we first make a local copy of it on the
   stack and use the copy. */

PTR
mmalloc_detach (PTR md)
{
  struct mdesc mtemp;
  struct mdesc *mdp = (struct mdesc*) md;
  unsigned int mapinfo;
  struct bitmapinfo *bitmap;
#ifndef MDCHECKER
  unsigned int pos;
#endif  

#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &md, sizeof(PTR), CHKR_RO);	/* FIXME */
#endif

  if (md != NULL)
    {
      mtemp = *(struct mdesc *) md;

      /* Prevent access from all mdesc and from this mdesc.  */
      if (mutex_atomic_swap (&mdesc_lock, 1)
          || mutex_atomic_swap (&mdp->lock, 1))
        MUTEX_FAIL;

      /* Unlink this mdesc.  */
      if (_firstmdesc == mdp)
        _firstmdesc = mdp->info.inmem.next_mdesc;
      else
        mdp->info.inmem.prev_mdesc->info.inmem.next_mdesc = mdp->info.inmem.next_mdesc;
      if (_lastmdesc == mdp)
        _lastmdesc = mdp->info.inmem.prev_mdesc;
      else
        mdp->info.inmem.next_mdesc->info.inmem.prev_mdesc = mdp->info.inmem.prev_mdesc;

      /* Access to other mdesc are now allowed.  */
      mutex_atomic_swap (&mdesc_lock, 0);
        
      mapinfo = mdp->info.inmem.mapinfo;
      bitmap = mdp->info.inmem.bitmap;

      if (mtemp.fd >= 0 && !(mtemp.flags & MMALLOC_DEVZERO))
        {
          /* Save the bitmap at the end of the file.  */
          ftruncate (mtemp.fd, mtemp.top - mtemp.base);
#ifndef MDCHECKER
          pos = lseek (mtemp.fd, 0, SEEK_END);
          write (mtemp.fd, (PTR)bitmap->base, bitmap->size);
          mdp->info.ondisk.off_bitmap = pos;
          mdp->info.ondisk.len_bitmap = bitmap->size;

#if 0 /* def linux */
	  /* Save the data.  */
          lseek (mtemp.fd, 0, SEEK_SET);
          write (mtemp.fd, mtemp.base, mtemp.top - mtemp.base);
#endif
#endif /* !MDCHECKER */
        }
      /* Now unmap all the pages associated with this region by asking for a
	 negative increment equal to the current size of the region.  */
      
      if ((mtemp.morecore (&mtemp, mtemp.base - mtemp.breakval)) == NULL)
	{
	  /* Update the original malloc descriptor with any changes.  */
	  *(struct mdesc *) md = mtemp;
	}
      else
	{
	  if (mtemp.flags & MMALLOC_DEVZERO)
            close (mtemp.fd);
            
          /* Remove the history.  */
          if (mtemp.info.inmem.history)
            sys_free((PTR)mtemp.info.inmem.history);
#ifndef MDCHECKER

	  /* Remove the bitmap.  */
          munmap ((PTR)bitmap->base, bitmap->pages * CHKR_PAGESIZE);
          sys_free ((PTR)bitmap);
          
          /* Remove the mapinfo.  */
          remove_heap (mapinfo);
#endif /* !MDCHECKER */
	  md = NULL;
	}
    }

  return md;
}
