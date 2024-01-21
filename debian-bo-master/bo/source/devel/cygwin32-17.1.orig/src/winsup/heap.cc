/* winsup heap manager.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "winsup.h"

/* Initialize the heap at process start up.  */

void
heap_init ()
{
  /* If we're the forkee, we must allocate the heap at exactly the same place
     as our parent.  If not, we don't care where it ends up.  */

  if (u->forkee)
    {
      char *p = (char *) VirtualAlloc (u->base, u->size,
				       MEM_RESERVE, PAGE_READWRITE);
      if (p == NULL)
	{
	  system_printf ("forkee heap_init: unable to allocate heap, win32 error %d\n",
			 GetLastError ());
	  api_fatal ("terminating");
	}
      if (p != u->base)
	{
	  system_printf ("forkee heap_init: heap allocated but not at %p\n",
			 u->base);
	  api_fatal ("terminating");
	}
      if (! VirtualAlloc (u->base, (char *) u->ptr - (char *) u->base,
			  MEM_COMMIT, PAGE_READWRITE))
	{
	  system_printf ("forkee heap_init: MEM_COMMIT failed, win32 error %d\n",
			 GetLastError ());
	  api_fatal ("terminating");
	}
    }
  else
    {
      u->size = s->heap_chunk_size ();
      u->base = (char *) VirtualAlloc (0, u->size,
				       MEM_RESERVE, PAGE_READWRITE);
      if (u->base == NULL)
	{
	  system_printf ("heap_init: unable to allocate heap, win32 error %d\n",
			 GetLastError ());
	  api_fatal ("terminating");
	}
      u->ptr = u->base;
    }
}

/* 
 * FIXME: size_t is the wrong type for incr, because size_t is unsigned
 * but the input to sbrk can be a negative number!
 */

static char *
commit_and_inc (size_t incr)
{
  char *res = (char *) u->ptr;

  if (!VirtualAlloc (u->ptr, incr, MEM_COMMIT, PAGE_READWRITE))
    {
      system_printf ("commit_and_inc: VirtualAlloc failed\n");
      set_errno (ENOMEM);
      res = (char *) -1;
    }
  else
    u->ptr = (char *) u->ptr + incr;

  return res;
}

/* FIXME: a negative incr through split heaps won't work.
   Need to rewrite.  How about only have sbrk use one heap, have malloc not
   use sbrk and instead have malloc use multiple heaps (and if that causes
   problems then abandon sbrk).  The fork code to copy r/w data from parent to
   child will have to be more sophisticated but that should be doable (and the
   result will be more robust).  */

void *
_sbrk (size_t incr_arg)
{
  char *res = (char *)(u->ptr);
  /* local signed version of incr (see FIXME above) */
  int incr = incr_arg;

  syscall_printf ("_sbrk (%d)\n", incr);
  syscall_printf ("_sbrk: base %p, ptr %p, end %p, size %d, avail %d\n",
		  u->base, u->ptr, (char *) u->ptr + u->size,
		  u->size, u->size - ((char *) u->ptr - (char *) u->base));

  /* zero == special case, just return current top */
  if (incr != 0)
    {
      if (incr < 0)
	{
	  /* less than zero: just decrease allocation */
	  res = (char *) u->ptr = 
	    (res + incr < u->base ? u->base : res + incr);
	}
      else 
	{
	  /* Want more space, see if it can be done in the current pool */
	  if ((char *) u->ptr + incr <= (char *) u->base + u->size)
	    {
	      /* have reserves: just commit what has been requested */
	      res = commit_and_inc (incr);
	    }
	  else
	    {
	      /* Try to add more in multiples of heap_chunk_size.  */
	      unsigned int newalloc = s->heap_chunk_size ();

	      while ((char *) u->ptr  + incr >= 
		     (char *) u->base + u->size + newalloc)
		newalloc += s->heap_chunk_size ();

	      debug_printf ("_sbrk: trying to extend heap by %d bytes\n",
			    newalloc);
	      if (!VirtualAlloc ((char *) u->base + u->size,
				 newalloc, 
				 MEM_RESERVE, PAGE_READWRITE))
		{
		  /* Unable to allocate a contiguous block following
		     the current area, try and allocate another area. */
		  char *r;
		  debug_printf ("_sbrk: extending heap failed, starting new one\n");
		  /* FIXME: it's possible that newalloc < incr.  */
		  r = (char *) VirtualAlloc (0, newalloc, MEM_RESERVE, PAGE_READWRITE);
		  if (!r) 
		    {
		      __seterrno ();
		      set_errno (ENOMEM); /* failed in VirtualAlloc */
		      res =  (char *) -1;
		    }
		  else 
		    {
		      debug_printf ("_sbrk: new heap at %p\n", r);
		      u->ptr = u->base = r;
		      u->size = newalloc;
		      u->self->split_heap_p = 1;
		      res = commit_and_inc (incr);
		    }
		}
	      else 
		{
		  /* Now must commit requested incr in two chunks: what was
		     previously in the pool, and what we just added.  */
		  unsigned long size1, size2;

		  debug_printf ("_sbrk: heap successfully extended\n");
		  size1 = (char *) u->base + u->size - (char *) u->ptr;
		  size2 = incr - size1;
		  if ((size1 && !VirtualAlloc (u->ptr, 
					      size1, 
					      MEM_COMMIT, 
					      PAGE_READWRITE))
		      ||  (size2 && !VirtualAlloc ((char *) u->base + u->size, 
						  size2,
						  MEM_COMMIT,
						  PAGE_READWRITE)))
		    {
		      system_printf ("_sbrk: unable to commit extension\n");
		      set_errno (ENOMEM); /* failed in VirtualAlloc */
		      res = (char *) -1;
		    }
		  else
		    {
		      /* succeeded, record new pool size and alloc end */
		      u->size += newalloc;
		      u->ptr = (char *) u->ptr + incr;
		    }
		}
	    }
	}
    }

  syscall_printf ("%p = sbrk (0x%x) (total 0x%x)\n", 
		  res, incr, (char *) u->ptr - (char *) u->base);
  return res;
}
