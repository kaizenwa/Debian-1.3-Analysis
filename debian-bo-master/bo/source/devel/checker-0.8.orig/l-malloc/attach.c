/* attach for mmalloc.  Heavily modified by Tristan Gingold.  */
/* Initialization for access to a mmap'd malloc managed region.
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
#define _MALLOC_INTERNAL
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#define NEED_MM
#include "malloc.h"
#include "errlist.h"


/* Forward declarations/prototypes for local functions.  */

static struct mdesc *reuse (int);

/* Add a new mdesc into the double-linked list.  */
static void
add_mdesc (struct mdesc *mdp)
{
  if (mutex_atomic_swap (&mdesc_lock, 1))
    MUTEX_FAIL;
  if (_lastmdesc)
    _lastmdesc->info.inmem.next_mdesc = mdp;
  mdp->info.inmem.prev_mdesc = _lastmdesc;
  _lastmdesc = mdp;
  mdp->info.inmem.next_mdesc = NULL_MDESC;
  if (!_firstmdesc)
    _firstmdesc = mdp;
  mutex_atomic_swap (&mdesc_lock, 0);
}

/* Initialize access to a mmalloc managed region.

   If FD is a valid file descriptor for an open file then data for the
   mmalloc managed region is mapped to that file, otherwise "/dev/zero"
   is used and the data will not exist in any filesystem object.

   If the open file corresponding to FD is from a previous use of
   mmalloc and passes some basic sanity checks to ensure that it is
   compatible with the current mmalloc package, then it's data is
   mapped in and is immediately accessible at the same addresses in
   the current process as the process that created the file.

   If BASEADDR is not NULL, the mapping is established starting at the
   specified address in the process address space.  If BASEADDR is NULL,
   the mmalloc package chooses a suitable address at which to start the
   mapped region, which will be the value of the previous mapping if
   opening an existing file which was previously built by mmalloc, or
   for new files will be a value chosen by mmap.

   Specifying BASEADDR provides more control over where the regions
   start and how big they can be before bumping into existing mapped
   regions or future mapped regions.

   On success, returns a "malloc descriptor" which is used in subsequent
   calls to other mmalloc package functions.  It is explicitly "void *"
   ("char *" for systems that don't fully support void) so that users
   of the package don't have to worry about the actual implementation
   details.

   On failure returns NULL. */

PTR
mmalloc_attach (int fd, PTR baseaddr)
{
  struct mdesc mtemp;
  struct mdesc *mdp;
  PTR mbase;
  struct stat sbuf;

#ifdef CHECK_INCOMING_ARGS
  chkr_check_addr ((PTR) &fd, sizeof (int), CHKR_RO);
  chkr_check_addr ((PTR) &baseaddr, sizeof (PTR), CHKR_RO);
#endif
  
  /* First check to see if FD is a valid file descriptor, and if so, see
     if the file has any current contents (size > 0).  If it does, then
     attempt to reuse the file.  If we can't reuse the file, either
     because it isn't a valid mmalloc produced file, was produced by an
     obsolete version, or any other reason, then we fail to attach to
     this file. */

  if (fd >= 0)
    {
      if (fstat (fd, &sbuf) < 0)
	{
	  return (NULL);
	}
      else if (sbuf.st_size > 0)
	{
	  return ((PTR) reuse (fd));
	}
    }

  /* We start off with the malloc descriptor allocated on the stack, until
     we build it up enough to call _mmalloc_mmap_morecore() to allocate the
     first page of the region and copy it there.  Ensure that it is zero'd and
     then initialize the fields that we know values for. */

  mdp = &mtemp;
  memset ((char *) mdp, 0, sizeof (mtemp));
  mdp->lock = 1;	/* prevent */
  strncpy (mdp -> magic, MMALLOC_MAGIC, MMALLOC_MAGIC_SIZE);
  mdp->headersize = sizeof (mtemp);
  mdp->version = MMALLOC_VERSION;
  mdp->morecore = __mmalloc_mmap_morecore;
  mdp->fd = fd;
  mdp->base = mdp -> breakval = mdp -> top = baseaddr;
#ifndef MDCHECKER
  mdp->info.inmem.bitmap = (struct bitmapinfo*) sys_malloc(sizeof(struct bitmapinfo));
  mdp->info.inmem.bitmap->base = (unsigned char*) find_bmbase(baseaddr);
  mdp->info.inmem.bitmap->pages = 0;
  mdp->info.inmem.bitmap->size = 0;
  mdp->info.inmem.mapinfo = new_heap(0, mdp);
#endif

  /* If we have not been passed a valid open file descriptor for the file
     to map to, then open /dev/zero and use that to map to. */

  if (mdp -> fd < 0)
    {
      if ((mdp -> fd = open ("/dev/zero", O_RDWR)) < 0)
	{
	  return (NULL);
	}
      else
	{
	  mdp -> flags |= MMALLOC_DEVZERO;
	}
    }

  /*  Now try to map in the first page, copy the malloc descriptor structure
      there, and arrange to return a pointer to this new copy.  If the mapping
      fails, then close the file descriptor if it was opened by us, and arrange
      to return a NULL. */

  if ((mbase = mdp -> morecore (mdp, sizeof (mtemp))) != NULL)
    {
      memcpy (mbase, mdp, sizeof (mtemp));
      mdp = (struct mdesc *) mbase;
      add_mdesc (mdp);
    }
  else
    {
      if (mdp -> flags & MMALLOC_DEVZERO)
	{
	  close (mdp -> fd);
	}
      mdp = NULL;
    }

  mdp->lock = 0;
  
  return (PTR) mdp;
}

/* Given an valid file descriptor on an open file, test to see if that file
   is a valid mmalloc produced file, and if so, attempt to remap it into the
   current process at the same address to which it was previously mapped.

   Note that we have to update the file descriptor number in the malloc-
   descriptor read from the file to match the current valid one, before
   trying to map the file in, and again after a successful mapping and
   after we've switched over to using the mapped in malloc descriptor 
   rather than the temporary one on the stack.

   Once we've switched over to using the mapped in malloc descriptor, we
   have to update the pointer to the morecore function, since it almost
   certainly will be at a different address if the process reusing the
   mapped region is from a different executable.

   Also note that if the heap being remapped previously used the mmcheck()
   routines, we need to update the hooks since their target functions
   will have certainly moved if the executable has changed in any way.
   We do this by calling mmcheck() internally.

   Returns a pointer to the malloc descriptor if successful, or NULL if
   unsuccessful for some reason.  */

static struct mdesc *
reuse (int fd)
{
  struct mdesc mtemp;
  struct mdesc *mdp = NULL;
#ifndef MDCHECKER
  unsigned char *bmbase;
#endif

  if ((lseek (fd, 0L, SEEK_SET) == 0) &&
      (read (fd, (char *) &mtemp, sizeof (mtemp)) == sizeof (mtemp)) &&
      (mtemp.headersize == sizeof (mtemp)) &&
      (strcmp (mtemp.magic, MMALLOC_MAGIC) == 0) &&
      (mtemp.version <= MMALLOC_VERSION))
    {
      mtemp.fd = fd;
#if 0 /* def linux */
      if (mmap (mtemp.base, mtemp.top - mtemp.base,
	       /* MAP_PRIVATE is used only because MAP_SHARED doesn't work. */
	       /* FIXME. TG 1994-09-02 with Linux 1.1.45 */  
	       PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_FIXED,
	       mtemp.fd, 0) == mtemp.base)
#else
      if (mmap (mtemp.base, mtemp.top - mtemp.base,
	       PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED,
	       mtemp.fd, 0) == mtemp.base)
#endif
	{
	  mdp = (struct mdesc *) mtemp.base;
	  mdp->fd = fd;
	  mdp->morecore = __mmalloc_mmap_morecore;
#ifndef MDCHECKER
          mdp->info.inmem.bitmap = (struct bitmapinfo*) sys_malloc(sizeof(struct bitmapinfo));
          /* Read the bitmap.  */
          bmbase = (unsigned char*) find_bmbase(mtemp.base);
          mdp->info.inmem.bitmap->base = bmbase;
          mdp->info.inmem.bitmap->size = mtemp.info.ondisk.len_bitmap;
          mdp->info.inmem.bitmap->pages =
          	(mtemp.info.ondisk.len_bitmap + CHKR_PAGESIZE-1) & ~CHKR_PAGESIZE;
          mmap ((PTR) bmbase, mdp->info.inmem.bitmap->pages, MM_PROT,
               MM_FLAGS, MM_FILE, 0);
          lseek (fd, mtemp.info.ondisk.off_bitmap, SEEK_SET);
          read (fd, (PTR)bmbase, mtemp.info.ondisk.len_bitmap);
	  ftruncate (fd, mtemp.info.ondisk.off_bitmap);
          mdp->info.inmem.mapinfo = new_heap(0, mdp);
          add_mdesc (mdp);
#endif /* !MDCHECKER */
#if 0
	  if (mdp -> mfree_hook != NULL)
	    {
	      mmcheck ((PTR) mdp, (void (*) PARAMS ((void))) NULL);
	    }
#endif
	}
    }
  mdp->lock = 0;
  return mdp;
}
