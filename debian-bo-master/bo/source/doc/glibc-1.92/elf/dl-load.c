/* _dl_map_object -- Map in a shared object's segments from the file.
Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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

#include <link.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include "dynamic-link.h"


/* On some systems, no flag bits are given to specify file mapping.  */
#ifndef MAP_FILE
#define MAP_FILE	0
#endif

/* The right way to map in the shared library files is MAP_COPY, which
   makes a virtual copy of the data at the time of the mmap call; this
   guarantees the mapped pages will be consistent even if the file is
   overwritten.  Some losing VM systems like Linux's lack MAP_COPY.  All we
   get is MAP_PRIVATE, which copies each page when it is modified; this
   means if the file is overwritten, we may at some point get some pages
   from the new version after starting with pages from the old version.  */
#ifndef MAP_COPY
#define MAP_COPY	MAP_PRIVATE
#endif


#include <endian.h>
#if BYTE_ORDER == BIG_ENDIAN
#define byteorder ELFDATA2MSB
#define byteorder_name "big-endian"
#elif BYTE_ORDER == LITTLE_ENDIAN
#define byteorder ELFDATA2LSB
#define byteorder_name "little-endian"
#else
#error "Unknown BYTE_ORDER " BYTE_ORDER
#define byteorder ELFDATANONE
#endif

#define STRING(x) #x

int _dl_zerofd = -1;
size_t _dl_pagesize;


/* Map in the shared object NAME, actually located in REALNAME, and already
   opened on FD.  */

struct link_map *
_dl_map_object_from_fd (const char *name, int fd, char *realname,
			struct link_map *loader, int l_type)
{
  struct link_map *l;
  void *file_mapping = NULL;
  size_t mapping_size = 0;

#define LOSE(s) lose (0, (s))
  void lose (int code, const char *msg)
    {
      (void) __close (fd);
      if (file_mapping)
	__munmap (file_mapping, mapping_size);
      if (l)
	{
	  /* Remove the stillborn object from the list and free it.  */
	  if (l->l_prev)
	    l->l_prev->l_next = l->l_next;
	  if (l->l_next)
	    l->l_next->l_prev = l->l_prev;
	  free (l);
	}
      free (realname);
      _dl_signal_error (code, name, msg);
    }

  inline caddr_t map_segment (ElfW(Addr) mapstart, size_t len,
			      int prot, int fixed, off_t offset)
    {
      caddr_t mapat = __mmap ((caddr_t) mapstart, len, prot,
			      fixed|MAP_COPY|MAP_FILE,
			      fd, offset);
      if (mapat == (caddr_t) -1)
	lose (errno, "failed to map segment from shared object");
      return mapat;
    }

  /* Make sure LOCATION is mapped in.  */
  void *map (off_t location, size_t size)
    {
      if ((off_t) mapping_size <= location + (off_t) size)
	{
	  void *result;
	  if (file_mapping)
	    __munmap (file_mapping, mapping_size);
	  mapping_size = (location + size + 1 + _dl_pagesize - 1);
	  mapping_size &= ~(_dl_pagesize - 1);
	  result = __mmap (file_mapping, mapping_size, PROT_READ,
			   MAP_COPY|MAP_FILE, fd, 0);
	  if (result == (void *) -1)
	    lose (errno, "cannot map file data");
	  file_mapping = result;
	}
      return file_mapping + location;
    }

  const ElfW(Ehdr) *header;
  const ElfW(Phdr) *phdr;
  const ElfW(Phdr) *ph;
  int type;

  /* Look again to see if the real name matched another already loaded.  */
  for (l = _dl_loaded; l; l = l->l_next)
    if (! strcmp (realname, l->l_name))
      {
	/* The object is already loaded.
	   Just bump its reference count and return it.  */
	__close (fd);
	free (realname);
	++l->l_opencount;
	return l;
      }

  if (_dl_pagesize == 0)
    _dl_pagesize = __getpagesize ();

  /* Map in the first page to read the header.  */
  header = map (0, sizeof *header);

  /* Check the header for basic validity.  */
  if (*(Elf32_Word *) &header->e_ident !=
#if BYTE_ORDER == LITTLE_ENDIAN
      ((ELFMAG0 << (EI_MAG0 * 8)) |
       (ELFMAG1 << (EI_MAG1 * 8)) |
       (ELFMAG2 << (EI_MAG2 * 8)) |
       (ELFMAG3 << (EI_MAG3 * 8)))
#else
      ((ELFMAG0 << (EI_MAG3 * 8)) |
       (ELFMAG1 << (EI_MAG2 * 8)) |
       (ELFMAG2 << (EI_MAG1 * 8)) |
       (ELFMAG3 << (EI_MAG0 * 8)))
#endif
      )
    LOSE ("invalid ELF header");
#define ELF32_CLASS ELFCLASS32
#define ELF64_CLASS ELFCLASS64
  if (header->e_ident[EI_CLASS] != ELFW(CLASS))
    LOSE ("ELF file class not " STRING(__ELF_WORDSIZE) "-bit");
  if (header->e_ident[EI_DATA] != byteorder)
    LOSE ("ELF file data encoding not " byteorder_name);
  if (header->e_ident[EI_VERSION] != EV_CURRENT)
    LOSE ("ELF file version ident not " STRING(EV_CURRENT));
  if (header->e_version != EV_CURRENT)
    LOSE ("ELF file version not " STRING(EV_CURRENT));
  if (! elf_machine_matches_host (header->e_machine))
    LOSE ("ELF file machine architecture not " ELF_MACHINE_NAME);
  if (header->e_phentsize != sizeof (ElfW(Phdr)))
    LOSE ("ELF file's phentsize not the expected size");

  if (_dl_zerofd == -1)
    {
      _dl_zerofd = _dl_sysdep_open_zero_fill ();
      if (_dl_zerofd == -1)
	{
	  __close (fd);
	  _dl_signal_error (errno, NULL, "cannot open zero fill device");
	}
    }

  /* Enter the new object in the list of loaded objects.  */
  l = _dl_new_object (realname, name, l_type);
  if (! l)
    lose (ENOMEM, "cannot create shared object descriptor");
  l->l_opencount = 1;
  l->l_loader = loader;

  /* Extract the remaining details we need from the ELF header
     and then map in the program header table.  */
  l->l_entry = header->e_entry;
  type = header->e_type;
  l->l_phnum = header->e_phnum;
  phdr = map (header->e_phoff, l->l_phnum * sizeof (ElfW(Phdr)));

  {
    /* Scan the program header table, collecting its load commands.  */
    struct loadcmd
      {
	ElfW(Addr) mapstart, mapend, dataend, allocend;
	off_t mapoff;
	int prot;
      } loadcmds[l->l_phnum], *c;
    size_t nloadcmds = 0;

    l->l_ld = 0;
    l->l_phdr = 0;
    l->l_addr = 0;
    for (ph = phdr; ph < &phdr[l->l_phnum]; ++ph)
      switch (ph->p_type)
	{
	  /* These entries tell us where to find things once the file's
	     segments are mapped in.  We record the addresses it says
	     verbatim, and later correct for the run-time load address.  */
	case PT_DYNAMIC:
	  l->l_ld = (void *) ph->p_vaddr;
	  break;
	case PT_PHDR:
	  l->l_phdr = (void *) ph->p_vaddr;
	  break;

	case PT_LOAD:
	  /* A load command tells us to map in part of the file.
	     We record the load commands and process them all later.  */
	  if (ph->p_align % _dl_pagesize != 0)
	    LOSE ("ELF load command alignment not page-aligned");
	  if ((ph->p_vaddr - ph->p_offset) % ph->p_align)
	    LOSE ("ELF load command address/offset not properly aligned");
	  {
	    struct loadcmd *c = &loadcmds[nloadcmds++];
	    c->mapstart = ph->p_vaddr & ~(ph->p_align - 1);
	    c->mapend = ((ph->p_vaddr + ph->p_filesz + _dl_pagesize - 1)
			 & ~(_dl_pagesize - 1));
	    c->dataend = ph->p_vaddr + ph->p_filesz;
	    c->allocend = ph->p_vaddr + ph->p_memsz;
	    c->mapoff = ph->p_offset & ~(ph->p_align - 1);
	    c->prot = 0;
	    if (ph->p_flags & PF_R)
	      c->prot |= PROT_READ;
	    if (ph->p_flags & PF_W)
	      c->prot |= PROT_WRITE;
	    if (ph->p_flags & PF_X)
	      c->prot |= PROT_EXEC;
	    break;
	  }
	}

    /* We are done reading the file's headers now.  Unmap them.  */
    __munmap (file_mapping, mapping_size);

    /* Now process the load commands and map segments into memory.  */
    c = loadcmds;

    if (type == ET_DYN || type == ET_REL)
      {
	/* This is a position-independent shared object.  We can let the
	   kernel map it anywhere it likes, but we must have space for all
	   the segments in their specified positions relative to the first.
	   So we map the first segment without MAP_FIXED, but with its
	   extent increased to cover all the segments.  Then we remove
	   access from excess portion, and there is known sufficient space
	   there to remap from the later segments.  */
 	caddr_t mapat;
	mapat = map_segment (c->mapstart,
			     loadcmds[nloadcmds - 1].allocend - c->mapstart,
			     c->prot, 0, c->mapoff);
	l->l_addr = (ElfW(Addr)) mapat - c->mapstart;

	/* Change protection on the excess portion to disallow all access;
	   the portions we do not remap later will be inaccessible as if
	   unallocated.  Then jump into the normal segment-mapping loop to
	   handle the portion of the segment past the end of the file
	   mapping.  */
	__mprotect ((caddr_t) (l->l_addr + c->mapend),
		    loadcmds[nloadcmds - 1].allocend - c->mapend,
		    0);
	goto postmap;
      }

    while (c < &loadcmds[nloadcmds])
      {
	if (c->mapend > c->mapstart)
	  /* Map the segment contents from the file.  */
	  map_segment (l->l_addr + c->mapstart, c->mapend - c->mapstart,
		       c->prot, MAP_FIXED, c->mapoff);

      postmap:
	if (c->allocend > c->dataend)
	  {
	    /* Extra zero pages should appear at the end of this segment,
	       after the data mapped from the file.   */
	    ElfW(Addr) zero, zeroend, zeropage;

	    zero = l->l_addr + c->dataend;
	    zeroend = l->l_addr + c->allocend;
	    zeropage = (zero + _dl_pagesize - 1) & ~(_dl_pagesize - 1);

	    if (zeroend < zeropage)
	      /* All the extra data is in the last page of the segment.
		 We can just zero it.  */
	      zeropage = zeroend;

	    if (zeropage > zero)
	      {
		/* Zero the final part of the last page of the segment.  */
		if ((c->prot & PROT_WRITE) == 0)
		  {
		    /* Dag nab it.  */
		    if (__mprotect ((caddr_t) (zero & ~(_dl_pagesize - 1)),
				    _dl_pagesize, c->prot|PROT_WRITE) < 0)
		      lose (errno, "cannot change memory protections");
		  }
		memset ((void *) zero, 0, zeropage - zero);
		if ((c->prot & PROT_WRITE) == 0)
		  __mprotect ((caddr_t) (zero & ~(_dl_pagesize - 1)),
			      _dl_pagesize, c->prot);
	      }

	    if (zeroend > zeropage)
	      {
		/* Map the remaining zero pages in from the zero fill FD.  */
		caddr_t mapat;
		mapat = __mmap ((caddr_t) zeropage, zeroend - zeropage,
				c->prot, MAP_ANON|MAP_PRIVATE|MAP_FIXED,
				_dl_zerofd, 0);
		if (mapat == (caddr_t) -1)
		  lose (errno, "cannot map zero-fill pages");
	      }
	  }

	++c;
      }

    if (l->l_phdr == 0)
      {
	/* There was no PT_PHDR specified.  We need to find the phdr in the
           load image ourselves.  We assume it is in fact in the load image
           somewhere, and that the first load command starts at the
           beginning of the file and thus contains the ELF file header.  */
	ElfW(Addr) bof = l->l_addr + loadcmds[0].mapstart;
	assert (loadcmds[0].mapoff == 0);
	l->l_phdr = (void *) (bof + ((const ElfW(Ehdr) *) bof)->e_phoff);
      }
    else
      /* Adjust the PT_PHDR value by the runtime load address.  */
      (ElfW(Addr)) l->l_phdr += l->l_addr;
  }

  /* We are done mapping in the file.  We no longer need the descriptor.  */
  __close (fd);

  if (l->l_type == lt_library && type == ET_EXEC)
    l->l_type = lt_executable;

  if (l->l_ld == 0)
    {
      if (type == ET_DYN)
	LOSE ("object file has no dynamic section");
    }
  else
    (ElfW(Addr)) l->l_ld += l->l_addr;

  l->l_entry += l->l_addr;

  elf_get_dynamic_info (l->l_ld, l->l_info);
  if (l->l_info[DT_HASH])
    _dl_setup_hash (l);

  return l;
}

/* Try to open NAME in one of the directories in DIRPATH.
   Return the fd, or -1.  If successful, fill in *REALNAME
   with the malloc'd full directory name.  */

static int
open_path (const char *name, size_t namelen,
	   const char *dirpath,
	   char **realname)
{
  char *buf;
  const char *p;
  int fd;

  p = dirpath;
  if (p == NULL || *p == '\0')
    {
      errno = ENOENT;
      return -1;
    }

  buf = __alloca (strlen (dirpath) + 1 + namelen);
  do
    {
      size_t buflen;

      dirpath = p;
      p = strpbrk (dirpath, ":;");
      if (p == NULL)
	p = strchr (dirpath, '\0');

      if (p == dirpath)
	{
	  /* Two adjacent colons, or a colon at the beginning or the end of
	     the path means to search the current directory.  */
	  (void) memcpy (buf, name, namelen);
	  buflen = namelen;
	}
      else
	{
	  /* Construct the pathname to try.  */
	  (void) memcpy (buf, dirpath, p - dirpath);
	  buf[p - dirpath] = '/';
	  (void) memcpy (&buf[(p - dirpath) + 1], name, namelen);
	  buflen = p - dirpath + 1 + namelen;
	}

      fd = __open (buf, O_RDONLY);
      if (fd != -1)
	{
	  *realname = malloc (buflen);
	  if (*realname)
	    {
	      memcpy (*realname, buf, buflen);
	      return fd;
	    }
	  else
	    {
	      /* No memory for the name, we certainly won't be able
		 to load and link it.  */
	      __close (fd);
	      return -1;
	    }
	}
      if (errno != ENOENT && errno != EACCES)
	/* The file exists and is readable, but something went wrong.  */
	return -1;
    }
  while (*p++ != '\0');

  return -1;
}

/* Map in the shared object file NAME.  */

struct link_map *
_dl_map_object (struct link_map *loader, const char *name, int type)
{
  int fd;
  char *realname;
  struct link_map *l;

  /* Look for this name among those already loaded.  */
  for (l = _dl_loaded; l; l = l->l_next)
    if (! strcmp (name, l->l_libname))
      {
	/* The object is already loaded.
	   Just bump its reference count and return it.  */
	++l->l_opencount;
	return l;
      }

  if (strchr (name, '/') == NULL)
    {
      /* Search for NAME in several places.  */

      size_t namelen = strlen (name) + 1;

      inline void trypath (const char *dirpath)
	{
	  fd = open_path (name, namelen, dirpath, &realname);
	}

      fd = -1;

      /* First try the DT_RPATH of the dependent object that caused NAME
	 to be loaded.  Then that object's dependent, and on up.  */
      for (l = loader; fd == -1 && l; l = l->l_loader)
	if (l && l->l_info[DT_RPATH])
	  trypath ((const char *) (l->l_addr +
				   l->l_info[DT_STRTAB]->d_un.d_ptr +
				   l->l_info[DT_RPATH]->d_un.d_val));
      /* If dynamically linked, try the DT_RPATH of the executable itself.  */
      l = _dl_loaded;
      if (fd == -1 && l && l->l_type != lt_loaded && l->l_info[DT_RPATH])
	trypath ((const char *) (l->l_addr +
				 l->l_info[DT_STRTAB]->d_un.d_ptr +
				 l->l_info[DT_RPATH]->d_un.d_val));
      /* Try an environment variable (unless setuid).  */
      if (fd == -1 && ! _dl_secure)
	trypath (getenv ("LD_LIBRARY_PATH"));
      /* Finally, try the default path.  */
      if (fd == -1)
	{
	  extern const char *_dl_rpath;	/* Set in rtld.c. */
	  trypath (_dl_rpath);
	}
    }
  else
    {
      fd = __open (name, O_RDONLY);
      if (fd != -1)
	{
	  size_t len = strlen (name) + 1;
	  realname = malloc (len);
	  if (realname)
	    memcpy (realname, name, len);
	  else
	    {
	      __close (fd);
	      fd = -1;
	    }
	}
    }

  if (fd == -1)
    _dl_signal_error (errno, name, "cannot open shared object file");

  return _dl_map_object_from_fd (name, fd, realname, loader, type);
}
