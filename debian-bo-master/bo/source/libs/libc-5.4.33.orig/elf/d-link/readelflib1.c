/* Load an ELF sharable library into memory.

   Copyright (C) 1993, Eric Youngdale.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */



/* This file contains the helper routines to load an ELF sharable
   library into memory and add the symbol table info to the chain. */

#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>
#include "hash.h"
#include "linuxelf.h"
#include "elf_target.h"
#ifdef IBCS_COMPATIBLE
#include <ibcs/unistd.h>
#else
#include <linux/unistd.h>
#endif
#include "syscall.h"
#include "string.h"
#ifdef USE_CACHE
#include "ld_so_config.h"
#endif

#ifdef USE_CACHE

/* Look up a library in the cache /etc/ld.so.cache */
static caddr_t  _dl_lookup_cache (char *libname) {
  int i, fd;
  struct stat st;
  caddr_t cache_addr = NULL;
  short libnamelen = _dl_strlen (libname);
  header_t *header;
  libentry_t *libent;

  if (_dl_stat (LDSO_CACHE, &st) || (fd = _dl_open (LDSO_CACHE, O_RDONLY)) < 0)
    return NULL;

  if ((cache_addr = (caddr_t) _dl_mmap (0, st.st_size, PROT_READ, MAP_SHARED, fd, 0)) ==
      (caddr_t) - 1) {
    _dl_printf("can't map cache\n");
    _dl_close (fd);
    return NULL;
  }
  _dl_close (fd);

  if (_dl_memcmp (((header_t *) cache_addr)->magic, LDSO_CACHE_MAGIC,
		   LDSO_CACHE_MAGIC_LEN)) {
    _dl_printf ("cache is corrupt\n");
    goto fail;
  }
  if (_dl_memcmp (((header_t *) cache_addr)->version, LDSO_CACHE_VER,
		   LDSO_CACHE_VER_LEN)) {
    _dl_printf ("cache has wrong version - expecting %s\n", 
		LDSO_CACHE_VER);
    goto fail;
  }
  for (i = 0,
       header = (header_t *) cache_addr,
       libent = (libentry_t *) (cache_addr + sizeof (header_t));
       cache_addr && i < header->nlibs; i++)
    if (libent[i].flags == LIB_ELF &&
	libnamelen <= (libent[i].libnamelen & 0xff) &&
	!_dl_memcmp (libname, cache_addr + header->liboffset +
		      libent[i].liboffset, libnamelen)) {
      char *ret = _dl_malloc ((libent[i].libnamelen & 0xff) +
			      libent[i].dirnamelen + 2);
      _dl_memcpy (ret,
		   cache_addr + header->diroffset +
		   libent[i].diroffset, libent[i].dirnamelen);
      *(ret + libent[i].dirnamelen) = '/';
      _dl_memcpy (ret + libent[i].dirnamelen + 1,
		   cache_addr + header->liboffset +
		   libent[i].liboffset, libent[i].libnamelen & 0xff);
      _dl_munmap (cache_addr, st.st_size);
      return ret;
    }
fail:
  _dl_munmap (cache_addr, st.st_size);
  return NULL;

}
#endif

/*
 * Used to return error codes back to dlopen et. al.
 */

unsigned int _dl_error_number;
unsigned int _dl_internal_error_number;

struct elf_resolve * _dl_load_shared_library(struct dyn_elf * dpnt,
	char * full_libname){
  char * pnt, *pnt1, *pnt2;
  struct elf_resolve * tpnt, *tpnt1;
  char mylibname[1024];
  char * libname;

  _dl_internal_error_number = 0;
  pnt = libname = full_libname;
  while (*pnt) {
    if(*pnt == '/') libname = pnt+1;
    pnt++;
  }

  if(libname == full_libname)
    for(; dpnt; dpnt = dpnt->next) {
      tpnt = dpnt->dyn;
      pnt1 = (char *) tpnt->dynamic_info[DT_RPATH];
      if(pnt1) {
	pnt1 += (unsigned int) tpnt->loadaddr + tpnt->dynamic_info[DT_STRTAB];
	while(*pnt1){
	  pnt2 = mylibname;
	  while(*pnt1 && *pnt1 != ':') *pnt2++ = *pnt1++;
	  if(pnt2[-1] != '/') *pnt2++ = '/';
	  pnt = libname;
	  while(*pnt) *pnt2++  = *pnt++;
	  *pnt2++ = 0;
	  tpnt1 = _dl_load_elf_shared_library(mylibname, 0);
	  if(tpnt1) return tpnt1;
	  if(*pnt1 == ':') pnt1++;
	}
      }
    }

  pnt1 = _dl_library_path;
  if(*pnt1 && libname == full_libname) {
    while(*pnt1){
      pnt2 = mylibname;
      while(*pnt1 && *pnt1 != ':' && *pnt1 != ';') *pnt2++ = *pnt1++;
      if(pnt2[-1] != '/') *pnt2++ = '/';
      pnt = libname;
      while(*pnt) *pnt2++  = *pnt++;
      *pnt2++ = 0;
      tpnt1 = _dl_load_elf_shared_library(mylibname, 0);
      if(tpnt1) return tpnt1;
      if(*pnt1 == ':' || *pnt1 == ';') pnt1++;
    }
  }

  /* Still no luck.  Try one last possibility */

  /* If the filename has any '/', try it straight and leave it at that.  For
   IBCS2 compatibility under linux, we substitute the string /usr/i486-sysv4/lib for
   /usr/lib in library names. */

  if(libname != full_libname) {
#ifdef IBCS_COMPATIBLE
    if(_dl_strncmp(full_libname, "/usr/lib/", 9) != 0) {
	tpnt1 = _dl_load_elf_shared_library(full_libname, 0);
    } else {
      pnt1 = "/usr/i486-sysv4/lib/";
      pnt = mylibname;
      while(*pnt1) *pnt++ = *pnt1++;
      pnt1 = full_libname + 9;
      while(*pnt1) *pnt++ = *pnt1++;
      *pnt++ = 0;
      tpnt1 = _dl_load_elf_shared_library(mylibname, 0);
    }
#else
	tpnt1 = _dl_load_elf_shared_library(full_libname, 0);
#endif
	if (!tpnt1) goto goof;
	return tpnt1;
  }

#ifndef IBCS_COMPATIBLE

#ifdef USE_CACHE
  /* Check the cache. We don't know if IBCS should use it
     yet. */
  if ((pnt1 = _dl_lookup_cache(libname)))
    if ((tpnt1 =  _dl_load_elf_shared_library(pnt1,0)))
      return tpnt1;
#endif

  /* try "/lib/". */
  pnt1 = "/lib/";
  pnt = mylibname;
  while(*pnt1) *pnt++ = *pnt1++;
  pnt1 = libname;
  while(*pnt1) *pnt++ = *pnt1++;
  *pnt++ = 0;

  tpnt1 = _dl_load_elf_shared_library(mylibname, 0);
  if(tpnt1) return tpnt1;
#endif


#ifdef IBCS_COMPATIBLE
  pnt1 = "/usr/i486-sysv4/lib/";
#else
  pnt1 = "/usr/lib/";
#endif
  pnt = mylibname;
  while(*pnt1) *pnt++ = *pnt1++;
  pnt1 = libname;
  while(*pnt1) *pnt++ = *pnt1++;
  *pnt++ = 0;

  tpnt1 = _dl_load_elf_shared_library(mylibname, 0);
  if(tpnt1) return tpnt1;
  
goof:
  /* Well, we shot our wad on that one.  All we can do now is punt */
  if (_dl_internal_error_number) _dl_error_number = _dl_internal_error_number;
	else _dl_error_number = DL_ERROR_NOFILE;
  return NULL;
}

/*
 * Read one ELF library into memory, mmap it into the correct locations and
 * add the symbol info to the symbol chain.  Perform any relocations that
 * are required.
 */

struct elf_resolve * _dl_load_elf_shared_library(char * libname, int flag){
  struct elfhdr * epnt;
  unsigned int dynamic_addr = 0;
  unsigned int dynamic_size = 0;
  struct dynamic * dpnt;
  struct elf_resolve * tpnt;
  struct elf_phdr * ppnt;
  int piclib;
  char * status;
#ifdef DEBUG
  int lastmapped = 0;
#endif
  int flags;
  char header[4096];
  int dynamic_info[24];
  int * lpnt;
  char  * zfile = "/dev/zero";
  unsigned int libaddr;
  unsigned int minvma=0xffffffff, maxvma=0;
  
  int i;
  int infile, zerofile;

  /* If this file is already loaded, skip this step */
  tpnt = _dl_check_hashed_files(libname);
  if(tpnt) return tpnt;

  libaddr = 0;
  infile = _dl_open(libname, O_RDONLY);
  if(infile < 0)
  {
#if 0
    /*
     * NO!  When we open shared libraries we may search several paths.
     * it is inappropriate to generate an error here.
     */
    _dl_printf("Unable to open %s.\n", libname);
#endif
    _dl_internal_error_number = DL_ERROR_NOFILE;
    return NULL;
  }
  zerofile = _dl_open(zfile, O_RDONLY);
  if(zerofile < 0) {
    _dl_printf ("Unable to open /dev/zero.  Please create.\n");
    _dl_internal_error_number = DL_ERROR_NOZERO;
    return NULL;
  }; 
 
  _dl_read(infile, header, sizeof(header));
  epnt = (struct elfhdr *) header;
  if (epnt->e_ident[0] != 0x7f ||
      epnt->e_ident[1] != 'E' ||
      epnt->e_ident[2] != 'L' ||
      epnt->e_ident[3] != 'F') {
    _dl_printf(libname);
    _dl_printf(" is not an ELF file\n");
    _dl_internal_error_number = DL_ERROR_NOTELF;
    return NULL;
  };
  
  if((epnt->e_type != ET_DYN) || 
     (epnt->e_machine != MAGIC1 
#ifdef MAGIC2
      && epnt->e_machine != MAGIC2
#endif
      )){
    _dl_internal_error_number = (epnt->e_type != ET_DYN ? DL_ERROR_NOTDYN : DL_ERROR_NOTMAGIC);
    _dl_printf(libname);
    _dl_printf(" is not an ELF executable for " ELF_TARGET "\n");
    return NULL;
  };

  ppnt = (struct elf_phdr *) &header[epnt->e_phoff];

  piclib = 1;
  for(i=0;i < epnt->e_phnum; i++){

    if(ppnt->p_type == PT_DYNAMIC) {
      if(dynamic_addr) _dl_printf("Error - more than one dynamic section\n");
      dynamic_addr = ppnt->p_vaddr;
      dynamic_size = ppnt->p_filesz;
    };

    if(ppnt->p_type == PT_LOAD) {
	/* See if this is a PIC library. */
	if(i == 0 && ppnt->p_vaddr > 0x1000000) {
	    piclib = 0;
	    minvma=ppnt->p_vaddr;
	}
	if(piclib && ppnt->p_vaddr < minvma) {
	    minvma = ppnt->p_vaddr;
	}
	if(((unsigned int)ppnt->p_vaddr + ppnt->p_memsz) > maxvma) {
	    maxvma = ppnt->p_vaddr + ppnt->p_memsz;
	}
     }
    ppnt++;
  };

  maxvma=(maxvma+0xfffU)&~0xfffU;
  minvma=minvma&~0xffffU;
  
  flags = MAP_PRIVATE;
  if(!piclib) flags|=MAP_FIXED;
  
  status = (char *) _dl_mmap((char *) (piclib?0:minvma),
			  maxvma-minvma, 
			  PROT_NONE, 
			  flags, zerofile,
			  0);
  if(_dl_mmap_check_error(status)) {
    _dl_printf("mmap failed.\n");
    _dl_internal_error_number = DL_ERROR_MMAP_FAILED;
     return NULL;
  };
  libaddr=(unsigned int)status;
  flags|=MAP_FIXED;
  
  /* Get the memory to store the library */
  ppnt = (struct elf_phdr *) &header[epnt->e_phoff];
  
  for(i=0;i < epnt->e_phnum; i++){
    if(ppnt->p_type == PT_LOAD) {

      /* See if this is a PIC library. */
      if(i == 0 && ppnt->p_vaddr > 0x1000000) {
	piclib = 0;
	/* flags |= MAP_FIXED; */
      }


      
      if(ppnt->p_flags & PF_W) {
	unsigned int map_size;
	char * cpnt;
	
	status = (char *) _dl_mmap((char *) (libaddr  +
					     (ppnt->p_vaddr & 0xfffff000)),
			  (ppnt->p_vaddr & 0xfff) + ppnt->p_filesz, 
			  LXFLAGS(ppnt->p_flags), 
			  flags, infile,
			  ppnt->p_offset & 0x7ffff000);
	
	if(_dl_mmap_check_error(status)) {
	    _dl_printf("mmap failed.\n");
	      _dl_internal_error_number = DL_ERROR_MMAP_FAILED;
	    return NULL;
	};
	
	if(!piclib) status = 0;
	
	/* Pad the last page with zeroes. */
	cpnt =(char *) (status + (ppnt->p_vaddr & 0xfff) + ppnt->p_filesz);
	while(((unsigned int) cpnt) & 0xfff) *cpnt++ = 0;
	
/* I am not quite sure if this is completely correct to do or not, but
   the basic way that we handle bss segments is that we mmap /dev/zero if
   there are any pages left over that are not mapped as part of the file */

	map_size = (ppnt->p_vaddr + ppnt->p_filesz + 0xfff) & 0xfffff000;
	if(map_size < ppnt->p_vaddr + ppnt->p_memsz)
	  status = (char *) _dl_mmap((char *) map_size + libaddr, 
			    ppnt->p_vaddr + ppnt->p_memsz - map_size,
			    LXFLAGS(ppnt->p_flags),
			    flags, zerofile, 0);
      } else
	status = (char *) _dl_mmap((char *) (ppnt->p_vaddr & 0xfffff000) + 
				   libaddr, 
			  (ppnt->p_vaddr & 0xfff) + ppnt->p_filesz, 
			  LXFLAGS(ppnt->p_flags), 
			  flags, infile, 
			  ppnt->p_offset & 0x7ffff000);
      if(_dl_mmap_check_error(status)) {
	_dl_printf("mmap failed.\n");
	  _dl_internal_error_number = DL_ERROR_MMAP_FAILED;
	  return NULL;
      };
      /* if(libaddr == 0 && piclib) {
	libaddr = (unsigned int) status;
	flags |= MAP_FIXED;
      }; */
    };
    ppnt++;
  };
  _dl_close(zerofile);
  _dl_close(infile);
  
  /* For a non-PIC library, the addresses are all absolute */
  if(!piclib) libaddr = 0;

  dynamic_addr += (unsigned int) libaddr;

 /* 
  * OK, the ELF library is now loaded into VM in the correct locations
  * The next step is to go through and do the dynamic linking (if needed).
  */
  
  /* Start by scanning the dynamic section to get all of the pointers */
  
  if(!dynamic_addr) {
    _dl_internal_error_number = DL_ERROR_NODYNAMIC;
    _dl_printf(libname);
    _dl_printf(" is missing a DYNAMIC section.\n");
    
    return NULL;
  }

  dpnt = (struct dynamic *) dynamic_addr;

  dynamic_size = dynamic_size / sizeof(struct dynamic);
  _dl_memset(dynamic_info, 0, sizeof(dynamic_info));
  for(i=0; i< dynamic_size; i++){
    dynamic_info[dpnt->d_tag] = dpnt->d_un.d_val;
    if(dpnt->d_tag == DT_TEXTREL ||
       SVR4_BUGCOMPAT) dynamic_info[DT_TEXTREL] = 1;
    dpnt++;
  };

  /* If the TEXTREL is set, this means that we need to make the pages
     writable before we perform relocations.  Do this now. They get set back
     again later. */

  ppnt = (struct elf_phdr *) &header[epnt->e_phoff];
  for(i=0;i < epnt->e_phnum; i++, ppnt++){
    if(ppnt->p_type == PT_LOAD && !(ppnt->p_flags & PF_W))
      _dl_mprotect((void *) (libaddr + (ppnt->p_vaddr & 0xfffff000)),
		   (ppnt->p_vaddr & 0xfff) + (unsigned int) ppnt->p_filesz,
		   PROT_READ | PROT_WRITE | PROT_EXEC);
  }

  tpnt = _dl_add_elf_hash_table(libname, (char *) libaddr, dynamic_info, dynamic_addr, 
			      dynamic_size);

  tpnt->ppnt = (struct elf_phdr *) (tpnt->loadaddr + epnt->e_phoff);
  tpnt->n_phent = epnt->e_phnum;

  /*
   * OK, the next thing we need to do is to insert the dynamic linker into
   * the proper entry in the GOT so that the PLT symbols can be properly
   * resolved. 
   */
  
  lpnt = (int *) dynamic_info[DT_PLTGOT];
  
  if(lpnt) {
    lpnt = (int *) (dynamic_info[DT_PLTGOT] + ((int) libaddr));
    lpnt[1] = (int) tpnt;
    lpnt[2] = (int) _dl_linux_resolve;
  };
  
  return tpnt;
}

/* Ugly, ugly.  Some versions of the SVr4 linker fail to generate COPY
   relocations for global variables that are present both in the image and
   the shared library.  Go through and do it manually.  If the images
   are guaranteed to be generated by a trustworthy linker, then this
   step can be skipped. */

int _dl_copy_fixups(struct dyn_elf * rpnt)
{
  int goof = 0;
  struct elf_resolve * tpnt;
#ifdef BROKEN_LINKER
  int hn, hn1, si, si1;
  unsigned int elf_hash_number;
  struct elf_resolve * tpnt1;
  struct dyn_elf * rpnt1;
  char * strtab, *strtab1;
  struct Elf32_Sym * symtab, *symtab1;
  char * pnt, *pnt1;
#endif

  if(rpnt->next) goof += _dl_copy_fixups(rpnt->next);
  else return 0;

  tpnt = rpnt->dyn;
	
  if (tpnt->init_flag & COPY_RELOCS_DONE) return goof;
  tpnt->init_flag |= COPY_RELOCS_DONE;
  
/* Use BROKEN_LINKER if the SVr4 linker is being used. */
#ifndef BROKEN_LINKER
  goof += _dl_parse_copy_information(rpnt, tpnt->dynamic_info[DT_REL],
					   tpnt->dynamic_info[DT_RELSZ], 0);

#else
  /* OK, now scan the symbol table for this module */

  symtab = (struct Elf32_Sym *) (tpnt->dynamic_info[DT_SYMTAB] + 
				 tpnt->loadaddr);
  strtab = (char *) (tpnt->dynamic_info[DT_STRTAB] + tpnt->loadaddr);

  for(hn = 0; hn < tpnt->nbucket; hn++) {
    if(!tpnt->elf_buckets[hn]) continue;
    for(si = tpnt->elf_buckets[hn]; si; si = tpnt->chains[si]) {
      if(ELF32_ST_TYPE(symtab[si].st_info) != STT_OBJECT) continue;
      if(ELF32_ST_BIND(symtab[si].st_info) != STB_GLOBAL) continue; 
      if(symtab[si].st_value == 0 || symtab[si].st_shndx == 0) continue;
      if(!symtab[si].st_size) continue;
      pnt = strtab + symtab[si].st_name;

      elf_hash_number = _dl_elf_hash(pnt);

      /* OK, we have a candidate.  Now search for the same symbol in other
	 libraries.  The hash number will be the same. */
      for(rpnt1 = rpnt->next; rpnt1; rpnt1 = rpnt1->next)
	{
	  tpnt1 = rpnt1->dyn;
	  hn1 = elf_hash_number % tpnt1->nbucket;
	  if(!tpnt1->elf_buckets[hn1]) continue;

	  symtab1 = (struct Elf32_Sym *) (tpnt1->dynamic_info[DT_SYMTAB] + 
					 tpnt1->loadaddr);
	  strtab1 = (char *) (tpnt1->dynamic_info[DT_STRTAB] + tpnt1->loadaddr);
	  for(si1 = tpnt1->elf_buckets[hn1]; si1; si1 = tpnt1->chains[si1]) {
	    pnt1 = strtab1 + symtab1[si1].st_name;
	    if(ELF32_ST_TYPE(symtab1[si1].st_info) != STT_OBJECT) continue;
	    if(ELF32_ST_BIND(symtab1[si1].st_info) != STB_GLOBAL) continue; 
	    if(symtab1[si1].st_value == 0 || symtab1[si1].st_shndx == 0) continue;
	    if(!symtab1[si1].st_size) continue;
	    if(symtab1[si1].st_size != symtab[si].st_size) continue;
	    pnt1 = strtab1 + symtab1[si1].st_name;
	    if(_dl_strcmp(pnt, pnt1) == 0) {
	      char * to, *from;
	      int count;
	      to = tpnt->loadaddr + symtab[si].st_value;
	      from = tpnt1->loadaddr + symtab1[si1].st_value;
	      count = symtab[si].st_size;
	      while(count--) *to++ = *from++;
	      /* Cannot use memcpy - SVr4 assembler complains about dup label*/
#if 0
	      _dl_printf("Global symbol in ");
	      _dl_printf(tpnt1->libname);
	      _dl_printf(" ");
	      _dl_printf(pnt);
	      _dl_printf("\n");
#endif
	    };
	  };
	};
    };
  };
#endif
  return goof;
}

