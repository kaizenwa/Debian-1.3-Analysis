/* dlfcn.h -- User functions for run-time dynamic loading.
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

#ifndef	_DLFCN_H
#define	_DLFCN_H 1


/* The MODE argument to `dlopen' contains one of the following: */
#define RTLD_LAZY	0x001	/* Lazy function call binding.  */
#define RTLD_NOW	0x002	/* Immediate function call binding.  */
#define	RTLD_BINDING_MASK 0x3	/* Mask of binding time value.  */

/* If the following bit is set in the MODE argument to `dlopen',
   the symbols of the loaded object and its dependencies are made
   visible as if the object were linked directly into the program.  */
#define RTLD_GLOBAL	0x100

/* Open the shared object FILE and map it in; return a handle that can be
   passed to `dlsym' to get symbol values from it.  */
extern void *dlopen (const char *__file, int __mode);

/* Unmap and close a shared object opened by `dlopen'.
   The handle cannot be used again after calling `dlclose'.  */
extern int dlclose (void *__handle);

/* Find the run-time address in the shared object HANDLE refers to
   of the symbol called NAME.  */
extern void *dlsym (void *__handle, const char *__name);

/* When any of the above functions fails, call this function
   to return a string describing the error.  Each call resets
   the error string so that a following call returns null.  */
extern char *dlerror (void);

/* Fill in *INFO with the following information about ADDRESS.
   Returns 0 iff no shared object's segments contain that address.  */
typedef struct
  {
    const char *dli_fname;	/* File name of defining object.  */
    void *dli_fbase;		/* Load address of that object.  */
    const char *dli_sname;	/* Name of nearest symbol.  */
    void *dli_saddr;		/* Exact value of nearest symbol.  */
  } Dl_info;
extern int dladdr (void *__address, Dl_info *__info);

#endif	/* dlfcn.h */
