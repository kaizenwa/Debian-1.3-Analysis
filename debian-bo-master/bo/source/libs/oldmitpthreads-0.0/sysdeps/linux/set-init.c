/* Copyright (C) 1991, 1992, 1994 Free Software Foundation, Inc.
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

#include <ansidecl.h>
#include <stddef.h>
#include <_G_config.h>

const char * const __linux_C_lib_version =
	"@(#) The Linux C library "_LINUX_C_LIB_VERSION;

#ifdef __ELF__
#include <gnu-stabs.h>

#if !defined(__PIC__) && !defined(__pic__)
/* We use this to make sure the __libc_subinit section is always
 * there for the static libraries. We may try weak symbol later. */

static void __init_dummy ( void );

static void
__init_dummy ()
{
}

text_set_element (__libc_subinit, __init_dummy);

#endif
#else
struct
  {
    size_t n;
    void EXFUN((*fn[0]), (int argc, char **argv, char **envp));
  } __libc_subinit;
#endif

void __libc_init( int, char **, char **);

void
DEFUN(__libc_init, (argc, argv, envp),
      int argc AND char **argv AND char **envp)
{
#ifdef __ELF__
  typedef EXFUN((*__libc_subinit_t),
	(int argc, char **argv, char **envp));
  __libc_subinit_t *fp;
  symbol_set_declare (__libc_subinit);

  for (fp = (__libc_subinit_t *)
	(symbol_set_first_element (__libc_subinit));
	!(int)symbol_set_end_p (__libc_subinit, fp); fp++)
    (*fp[0]) (argc, argv, envp);
#else
  size_t i;

  for (i = 0; i < __libc_subinit.n; ++i)
    (*__libc_subinit.fn[i]) (argc, argv, envp);
#endif
}
