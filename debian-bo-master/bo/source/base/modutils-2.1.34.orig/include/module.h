/* Definitions for the Linux module syscall interface.
   Copyright 1996, 1997 Linux International.

   Contributed by Richard Henderson <rth@tamu.edu>

   This file is part of the Linux modutils.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */


#ifndef MODUTILS_MODULE_H
#define MODUTILS_MODULE_H 1

/* This file contains the structures used by the 2.0 and 2.1 kernels.
   We do not use the kernel headers directly because we do not wish
   to be dependant on a particular kernel version to compile insmod.  */


/*======================================================================*/
/* The structures used by Linux 2.0.  */

/* The symbol format used by get_kernel_syms(2).  */
struct old_kernel_sym
{
  unsigned long value;
  char name[60];
};

struct old_module_ref
{
  unsigned long module;		/* kernel addresses */
  unsigned long next;
};

struct old_module_symbol
{
  unsigned long addr;
  unsigned long name;
};

struct old_symbol_table
{
  int size;			/* total, including string table!!! */
  int n_symbols;
  int n_refs;
  struct old_module_symbol symbol[0]; /* actual size defined by n_symbols */
  struct old_module_ref ref[0];	/* actual size defined by n_refs */
};

struct old_mod_routines
{
  unsigned long init;
  unsigned long cleanup;
};

struct old_module
{
  unsigned long next;
  unsigned long ref;		/* the list of modules that refer to me */
  unsigned long symtab;
  unsigned long name;
  int size;			/* size of module in pages */
  unsigned long addr;		/* address of module */
  int state;
  unsigned long cleanup;	/* cleanup routine */
};

/* Sent to init_module(2) or'ed into the code size parameter.  */
#define OLD_MOD_AUTOCLEAN 0x40000000 /* big enough, but no sign problems... */

int get_kernel_syms(struct old_kernel_sym *);
int old_sys_init_module(const char *name, char *code, unsigned codesize,
			struct old_mod_routines *, struct old_symbol_table *);


/*======================================================================*/
/* The structures used in Linux 2.1.  */

struct new_module_symbol
{
  unsigned long value;
  unsigned long name;
};

struct new_module_ref
{
  unsigned long dep;		/* kernel addresses */
  unsigned long ref;
  unsigned long next_ref;
};

struct new_module_persist;

struct new_module
{
  unsigned long size_of_struct;	/* == sizeof(module) */
  unsigned long next;
  unsigned long name;
  unsigned long size;

  long usecount;
  unsigned long flags;		/* AUTOCLEAN et al */

  unsigned nsyms;
  unsigned ndeps;

  unsigned long syms;
  unsigned long deps;
  unsigned long refs;
  unsigned long init;
  unsigned long cleanup;
  unsigned long ex_table_start;
  unsigned long ex_table_end;
#ifdef __alpha__
  unsigned long gp;
#endif
  /* Everything after here is extension.  */
  unsigned long persist_start;
  unsigned long persist_end;
  unsigned long can_unload;
};

struct new_module_info
{
  unsigned long addr;
  unsigned long size;
  unsigned long flags;
  	   long usecount;
};

/* Bits of module.flags.  */
#define NEW_MOD_RUNNING		1
#define NEW_MOD_DELETED		2
#define NEW_MOD_AUTOCLEAN	4
#define NEW_MOD_VISITED		8
#define NEW_MOD_USED_ONCE	16

int new_sys_init_module(const char *name, const struct new_module *);
int query_module(const char *name, int which, void *buf, size_t bufsize,
		 size_t *ret);

/* Values for query_module's which.  */

#define QM_MODULES	1
#define QM_DEPS		2
#define QM_REFS		3
#define QM_SYMBOLS	4
#define QM_INFO		5

/*======================================================================*/
/* The system calls unchanged between 2.0 and 2.1.  */

unsigned long create_module(const char *, size_t);
int delete_module(const char *);


#endif /* module.h */
