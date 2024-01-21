/* Elf file, section, and symbol manipulation routines.
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


#include <string.h>
#include <assert.h>
#include <alloca.h>

#include <obj.h>
#include <util.h>

/*======================================================================*/

/* Standard ELF hash function.  */
inline unsigned long
obj_elf_hash_n(const char *name, unsigned long n)
{
  unsigned long h = 0;
  unsigned long g;
  unsigned char ch;

  while (n > 0)
    {
      ch = *name++;
      h = (h << 4) + ch;
      if ((g = (h & 0xf0000000)) != 0)
	{
	  h ^= g >> 24;
	  h &= ~g;
	}
      n--;
    }
  return h;
}

unsigned long
obj_elf_hash (const char *name)
{
  return obj_elf_hash_n(name, strlen(name));
}

void
obj_set_symbol_compare (struct obj_file *f,
		        int (*cmp)(const char *, const char *),
			unsigned long (*hash)(const char *))
{
  if (cmp)
    f->symbol_cmp = cmp;
  if (hash)
    {
      struct obj_symbol *tmptab[HASH_BUCKETS], *sym, *next;
      int i;

      f->symbol_hash = hash;

      memcpy(tmptab, f->symtab, sizeof(tmptab));
      memset(f->symtab, 0, sizeof(f->symtab));

      for (i = 0; i < HASH_BUCKETS; ++i)
	for (sym = tmptab[i]; sym ; sym = next)
	  {
	    unsigned long h = hash(sym->name) % HASH_BUCKETS;
	    next = sym->next;
	    sym->next = f->symtab[h];
	    f->symtab[h] = sym;
	  }
    }
}

struct obj_symbol *
obj_add_symbol (struct obj_file *f, const char *name,
		int info, int secidx, ElfW(Addr) value, unsigned long size)
{
  struct obj_symbol *sym;
  unsigned long hash = f->symbol_hash(name) % HASH_BUCKETS;

  for (sym = f->symtab[hash]; sym; sym = sym->next)
    if (f->symbol_cmp(sym->name, name) == 0)
      {
	int o_secidx = sym->secidx;
	int o_info = sym->info;
	int o_type = ELFW(ST_TYPE)(o_info);
	int o_binding = ELFW(ST_BIND)(o_info);
	int n_type = ELFW(ST_TYPE)(info);
	int n_binding = ELFW(ST_BIND)(info);

	/* A redefinition!  Is it legal?  */

	if (secidx == SHN_UNDEF)
	  return sym;
	else if (o_secidx == SHN_UNDEF)
	  goto found;
	else if (n_binding == STB_LOCAL)
	  goto found;
	else if (o_binding == STB_LOCAL)
	  return sym;
	else if (n_binding == STB_WEAK)
	  return sym;
	else if (o_binding == STB_WEAK)
	  goto found;
	/* Don't unify COMMON symbols with object types the programmer
	   doesn't expect.  */
	else if (secidx == SHN_COMMON
		 && (o_type == STT_NOTYPE || o_type == STT_OBJECT))
	  return sym;
	else if (o_secidx == SHN_COMMON
		 && (n_type == STT_NOTYPE || n_type == STT_OBJECT))
	  goto found;
	else
	  {
	    /* Don't report an error if the symbol is coming from
	       the kernel or some external module.  */
	    if (secidx <= SHN_HIRESERVE)
	      error("%s multiply defined", name);
	    return sym;
	  }
      }

  /* Completely new symbol.  */
  sym = arch_new_symbol();
  sym->next = f->symtab[hash];
  f->symtab[hash] = sym;
  sym->ksymidx = -1;

found:
  sym->name = name;
  sym->value = value;
  sym->size = size;
  sym->secidx = secidx;
  sym->info = info;

  return sym;
}

struct obj_symbol *
obj_find_symbol (struct obj_file *f, const char *name)
{
  struct obj_symbol *sym;
  unsigned long hash = f->symbol_hash(name) % HASH_BUCKETS;

  for (sym = f->symtab[hash]; sym; sym = sym->next)
    if (f->symbol_cmp(sym->name, name) == 0)
      return sym;

  return NULL;
}

ElfW(Addr)
obj_symbol_final_value (struct obj_file *f, struct obj_symbol *sym)
{
  if (sym)
    {
      if (sym->secidx >= SHN_LORESERVE)
	return sym->value;

      return sym->value + f->sections[sym->secidx]->header.sh_addr;
    }
  else
    {
      /* As a special case, a NULL sym has value zero.  */
      return 0;
    }
}

struct obj_section *
obj_find_section (struct obj_file *f, const char *name)
{
  int i, n = f->header.e_shnum;

  for (i = 0; i < n; ++i)
    if (strcmp(f->sections[i]->name, name) == 0)
      return f->sections[i];

  return NULL;
}

static int
obj_load_order_cmp(struct obj_section *a, struct obj_section *b)
{
  unsigned long af, bf, ac, bc;

  af = a->header.sh_flags;
  bf = b->header.sh_flags;

  ac = 0;
  if (af & SHF_ALLOC) ac |= 16;
  if (!(af & SHF_WRITE)) ac |= 8;
  if (af & SHF_EXECINSTR) ac |= 4;
  if (a->header.sh_type != SHT_NOBITS) ac |= 2;

  bc = 0;
  if (bf & SHF_ALLOC) bc |= 16;
  if (!(bf & SHF_WRITE)) bc |= 8;
  if (bf & SHF_EXECINSTR) bc |= 4;
  if (b->header.sh_type != SHT_NOBITS) bc |= 2;

  return bc - ac;
}

void
obj_insert_section_load_order (struct obj_file *f, struct obj_section *sec)
{
  struct obj_section **p;
  for (p = f->load_order_search_start; *p ; p = &(*p)->load_next)
    if (obj_load_order_cmp(sec, *p) < 0)
      break;
  sec->load_next = *p;
  *p = sec;
}

struct obj_section *
obj_create_alloced_section (struct obj_file *f, const char *name,
			    unsigned long align, unsigned long size)
{
  int newidx = f->header.e_shnum++;
  struct obj_section *sec;

  f->sections = xrealloc(f->sections, (newidx+1) * sizeof(sec));
  f->sections[newidx] = sec = arch_new_section();

  memset(sec, 0, sizeof(*sec));
  sec->header.sh_type = SHT_PROGBITS;
  sec->header.sh_flags = SHF_WRITE|SHF_ALLOC;
  sec->header.sh_size = size;
  sec->header.sh_addralign = align;
  sec->name = name;
  sec->idx = newidx;
  if (size)
    sec->contents = xmalloc(size);

  obj_insert_section_load_order(f, sec);

  return sec;
}

struct obj_section *
obj_create_alloced_section_first (struct obj_file *f, const char *name,
				  unsigned long align, unsigned long size)
{
  int newidx = f->header.e_shnum++;
  struct obj_section *sec;

  f->sections = xrealloc(f->sections, (newidx+1) * sizeof(sec));
  f->sections[newidx] = sec = arch_new_section();

  memset(sec, 0, sizeof(*sec));
  sec->header.sh_type = SHT_PROGBITS;
  sec->header.sh_flags = SHF_WRITE|SHF_ALLOC;
  sec->header.sh_size = size;
  sec->header.sh_addralign = align;
  sec->name = name;
  sec->idx = newidx;
  if (size)
    sec->contents = xmalloc(size);

  sec->load_next = f->load_order;
  f->load_order = sec;
  if (f->load_order_search_start == &f->load_order)
    f->load_order_search_start = &sec->load_next;

  return sec;
}

void *
obj_extend_section (struct obj_section *sec, unsigned long more)
{
  unsigned long oldsize = sec->header.sh_size;
  sec->contents = xrealloc(sec->contents, sec->header.sh_size += more);
  return sec->contents + oldsize;
}
