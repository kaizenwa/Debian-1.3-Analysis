/*
 * elfupdate.c  - create/update psdatabase for ELF kernels.
 * 
 * Copyright (c) 1995, 1996 Jeffrey A. Uphoff <juphoff@nrao.edu>
 * 
 * This file is based (loosly) on the following:
 * ------------------------------------------------------------------
 * update_db.c - create/update psdatabase
 * 
 * Copyright (c) 1992 Branko Lankester
 * 
 * munged into psupdate.c by Michael K. Johnson for the procps suite.
 * ------------------------------------------------------------------ 
 *
 * (All of the ELF-specific code is new.)
 */

#include "psupdate.h"

#ifdef ELF_CAPABLE

#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>
#include <elf.h>

extern unsigned long k_addr (char *);

static Elf32_Ehdr E_hdr;
static Elf32_Sym *E_sym;
extern char *strings;
extern long nsym, stringsize;

int
check_elf_magic (int fd, const char *systemfile)
{
  MLSEEK (fd, 0, SEEK_SET, systemfile);
  MREAD (fd, &E_hdr, sizeof (E_hdr), systemfile);
  return strncmp (&E_hdr.e_ident[EI_MAG0], ELFMAG, SELFMAG);
}

/*
 * This whole thing could probably be rewritten to use libbfd.
 */
int
read_elf_nlist (int fd, const char *systemfile)
{
  Elf32_Shdr E_shdr[2];
  int i;
#ifdef DEBUG
  int j;
#endif

  /* Go to the section header table. */
  MLSEEK (fd, E_hdr.e_shoff, SEEK_SET, systemfile);
  /*
   * Find the symbol table entry.  There can be only one.  (Thanks
   * Highlander.)
   */
  for (i = 0; i < E_hdr.e_shnum; i++) {
    MREAD (fd, &E_shdr[0], sizeof (Elf32_Shdr), systemfile);
    if (E_shdr[0].sh_type == SHT_SYMTAB)
      break;
  }
  if (i == E_hdr.e_shnum) {    /* Uh, oh. */
    fprintf (stderr, "%s:\n"
	     "No symbol table entry in section header table!  (Stripped kernel?)\n",
	     systemfile);
    exit (1);
  }
  /* We found it.  Now go to string table entry.  */
  MLSEEK (fd, E_hdr.e_shoff + E_shdr[0].sh_link * sizeof (Elf32_Shdr), SEEK_SET,
	  systemfile);
  MREAD (fd, &E_shdr[1], sizeof (Elf32_Shdr), systemfile);
  assert (E_shdr[1].sh_type == SHT_STRTAB); /* Sanity! */
  strings = (char *)xmalloc (stringsize = E_shdr[1].sh_size);
  /* Go to the string table. */
  MLSEEK (fd, E_shdr[1].sh_offset, SEEK_SET, systemfile);
  /* Slurp it in. */
  MREAD (fd, strings, E_shdr[1].sh_size, systemfile);
  E_sym = (Elf32_Sym *) xmalloc (E_shdr[0].sh_size);
  /* Go to the symbol table.  I like to do things backwards in July. */
  MLSEEK (fd, E_shdr[0].sh_offset, SEEK_SET, systemfile);
  nsym = 0;
#ifdef DEBUG
  j = 0;
#endif
  for (i = 0; i < (E_shdr[0].sh_size / E_shdr[0].sh_entsize); i++) {
    MREAD (fd, &E_sym[i], E_shdr[0].sh_entsize, systemfile);
#ifdef DEBUG    
    if (E_sym[i].st_name != 0 && ELF32_ST_TYPE (E_sym[i].st_info) != STT_FILE)
      ++j;
#endif
  }
  nsym = --i;
#ifdef DEBUG
  fprintf (stderr, "read %d symbols (%d total) from %s\n", j, nsym, systemfile);
#endif
  return (0);
}

/*
 * ELF is nice; it's easier to use one relatively generic function for
 * making both the function and variable tables.
 */
struct sym_s *
make_elf_tbl (struct sym_s *p, int vartable)
{
  int i, type;

  type = vartable ? ELF_OBJECT : ELF_FUNC;
  /*
   * This is not really needed; it's here as a sanity check.  If the
   * values for various ELF internals are changed, we'll have other
   * (bigger) problems most likely.  Still, it's here for cleanliness: I
   * need to do this for now to keep all ELF-specific stuff out of
   * psupdate.c.
   */
  assert (type == ELF_OBJECT || type == ELF_FUNC);
  type = (type == ELF_OBJECT) ? STT_OBJECT : STT_FUNC;

  for (i = 0; i < nsym; ++i)
    if (ELF32_ST_TYPE (E_sym[i].st_info) == type) {
      p->addr = E_sym[i].st_value;
      p->name = E_sym[i].st_name;
      ++p;
    }
  return p;
}

void
read_elf_uts (int fd, struct new_utsname * uts, const char *syspath)
{
  /* 
   * Simply seeking to k_addr ("sym") works fine when offset properly
   * (at least I've yet to see it fail on a kernel image).
   */
  MLSEEK (fd, k_addr ("system_utsname") - E_hdr.e_entry, SEEK_SET, "lseek");
  MREAD (fd, uts, sizeof (*uts), syspath);
}

#endif /* ELF_CAPABLE */

