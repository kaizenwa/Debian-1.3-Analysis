#include "psupdate.h"

#ifdef BFD_CAPABLE

#include <assert.h>
#include <bfd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DEBUG
# define Debug	DEBUG
#else
# define Debug	0
#endif

static bfd *abfd;
static asymbol **namelist;

extern long nsym;
extern char *strings;

/*
 * The string table implementation does not take care of duplicate
 * strings and could be made much more efficient.  For now, it's
 * just something that works.
 */
static struct strtab {
  int		size;
  int		alloced;
  char *	strings;
} strtab = {0, 0, 0};


static int
strtab_enter (struct strtab * tab, const char * str)
{
  int len = strlen (str) + 1, index;

  if (tab->size + len > tab->alloced) {
    assert (len < 8192);
    tab->alloced += 8192;
    tab->strings = xrealloc(tab->strings, tab->alloced);
  }
  index = tab->size;
  strcpy(tab->strings + index, str);
  tab->size += len;
  return index;
}


int
check_bfd_magic (int fd, const char * systemfile)
{
  abfd = bfd_openr (systemfile, 0);
  if (!abfd)
    {
      bfd_perror(systemfile);
      return -1;
    }
  if (!bfd_check_format (abfd, bfd_object))
    {
      bfd_perror ("bfd_check_format");
      return -1;
    }
  return 0;
}


int
read_bfd_nlist (int fd, const char *systemfile)
{
  long storage;

  storage = bfd_get_symtab_upper_bound(abfd);
  if (storage < 0)
    {
      bfd_perror ("bfd_get_symtab_upper_bound");
      exit (1);
    }
  namelist = (asymbol **) xmalloc(storage);
  nsym = bfd_canonicalize_symtab(abfd, namelist);
  if (nsym < 0)
    {
      bfd_perror("bfd_canonicalize_symtab");
      exit(1);
    }
  if (Debug > 1)
    fprintf(stderr, "read %ld symbols from %s\n", nsym, systemfile);
  return 0;
}


void
read_bfd_uts (int fd, struct new_utsname * uts, const char *syspath)
{
  long i;

  for (i = 0; i < nsym; ++i)
    {
      if (strcmp (bfd_asymbol_name(namelist[i]), "system_utsname") == 0)
	break;
    }
  if (i >= nsym)
    {
      fprintf (stderr, "%s: kernel does not contain `system_utsname' symbol\n",
	       syspath);
      memset(uts, 0, sizeof (*uts));
      return;
    }
  if (!bfd_get_section_contents (abfd, namelist[i]->section, uts,
				 (bfd_asymbol_value (namelist[i])
				  - bfd_asymbol_base (namelist[i])),
				 sizeof (*uts)))
    {
      bfd_perror (syspath);
    }
}


/* Make list of all text/variable symbols.  */
struct sym_s *
make_bfd_tbl (struct sym_s * sp, int vartable)
{
  long i, type;

  type = vartable ? SEC_DATA : SEC_CODE;

  for (i = 0; i < nsym; ++i)
    {
      if ((namelist[i]->section->flags & type)
	  && !(namelist[i]->flags & BSF_DEBUGGING)
	  && !strpbrk(bfd_asymbol_name(namelist[i]), ".$"))
	{
	  sp->addr = bfd_asymbol_value(namelist[i]);
	  sp->name = strtab_enter(&strtab, bfd_asymbol_name(namelist[i]));
	  if (Debug > 2) {
	    fprintf(stderr, "added %s `%s' at %#lx (index=%d)\n",
		    vartable ? "variable" : "function",
		    bfd_asymbol_name(namelist[i]), sp->addr, sp->name);
	  }
	  ++sp;
	}
    }
  strings = strtab.strings;
  stringsize = strtab.size;
  return sp;
}

#endif /* BFD_CAPABLE */
