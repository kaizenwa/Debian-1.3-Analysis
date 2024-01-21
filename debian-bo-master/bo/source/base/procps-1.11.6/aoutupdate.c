/*
 * aoutupdate.c - create/update psdatabase for a.out kernels.
 * 
 * Copyright (c) 1995, 1996 Jeffrey A. Uphoff <juphoff@nrao.edu>
 * 
 * This file is based on the following:
 * ------------------------------------------------------------------
 * update_db.c - create/update psdatabase
 * 
 * Copyright (c) 1992 Branko Lankester
 * 
 * munged into psupdate.c by Michael K. Johnson for the procps suite.
 * ------------------------------------------------------------------
 */

#include "psupdate.h"

#ifdef AOUT_CAPABLE

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <a.out.h>

extern unsigned long k_addr (char *);

static unsigned long data_start;
static struct exec hdr;
static struct nlist *namelist;
extern char *strings;
extern long nsym, stringsize;

int
check_aout_magic (int fd, const char *systemfile)
{
  MLSEEK (fd, 0, SEEK_SET, systemfile);
  MREAD (fd, &hdr, sizeof (hdr), systemfile);
  if (N_BADMAG (hdr))
    return 1;
  else
    return 0;
}

int
read_aout_nlist (int fd, const char *systemfile)
{
  unsigned size, symsize;

  if (N_STROFF (hdr) == 0) {
    fprintf (stderr, "%s has no symbols\n", systemfile);
    exit (1);
  }
  if ((data_start = N_DATOFF (hdr)) == 0) {
    fprintf (stderr, "%s has no data\n", systemfile);
    exit (1);
  }
  MLSEEK (fd, N_STROFF (hdr), SEEK_SET, systemfile);
  MREAD (fd, (char *)&stringsize, sizeof (stringsize), systemfile);
  symsize = N_STROFF (hdr) - N_SYMOFF (hdr);
  size = symsize + stringsize;
  namelist = (struct nlist *)xmalloc (size);
  MLSEEK (fd, N_SYMOFF (hdr), SEEK_SET, systemfile);
  MREAD (fd, (char *)namelist, size, systemfile);
  strings = ((char *)namelist) + symsize;
  nsym = symsize / sizeof (struct nlist);
#ifdef DEBUG
  fprintf (stderr, "read %ld symbols from %s\n", nsym, systemfile);
#endif
  return (0);
}

static void *
make_aout_fnctbl (struct sym_s *fp)
{
  int i;

  for (i = 0; i < nsym; ++i)
    if ((namelist[i].n_type & ~N_EXT) == N_TEXT &&
	!strchr (strings + namelist[i].n_un.n_strx, '.')) {
      fp->addr = namelist[i].n_value;
      fp->name = namelist[i].n_un.n_strx;
      ++fp;
    }
  return fp;
}

static void *
make_aout_vartbl (struct sym_s *vp)
{
  int i, typ;

  for (i = 0; i < nsym; ++i) {
    typ = namelist[i].n_type & ~N_EXT;
    if (typ == N_DATA || typ == N_BSS) {
      vp->addr = namelist[i].n_value;
      vp->name = namelist[i].n_un.n_strx;
      ++vp;
    }
  }
  return vp;
}

struct sym_s *
make_aout_tbl (struct sym_s *vp, int vartable)
{
  return vartable ? make_aout_vartbl (vp) : make_aout_fnctbl (vp);
}

void
read_aout_uts (int fd, struct new_utsname * uts, const char *syspath)
{
  MLSEEK (fd, k_addr ("_system_utsname") - hdr.a_entry + sizeof (struct exec), 
	  SEEK_SET, "lseek");
  MREAD (fd, uts, sizeof (*uts), syspath);
}

#endif /* AOUT_CAPABLE */
