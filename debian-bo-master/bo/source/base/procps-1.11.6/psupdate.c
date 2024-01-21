/*
 * psupdate.c - create/update psdatabase
 *
 * Based on update_db.c: Copyright (c) 1992 Branko Lankester
 *
 * (Munged into psupdate.c by Michael K. Johnson for the procps suite.)
 *
 * ELF capability added, many things rewritten, by Jeffrey A. Uphoff
 * <juphoff@nrao.edu>, 1995, 1996.
 */

#include "psupdate.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

/* This has changed recently to the following: */
#define	SYS_PATH	"/usr/src/linux/vmlinux"

int write_tbl (int, struct dbtbl_s *, struct tbl_s *);
unsigned long k_addr (char *);
void make_fnctbl (void);
void make_vartbl (void);
void update_psdb (char *);
int addrcmp (struct sym_s *, struct sym_s *);
int varcmp (struct sym_s *, struct sym_s *);

static struct {
  int		(*check_magic) (int fd, const char * path);
  int		(*read_nlist) (int fd, const char * path);
  void		(*read_uts) (int fd, struct new_utsname * uts,
			     const char * path);
  struct sym_s *(*make_tbl) (struct sym_s * fp, int make_vartable);
} *binfmt, binfmts[] = {
#ifdef AOUT_CAPABLE
    {check_aout_magic, read_aout_nlist, read_aout_uts, make_aout_tbl},
#endif
#ifdef ELF_CAPABLE
    {check_elf_magic,  read_elf_nlist,  read_elf_uts,  make_elf_tbl},
#endif
#ifdef BFD_CAPABLE
    {check_bfd_magic,  read_bfd_nlist,  read_bfd_uts,  make_bfd_tbl},
#endif
    {0, },
};

char *strings;
long nsym, stringsize;

int
main (int argc, char **argv)
{
  char nsyspath[128];		/* Arbitrary, but should be big enough. */

  if (argc == 1)
    update_psdb (SYS_PATH);
  if (argc == 2) {
    sscanf (argv[1], "%s", nsyspath);
    update_psdb (nsyspath);
  }
  return 0;
}

void
update_psdb (char *syspath)
{
  int fd, sysfd;
  struct new_utsname uts;
  extern char procps_version[];

  if ((sysfd = open (syspath, O_RDONLY)) == -1) {
    perror (syspath);
    exit (errno);
  }
  for (binfmt = binfmts; binfmt->check_magic; ++binfmt) {
      if ((*binfmt->check_magic)(sysfd, syspath) == 0)
          break;
  }
  if (!binfmt->check_magic) {
      fprintf (stderr, "%s: Not an executable image.  I quit.\n", syspath);
      exit (1);
  }
  binfmt->read_nlist (sysfd, syspath);
  
  if ((fd = open (PSDATABASE, O_RDWR | O_TRUNC | O_CREAT, 0666)) == -1) {
    perror (PSDATABASE);
    exit (errno);
  }
  if (*syspath != '/') {
      memset (db_hdr.sys_path, 0, sizeof db_hdr.sys_path);
      if (getcwd (db_hdr.sys_path, sizeof db_hdr.sys_path - 2) != NULL)
          strcat (db_hdr.sys_path, "/");
      strncat (db_hdr.sys_path, syspath, sizeof db_hdr.sys_path -
               strlen (db_hdr.sys_path) - 1);
  } else
      strncpy (db_hdr.sys_path, syspath, sizeof db_hdr.sys_path);
  strncpy (db_hdr.magic, procps_version, sizeof db_hdr.magic);
  make_vartbl ();
  make_fnctbl ();
  /* Leave gap for header. */
  MLSEEK (fd, sizeof(db_hdr), SEEK_SET, PSDATABASE);
  write_tbl (fd, &db_hdr.fncs, &fncs);
  write_tbl (fd, &db_hdr.vars, &vars);
  (*binfmt->read_uts)(sysfd, &uts, syspath);
  close (sysfd);
  strncpy (db_hdr.uts_release, uts.release, sizeof db_hdr.uts_release);
  strncpy (db_hdr.uts_version, uts.version, sizeof db_hdr.uts_version);
  /* Press rewind. */
  MLSEEK (fd, 0L, SEEK_SET, PSDATABASE);
  /* Fill header into gap. */
  if (write (fd, (char *)&db_hdr, sizeof db_hdr) != sizeof db_hdr) {
    perror (PSDATABASE);
    exit (errno);
  }
  close (fd);
}

/*
 * make list of all text symbols, sorted on address for easy lookup of wait
 * channel.
 */
void
make_fnctbl (void)
{
  struct sym_s *fp;
#if DEBUG > 1
  int i;
#endif

  fp = fncs.tbl = (struct sym_s *)xmalloc (nsym * sizeof (struct sym_s));
  fp = (*binfmt->make_tbl)(fp, 0);
  fncs.strings = strings;
  fncs.nsym = fp - fncs.tbl;
  qsort (fncs.tbl, fncs.nsym, sizeof (struct sym_s), (__compar_fn_t) addrcmp);
#ifdef DEBUG
  fprintf (stderr, "%d text symbols\n", fncs.nsym);
#if DEBUG > 1
  for (i = 1; i < fncs.nsym; ++i)
    if (fncs.tbl[i].addr == fncs.tbl[i - 1].addr)
      fprintf (stderr, "text symbols %s and %s both have address %lx\n",
	       strings + fncs.tbl[i - 1].name,
	       strings + fncs.tbl[i].name, fncs.tbl[i].addr);
#endif
#endif
}

void
make_vartbl (void)
{
  struct sym_s *vp;

  vp = vars.tbl = (struct sym_s *)xmalloc (nsym * sizeof (struct sym_s));
  vp = (*binfmt->make_tbl)(vp, 1);
  vars.strings = strings;
  vars.nsym = vp - vars.tbl;
  qsort (vars.tbl, vars.nsym, sizeof (struct sym_s), (__compar_fn_t) varcmp);
#ifdef DEBUG
  fprintf (stderr, "%d data/bss symbols\n", vars.nsym);
#endif
}

/*
 * write table tbl to descriptor fd, header structure dbtdl is updated
 */
int
write_tbl (int fd, struct dbtbl_s *dbtbl, struct tbl_s *tbl)
{
  char *s;
  int i, strsize, symsize;
  struct sym_s *p;

  if ((dbtbl->off = lseek (fd, 0L, SEEK_CUR)) == -1) {
    perror (PSDATABASE);
    exit (errno);
  }
  s = tbl->strings = xmalloc (stringsize);
  for (i = tbl->nsym, p = tbl->tbl; i--;) {
    strcpy (s, strings + p->name);
    p->name = s - tbl->strings;
    ++p;
    s += strlen (s) + 1;
  }
  symsize = tbl->nsym * sizeof (struct sym_s);
  if (write (fd, (char *)tbl->tbl, symsize) != symsize)
    return -1;
  strsize = (s - tbl->strings + 3) & ~3;
  if (write (fd, tbl->strings, strsize) != strsize)
    return -1;
  dbtbl->size = strsize + symsize;
  dbtbl->nsym = tbl->nsym;
  return (0);
}

/*
 * fncs are sorted on address
 */
int
addrcmp (struct sym_s * p1, struct sym_s * p2)
{
  return (p1->addr > p2->addr) - (p1->addr < p2->addr);
}

/*
 * vars are sorted on name
 */
int
varcmp (struct sym_s * p1, struct sym_s * p2)
{
  return (strcmp (vars.strings + p1->name,
		  vars.strings + p2->name));
}

/*
 * get address of data symbol
 */
unsigned long
k_addr (char *sym)
{
  struct sym_s key, *p;

  if (vars.tbl == NULL)
    read_tbl (&db_hdr.vars, &vars);
  key.name = sym - vars.strings;
  p = (struct sym_s *)bsearch (&key, vars.tbl, vars.nsym,
			     sizeof (struct sym_s), (__compar_fn_t) varcmp);
  return (p ? p->addr : -1);
}
