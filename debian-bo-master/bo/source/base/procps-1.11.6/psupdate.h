#ifndef psupdate_h
#define psupdate_h

#include "proc/psdata.h"

/*
 * You can define ELF_CAPABLE at compile-time to get an a.out binary that can
 * handle ELF kernel images.
 */
#if defined(__ELF__) && !defined(NO_ELF_CAPABILITY)
# define ELF_CAPABLE
# define ELF_OBJECT 1
# define ELF_FUNC 2
#endif

#if defined(__alpha__) || defined(__sparc__)
# define BFD_CAPABLE
#else
# define AOUT_CAPABLE
#endif

#ifdef AOUT_CAPABLE
extern int	check_aout_magic (int, const char *);
extern int	read_aout_nlist (int, const char *);
extern void	read_aout_uts (int, struct new_utsname *, const char *);
extern struct sym_s *make_aout_tbl (struct sym_s *, int);
#endif

#ifdef ELF_CAPABLE
extern int	check_elf_magic (int, const char *);
extern int	read_elf_nlist (int, const char *);
extern void	read_elf_uts (int, struct new_utsname *, const char *);
extern struct sym_s *make_elf_tbl (struct sym_s *, int);
#endif

#ifdef BFD_CAPABLE
extern int	check_bfd_magic (int, const char *);
extern int	read_bfd_nlist (int, const char *);
extern void	read_bfd_uts (int, struct new_utsname *, const char *);
extern struct sym_s *make_bfd_tbl (struct sym_s *, int);
#endif

extern char *	strings;
extern long	nsym, stringsize;

#endif psupdate_h
