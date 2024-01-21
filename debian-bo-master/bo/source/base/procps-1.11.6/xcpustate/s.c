/*
 * for each type of machine, stick the appropriate ifdef's in here and define
 * s_included.  If s_included is not defined, it assumes BSD (i.e look for
 * _cp_time in kmem)
 */
#ifdef sgi
# ifdef mips
#  include "s-iris4d.c"
#  define s_included
# endif
#endif

#ifdef CRAY1
# include "s-cray-xmp.c"
# define s_included
#endif

#ifdef linux
# include "s-linux.c"
# define s_included
#endif

/* This should work on most BSD machines */
#ifndef s_included
# include "s-bsd.c"
#endif
/* Do not add anything after this line */
