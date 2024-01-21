#ifndef _LD_CONFIG_H
#define _LD_CONFIG_H

/* Use the internal textdomain used for libc messages.  */
#define PACKAGE _libc_intl_domainname
#ifndef VERSION
/* Get libc version number.  */
#include <_G_config.h>
#define VERSION _LINUX_C_LIB_VERSION
#endif

#define DEFAULT_CHARMAP "POSIX"

#ifndef PARAMS
# if __STDC__
#  define PARAMS(args) args
# else
#  define PARAMS(args) ()
# endif
#endif



#define HAVE_VPRINTF 1


typedef unsigned int wint_t;


#endif
