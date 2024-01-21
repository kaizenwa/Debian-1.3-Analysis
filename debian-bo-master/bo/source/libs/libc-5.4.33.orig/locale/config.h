#ifndef _CONFIG_H
#define _CONFIG_H

#define PACKAGE "localedef"
#define VERSION "0.1"

#define DEFAULT_CHARMAP "POSIX"
/*#define CHARMAP_PATH "/usr/share/nls/charmap"
#define LOCALE_PATH "/usr/share/locale"*/


#define HAVE_VPRINTF 1
#define STDC_HEADERS 1
#define HAVE_STRERROR 1
#define HAVE_CATGETS 1
#define HAVE_LIBINTL_H 1

typedef unsigned short u16;
typedef int i32;
typedef int u32;

#endif /* config.h */
