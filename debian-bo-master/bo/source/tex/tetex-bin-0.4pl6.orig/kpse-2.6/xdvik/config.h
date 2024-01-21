/* config.h: master configuration file, included first by all compilable
   source files (not headers).  */

#ifndef CONFIG_H
#define CONFIG_H

/* The stuff from the path searching library.  */
#include <kpathsea/config.h>

#include <setjmp.h>

#ifndef HAVE_VPRINTF
#ifdef HAVE_DOPRNT
#define	vfprintf(stream, message, args)	_doprnt(message, args, stream)
/* If we have neither, should fall back to fprintf with fixed args.  */
#endif
#endif

/* Some xdvi options we want by default.  */
#define USE_PK
#define USE_GF
#ifndef NOSELFILE
#define SELFILE
#endif
#ifndef NOBUTTONS
#define BUTTONS
#endif
#ifndef NOGREY
#define GREY
#endif
#ifndef NOTEXXET
#define TEXXET
#endif
#ifdef NOMAKEPK
#define MAKEPK_BY_DEFAULT_STRING "false"
#define MAKEPK_BY_DEFAULT_BOOL False
#else
#define MAKEPK_BY_DEFAULT_STRING "true"
#define MAKEPK_BY_DEFAULT_BOOL True
#endif

/* xdvi's definitions.  */
#include "xdvi.h"

#endif /* not CONFIG_H */
