/*
 * nls.h: assign nls and locale macros
 *  
 * Copyright (C), 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with the man_db
 * distribution.
 *
 * Sat Oct 29 13:09:31 GMT 1994  Wilf. (G.Wilford@ee.surrey.ac.uk) 
 */

#ifndef NLS_H
#define NLS_H

#ifdef HAVE_SETLOCALE
#  include <locale.h>

/* if LC_MESSAGES is not defined in <locale.h>, define it as LC_ALL */
#  ifndef LC_MESSAGES
#    define LC_MESSAGES LC_ALL	/* sad but true */
#  endif /* LC_MESSAGES */
#endif /* HAVE_SETLOCALE */

#ifdef NLS

#  ifdef STDC_HEADERS
#    include <stdlib.h>
#  endif

#  include <nl_types.h>
#  include "man_db-nls.h"

extern nl_catd catfd;		/* the catalogue descriptor */

/* define the macro EXIT_FUNC as an appropriate `at exit' function */
#  if defined(HAVE_ATEXIT)
#    define EXIT_FUNC		(void)atexit((void *)close_catalogue)	

extern void close_catalogue (void);

#  elif defined (HAVE_ON_EXIT)
#    define EXIT_FUNC		(void)on_exit((void *)close_catalogue,(void *)NULL)

extern void close_catalogue (int status, char *arg);

#  else /* !HAVE_ATEXIT && !HAVE_ON_EXIT */
#    define EXIT_FUNC
#  endif /* HAVE_ATEXIT */

/* define the NLS function macros */

#  include "gencat/msgcat.h"

#  define NLS_INIT		setlocale(LC_MESSAGES,"");\
				catfd=catopen("man_db",MCLoadBySet);\
				EXIT_FUNC
#  define CATGETS(id, def_msg)	catgets((catfd), (NLS_SET), (id), (def_msg))
#  define NLS_CLOSE		catclose(catfd)

#else /* !NLS */

/* define the NLS function macros */
#  ifdef HAVE_SETLOCALE
#    define NLS_INIT		 setlocale(LC_MESSAGES,"")
#  else
#    define NLS_INIT
#  endif /* HAVE_SETLOCALE */
#  define CATGETS(id, def_msg)    def_msg
#  define NLS_CLOSE

#endif /* NLS */
#endif /* NLS_H */
