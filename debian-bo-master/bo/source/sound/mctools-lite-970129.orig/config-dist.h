#ifndef _CONFIG_H_
#define _CONFIG_H_
/******************************************************************************
 *
 * Installation directories, modify to your taste...
 *
 * If you don't define them, imake will use default values which usually
 * means that the programs end up somewhere in the X11 tree.
 *
 */
#define INSTALL_BINDIR		/usr/local/bin
#define INSTALL_MANDIR		/usr/local/man
#define INSTALL_LIBDIR		/usr/local/lib
#define INSTALL_DEFAULTDIR	/usr/local/lib/app-defaults

/* Only used with gettext, see below */
#define INSTALL_LOCALEDIR	/usr/share/locale

/******************************************************************************
 *
 * System properties are configured in the next section. I've chosen
 * reasonable defaults; so if you don't know what I'm talking about below,
 * just keep them and everything should work.
 *
 */

/*
 * This define doesn't do anything on an a.out system.
 *
 * The library file is currently about 100k. If you insist on building it as
 * a shared library (ELF ONLY) then uncomment this.
 *
 * If you do, you will have to install the library before you can try
 * the programs.
 *
 * When compiling an ELF shared library, make sure that INSTALL_LIBDIR is
 * also in /etc/ld.so.conf.
 *
 * BE SURE TO DO A `build clean' AFTER CHANGING THIS DEFINE!
 *
 */
/* #define BUILD_ELF_SHARED_LIBRARY */

/*
 * If your select() call modifies the timeout value to reflect the time
 * remaining, then define this, it's a GoodThing(tm).
 * If you are unsure, don't define it.
 */
#ifdef linux
#define SELECT_MODIFIES_TIMEOUT
#endif

/*****************************************************************************
 *
 * If you have GNU gettext on your system then enable this to get support
 * for other languages than english.
 *
 * The translation can easily be done even by non programmers, contact me
 * if you are interested in writing or correcting translations.
 *
 */
#undef HAVE_GETTEXT

/*
 * With gettext, this is the directory where the program expects the message
 * catalogs. If this is not defined, gettext will use the default, which is
 * /usr/share/locale according to the Linux filesystem standard; so you
 * should better not change it.
 *
 * The translated message file is then searched in
 *   <LOCALEDIR>/<language>/LC_MESSAGES/<programname>.mo
 *
 * This default may be overridden with the *localedir resource.
 *
 */
#define LOCALEDIR		"/usr/share/locale"

/******************************************************************************
 *
 * The stuff below is only for people with good programming knowledge who
 * want to debug.
 * Others may safely ignore anything below and start compiling.
 *
 */

/*
 * Chose debugging or not...
 */
/* #define DEBUG_FLAGS -O -g -DDEBUG_CODE -Wall -Wmissing-prototypes -Wshadow -Wnested-externs -Winline -Wunused */
/* #define EFENCE */
/* This is for malloc statistics, works only if you've GNU-malloc somewhere */
/* #define HAVE_MSTATS */
/* #define MALLOC_LIB /usr/lib/gmalloc.o */

/******************************************************************************
 *
 * Abandon hope all ye who enter here... No user servicable parts below.
 *
 */

#ifdef HAVE_GETTEXT
# define GETTEXT_LIB -lintl
# ifndef IMAKE_TEMPLATE
#  include <libintl.h>
#  include <locale.h>
# endif
# define _(String) gettext(String)
# define __(String) (String)
#else
# define _(String) (String)
# define __(String) (String)
# define gettext(String) (String)
# define GETTEXT_LIB /* empty */
#endif

#ifdef EFENCE
# define EFENCE_LIB -lefence
#else
# define EFENCE_LIB /* empty */
#endif

#if defined(DEBUG_FLAGS) && defined(BUILD_ELF_SHARED_LIBRARY)
#undef BUILD_ELF_SHARED_LIBRARY
#endif

#ifdef BUILD_ELF_SHARED_LIBRARY
#define MCTOOLS -L../McTools -lMcTools
#else
#define MCTOOLS ../McTools/libMcTools.a
#endif

#if !defined(DEBUG_FLAGS) && defined(MALLOC_LIB)
#undef MALLOC_LIB
#endif

#ifndef MALLOC_LIB
#define MALLOC_LIB /* empty */
#endif

#define MCTOOL_LIBRARIES MCTOOLS -lm -lXpm -lX11 EFENCE_LIB GETTEXT_LIB MALLOC_LIB

/******************************************************************************
 *
 * Stuff for Makefiles
 *
 */
#ifdef IMAKE_TEMPLATE

# ifdef INSTALL_BINDIR
BINDIR = INSTALL_BINDIR
# endif
# ifdef INSTALL_LIBDIR
LIBDIR = INSTALL_LIBDIR
SHLIBDIR = INSTALL_LIBDIR
# endif
# ifdef INSTALL_MANDIR
MANDIR = INSTALL_MANDIR
# endif
#ifdef INSTALL_LOCALEDIR
LOCALEDIR = INSTALL_LOCALEDIR
#endif
# ifdef INSTALL_DEFAULTDIR
XAPPLOADDIR = INSTALL_DEFAULTDIR
# endif

#ifdef HAVE_GETTEXT

#define McInstallMessageTarget(files,package)                             @@\
install:: install.msg                                                     @@\
									  @@\
install.msg::                                                             @@\
	@if test -d po && test files && test package; then		 \@@\
	  cd po; for file in files; do					 \@@\
	    if test -f $$file; then					 \@@\
	      DIR=$(LOCALEDIR)/`basename $$file | cut -d. -f1`/LC_MESSAGES;\@@\
	      $(MKDIRHIER) $$DIR;					 \@@\
	      (set -x;							 \@@\
	       $(INSTALL) $(INSTALLFLAGS) $$file $$DIR/package.mo;	 \@@\
	      );							 \@@\
	     fi								 \@@\
	   done								 \@@\
	 fi

#else

#define McInstallMessageTarget(files,package) /* nothing */

#endif

#ifdef DEBUG_FLAGS
CDEBUGFLAGS = DEBUG_FLAGS
#endif

#endif

#endif /* _CONFIG_H_ */



