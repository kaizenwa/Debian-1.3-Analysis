/*
 *                            COPYRIGHT
 *
 *  PCB, interactive printed circuit board design
 *  Copyright (C) 1994,1995,1996 Thomas Nau
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Contact addresses for paper mail and Email:
 *  Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
 *  Thomas.Nau@rz.uni-ulm.de
 *
 *  RCS: $Id: config.h,v 143.1 1996/09/16 09:09:31 nau Exp $
 */

/* ------------------- start of user configuration ---------------------------
 *
 *   INFODIR:  	      where to install the GNU-info files
 *   PCBLIBDIR:       where to install fonts...
 *   DEFAULTFONT:     the name of the default-font file
 *   DEFAULTLIBRARY:  the name of the default-library file
 *
 * PCBLIBDIR is set, for example, to /usr/lib/X11/pcb
 * (LIBDIR is defined by imake)
 *
 */
INFODIR = /usr/local/info
PCBLIBDIR = $(LIBDIR)/pcb
DEFAULTFONT = default_font
DEFAULTLIBRARY = pcblib

/* ---------------------------------------------------------------------------
 * the name of GNU m4 (you MUST use GNU m4)
 */
GNUM4 = m4

/* ----------------------------------------------------------------------
 * BTNMOD determies the modifier key to be used for the
 * function key and buttons in the default translations.
 * The default, <mod1>, works fine if you do not use the HP-VUE
 * window manager which uses <mod1> itself. It's save to use <ctrl> instead.
 */
BTNMOD = Mod1

/* ----------------------------------------------------------------------
 * You will probably need an additional searchpath for include files
 * if you use OpenWindows
 */
/* EXTRA_INCLUDES  = -I/usr/openwin/include */

/* ----------------------------------------------------------------------
 * Check the following section of architecture dependend settings.
 * If there's a #ifdef - #endif pair for your machine fine; if
 * not you may create a new one (and mail it to the author too if
 * it works) or try the default. Here's some more information:
 *
 * if your system supports atexit() define EXITCALL as -DHAS_ATEXIT
 * if your system has on_exit() but no atexit() like suns with BSD
 * define it as -DHAS_ON_EXIT
 *
 * setup the system libraries needed for lex (flex), also add -lm
 *
 * a symbol PATCHES is used to pass additional information to
 * the compiler. Additional options are:
 *   -DNEED_STRDUP       if your system doesn't have strdup()
 */

/* ----------------------------------------------------------------------
 * some default values which can be changed in the architecture
 * depending section
 *
 * HAS_REGEX     defined if your compiler supports regular expressions
 *               Undefined if unsupported.
 */
HAS_REGEX = -DHAS_REGEX
SYS_LIBRARIES = -ll -lm         /* these are the default libraries */ 
#ifdef SYSV                     /* System V has atexit() */
      EXITCALL = -DHAS_ATEXIT
#endif

/* ----------------------------------------------------------------------
 * the architecture depending section
 */
#ifdef AnsiDefines              /* some ANSI CC need additional defines */
     EXTRA_DEFINES  = AnsiDefines
#endif

#ifdef LinuxArchitecture        /* LINUX */
      SYS_LIBRARIES = -lfl -lm
      EXITCALL = -DHAS_ATEXIT
#endif

#ifdef SGIArchitecture          /* Silicon Graphics */
      CCOPTIONS = -D__STDC__    /* thanks to Bernd Leibing */
      PROTO_DEFINES =           /* Bernd.Leibing@e-technik.uni-ulm.de */
      EXITCALL = -DHAS_ATEXIT
#endif

#ifdef HPArchitecture           /* HP */
      EXITCALL = -DHAS_ATEXIT
#endif

#ifdef SunArchitecture          /* Sun */
#if OSMajorVersion < 5          /* BSD */
      EXITCALL = -DHAS_ON_EXIT
#else                           /* Solaris */
      EXITCALL = -DHAS_ATEXIT
#endif
#endif

#ifdef ArmArchitecture          /* Acorn RISCiX, BSD libraries */
      EXITCALL = -DHAS_ATEXIT   /* thanks to Adrian Godwin */
                                /* agodwin@acorn.co.uk */
                                /* the BSD environment does not define M_PI */
      PATCHES = -DNEED_STRDUP
      CC = /usr/ucb/cc
#endif

/* ------------------- end of user configuration ------------------------- */

PATCHLEVEL = 3
RELEASE = 1.4.$(PATCHLEVEL)

COMPRESS = gzip
SUFFIX   = gz

DEFINES = -DRELEASE=\"$(RELEASE)\" \
 -DPCBLIBDIR=\"$(PCBLIBDIR)\" \
 -DBTNMOD=\"$(BTNMOD)\" \
 -DFONTFILENAME=\"$(DEFAULTFONT)\" \
 -DLIBRARYFILENAME=\"$(DEFAULTLIBRARY)\" \
 -DGNUM4=\"$(GNUM4)\" \
 $(EXITCALL) \
 $(HAS_REGEX) \
 $(PATCHES)

