/* model.h						  */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

/*
       Allgemeine Modell-Definitionen      */

#define MODELL

/*  System Definition   */

#define UNIX
#undef DJGPP
#undef DOS

/*  Wirkungen (nicht veraendern)  */

#define WEUNDO
#define CHECKHEADER
#undef FREETEST

#ifdef DJGPP
#define NODEBUGGER
#undef NOPROG
#define NOXWINDOW
#define NOPRINTER
#define NONEWSTYLE
#define MOUSE 1
/*  #define DESTDIR "D:\\USR\\KRUSE\\BIN"   */
#ifndef UNIX
#define UNIX
#endif
#endif

#ifdef DOS

#define MOUSE   1        /*  Maus ein- und ausschalten  */
#undef NOPRINTER         /*  Druckoption einschalten  */
#define UHR
#undef UNIX
#undef DEFPGC            /*  Definiere Makros  putc  und  getc   */
#undef NOANSI
#undef SWAP

#else

/*  XWindow Definition  */

#ifndef NOXWINDOW
#define XWINDOW
#endif
#if defined(DJGPP) || defined(XWINDOW)
#define MOUSE   1        /*  Maus einschalten  */
#else
#define MOUSE   0        /*  Maus ausschalten  */
#endif

/*  Programming Environment  */

#ifndef NOPROG
#define PROG
#ifndef NODEBUGGER
#define DEBUGGER
#endif
#endif

/*  Newstyle only for XWindow   */
#if defined(XWINDOW) && !defined(NONEWSTYLE)
#define NEWSTYLE
#endif
#undef UHR
#undef SWAP
#endif

/*
#ifndef LIBDIR
#define LIBDIR DESTDIR/../lib
#endif

#ifndef WPELIBDIR
#define WPELIBDIR LIBDIR/wpe
#endif
*/

