/*
   Time-stamp: <96/07/19 20:15:31 yusuf>

   $Id: tailor.h,v 1.16 1996/07/27 20:42:32 yusuf Exp $	

*/



/* This module of code has been shamelessley ripped off from the
 * GNU gzip package - version 1.2.4.
 * 
 * Not only has it been ripped off, it has been hopelessly adulterated
 * for use by taper.
 * 
 * Since gzip works pretty well, any bugs found are more than likely
 * to have been caused me in the modification process so blame me
 * and not the GNU team.
 * 
 * 
 * Yusuf Nagree
 * 
*/



/* tailor.h -- target dependent definitions
 * Copyright (C) 1992-1993 Jean-loup Gailly.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */

/* The target dependent definitions should be defined here only.
 * The target dependent functions should be defined in tailor.c.
 */

/* $Id: tailor.h,v 1.16 1996/07/27 20:42:32 yusuf Exp $ */



/* Common defaults */

#ifndef OS_CODE
#  define OS_CODE  0x03  /* assume Unix */
#endif

#ifndef PATH_SEP
#  define PATH_SEP '/'
#endif

#ifndef casemap
#  define casemap(c) (c)
#endif

#ifndef OPTIONS_VAR
#  define OPTIONS_VAR "GZIP"
#endif

#ifndef Z_SUFFIX
#  define Z_SUFFIX ".gz"
#endif

#ifdef MAX_EXT_CHARS
#  define MAX_SUFFIX  MAX_EXT_CHARS
#else
#  define MAX_SUFFIX  30
#endif

#ifndef MAKE_LEGAL_NAME
#  ifdef NO_MULTIPLE_DOTS
#    define MAKE_LEGAL_NAME(name)   make_simple_name(name)
#  else
#    define MAKE_LEGAL_NAME(name)
#  endif
#endif

#ifndef MIN_PART
#  define MIN_PART 3
   /* keep at least MIN_PART chars between dots in a file name. */
#endif

#ifndef EXPAND
#  define EXPAND(argc,argv)
#endif

#ifndef RECORD_IO
#  define RECORD_IO 0
#endif

#ifndef SET_BINARY_MODE
#  define SET_BINARY_MODE(fd)
#endif

#ifndef OPEN
#  define OPEN(name, flags, mode) open(name, flags, mode)
#endif

#ifndef get_char
#  define get_char() get_byte()
#endif

#ifndef put_char
#  define put_char(c) put_byte(c)
#endif
