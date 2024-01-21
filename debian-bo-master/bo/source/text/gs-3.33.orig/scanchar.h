/* Copyright (C) 1990, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* scanchar.h */
/* Character scanning table for Ghostscript */
/* Requires scommon.h */

/* An array for fast scanning of names, numbers, and hex strings. */
/*  Indexed by character code (including exceptions), it contains: */
/*	0 - max_radix-1 for valid digits, */
/*	ctype_name for other characters valid in names, */
/*	ctype_btoken for characters introducing binary tokens */
/*	  (if the binary token feature is enabled), */
/*	ctype_space for whitespace characters, */
/*	ctype_exception for exceptions (see scommon.h), and */
/*	ctype_other for everything else. */
/* This table is defined in iscan.c, used in iscan.c and stream.c. */
/* NOTE: If any of the values below change, you must edit the table. */
/* Exceptions are negative values; we bias the table origin accordingly. */
extern const byte scan_char_array[max_stream_exception + 256];
#define scan_char_decoder (&scan_char_array[max_stream_exception])
#define min_radix 2
#define max_radix 36
#define ctype_name 100
#define ctype_btoken 101
#define ctype_space 102
#define ctype_other 103
#define ctype_exception 104
/* Special characters with no \xxx representation */
#define char_NULL 0
#define char_EOT 004			/* ^D, job delimiter */
#define char_VT 013			/* ^K, vertical tab */
#define char_DOS_EOF 032		/* ^Z */
/*
 * Most systems define '\n' as 0x0a and '\r' as 0x0d; however, OS-9
 * has '\n' = '\r' = 0x0d and '\l' = 0x0a.  To deal with this,
 * we introduce abstract characters char_CR and char_EOL such that
 * any of [char_CR], [char_CR char_EOL], or [char_EOL] is recognized
 * as an end-of-line sequence.
 */
#define char_CR '\r'
#if '\r' == '\n'
#  define char_EOL 0x0a		/* non-OS-9 compilers complain about '\l' */
#else
#  define char_EOL '\n'
#endif
