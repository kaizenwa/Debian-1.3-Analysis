/* Special definitions for ptx, processed by autoheader.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Francois Pinard <pinard@iro.umontreal.ca>, 1994.
*/

/* In regex, request the capability of modifying the letter syntax.  */
#define SYNTAX_TABLE 1

/* In regex, use 8 bits per character.  */
#define CHAR_SET_SIZE 256

@TOP@

/* Define to the name of the distribution.  */
#undef PRODUCT

/* Define to 1 if ANSI function prototypes are usable.  */
#undef PROTOTYPES

/* Define to the version of the distribution.  */
#undef VERSION

/* Define to 1 for better use of the debugging malloc library.  See 
   site ftp.antaire.com in antaire/src, file dmalloc/dmalloc.tar.gz.  */
#undef WITH_DMALLOC
