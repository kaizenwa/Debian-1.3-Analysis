#ifndef _gettext_h_
#define _gettext_h_ 1

/* gettext.h - decls and macros for GNU internationalization support

   Copyright (C) 1994 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Written by Jim Meyering <meyering@gnu.ai.mit.edu>.  */

struct _msg_ent
  {
    const char *_msg;
    int _msg_number;
  };

/* For those concerned about the overhead of the linear string look-up in
   the message table, consider the following GCC statement expression
   macros.  Then, if there were, for example, a `printf (LC_CATGETS(msg),
   arg1)' in a loop, the additional overhead of mapping from MSG to
   the other catgets arguments would be incurred only for the first
   invocation.  All subsequent references would get the proper string
   for the price of a simple pointer comparison.  */

/* #if defined (__GNUC__) && !defined (__STRICT_ANSI__)

#define gettext(Message)						\
  ({									\
    static const char *_translation;					\
									\
    if (!_translation)							\
      _translation = __gettext ((Message));				\
    _translation;							\
  })

#else */

/* If you don't use gcc, you'll incur the lookup cost every time.  */
#define gettext(msg) __gettext ((msg))

/* #endif */

#ifndef __P
# if defined (__GNUC__) || (defined (__STDC__) && __STDC__)
#  define __P(args) args
# else
# define __P(args) ()
# endif
#endif

const char *__gettext __P ((const char *_msg));

/* A table of (english-message, msg_code_number) pairs.  */
extern const struct _msg_ent _msg_tbl[];

/* Number of entries in _msg_tbl.  */
extern int _msg_tbl_length;

/* The file name of the message catalog to be used in the catopen
   call below.  */
extern const char *_msg_catalog_file_name;

#endif /* _gettext_h_ */
