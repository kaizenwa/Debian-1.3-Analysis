/* ########################################################################

			     global_parse.h

   File: global_parse.h
   Path: /home/fournigault/c/X11/xcoral-2.31/global_parse.h
   Description: 
   Created: Fri Jan 27 11:04:38 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 11:04:39 MET 1995
   Last maintained by: Dominique Leveque

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Dominique Leveque

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

   ######################################################################## */


#ifndef GLOBALPARSE_H
#define GLOBALPARSE_H


typedef struct _lex_info_type {
  char  text[1024];
  int   precedence;
  int   position;
  int   is_virtual;
} lex_info_type;

#define YYSTYPE lex_info_type


extern char yy_class_name[];

extern int line_count;


#endif  /*  GLOBALPARSE_H  */
