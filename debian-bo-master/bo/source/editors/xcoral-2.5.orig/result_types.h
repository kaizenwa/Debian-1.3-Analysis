/* ########################################################################

			     result_types.h

   File: result_types.h
   Path: /home/fournigault/c/X11/xcoral-2.31/result_types.h
   Description: 
   Created: Fri Jan 27 11:27:37 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 11:27:40 MET 1995
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


#ifndef RESULTTYPES_H
#define RESULTTYPES_H


/*------------------------------------------------------------------------------
//                     Le pointeur null
//------------------------------------------------------------------------------
*/
#define Null 0


/*------------------------------------------------------------------------------
//                     Le type des differents resultats 
//------------------------------------------------------------------------------
*/
typedef int LineNumber;


typedef char* StringTable[];


struct Position {
  char*      file_name;
  LineNumber line_number;
};

typedef struct Position Position;


/*------------------------------------------------------------------------------
//                     Le type des differents resultats 
//------------------------------------------------------------------------------
*/

#define FILE_PREFIX        ""

#define FILE_PLENGTH       0


#define PROC_PREFIX        "[   ]  "

#define PROC_PLENGTH       7


#define PARENT_PREFIX      "[ ]  "

#define PARENT_PLENGTH     5


#define METHOD_PREFIX      "[    ]  "

#define METHOD_PLENGTH     8


#define CLASS_PREFIX       "[ ]  "

#define CLASS_PLENGTH      5


/*------------------------------------------------------------------------------
//   Les differentes macros applicable aux resultats retournes par le browser 
//------------------------------------------------------------------------------
*/
#define GET_FILE_INFO(pt)       (pt - FILE_PLENGTH)
#define REMOVE_FILE_INFO(pt) (pt+FILE_PLENGTH)

#define GET_PROCEDURE_INFO(pt)  (pt - PROC_PLENGTH)
#define REMOVE_PROCEDURE_INFO(pt) (pt+PROC_PLENGTH)

#define GET_PARENT_INFO(pt)     (pt - PARENT_PLENGTH)
#define REMOVE_PARENT_INFO(pt) (pt+PARENT_PLENGTH)

#define GET_SON_INFO(pt)        (pt - CLASS_PLENGTH)
#define REMOVE_SON_INFO(pt) (pt+SON_PLENGTH)

#define GET_METHOD_INFO(pt)     (pt - METHOD_PLENGTH)
#define REMOVE_METHOD_INFO(pt) (pt+METHOD_PLENGTH)

#define GET_CLASS_INFO(pt)      (pt - CLASS_PLENGTH)
#define REMOVE_CLASS_INFO(pt) (pt+CLASS_PLENGTH)

#define GET_METHOD_CLASS(pt)    (get_method_class(pt))

extern char* get_method_class();


#endif  /*  RESULTTYPES_H  */
