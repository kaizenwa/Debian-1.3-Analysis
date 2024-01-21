/* ########################################################################

				 mode.h

   File: mode.h
   Path: /home/fournigault/c/X11/xcoral-2.31/mode.h
   Description: 
   Created: Fri Jan 27 11:21:09 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:21:10 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

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


#ifndef  _MODE_H_
#define  _MODE_H_

#include "proto_decl.h"

typedef struct {
    void (* func) (); /* La fonction associee a la clef */
    int type;         /* Fonction de l'editeur ou de l'utilisateur */
} MK;
  
/*
 * Le Mode
 */
typedef struct _mode {
    char *name;
    char *suffixes;
    XFontStruct *font;
    MK key [512];
    MK ctrX_key [512];
    MK esc_key [512];
    struct _mode *next;
} Mode;

FCT (Mode *, CreateNewMode, (char *mode_name) );

#ifdef __alpha
FCT (long, GetFuncNameFromString, (char *s) );
#else
FCT (int, GetFuncNameFromString, (char *s) );
#endif

FCT (Mode *, GetMode, (char *name) );
FCT (void, InitMode, () );
FCT (Mode *, SearchMode, (char *filename) );
FCT (void, SetModeFont, (Mode *mode, char *font_name) );
FCT (void, SetModeSuffixe, (Mode *mode, char *suffixes) );

#endif /* _MODE_H_ */
