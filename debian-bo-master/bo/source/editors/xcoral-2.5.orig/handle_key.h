/* ########################################################################

			      handle_key.h

   File: handle_key.h
   Path: /home/fournigault/c/X11/xcoral-2.31/handle_key.h
   Description: 
   Created: Fri Jan 27 11:05:43 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:05:43 MET 1995
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


#ifndef _HANDLE_KEY_H_
#define _HANDLE_KEY_H_

#include "main_text.h"

#include "proto_decl.h"

typedef struct _InfosKey {
	int	type;
	int	ch;
} InfosKey;

typedef struct _XKKey{
  char * name;
  KeySym xk_code;
} XKKey;

extern XKKey XKInfos[];

typedef struct _Trans{
  int	type;
  void 	(* fnt) ();
  void	* dest_stat;
} Trans;

typedef struct _ST { 
  Trans trans[9];
} ST;

FCT (void, AbortCurrentCmd, (Text *text) );
FCT (void, DeleteCurrentWindow, (Text *text) );
FCT (void, GotoLine, (Text *text) );
FCT (void, DisplayLineNumber, (Text *text) );
FCT (void, KillLines, (Text *text) );
FCT (void, PlayMacro, (Text *text) );
FCT (ST *, automate, (Text *text, XKeyEvent *event, ST *current_st) );

FCT (int, Xk_Code,(char *));
FCT (int, XK_ksym_to_code,(KeySym ksym));

#define MAXREPEAT	9999
#define KEY		10
#define CONTROL		11
#define CONTROL_AND_KEY	12
#define ESCAPE		0x1b
#define CONTROL_AND_X	14
#define SPECIAL		15
#define DIGIT		17

#define	RETURN		0x0d
#define	LINEFEED	0x0a
#define	BACKSPACE	0x08
#define	DELETE		0x7f
#define	TAB		0x09

#define Ctr_sp		0x00
#define	CtrA		0x01
#define	CtrB		0x02
#define	CtrC		0x03
#define	CtrD		0x04
#define	CtrE		0x05
#define	CtrF		0x06
#define	CtrG		0x07
#define	CtrH		0x08
#define	CtrI		0x09
#define	CtrJ		0x0a
#define	CtrK		0x0b
#define	CtrL		0x0c
#define	CtrM		0x0d
#define	CtrN		0x0e
#define	CtrO		0x0f
#define	CtrP		0x10
#define	CtrQ		0x11
#define	CtrR		0x12
#define	CtrS		0x13
#define	CtrT		0x14
#define	CtrU		0x15
#define	CtrV		0x16
#define	CtrW		0x17
#define	CtrX		0x18
#define	CtrY		0x19
#define CtrZ		0x1a

#endif /* _HANDLE_KEY_H_ */
