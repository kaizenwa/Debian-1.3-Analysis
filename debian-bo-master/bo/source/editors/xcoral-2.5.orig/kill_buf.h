/* ########################################################################

			       kill_buf.h

   File: kill_buf.h
   Path: /home/fournigault/c/X11/xcoral-2.31/kill_buf.h
   Description: 
   Created: Fri Jan 27 11:13:52 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:13:53 MET 1995
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


#ifndef _KILL_BUF_H_
#define _KILL_BUF_H_

#include "proto_decl.h"

/*
**	Le Kill_Buffer est une liste chainee de structure kb decrite si-dessous
**	Pour chaque operation Ctr-K ou Ctr-W dans le texte courant, une structure kb
**	est allouee et les champs, concernant l'objet tue, sont positionnes.
**	Cette structure est ensuite liee a celles deja existantes et devient le
**	kill-buffer courrant (current_kb).C'est tout bete.
*/
typedef struct _kb {
  char *p;		/* Le buffer */
  unsigned int s_len;	/* Longueur de s dans le buffer */
  unsigned int s_lines;	/* Nb de lignes */
  struct _kb *next;	/* Les liens */
  struct _kb *prev;
} kb;

FCT (void, LoadKillBuffer, (Buf *buf) );
FCT (char *, RestoreKillBuf, (int i, int *len, int *dn) );
FCT (void, StoreInKillBuf, (char *s, int len, int n) );

#endif /* _KILL_BUF_H_ */
