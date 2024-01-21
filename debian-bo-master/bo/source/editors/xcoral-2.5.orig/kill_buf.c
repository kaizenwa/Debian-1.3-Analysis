/* ########################################################################

			       kill_buf.c

   File: kill_buf.c
   Path: /home/fournigault/c/X11/xcoral-2.31/kill_buf.c
   Description: 
   Created: Fri Jan 27 11:12:53 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:12:54 MET 1995
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


#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include "main_text.h"
#include "kill_buf.h"

static kb *current_kb = 0;	/* current kill buffer */

FCT (static kb *, AllocKillBuf, () );


/*
**	Function name : AllocKillBuf
**
**	Description : Alloue une structure kill-buf et reset tous ses champs.
**	Input : 
**	Ouput : Un pointeur sur kb.
*/
static kb *AllocKillBuf ()
{
    kb *tmp;
    
    if (( tmp = (kb *) malloc ((unsigned) sizeof (kb))) == 0 )
      return 0;
    
    bzero ( (char *) tmp, sizeof (kb));
    return (tmp);
}

/*
**	Function name : StoreInKillBuf
**
**	Description : Met la chaine s de longeur n et contenant n lignes dans 
**		une structure kb
**
**	Input : La chaine, sa longueur, le nb de lignes.
**	Ouput :
*/
void StoreInKillBuf ( s, len, n )
    char *s;
    int len;
    int n;
{
    kb *new_kb;
    
    if ( (s == 0) || (len == 0) )
      return;
    /*
       * Pas les lignes vides
    */
    if ( (len == 1) && (*s == '\n' ))
      return;
    
    if ( (new_kb = AllocKillBuf ()) == 0 ) 
      (void) fprintf ( stderr, "Hum... AllocKillBuf memory error\n" );
    
    if ( current_kb != 0 )
      new_kb -> next = current_kb;
    current_kb = new_kb;
    current_kb -> p = (char *) malloc ( (unsigned) len + 1 );
    
    /*
       * Copie et mis a jour des infos.
    */       
    /*  (void) strncpy ( current_kb -> p, s, len ); */
    (void) bcopy ( s, current_kb -> p, len );
    
    current_kb -> s_len = len;
    current_kb -> s_lines = n;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "Store next= %d lines = %d\n", 
		    current_kb -> next, current_kb -> s_lines );
    new_kb = current_kb;
    while (1) {
	if  ( new_kb -> next == 0 ) {
	    printf ( "end\n" );
	    break;
	}
	new_kb = new_kb -> next;
	printf ( "new_kb = %d\n", new_kb );
    }
    
#endif
}

/*
**	Function name : RestoreKillBuff
**
**	Description : Restore la chaine contenu dans le nieme buffer.
**
**	Input : Le numero dans la liste, longueur, et nb lignes.
**	Ouput : La chaine.
*/
char *RestoreKillBuf ( i, len, dn )
    int i;
    int *len; /* Return */
    int *dn;  /* Return */
{
    kb *tmp = current_kb;
    
    if ( tmp == 0 )
      return 0;
    while ( tmp -> next != 0 ) {
	if ( i == 0 ) break;
	tmp = tmp -> next;
	i --;
    }
    
    *len = tmp -> s_len;
    *dn =  tmp -> s_lines;
    return ( (char *) tmp -> p );
}


/*
**	Function name : LoadKillBuffer
**
**	Description : Charge les debuts de lignes dans buf
**		Cette fonction sert a l'affichage.
**
**	Input : Le buffer utilise pour afficher le kill buffer.
**	Ouput :
*/
void LoadKillBuffer ( buf )
    Buf *buf;
{
    int i, len;
    char tmp[8];
    char *p;
    
    kb *ckb = current_kb;
    
    if ( ckb == 0 )
      return;
#define MAX_LEN	40
    i = 0;
    while ( True ) {
	bzero ( tmp, 8 );
	(void) sprintf ( tmp, "%d  ", i + 1 );
	InsertNchar ( buf, tmp, strlen(tmp) );
	if ( ckb -> s_lines == 0 ) {
	    if ( (len = ckb -> s_len) > MAX_LEN ) 
	      len = MAX_LEN;
	}
	else {
	    p = ckb -> p;
	    while ( *p != '\n' ) p++;
	    len = p - ckb -> p;
	    if ( len > MAX_LEN )
	      len = MAX_LEN;
	} 
	if ( len != 0 ) 
	  InsertNchar ( buf, ckb -> p, len );
	
	InsertNchar ( buf, " ...\n", 5 ); 
	i++;
	if ( ckb -> next == 0 )
	  break;
	ckb = ckb -> next;
    } 
}


