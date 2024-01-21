/* ########################################################################

			     main_buffer.c

   File: main_buffer.c
   Path: /home/fournigault/c/X11/xcoral-2.31/main_buffer.c
   Description: 
   Created: Fri Jan 27 11:16:36 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:16:37 MET 1995
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
#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include "main_buffer.h"

static void MoreBuffer ();

#ifdef DEBUG
FTC (static void, StatBuf (Buf *buf) );
#endif

FCT (static int, HoleSize, (Buf *buf) );
FCT (static void, MoreBuffer, (Buf *buf, int n) );

/*
**	Les fonctions suivantes creent et manipulent
**	un buffer a bulle. La structure de celui-ci
**	est illustree ci-dessous.
**	
**     turlututu chapeau pointu
**            ^
**	      |
**	    curseur
**
**     turlutu_............._tu chapeau pointu
**     ^      ^             ^                ^
**     |      |             |                |
**    top   l_cur          r_cur           bottom
**	
 */

/*
**	Function name : GetBuffer
**
**	Description : Retourne une structure Buf contenant
**		les pointeurs suivants :
**			l_cur, r_cur : position de la bulle.
**			top : le debut du buffer
**			bottom : la fin du buffer
**
**	Input : la taille souhaitee pour le buffer.
**	Ouput : Un pointeur sur Buf.
*/
Buf *GetBuffer ( size )
    unsigned int size;
{
    Buf	*buf;
    
    buf = (Buf *) malloc ((unsigned) sizeof (Buf));
    
    /* 
       * Alloue la place necessaire pour le buffer.
    */
    if ( size <= 0 ) size = SIZEOF_BUFFER;
    buf -> top = ( char * ) malloc ( size );
    if ( buf -> top == 0 ) 
      return ( ( Buf *) 0 );
    /*
       * Positionne la bulle initiale.
    */	
    buf -> l_cur = buf -> top;
    buf -> bottom = buf -> top  + ( size - 1 );
    buf -> r_cur = buf -> bottom;
    
    return ( buf );
}


/*
**	Function name : DeleteBuffer
**
**	Description : Comme son nom l'indique.
**
**	Input : Le buffer.
**	Ouput : 
*/
void DeleteBuffer ( buf )
    Buf *buf;
{
    if ( buf -> top != 0 )
      (void) free ( buf -> top ); 	  /* Le buffer */
    if ( buf != 0 )
      (void) free ( (char *) buf ); /* La structure associee */
}


/*
**	Function name : ClearBuffer
**
**	Description : Pour 'effacer' le contenu du buffer,
**		il suffit de positionner les pointeurs de la
**		bulle pour que celle-ci occupe tout le buffer.
**
**	Input : un buffer
**	Ouput :
*/
void ClearBuffer ( buf )
    Buf *buf;
{
    buf -> l_cur = buf -> top;
    buf -> r_cur = buf -> bottom;
}


/*
**	Function name : MoreBuffer
**
**	Description : Augmente la taille du buffer.
**
**	Input : un buffer, n bytes
**	Ouput :
*/
static void MoreBuffer ( buf, n )
    Buf *buf;
    int n;
{
    int size, r_size, l_size;
    char *b;
    
    /*
       * Sauvegarde du contexte
    */
    l_size = buf -> l_cur - buf -> top;
    r_size = buf -> bottom - buf -> r_cur;
    
    /*
       * Calcul de la nouvelle taille
    */
    size = (( buf -> bottom - buf -> top + 1 + n ) / PSIZE  + 1 ) * PSIZE;
#ifdef DEBUG
    (void) fprintf ( stderr, 
		    "More buffer l_size = %d r_size = %d size = %d\n", 
		    l_size, r_size, size );
#endif
    /*
       * Le nouveau buffer
    */
    if ( (b = ( char * ) malloc ( (unsigned) size )) == 0 )
      (void) fprintf ( stderr, "Malloc error\n" );
    
    /* 
       * On recopie l'ancien contexte
    */
    if ( l_size != 0 )
      bcopy ( buf -> top, b, l_size );
    if ( r_size != 0 )
      bcopy ( buf -> r_cur, b + ( size - 1 ) - r_size, r_size );
    
    /*
       * Liberation de l'ancien buffer
    */
    if ( buf -> top != 0 )
      (void) free ( (char *) buf -> top ); 
    
    /*
       * On positionne les nouveaux pointeurs
    */
    buf -> top = b;
    buf -> l_cur = buf -> top + l_size;
    buf -> bottom = buf -> top + ( size - 1 );
    buf -> r_cur = buf -> bottom - r_size;
#ifdef DEBUG
    StatBuf ( buf );
#endif
}

/*
**	Function name : HoleSize
**
**	Description : Quelle est la place disponible dans
**		le buffer.
**
**	Input : Le buffer
**	Ouput : Retourne en octets l'espace libre.
*/
static int HoleSize ( buf )
    Buf *buf;
{
    return ( buf -> r_cur - buf -> l_cur + 1 );
}


/*
**	Function name : LoadFileInBuffer
**
**	Description : Copie le fichier, reference par fd, dans
**		le buffer
**
**	Input : Le buffer, le fichier, la taille en octects
**		du fichier, un flag
**	Ouput : 0 si Ok ou -1 si problemes
*/
int LoadFileInBuffer ( buf, fd, len, flag )
    Buf *buf;
    FILE *fd;
    int len, flag;
{
    char   *pp;
    
    /*
       * L'espace libre est-il suffisant ?
    */
    if ( HoleSize ( buf ) < len ) {
	MoreBuffer ( buf, len );
    }
    
    /*
       * Le fichier est-il a inserer dans le buffer
       * ou pas ?
    */
    if ( flag == NEW ) {
	pp = buf -> bottom - len + 1;
	buf -> l_cur = buf -> top;
	buf -> r_cur = buf -> bottom - len;
    }
    else { /* Insert */
	pp = buf -> r_cur - len + 1;
	buf -> r_cur -= len;
    }
    
    /* 
       * Copie
    */
    while ( len >= PSIZE ) {
	if ( fread( pp, PSIZE, 1, fd) != 1 ) {
	    perror("Read error\n");
	    return ( -1 );
	}
	len -= PSIZE;
	pp += PSIZE;
    }
    if (len) {
	if ( fread ( pp, len, 1, fd ) != 1 ) {
	    perror ("Read error\n");
	    return ( -1 );
	}
    }
#ifdef DEBUG
    StatBuf ( buf );
#endif
    return 0;
}


/*
**	Function name : WriteCurrentFile
**
**	Description : Copie du buffer dans le fichier reference
**		par df.
**
**	Input : Le buffer, le fichier
**	Ouput : La taille en octets du fichier.
*/
int WriteCurrentFile ( buf, fd )
    Buf *buf;
    FILE *fd;
{
    /* 
       * La taille des parties droite et gauche
       * (de part et d'autre du curseur).
    */
    int l_size = buf -> l_cur - buf -> top;
    int r_size = buf -> bottom - buf -> r_cur;
    
    /*
       * Ecriture partie gauche
    */
    if ( l_size ) {
#ifdef DEBUG
	(void) fprintf ( stderr, "l_size = %d\n", l_size );
#endif
	if ( fwrite ( buf -> top, l_size, 1, fd ) != 1 ) {
	    perror ( "Write error\n" );
	    return (-1);
	}
    }
    /*
       * Ecriture partie droite
    */
    if ( r_size ) {
#ifdef DEBUG
	(void) fprintf ( stderr, "r_size = %d\n", r_size );
#endif
	if ( fwrite ( buf -> r_cur + 1, r_size, 1, fd ) != 1 ) {
	    perror ( "Write error\n" );
	    return (-1);
	}
    }
    return ( l_size + r_size );
}


/*
**	Function name : MoveToLine
**
**	Description : Deplacement a la ligne n.
**		L'argument n est relatif par rappor a la
**		ligne courante (e.g -1 ligne precedante).
**
**	Input : Le buffer, ligne
**	Ouput : 0 si OK ou -1 si problemes.
*/
int MoveToLine ( buf, n )
    Buf *buf;
    int n;
{
    int len = 0;
    
    if (n == 0)
      return 0;
    
    /*
       * Just pour avoir la longueur du deplacement
    */
    if ( n > 0 ) 
      (void)  GetForwardLine ( buf, n, &len );
    else
      (void)  GetBackwardLine ( buf, n, &len );
    
    if ( ! len )
      return -1;
    
    if ( MoveHole ( buf, len ) == -1 ) {
	(void) fprintf ( stderr, "Internal Error 6\n" );
	return -1;
    }
    else
      return 0;
}

/*
**	Function name : MoveHole
**
**	Description : Deplacement de la bulle de n octets
**
**	Input : Le buffer, n octets.
**	Ouput : 0 si OK ou -1 si problemes.
*/
int MoveHole ( buf, n )
    Buf *buf;
    int n;
{
    if ( n == 0 ) {
#ifdef DEBUG
	(void) fprintf ( stderr, "n = 0\n" );
#endif
	return 0;
    }
    
    /*
       * Deplacement a droite
    */
    if ( n > 0 ) {
	/*
	   * Va-t on aux fraises ?
	*/
	if ( buf -> r_cur + n > buf -> bottom )  
	  return (-1);
	/*
	   * 1 octet seulement.
	*/
	if ( n == 1 ) {
	    buf -> r_cur ++;
	    *buf -> l_cur = *buf -> r_cur;
	    buf -> l_cur ++;
#ifdef DEBUG
	    StatBuf ( buf );
#endif
	    return 0;
	}
	
	/*
	 * Plusieurs caracteres
	 */
	bcopy ( buf -> r_cur + 1, buf -> l_cur, n );
	buf -> r_cur += n;
	buf -> l_cur += n;
#ifdef DEBUG
	StatBuf ( buf );
#endif
    }
    
    /*
     * Deplacement a gauche.
     */
    else {
	if ( buf -> l_cur - ( - n ) < buf -> top )
	  return ( -1 );
#ifdef DEBUG
	(void) fprintf ( stderr, "Left Move n = %d\n", n );
#endif
	if ( n == -1 ) {
	    buf -> l_cur --;
	    *buf -> r_cur = *buf -> l_cur;
	    buf -> r_cur --;
#ifdef DEBUG
	    StatBuf ( buf );
#endif
	    return 0;
	}
	buf -> l_cur -= ( -n );
	buf -> r_cur -= ( -n );
	bcopy ( buf -> l_cur, buf -> r_cur + 1 , -n );
	
#ifdef DEBUG
	StatBuf ( buf );
#endif
    }
    return 0;
}


/*
**	Function name : StatBuf
**
**	Description : Pour le mode DEBUG. Imprime les infos.
**
**	Input : Le buffer
**	Ouput :
*/
#ifdef DEBUG
static void StatBuf ( buf )
	Buf *buf;
{
  (void) fprintf ( stderr, 
		  "Hole : buf = %d top = %d bottom = %d l_cur = %d r_cur = %d\n", 
		  buf, buf -> top, buf -> bottom, buf -> l_cur, buf -> r_cur );
}
#endif


/*
**	Function name : HoleToRight
**
**	Description : Deplacement de la bulle completement
**		a droite. Cela revient a deplacer le curseur
**		en fin de buffer.
**
**	Input : Le buffer
**	Ouput :
*/
void HoleToRight ( buf )
    Buf *buf;
{
    
#ifdef DEBUG
    (void) fprintf ( stderr, "HoleToRight\n" );
#endif
    
    if ( MoveHole ( buf, buf -> bottom - buf -> r_cur ) == -1 )
      (void) fprintf ( stderr,"Internal Error 7...\n" );
}


/*
**	Function name : HoleToLeft
**
**	Description : Deplacement de la bulle completement
**		a gauche. Cela revient a deplacer le curseur
**		au debut du buffer.
**
**	Input : Le buffer
**	Ouput :
*/
void HoleToLeft ( buf )
    Buf *buf;
{
    
#ifdef DEBUG
    (void) fprintf ( stderr, "HoleToleft\n" );
#endif
    
    if ( MoveHole ( buf, buf -> top - buf -> l_cur ) == -1 )
      (void) fprintf ( stderr, "Internal Error 8...\n" );	
}


/*
**	Function name : InsertNchar
**
**	Description : Insere n caracteres a partir de la 
**		position courante.
**
**	Input : Le buffer, chaine a inserer, sa taille.
**	Ouput : 
*/
void InsertNchar ( buf, s, n )
    Buf *buf;
    char *s;
    int n;
{
    /*
       * Reste-il de la place ?
    */
    if ( HoleSize ( buf ) < n ) {
	(void) MoreBuffer ( buf, n );
    }
    
    if ( n == 1 ) {
	*buf -> l_cur = *s;
	buf -> l_cur ++;
	return;
    }
    
    bcopy ( s, buf -> l_cur, n );
    buf -> l_cur += n;
    return;
}


/*
**	Function name : DeleteNchar
**
**	Description : Efface n caracteres a partir de la 
**		position courante.
**
**	Input : Le buffer, nb de caracteres a effacer.
**	Ouput :
*/
void DeleteNchar ( buf, n )
    Buf *buf;
    int n;
{
    
#ifdef DEBUG
    (void) fprintf ( stderr, "Delete n = %d\n", n );
#endif
    
    if ( buf -> l_cur <= buf -> top )
      return;
    
    if ( buf -> l_cur == buf -> r_cur )
      (void) fprintf ( stderr, "End of buf\n" );
    buf -> l_cur -= n;
}


/*
**	Function name : GetForwardLine
**
**	Description : Recherche a partir de la position 
**		courante, le debut de la n-ieme ligne
**		  
**	Input : Le buffer, no ligne, deplacement ( il
**		s'agit du nombres de caracteres entre
**		la position courante et la position
**		recherchee );
**	Ouput : pointeur sur n-ieme ligne ou 0 si problemes.
*/
char *GetForwardLine ( buf, n, len )
    Buf *buf;
    int n;
    int *len;	/* Return */
    
{
    char *p;
    int i = 0;
    
    p = buf -> r_cur + 1 ;
    
    while ( p <= buf -> bottom ) { 
	if ( *p == '\n' )
	  n--;
	if ( n == 0 ) {
	    if ( p == buf -> bottom ) {
		*len = ++i;
		return 0;
	    }
	    else {
		*len = ++i;
		return ++p;
	    }
	}
	p++;
	i++;
    }
    return 0;
}


/*
**	Function name : GetBackwardLine
**
**	Description : Recherche a partir de la position 
**		courante, le debut de la n-ieme ligne
**		  
**	Input : Le buffer, no ligne, deplacement ( il
**		s'agit du nombres de caracteres entre
**		la position courante et la position
**		recherchee );
**	Ouput : pointeur sur n-ieme ligne ou 0.
*/
char *GetBackwardLine ( buf, n, len )
    Buf *buf;
    int n;
    int *len; /* Return */
{
    char *p;
    int i = 0;
    
    p = buf -> l_cur - 1;
    n --;
    while ( p > buf -> top ) {
	if ( *p == '\n' ) n++;
	if ( n == 0 ) {
	    *len = - ( i );
	    return ++p;
	}
	p--;
	i++;
    }
    /* Debut du buffer */
    
    if ( (n <= -2) && (*buf -> top != '\n') ) {
	*len = 0;
	return 0;
    }
    if ( (n < -2) && (*buf -> top == '\n') ) {
	*len = 0;
	return 0;
    }
    /* Cas special si 1 ere ligne vide */
    if ( (*buf -> top == '\n') && (n == -1) ) {
	*len = -i;
	return ++p;
    }
    else {
	*len = - (i+1);
	return p;
    }
}


/*
**	Function name : GetCurrentLine
**
**	Description : Assemblage de la ligne courante.
**		Cette fonction est le coeur de l'application.
**		
**	Input : Le buffer, la taille (return)
**	Ouput : Le pointeur sur la ligne courante.
*/
char *GetCurrentLine ( buf, len )
    Buf *buf;
    int *len; /* Return */
{
#define MAX_SLEN	64
    static int mbuf_len = 0;
    static char sbuf [MAX_SLEN], *mbuf, *mbuf_save = 0;
    int i, j;
    char *p, *pp, *p_tmp;
    
    *len = 0;
    /*
     *  Partie gauche : positionnement du pointeur et calcul
     *	du deplacement par rapport a la position
     *	courante.
     */
    p = buf -> l_cur - 1;
    j = 0;
    while ( p > buf -> top ) {
	if ( *p == '\n' )
	  break;
	p--;
	j++;
    }
    if ( p != buf -> top )
      p++;
    else {	
	/* Debut du buffer */
	if ( *p != '\n' ) 
	  j++;
	else
	  /* 1er ligne : vide */
	  p++;
    }
    *len = j;
    
    /* On garde p pour plus tard */
    pp = p;
#ifdef DEBUG
    (void) fprintf ( stderr, 
		    "len = %d Left side s = %s\n", *len, sbuf );
#endif
    /*
     * Partie droite : positionnement du pointeur et calcul
     *	du deplacement par rapport a la position
     *	courante.
     */
    p = buf -> r_cur + 1;
    i = 0;
    while ( p < buf -> bottom ) {
	if ( *p == '\n' )
	  break;
	p++;
	i++;
    }
    /* On teste si la longueur de la ligne est > a MAX_SLEN. */
    if ( ( i + j + 8 ) > MAX_SLEN ) {
	/* Il faut de la place */
	if ( mbuf_save ) {
	    /*
	     * Si le buffer deja alloue est assez grand on prend
	     * celui-ci, sinon on en alloue un autre.
	     */
	    if (( i + j + 8 ) > mbuf_len) {
		if ( mbuf_save != 0 )
		  (void) free ( mbuf_save );
		mbuf_len = ( 2 * ( i + j + 8 ));
		mbuf = ( char *) malloc ( (unsigned int) mbuf_len );
		mbuf_save = mbuf;
	    }
	    else
	      mbuf = mbuf_save;
	}
	else {
	    mbuf_len = i + j + 8;
	    mbuf = ( char *) malloc ( (unsigned int) mbuf_len );
	    mbuf_save = mbuf;
	}
	bzero ( mbuf, mbuf_len  );
    }
    else {
	/* On se sert du buffer static */
	bzero ( sbuf, MAX_SLEN );
	mbuf = sbuf;
    }
    /* On copie la partie gauche */
    (void) bcopy ( pp, mbuf, j );
    p_tmp = mbuf + j;
    j = 0;
    
    /*
     * Fin du buffer et pas de retour chariot
     */
    if ( (p == buf -> bottom) && (*p != '\n') ) {
	*len += 1;
	/* On ajoute la partie droite */
	(void) bcopy ( p - i, p_tmp, i + 1 );
	j = 1;
    }
    else
      /* On ajoute la partie droite */
      (void) bcopy ( p - i, p_tmp, i );
    
    *len += i;
    *(mbuf + *len) = '\n';
    
#ifdef DEBUG
    (void) fprintf ( stderr,
		    "i = %d len = %d Current line : %s\n", i, *len, mbuf );
    (void) fprintf ( stderr, "Deplace = %d\n", i+1 );
#endif
    return mbuf;
}


/*
**	Function name : GetNcFromLeft
**
**	Description : Recherche le debut de la ligne
**		courante.
**
**	Input : Le buffer
**	Ouput : Le nombre de caracteres pour aller en debut
**		de ligne.
*/
int GetNcFromLeft ( buf )
    Buf *buf;
{
    int n = 0;
    char *p = buf -> l_cur - 1;
    
    while ( p > buf -> top ) {
	if ( *p == '\n' )
	  break;
	p--;
	n++;
    }
    
    /*
       * Est-ce la 1 ere ligne et traitement
       * du cas ou celle-ci est vide
    */
    if ( (p == buf -> top) && (*p != '\n') )
      n++;
    return n;
}


/*
**	Function name : GetNcFromRight
**
**	Description : Recherche de la fin de ligne.
**
**	Input : Le buffer
**	Ouput : Le nombre de caracteres pour aller en fin
**		de ligne (le '\n' n'est pas compte).
*/
int GetNcFromRight ( buf )
    Buf *buf;
{
    int n = 0;
    char *p = buf -> r_cur + 1;
    
    while ( p < buf -> bottom ) {
	if ( *p == '\n' ) {
	    break;
	}
	p++;
	n++;
    }
    if ( (p == buf -> bottom) && (*p != '\n') ) {
	n++;
    }
    return n;
}


/*
**	Function name : GetNumberOfLineInBuf
**
**	Description : Combien de lignes dans le buffer.
**
**	Input : Le buffer
**	Ouput : Le nombre de lignes.
*/
int GetNumberOfLineInBuf ( buf )
    Buf *buf;
{
    int n = 1;
    char *p = buf -> top;
    
    /*
       * Partie gauche
    */
    while ( p < buf -> l_cur ) {
	if ( *p == '\n' )
	  n++;
	p++;
    }
    
    /*
       * Partie droite
    */
    p = buf -> r_cur + 1;
    while ( p < buf -> bottom ) {
	if ( *p == '\n' )
	  n++;
	p++;
    }
    
    /*
       * Traitement du '\n' en fin de buffer
    */
    if ( (buf -> bottom != buf -> r_cur)
	&& (*(buf -> bottom) == '\n' ))
      n++;
    
#ifdef DEBUG
    (void) fprintf ( stderr, " nb lines = %d\n", n );
#endif
    
    return n;
}


/*
**	Function name : GetNewLine
**
**	Description : Recherche dans un chaine de longueur
**		donnee, le nombre de ligne.
**
**	Input : La chaine, la longueur de celle-ci.
**	Ouput : Le nombre de 'new_line'
*/
int GetNewLine ( t, len )
    char *t; /* le texte */
    int len; /* la longeur du texte */
{
    char *start = t;
    int nl = 0;
    
    if ( len == 0 ) return nl;
    
    if ( len > 0 ) {
	while ( t < (start + len) ) {
	    if ( *t == '\n' ) nl++;
	    t++;
	}
	return nl;
    }
    else {
	while ( t > (start + len) ) {
	    if ( *t == '\n' ) nl++;
	    t--;
	}
	return nl;
    }
}


/*
**	Function name : GetCurrentChar
**
**	Description : Donne le caractere courant.
**
**	Input : Le buffer
**	Ouput :
*/
int GetCurrentChar ( buf, c )
    Buf *buf;
    char *c; /* Return */
{
    if ( (buf -> r_cur) == buf -> bottom ) {
	*c = *(buf -> r_cur);
	return -1;
    }
    else
      *c = *(buf -> r_cur + 1);
    return 0;
}


/*
**	Function name : GetPrevChar
**
**	Description : Donne le caractere precedent.
**
**	Input : Le buffer
**	Ouput :
*/
void GetPrevChar ( buf, c )
    Buf *buf;
    char *c;
{
    if ( buf -> l_cur <= buf -> top )
      *c = '\0';
    else
      *c = *(buf -> l_cur -1);
}


/*
**	Function name : DeleteLines
**
**	Description : Efface n lines a partir de la position courante.
**		Positionne le nombre de caracteres et de lignes
**		effectivement effacees. Comme toujours les resultats ne
**		sont pas les memes suivant que le buffer se termine
**		ou non par un new-line.
**	Input : Le buffer
**	Ouput : Position courante ( curseur droit ) apres effacement
**		des lignes
*/
char *DeleteLines ( buf, n, len, dn )
    Buf *buf;
    int n;      /* nb lignes demandees */
    int *len;   /* caracteres deletes (return) */
    int *dn;    /* nb lignes deletees (return) */
{
    int i = 0;
    int end_line = 0;
    char *p = buf -> r_cur + 1;
    
    if ( n == 0 ) return 0;
    if ( n == 1 ) end_line = 1;
    
    *dn = 0;
    
    if ( n > 1 ) 
      i++;
    
    while ( p < buf -> bottom ) {
	if ( *p == '\n' ) {
	    n --;
	    (*dn) ++;
	}
	if ( n == 0 ) {
	    if ( end_line == 1 ) {  /* Delete end of line */
		*dn = 0;
		if ( (*p == '\n') && (i == 0)) { /* Ligne vide */
		    *dn = i = 1;;
		}
	    }
	    break;
	}
	i ++;
	p++;
    }
    if ( p == buf -> bottom ) {  /* Fin du buffer */
	if ( end_line == 0 )     /* Plusieurs lignes */
	  (*dn) ++; 
	if ( (end_line == 1) && ( *p != '\n' ) )
	  i++;
    }
    p = buf -> r_cur + 1;
    buf -> r_cur += i;
    *len = i;
    return p;
}
