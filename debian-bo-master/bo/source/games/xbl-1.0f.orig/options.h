/*
    XBlockOut a 3D Tetris

    Copyright (C) 1992,1993,1994  Thierry EXCOFFIER

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Contact: Thierry.EXCOFFIER@ligia.univ-lyon1.fr
*/
struct options	{
		char *nom ;	/* Nom de l'option	*/
		char type ;	/* Type de l'option	*/
		void *valeur ;	/* Endroit ou stocker   */
		char *help ;  	/*  Chaine d'aide       */
		} ;

extern void prendoptions(R3(struct options *o,int *argc,char **argv)) ;
extern void proptions(R1(struct options *o)) ;
extern void stringoption(R2(struct options *o,char *t)) ;
extern int comparok(R3(char *v,int l,char *h)) ;

