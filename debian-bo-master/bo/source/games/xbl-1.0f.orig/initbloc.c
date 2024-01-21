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
#include "movingbloc.h"

void initbloc(b)
struct movingbloc *b ;
{
int i ;

i = 0 ;

/* 1 */
b->piece[i++] = createbloc("") ;
/* 2 */
b->piece[i++] = createbloc("l") ;
/* 3 */
b->piece[i++] = createbloc("ll") ;
b->piece[i++] = createbloc("lu") ;
/* 4 */
b->piece[i++] = createbloc("lur") ;
b->piece[i++] = createbloc("rudr") ;
b->piece[i++] = createbloc("rur") ;
b->piece[i++] = createbloc("drr") ;
b->piece[i++] = createbloc("rrr") ;
/* 5 */
b->piece[i++] = createbloc("drru") ;
b->piece[i++] = createbloc("urur") ;
b->piece[i++] = createbloc("ruddur") ;
b->piece[i++] = createbloc("urru") ;
b->piece[i++] = createbloc("urudr") ;
b->piece[i++] = createbloc("uudrr") ;
b->piece[i++] = createbloc("ddrr") ;
b->piece[i++] = createbloc("drdl") ;
b->piece[i++] = createbloc("rrru") ;
b->piece[i++] = createbloc("rurr") ;
b->piece[i++] = createbloc("rudrr") ;
b->piece[i++] = createbloc("rrrr") ;
b->flat = i ;

/* simple volume Pieces (and some flat) */

/* 4 */
b->piece[i++] = createbloc("udrlb") ;
b->piece[i++] = createbloc("fru") ; /* Non symetric */
b->piece[i++] = createbloc("ldb") ;
/* 5 */
b->piece[i++] = createbloc("drlbfd") ;
b->piece[i++] = createbloc("frul") ;
b->simple = i ;

/* Complex Pieces */

/* 5 */
b->piece[i++] = createbloc("fuur") ; /* Non symetric */
b->piece[i++] = createbloc("frru") ;
b->piece[i++] = createbloc("furlu") ; /* Non symetric */
b->piece[i++] = createbloc("frudr") ;
b->piece[i++] = createbloc("furr") ; /* Non symetric */
b->piece[i++] = createbloc("fruu") ; 
b->piece[i++] = createbloc("rfdr") ; /* Non symetric */
b->piece[i++] = createbloc("rfur") ; 
b->piece[i++] = createbloc("furu") ; /* Non symetric */
b->piece[i++] = createbloc("frur") ;
b->piece[i++] = createbloc("rbfur") ; /* Non symetric */
b->piece[i++] = createbloc("rubfr") ;
b->piece[i++] = createbloc("fldb") ;
b->piece[i++] = createbloc("fudrr") ;
b->complex = i ;
b->work = 0 ;
}


