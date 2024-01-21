/*
 * Programm XBLAST V2.1.3 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * July 11th 1996
 * started August 1993
 *
 * File: expl.c
 * bitmap data of bombs and explosions
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _EXPL_H
#define _EXPL_H

/*
 * constants
 */
#define MAX_EXPLOSION 16
#define MAX_BOMBS 2

#define BB_NORMAL 0
#define BB_MINI   1

#ifndef _EXPL_C
extern BitmapStruct expl_mask[MAX_EXPLOSION];
extern BitmapStruct expl_bits[MAX_EXPLOSION];
extern BitmapStruct expl_addon[MAX_EXPLOSION];
extern BitmapStruct bomb_mask[MAX_BOMBS];
extern BitmapStruct bomb_bits[MAX_BOMBS];
extern BitmapStruct bomb_addon[MAX_BOMBS];
#endif

#endif
/*
 * end of expl.h
 */

