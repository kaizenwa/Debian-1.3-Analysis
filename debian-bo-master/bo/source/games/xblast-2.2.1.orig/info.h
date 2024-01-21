/*
 * Programm XBLAST V2.1.8 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 8th 1996
 * started August 1993
 *
 * File: info.h
 * include file for info.c
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will be entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _INFO_H
#define _INFO_H

#define MAX_INFO 6
#define INFO_LENGTH 256

/*
 * extra probability ratings
 */
#define XR_None      0
#define XR_Scarce    1
#define XR_Rare      2
#define XR_Uncommon  3
#define XR_Common    4
#define XR_Plentiful 5

#ifdef _INFO_C
#define _EXTERN 
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN void reset_info (void);
_EXTERN void set_info_shrink (BMShrinkData *data);
_EXTERN void set_info_func (BMFuncData *data);
_EXTERN void set_info_player (BMPlayerData *data);
_EXTERN void set_info_map (BMMapData *data);
_EXTERN void set_info_bombs (BMBombData *data);
_EXTERN void set_info_graphics (BMGraphicsData *data);
_EXTERN void get_info (char ***extra, char ***level, char ***player);
#else
_EXTERN void reset_info ();
_EXTERN void set_info_shrink ();
_EXTERN void set_info_func ();
_EXTERN void set_info_player ();
_EXTERN void set_info_map ();
_EXTERN void set_info_bombs ();
_EXTERN void set_info_graphics ();
_EXTERN void get_info ();
#endif

#undef _EXTERN

#endif
