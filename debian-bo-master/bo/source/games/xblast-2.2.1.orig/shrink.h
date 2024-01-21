/*
 * Programm XBLAST V2.1.9 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * Spetember 25th 1996
 * started August 1993
 *
 * File: shrink.h 
 * include file for shrink.c
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as published
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

#ifndef _SHRINK_H
#define _SHRINK_H

/*
 * prototypes
 */
#ifdef _SHRINK_C
#define _EXTERN 
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN void setup_shrink (BMShrinkData *shrink);
_EXTERN void shrink_void (void);
_EXTERN void shrink_spiral (void);
_EXTERN void shrink_speed_spiral (void);
_EXTERN void shrink_spiral_plus (void);
_EXTERN void shrink_spiral_3 (void);
_EXTERN void shrink_early_spiral (void);
_EXTERN void shrink_compound (void);
_EXTERN void shrink_compound_f (void);
_EXTERN void shrink_compound_2_f (void);
_EXTERN void shrink_lazy_compound_f (void);
_EXTERN void shrink_compound_solid (void);
_EXTERN void shrink_savage_compound (void);
_EXTERN void shrink_compound_extra (void);
_EXTERN void shrink_down (void);
_EXTERN void shrink_down_f (void);
_EXTERN void shrink_quad (void);
_EXTERN void shrink_constrict_wave (void);
_EXTERN void do_shrink (int g_time);
_EXTERN void do_scramble2 (int g_time);
#else
_EXTERN void setup_shrink ();
_EXTERN void shrink_void ();
_EXTERN void shrink_spiral ();
_EXTERN void shrink_speed_spiral ();
_EXTERN void shrink_spiral_plus ();
_EXTERN void shrink_spiral_3 ();
_EXTERN void shrink_early_spiral ();
_EXTERN void shrink_compound ();
_EXTERN void shrink_compound_f ();
_EXTERN void shrink_compound_2_f ();
_EXTERN void shrink_lazy_compound_f ();
_EXTERN void shrink_compound_solid ();
_EXTERN void shrink_savage_compound ();
_EXTERN void shrink_compound_extra ();
_EXTERN void shrink_down ();
_EXTERN void shrink_down_f ();
_EXTERN void shrink_quad ();
_EXTERN void shrink_constrict_wave ();
_EXTERN void do_shrink ();
_EXTERN void do_scramble2 ();
#endif

#undef _EXTERN 

#endif
/*
 * end of file shrink.h
 */








