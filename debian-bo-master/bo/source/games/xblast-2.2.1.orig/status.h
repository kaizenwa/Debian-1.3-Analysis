/*
 * Programm XBLAST V2.1.9 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 25th 1996
 * started August 1993
 *
 * File: status.h 
 * include file for status.c
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
#ifndef _STATUS_H
#define _STATUS_H

#ifdef _STATUS_C
#define _EXTERN
#else
#define _EXTERN extern
#endif

/*
 * prototypes
 */
#ifdef __STDC__
_EXTERN void init_status_bar (XBConfig *config, BMPlayer *ps,  char *msg, 
			      int flag);
_EXTERN void reset_status_bar (BMPlayer *ps,  char *msg, int flag);
_EXTERN void update_status_bar (BMPlayer *ps, int g_time, int double_flag);
_EXTERN void set_message (char *msg, int perm);
_EXTERN void reset_message (void);
#else
_EXTERN void init_status_bar ();
_EXTERN void reset_status_bar ();
_EXTERN void update_status_bar ();
_EXTERN void set_message ();
_EXTERN void reset_message ();
#endif

#undef _EXTERN 
#endif
/*
 * end of file status.h
 */
