/*
 * Programm XBLAST V2.1.10 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * November 5th 1996
 * started August 1993
 *
 * File: pipe.h
 * include file for pipe.c
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

#ifndef _PIPE_H
#define _PIPE_H

/* global variables */
#ifndef _PIPE_C
extern int num_children;
#endif


/*
 * constants
 */
#define CM_None   0
#define CM_Parent 1
#define CM_Child  2

/* prototypes */

#ifdef _PIPE_C
#define _EXTERN
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN int random_number (int max_val);
_EXTERN void seed_random (unsigned seed);
_EXTERN int create_child (void);
_EXTERN void no_keys_to_server (void);
_EXTERN void no_keys_to_clients (void);
_EXTERN void child_link_keys (PlayerAction *pa, int p1, int p2);
_EXTERN void parent_link_keys (int child, PlayerAction *pa, int p1, int p2);
_EXTERN void send_keys_to_parent (void);
_EXTERN void get_keys_from_parent (void);
_EXTERN void get_keys_from_children (void);
_EXTERN void send_keys_to_children (void);
_EXTERN void string_to_children (char *buf) ;
_EXTERN int string_from_children (char *buf);
_EXTERN void string_to_parent (char *buf);
_EXTERN void string_from_parent (char *buf);
_EXTERN void buffer_to_children (int nbytes, caddr_t buf);
_EXTERN void buffer_from_parent (int nbytes, caddr_t buf);
#else
_EXTERN int random_number ();
_EXTERN void seed_random ();
_EXTERN int create_child ();
_EXTERN void no_keys_to_server ();
_EXTERN void no_keys_to_clients ();
_EXTERN void child_link_keys ();
_EXTERN void parent_link_keys ();
_EXTERN void send_keys_to_parent ();
_EXTERN void get_keys_from_parent ();
_EXTERN void get_keys_from_children ();
_EXTERN void send_keys_to_children ();
_EXTERN void string_to_children ();
_EXTERN int string_from_children ();
_EXTERN void string_to_parent ();
_EXTERN void string_from_parent ();
_EXTERN void buffer_to_children ();
_EXTERN void buffer_from_parent ();
#endif

#undef _EXTERN

#endif
/*
 * end of file pipe.h
 */
