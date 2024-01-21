/*
   gr.h

   This file is part of libgtools.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
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
*/

#ifndef _gr_h_
#define _gr_h_

#include <vga.h>
#include <string.h>

#include <gtools/screen.h>
#include <gtools/font.h>

/*
 * If buffering is not used, graph_buffer == graph_mem.
 * If buffering is used, graph_buffer points into
 * seperate memory and graph_mem must be updated
 * manually.
 */
#ifndef _gr_cc_		
extern unsigned char *graph_buffer;		/* Defined in gr.cc */
extern int gr_no_updates;				/* Defined in gr.cc */
#endif

/* Sets up 320x200x256 graphics, with or without buffering */
int gr_init( int buffered );
int gr_close();

/* Update graph_mem from graph_buffer */
//#define gr_update() if ( graph_mem != graph_buffer ) \
//						memcpy( graph_mem, graph_buffer, 320*200 )
void gr_update();
void gr_hline( int x1, int y, int x2, int c );
void gr_vline( int x, int y1, int y2, int c );

void gr_box( int x1, int y1, int x2, int y2, int c );
void gr_fillbox( int x1, int y1, int x2, int y2, int c );
void gr_frame( int x1, int y1, int x2, int y2, int w, int h,
			  int normal, int bright, int dark, int inside );
			  
void gr_textxy( char *str, int x, int y, Font *font );

/* Centered */
void gr_textxy_c( char *str, int x, int y, Font *font );

/* Underlined */
void gr_textxy_u( char *str, int x, int y, Font *font );

/* Centered (about x,y) and underlined */
void gr_textxy_cu( char *str, int x, int y, Font *font );

/* Returns width of char on screen */
int gr_putc( char c, int x, int y, Font *font );

int gr_textw( char *str, Font *font );
int gr_texth( char *str, Font *font );

#endif

