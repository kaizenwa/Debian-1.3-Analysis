/*
   grid.cc

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

#include <gtools/grid.h>
#include <gtools/gr.h>
#include <math.h>

BitmapGrid::BitmapGrid( Rect &rect, Bitmap *bmp )
{
  x1 = rect.a.x;
  y1 = rect.a.y;
  x2 = rect.b.x;
  y2 = rect.b.y;
  
  map = bmp;
  
  clr = WHITE;
  
  cx = (int)floor( (x2-x1+1) / map->w );
  cy = (int)floor((y2-y1+1) / map->h);

  x2 = x1 + map->w * cx;
  y2 = y1 + map->h * cy;
  
  draw_grid();
  fill_grid();
}

BitmapGrid::~BitmapGrid()
{}

void BitmapGrid::shift_up()
{
  int i, j;
  unsigned char c;
  unsigned char *m;

  /* For each col... */
  for( i=0; i<map->w; ++i )
	{
	  /* For each row... */
	  m = map->map + i;
	  c = *m;
	  
	  for( j=0; j<map->h-1; ++j )
		{
		  *m = *(m+map->w);
		  m += map->w;
		}
	  *m = c;
	}

  draw_grid();
  fill_grid();
}

void BitmapGrid::flip_ud()
{
  int i, j, z;
  unsigned char *m;
  unsigned char c;

  /* For each col... */
  for( i=0; i<map->w; ++i )
	{
	  /* For each row... */
	  m = map->map + i;
	  z = map->h - 1;

	  for ( j=0; j<(map->h/2); ++j, --z )
		{
		  c = m[z*map->w];
		  m[z*map->w] = m[j*map->w];
		  m[j*map->w] = c;
		}
	}

  draw_grid();
  fill_grid();
}

void BitmapGrid::flip_lr()
{
  int i, j, z;
  unsigned char *m;
  unsigned char c;
  
  /* For each row... */
  for( i=0; i<map->h; ++i )
	{
	  /* For each col... */
	  m = map->map + i*map->w;
	  z = map->w - 1;

	  for( j=0; j<(map->w/2); ++j, --z )
		{
		  c = m[z];
		  m[z] = m[j];
		  m[j] = c;
		}
	}

  draw_grid();
  fill_grid();
}

void BitmapGrid::shift_down()
{
  int i, j;
  unsigned char c;
  unsigned char *m;

  /* For each col... */
  for( i=0; i<map->w; ++i )
	{
	  /* For each row... */
	  m = map->map + (map->h-1)*map->w + i;
	  c = *m;
	  
	  for( j=map->h-1; j>0; --j )
		{
		  *m = *(m-map->w);
		  m -= map->w;
		}
	  *m = c;
	}

  draw_grid();
  fill_grid();
}

void BitmapGrid::shift_left()
{
  int i, j;
  unsigned char c;
  unsigned char *m;
  
  /* For each row... */
  for( i=0; i<map->h; ++i )
	{
	  m = map->map + i*map->w;

	  /* For each column */
	  c = *m;
	  for( j=0; j<map->w-1; ++j )
		m[j] = m[j+1];
	  m[map->w-1] = c;
	}

  draw_grid();
  fill_grid();
}

void BitmapGrid::shift_right()
{
  int i, j;
  unsigned char c;
  unsigned char *m;
  
  /* For each row... */
  for( i=0; i<map->h; ++i )
	{
	  m = map->map + i*map->w;

	  /* For each column */
	  c = m[map->w-1];
	  for( j=map->w-1; j>0; --j )
		m[j] = m[j-1];
	  m[0] = c;
	}

  draw_grid();
  fill_grid();
}

void BitmapGrid::draw_grid()
{
  int i;

  /* Draw horizontal lines */
  for( i=0; i<=map->h; ++i )
	gr_hline( x1, y1 + cy*i, x2, WHITE );

  /* Draw vertical lines */
  for( i=0; i<=map->w; ++i )
	gr_vline( x1 + cx*i, y1, y2, WHITE );
}

void BitmapGrid::fill_grid()
{
  int i, j;
  unsigned char *m;
  int y1, y2, x1, x2;
  
  m = map->map;
  
  for( i=0; i<map->h; ++i )
	{
	  y1 = i*cy + 1;
	  y2 = y1 + cy - 2;
	  
	  for( j=0; j<map->w; ++j )
		{
		  x1 = j*cx + 1;
		  x2 = x1 + cx - 2;
		  
		  gr_fillbox( x1,y1,x2,y2,*m );
		  ++m;
		}
	}
}

int BitmapGrid::handle_mouse( int x, int y )
{
  int cell_x, cell_y;
  unsigned char *m;
  
  if ( x >= x1 && x <= x2 && y >= y1 && y <= y2 )
	{
	  cell_x = (x-x1) / cx;
	  cell_y = (y-y1) / cy;

	  m = map->map + cell_y * map->w + cell_x;
	  *m = clr;

	  gr_fillbox( cell_x * cx + 1, cell_y * cy + 1,
				 cell_x * cx + cx - 1, cell_y * cy + cy - 1,
				 clr );
	}

  return 0;
}

int BitmapGrid::set_draw_color( int new_clr )
{
  clr = new_clr;
  return 0;
}
