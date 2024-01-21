/* tron.c - xtron v1.1 player routines
 *
 *   Copyright (C) 1995 Rhett D. Jacobs <rhett@hotel.canberra.edu.au>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Last Modified: 16/4/95
 */

#include "xtron.h"

void plr_setup(void)
{
  int i;

  /* set starting directions and player types, plus scores */
  p[0].plr_dir = left;
  p[1].plr_dir = right;
  
  for (i=0; i < 2; i++) {
    p[i].plr_type = computer;
    p[i].alive = 1;
    p[i].score = 0;
    p[i].co_ords[1] = MAXVERT/2;
  }
  p[0].co_ords[0] = (MAXHORZ/2)-3;
  p[1].co_ords[0] = (MAXHORZ/2)+3;
}


int plr_checkmove(int p_num, int new_val, int axis_type, enum directions dir)
{
  enum directions temp = left;

  switch (p[p_num].plr_dir) {
  case left:
    temp = right; break;
  case right:
    temp = left; break;
  case up:
    temp = down; break;
  case down:
    temp = up; break;
  }
  
  /* if move is in the opposite direction - invalid */
  if (dir == temp)
    return(0);
  return(1);
}


void plr_turn(int p_num, enum directions dir)
{
  switch(dir) {
  case left:
    if (plr_checkmove(p_num, (p[p_num].co_ords[0])-1, 0, dir))
      p[p_num].plr_dir = left;
    break;
  case right:
    if (plr_checkmove(p_num, (p[p_num].co_ords[0])+1, 0, dir))
      p[p_num].plr_dir = right;
    break;
  case up:
    if (plr_checkmove(p_num, (p[p_num].co_ords[1])-1, 1, dir))
      p[p_num].plr_dir = up;
    break;
  case down:
    if (plr_checkmove(p_num, (p[p_num].co_ords[1])+1, 1, dir))
      p[p_num].plr_dir = down;
    break;
   }
}


void brd_setup(void)
{
  int i,j;

  /* clear board */
  for(i=0; i< DIMS; i++)
    for(j=0;j< DIMS;j++)
      b.contents[i][j] = 0;
  
  /* inital player pieces */
  brd_newcontents((MAXHORZ/2)-3, MAXVERT/2, 1);
  brd_newcontents((MAXHORZ/2)+3, MAXVERT/2, 2);
}


int brd_newcontents(int x, int y, int what)
{
  /* 0 - Empty, 1 - Player 1, 2 - Player 2 */ 
  if (x > DIMS || x < 0)
    return(0);
  if (y > DIMS || y < 0)
    return(0);
  if (b.contents[x][y] != 0)
    return(0);
  else {
    b.contents[x][y] = what;
    return(1);
  }
}
