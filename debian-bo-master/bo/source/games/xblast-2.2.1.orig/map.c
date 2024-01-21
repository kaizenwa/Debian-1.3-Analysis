/*
 * Program XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: map.c
 * level map management
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define _MAP_C
#include "include.h"
#include "mytypes.h"
#include "const.h"
#include "graphics.h"
#include "maze.h"
#include "data.h"
#include "expl.h"
#include "sprite.h"
#include "map.h"
#include "bomb.h"
#include "info.h"
#include "pipe.h"

/*
 * local variables
 */ 
typedef short col[MAZE_H];

static col em1[MAZE_W];
static col em2[MAZE_W];
static col *expl_maze = em1;
static col *old_expl_maze = em2;
static unsigned expl_dirty;
static unsigned expl_olddirty;


static unsigned redraw_maze[MAZE_H];
static unsigned redraw_stat[STAT_H];

static short maze[MAZE_W][MAZE_H];
static short extra[MAZE_W][MAZE_H];

static short dist_extra[MAZE_W][MAZE_H];
static short dist_x[MAZE_W*MAZE_H], dist_y[MAZE_W*MAZE_H];
static int distrib_extras;

static BMExtraProb extra_prob;
static Sprite sprite_list[255];
static Sprite *sprite_max = sprite_list;

/*
 * shadow functions
 */
#ifdef __STDC__
static void set_shadow_none (int x, int y);
static void set_shadow_block (int x, int y);
static void set_shadow_extra (int x, int y);
static void set_shadow_both (int x, int y);
#else
static void set_shadow_none ();
static void set_shadow_block ();
static void set_shadow_extra ();
static void set_shadow_both ();
#endif

static PFV set_shadow_func[MAX_SHADOW] = {
 set_shadow_none,
 set_shadow_block,
 set_shadow_extra,
 set_shadow_both,
};

static PFV set_shadow;



/*
 * public function: setup_graphics
 */
#ifdef __STDC__
void
setup_graphics (int num_disp, BMGraphicsData *cur_level)
#else
void
setup_graphics (num_disp, cur_level)
     int num_disp;
     BMGraphicsData *cur_level;
#endif
{
  int i, disp;
  
  set_info_graphics(cur_level);

  for (disp=0; disp<num_disp; disp++) {
    for (i=0; i< MAX_BLOCK; i++) {
      init_block(disp, cur_level->block[i].id, i, 
		 cur_level->block[i].fg, 
		 cur_level->block[i].bg,
		 cur_level->block[i].add);
    }
    init_explosion_blocks(disp);
  }
}



/*
 * public function setup_map
 */
#ifdef __STDC__
void
setup_map (BMMapData *cur_level)
#else
void
setup_map (cur_level)
     BMMapData *cur_level;
#endif
{
  int x, y;

  set_info_map(cur_level);

  distrib_extras = cur_level->distrib_extras;
  extra_prob = cur_level->prob;
    set_shadow = set_shadow_func[cur_level->shadow];

  for (x = 0; x < MAZE_W; x++) {
    for (y = 0; y < MAZE_H; y++) {
      expl_maze[x][y] = 0;
      old_expl_maze[x][y] = 0;
      set_maze_block(x, y, cur_level->maze[x][y]);
      redraw_maze[y] &= ~(1<<x);
    }
  }
}



/* 
 * public unload_blocks 
 */
#ifdef __STDC__
void 
unload_blocks (int num_disp)
#else
void 
unload_blocks (num_disp)
     int num_disp;
#endif
{
  int disp, i;

  for(disp = 0; disp < num_disp; disp ++) {
    for (i = 0; i< MAX_BLOCK; i++) {
	free_block(disp, i);
      }
    free_explosion_blocks(disp);
  }
}



/* public function draw_maze */
#ifdef __STDC__
void 
draw_maze (int num_disp)
#else
void 
draw_maze (num_disp)
     int num_disp;
#endif
{
  int x,y;
  int disp;
  
  for (x=0; x<MAZE_W; x++) {
    for (y=0; y<MAZE_H; y++) {
      draw_block(x, y, (int) maze[x][y]);
    }
  }
  
  for (disp = 0; disp < num_disp; disp++) {
    flush_blocks(disp, disp == (num_disp-1));
  }
}



/* public function redraw_rectangles */

#ifdef __STDC__
void 
set_redraw_rectangles (void)
#else
void 
set_redraw_rectangles ()
#endif
{
  int x,y;

  for (y=0; y<MAZE_H; y++) {
    if ((expl_dirty | expl_olddirty) & (1<<y)) {
      for (x=0; x<MAZE_W; x++) { 
	if (redraw_maze[y] & (1<<x)) { 
	  add_maze_rectangle(x, y);
	} else if (expl_maze[x][y] != old_expl_maze[x][y])  {
	  add_maze_rectangle(x, y);
	  redraw_maze[y] |= (1<<x);
	}
      }
    } else if (redraw_maze[y]) {
      for (x=0; x<MAZE_W; x++) { 
	if (redraw_maze[y] & (1<<x)) { 
	  add_maze_rectangle(x, y);
	} 
      }
    }
  }
  
  for (y=0; y<STAT_H; y++) {
    if (redraw_stat[y]) {
      for (x=0; x<STAT_W; x++) { 
	if (redraw_stat[y] & (1<<x)) {
	  add_stat_rectangle(x, y);
	}
      }
    }
  }
}



/* public function mark_maze */
#ifdef __STDC__
void 
mark_maze (int x1,
	   int y1, 
	   int x2, 
	   int y2)
#else
void 
mark_maze (x1,y1, x2, y2)
     int x1, y1, x2, y2;
#endif
{
  int x,y;

  for (y=MAX(y1,0); y<=MIN(y2,MAZE_H+1); y++) {
    if (y<MAZE_H) {
      for (x=MAX(x1,0); x<=MIN(x2,MAZE_W-1); x++) {
	redraw_maze[y] |= (1<<x);
      }
    } else {
      for (x=MAX(x1,0); x<=MIN(x2,STAT_W-1); x++) {
	redraw_stat[y-MAZE_H] |= (1<<x);
      }
    }
  }
}

/* public function mark_maze_tile */
#ifdef __STDC__
void
mark_maze_tile (int x, int y)
#else
void
mark_maze_tile (x, y)
     int x, y;
#endif
{
  if (y < MAZE_H) {
    redraw_maze[y] |= (1<<x);
  } else {
    redraw_stat[y-MAZE_H] |= (1<<x);
  }
}

/* public function mark_maze_tile */
#ifdef __STDC__
void
mark_maze_rect (int x, int y,
		int w, int h)
#else
void
mark_maze_rect (x, y, w, h)
     int x, y;
     int w, h;
#endif
{
  mark_maze (x/BLOCK_WIDTH, y/BLOCK_HEIGHT,
	     (x+w)/BLOCK_WIDTH, (y+h)/BLOCK_HEIGHT);
}

/* public function clear_redraw_map */
#ifdef __STDC__
void 
clear_redraw_map (void)
#else
void 
clear_redraw_map ()
#endif
{
  col *swap;

  swap = old_expl_maze;
  old_expl_maze = expl_maze;
  expl_maze = swap;

  expl_olddirty = expl_dirty;
  expl_dirty = 0;

#ifdef _BSD_SOURCE
  bzero(expl_maze, MAZE_W*sizeof(col));
  bzero(redraw_stat, sizeof(redraw_stat));
  bzero(redraw_maze, sizeof(redraw_maze));
#else
  memset(expl_maze, 0, MAZE_W*sizeof(col) );
  memset(redraw_maze, 0, sizeof(redraw_maze) );
  memset(redraw_stat, 0, sizeof(redraw_stat) );
#endif
}



/* public function update_maze */
#ifdef __STDC__
void 
update_maze (int num_disp)
#else
void 
update_maze (num_disp)
     int num_disp;
#endif
{
  int x,y;
  int disp;

  for (y=0; y<MAZE_H; y++) {
    if (redraw_maze[y]) {
      /* check if explosions are needed in this row */
      if (expl_dirty & (1<<y)) {
	for (x=0; x<MAZE_W; x++) {
	  if (redraw_maze[y] & (1<<x)) {
	    if (expl_maze[x][y] & 0x10) {
	      draw_explosion(x, y, expl_maze[x][y] & 0xf);
	    } else {
	      draw_block(x, y, (int) maze[x][y]);
	    }
	  }
	}
      } else {
	/* only blocks are needed */
	for (x=0; x<MAZE_W; x++) {
	  if (redraw_maze[y] & (1<<x)) {
	    draw_block(x, y, (int) maze[x][y]);
	  }
	}
      }
    }
  }

  for (disp=0; disp<num_disp; disp++) {
    flush_blocks(disp, disp == (num_disp-1) );
  }
}


#ifdef __STDC__
void 
update_expl (int disp)
#else
void 
update_expl (disp)
     int disp;
#endif
{
  int x,y;

  for (y=0; y<MAZE_H; y++) {
    if (redraw_maze[y]) {
      for (x=0; x<MAZE_W; x++) {
	if ( (redraw_maze[y] & (1<<x)) && (expl_maze[x][y] & 0x10) ) {
	  draw_explosion_sprite(disp, x, y, expl_maze[x][y] & 0xf);
	}
      }
    }
  }
}



/* public function check_maze */
#ifdef __STDC__
int 
check_maze (int x, 
	    int y)
#else
int 
check_maze (x,y)
     int x,y;
#endif
{
  return( (maze[x][y]==BTBlock) 
	 || (maze[x][y]==BTBlockRise) 
	 || (maze[x][y]==BTExtra) 
	 || (maze[x][y]==BTExtraOpen ) 
	 || (maze[x][y]==BTVoid ) 
	 || (maze[x][y]<0) );
}



/* public function check_maze_free */
#ifdef __STDC__
int 
check_maze_free (int x,
		 int y)
#else
int 
check_maze_free (x,y)
     int x,y;
#endif
{
  return( (maze[x][y]==BTFree) || (maze[x][y]==BTShadow) );
}

/* public function check_maze_wall */
#ifdef __STDC__
int 
check_maze_wall (int x,
		 int y)
#else
int 
check_maze_wall (x,y)
     int x,y;
#endif
{
  return( (maze[x][y]==BTBlock) || (maze[x][y]==BTBlockRise) );
}

/* public function check_maze_wall */
#ifdef __STDC__
int 
check_maze_solid (int x,
		 int y)
#else
int 
check_maze_solid (x,y)
     int x,y;
#endif
{
  return( (maze[x][y]==BTBlock) || (maze[x][y]==BTBlockRise) 
	 || (maze[x][y]==BTExtra) || (maze[x][y]==BTExtraOpen) );
}

/* local function check maze open */
#ifdef __STDC__
int 
check_maze_open (int x,
		 int y)
#else
int 
check_maze_open (x,y)
     int x,y;
#endif
{
  return((maze[x][y]==BTFree)||(maze[x][y]==BTShadow)||(maze[x][y]==BTVoid));
}


/* local function check maze open */
#ifdef __STDC__
int 
check_maze_extra (int x,
		  int y)
#else
int 
check_maze_extra (x,y)
     int x,y;
#endif
{
  return (maze[x][y]==BTExtra);
}

/* public function check_explosion */
#ifdef __STDC__
int
check_explosion (int x, int y)
#else
int
check_explosion (x, y)
     int x, y;
#endif
{
  return (0 != expl_maze[x][y]);
}

/* public function set_explosion_block */
#ifdef __STDC__
void
set_expl_block (int x, int y,
		int value)
#else
void
set_expl_block (x, y, value)
     int x,y,value;
#endif
{
  expl_maze[x][y] |= value;
  expl_dirty |= (1<<y);
}


/* public function set_extraosion_block */
#ifdef __STDC__
void
set_block_extra (int x, int y, int value)
#else
void
set_block_extra (x, y, value)
     int x,y,value;
#endif
{
  extra[x][y] = value;
}


/*
 * shadow mode functions
 */
#ifdef __STDC__
static void
set_shadow_none (int x, int y)
#else
static void
set_shadow_none (x, y)
     int x, y;
#endif
{
}

#ifdef __STDC__
static void
set_shadow_block (int x, int y)
#else
static void
set_shadow_block (x, y)
     int x, y;
#endif
{
  /* set shadows */
  if ( check_maze_wall(x,y) && (x != (MAZE_W-1)) && (maze[x+1][y] == BTFree) ) {
    maze[x+1][y] = BTShadow;
    mark_maze_tile(x+1,y);
  } else if ( !check_maze_solid(x,y) &&  (x != (MAZE_W-1) ) ) {
    if (maze[x+1][y] == BTShadow) {
      maze[x+1][y] = BTFree;
      mark_maze_tile(x+1,y);
    }
    if ( check_maze_wall(x-1,y) ) {
      if (maze[x][y] == BTFree) {
	maze[x][y] = BTShadow;
      }
    } else {
      if (maze[x][y] == BTShadow) {
	maze[x][y] = BTFree;
      }
    }
  } 
}

#ifdef __STDC__
static void
set_shadow_extra (int x, int y)
#else
static void
set_shadow_extra (x, y)
     int x, y;
#endif
{
  if ( check_maze_extra(x,y) && (x != (MAZE_W-1)) && (maze[x+1][y] == BTFree) ) {
    maze[x+1][y] = BTShadow;
    mark_maze_tile(x+1,y);
    /* Corrected shadow code by Chris Doherty */
  } else if ( !check_maze_solid(x,y) &&  (x != (MAZE_W-1) ) ) {
    if (maze[x+1][y] == BTShadow) {
      maze[x+1][y] = BTFree;
      mark_maze_tile(x+1,y);
    }
    if (maze[x-1][y] == BTExtra) {
      if (maze[x][y] == BTFree) {
	maze[x][y] = BTShadow;
      }
    } else {
      if (maze[x][y] == BTShadow) {
	maze[x][y] = BTFree;
      }
    }
  } 
}


#ifdef __STDC__
static void
set_shadow_both (int x, int y)
#else
static void
set_shadow_both (x, y)
     int x, y;
#endif
{
  if ( check_maze_solid(x,y) && (x != (MAZE_W-1)) && (maze[x+1][y] == BTFree) ) {
    maze[x+1][y] = BTShadow;
    mark_maze_tile(x+1,y);
  } else if ( !check_maze_solid(x,y) &&  (x != (MAZE_W-1) ) ) {
    if (maze[x+1][y] == BTShadow) {
      maze[x+1][y] = BTFree;
      mark_maze_tile(x+1,y);
    }
    if ( check_maze_solid(x-1,y) ) {
      if (maze[x][y] == BTFree) {
	maze[x][y] = BTShadow;
      }
    } else {
      if (maze[x][y] == BTShadow) {
	maze[x][y] = BTFree;
      }
    }
  } 
}


/* 
 * public function set_maze_block 
 */
#ifdef __STDC__
void 
set_maze_block (int x,
		int y, 
		int block)
#else
void 
set_maze_block (x,y,block)
int x,y, block;
#endif
{
  int rnd;
  
  maze[x][y] = block;
  mark_maze_tile(x,y);

  (*set_shadow)(x, y);

  /* create new extra */
  if (maze[x][y] == BTExtra) {
    rnd = random_number(64);
    if (rnd < extra_prob.bomb) {
      extra[x][y] = BTBomb;
    } else if (rnd < extra_prob.range) {
      extra[x][y] = BTRange;
    } else if (rnd < extra_prob.ill) {
      extra[x][y] = BTSick;
    } else if (rnd < extra_prob.invinc) {
      extra[x][y] = BTSpecial;
    } else if (rnd < extra_prob.evil) {
      extra[x][y] = BTEvil;
    } else {
      extra[x][y] = BTFree;
    }
  } else if (maze[x][y] != BTExtraOpen) {
    extra[x][y] = BTFree;
  }
}


/* public function get_extra */
#ifdef __STDC__
int 
get_extra (int invincible,
	   int x,
	   int y)
#else
int 
get_extra (invincible, x, y)
     int invincible, x, y;
#endif
{
  int extra_block;

  extra_block = ( maze[x][y]<=BTExtraOpen ? 0 : maze[x][y]);
  if ( (invincible>0)  && (extra_block == BTSick)) {
    extra_block = 0;
  }

  if (extra_block != 0) {
    set_maze_block(x, y, BTFree);
  }

  /* check if special extras is distributed immediately */
  if ( (extra_block == BTSpecial) && (distrib_extras == DEget) ) {
    distribute_extras (0, 0, -1, 0);
  }

  return extra_block ;
}



#ifdef __STDC__
static void 
my_swap (short *a, 
	 short *b)
#else
static void 
my_swap (a, b)
     short *a, *b;
#endif
{
  int s;

  s = *a;
  *a = *b;
  *b = s;
}



/* Do we distribute special bombs? */
#ifdef __STDC__
int 
distrib_special (void)
#else
int 
distrib_special ()
#endif
{
  return (distrib_extras == DEspecial);
}



/* global function */
#ifdef __STDC__
void 
distribute_extras (int bombs, 
		   int range, 
		   int extras, 
		   int specials)
#else
void 
distribute_extras(bombs, range, extras, specials)
     int bombs, range, extras, specials;
#endif
{
  int x,y;
  int i, where;
  int free_blocks = 0;
  int n_extras = 0;

    
  /* Create Extra Distribution Map */
  if (bombs + range + extras + specials != 0)  {
    /* First check for free Blocks */
    for (x=0; x < MAZE_W; x++) {
      for (y=0; y < MAZE_H; y++) {
	if ( (maze[x][y] == BTShadow) || (maze[x][y] == BTFree) ) {
	  dist_extra[x][y] = TRUE;
	  free_blocks ++;
	} else {
	  dist_extra[x][y] = FALSE;
	}
      }
    }

    free_blocks = check_distrib_expl (dist_extra, free_blocks);
    
    /* fill dist_koord array */
    i = 0;
    for (x=0; x < MAZE_W; x++) {
      for (y=0; y < MAZE_H; y++) {
	if (dist_extra[x][y] == TRUE) {
	  dist_x[i] = x;
	  dist_y[i] = y;
	  i ++;
	}
      }
    }
    
    /* Special extras */
    
    /* How many extras ?*/
    switch (distrib_extras) {
    case DEnone:
      n_extras = 0;
      break;

    case DEsingle:
      n_extras = ((extras>0) ? (1) : (0));
      break;

    case DEall:
      n_extras = (extras > 0) ? extras : 0;
      break;

    case DEspecial:
      n_extras = (specials + 2)/3;
      break;

    case DEget:
      /* use -1 for direct distribution on get extra */
      n_extras = (extras < 0) ? 1 :0;
      break;
    }

    /* Distribute special extras */
    if (free_blocks > 0) {
      for(i = 0; (free_blocks > 0) && (i < n_extras); i++) {
	where = random_number(free_blocks);
	set_maze_block(dist_x[where],dist_y[where], BTSpecial);
	
	free_blocks--;
	/* this position is used */
	if (where != free_blocks) {
	  my_swap( dist_x + where, dist_x + free_blocks );
	  my_swap( dist_y + where, dist_y + free_blocks );
	}
      }
    }
    
    if (free_blocks > 0) {
      for(i = 0; (free_blocks > 0) && (i < range); i++) {
	where = random_number(free_blocks);
	set_maze_block(dist_x[where],dist_y[where], BTRange);
	
	free_blocks--;
	/* this position is used */
	if (where != free_blocks)  {
	  my_swap( dist_x + where, dist_x + free_blocks );
	    my_swap( dist_y + where, dist_y + free_blocks );
	}
      }
    }
    
    if (free_blocks > 0) {
      for(i = 0; (free_blocks > 0) && (i < bombs); i++) {
	where = random_number(free_blocks);
	set_maze_block(dist_x[where],dist_y[where], BTBomb);
	
	free_blocks--;
	/* this position is used */
	if (where != free_blocks) {
	  my_swap( dist_x + where, dist_x + free_blocks );
	  my_swap( dist_y + where, dist_y + free_blocks );
	}
      }
    }
  }
}


/* local function blast_extra_block */
#ifdef __STDC__
void
blast_extra_block (int x, int y)
#else
void
blast_extra_block (x, y)
     int x, y;
#endif
{
  switch(maze[x][y]) {

    /* open extra block */
  case BTExtraOpen:
    if (extra[x][y] != BTEvil) {
      set_maze_block(x, y, extra[x][y]);
    } else {
      set_maze_block(x, y, BTFree);
      new_evil_bomb(x, y);
    }
    break;
	
    /* blast away extra and correct shadow */
  case BTSick:
  case BTBomb:
  case BTRange:
  case BTSpecial:
      set_maze_block(x, y, BTFree);
    break;
  }
}


/* public copy_expl_block */
#ifdef __STDC__
void 
copy_expl_block (int x,
		 int y,
		 int block[CHARH][CHARW])
#else
void 
copy_expl_block (x, y, block)
     int x,y;
     int block[CHARH][CHARW];
#endif
{
  int xp,yp;

  for (xp = x; xp < x+CHARW; xp ++) {
    for (yp = y; yp < y+CHARH; yp ++) {
      expl_maze[xp][yp] = block[yp-y][xp-x];
      expl_dirty |= (1<<yp);
      redraw_maze[yp] |= (1<<xp);
    }
  }
}

/*
 * sprite handling
 */

/* public function add_to_trophy_list */

#ifdef __STDC__
void 
add_trophy_to_sprite_list (int x,
			   int y)
#else
void 
add_trophy_to_sprite_list (x, y)
     int x, y;
#endif
{
  sprite_max->type = STBomb;
  sprite_max->bomb.draw = draw_bomb_sprite;
  sprite_max->bomb.ysort = y;
  sprite_max->bomb.y = y;
  sprite_max->bomb.x = (x+6) * BLOCK_WIDTH;
  sprite_max->bomb.anime = BB_NORMAL;
  sprite_max->bomb.mode = TRUE;
  sprite_max ++;
}




/* public function add_bomb_to_sprite_list */
#ifdef __STDC__
void 
add_bomb_to_sprite_list (int x, 
			 int y, 
			 int an, 
			 int mode)
#else
void 
add_bomb_to_sprite_list (x, y, an, mode)
     int x, y, an, mode;
#endif
{
  sprite_max->type = STBomb;
  sprite_max->bomb.draw = draw_bomb_sprite;
  sprite_max->bomb.ysort = y + 6 ;
  sprite_max->bomb.x = x ;
  sprite_max->bomb.y = y ;
  sprite_max->bomb.anime = an;
  sprite_max->bomb.mode = mode;
  sprite_max ++;
}



/* public function add_to_sprite_list */
#ifdef __STDC__
void 
add_player_to_sprite_list (int pl, 
			   int x, 
			   int y, 
			   int an, 
			   int m)
#else
void 
add_player_to_sprite_list (pl, x, y, an, m)
     int pl;
     int x, y, an;
     int m;
#endif
{
  sprite_max->type = STPlayer;
  sprite_max->player.draw = draw_player_sprite;
  sprite_max->player.player = pl;
  sprite_max->player.ysort = y + BLOCK_HEIGHT;
  sprite_max->player.x = 
    x + ( (an==WINNER_ANIME) ? WINNER_X_OFF : SPRITE_X_OFF );
  sprite_max->player.y = 
    y +( (an==WINNER_ANIME) ? WINNER_Y_OFF : SPRITE_Y_OFF );
  sprite_max->player.anime = an;
  sprite_max->player.mode = m;
  sprite_max ++;
}



/* public function clear_sprite_list */
#ifdef __STDC__
void 
clear_sprite_list (void)
#else
void 
clear_sprite_list ()
#endif
{
  sprite_max = sprite_list;
}


/* local function cmp_sprite */
#ifdef __STDC__
static int 
cmp_sprite (void *a,
	    void *b)
{
  return ( ((Sprite *)b)->any.ysort - ((Sprite *)a)->any.ysort);
}
#else
static int 
cmp_sprite (a, b)
	    Sprite *a, *b;
{
  return (b->any.ysort - a->any.ysort);
}
#endif


/* public sort_sprite_list */

#ifdef __STDC__
void 
sort_sprite_list (void)
#else
void 
sort_sprite_list ()
#endif
{
#ifdef __STDC__
  qsort (sprite_list, sprite_max-sprite_list, sizeof(Sprite),
	 (int (*)(const void *_a,const void *_b)) cmp_sprite);
#else
  qsort (sprite_list, sprite_max-sprite_list, sizeof(Sprite), cmp_sprite);
#endif
}

/* public function draw_sprites */
#ifdef __STDC__
void 
draw_sprites (int disp)
#else
void 
draw_sprites (disp)
     int disp;
#endif
{
  Sprite *spl;

  if (win_is_mapped(disp)) {
    spl = sprite_max;
    while  (spl != sprite_list) {
      spl --;
      if (spl->any.mode & SPM_AT_DISPLAY(disp) ) {
	(*spl->any.draw)(disp, spl);
      }
    }
  }
}

/*
 * end of file map.c
 */




