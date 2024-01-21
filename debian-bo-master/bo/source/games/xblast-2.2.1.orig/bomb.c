/*
 * Program XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26, 1997
 * started August 1993
 *
 * File: bomb.c
 * bombs and explosions
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
#define _BOMB_C
#include "include.h"
#include "mytypes.h"
#include "const.h"
#include "maze.h"
#include "map.h"
#include "main.h"
#include "graphics.h"
#include "expl.h"
#include "data.h"
#include "info.h"
#include "bomb.h"
#include "pipe.h"

#if defined(XBLAST_SOUND)
#include "sound.h"
#endif

#define BOMB_ERROR_PROB 16
#define BOMB_DELAY 5

/*
 * local variables
 */
static Explosion *bomb_maze[MAZE_W][MAZE_H];
static Explosion *expl_list;
static Explosion *expl_end;
static int num_expl = 0 ;
static fuse_times[NUM_FUSES] = {SHORT_FUSE, BOMB_TIME, LONG_FUSE};
static int initial_bomb_dir;
static int cur_bomb_time;
static int defaultBMT, specialBMT, evilBMT;

/*
 * bomb click functions
 */
PFV do_bomb_click;
PFV do_wall_click;
PFV do_player_click;

/*
 * local prototypes
 */
#ifdef __STDC__
static int new_explosion (BMPlayer *player, int x, int y, int range, 
			  int remote_controlled, int malfunction, 
			  BMBombType type, int type_extr, 
			  BMDirection initialdir);
#else
static int new_explosion ();
#endif

/*
 * special bomb functions
 */
#ifdef __STDC__
static int special_bomb_normal (Explosion *ptr);
static int special_bomb_napalm (Explosion *ptr);
static int special_bomb_firecracker (Explosion *ptr);
static int special_bomb_construction (Explosion *ptr);
static int special_bomb_threebombs (Explosion *ptr);
static int special_bomb_grenade (Explosion *ptr);
static int special_bomb_trianglebombs (Explosion *ptr);
static int special_bomb_destruction (Explosion *ptr);
static int special_bomb_fungus (Explosion *ptr);
static int special_bomb_renovation (Explosion *ptr);
static int special_bomb_pyro (Explosion *ptr);
#else
static int special_bomb_normal ();
static int special_bomb_napalm ();
static int special_bomb_firecracker ();
static int special_bomb_construction ();
static int special_bomb_threebombs ();
static int special_bomb_grenade ();
static int special_bomb_trianglebombs ();
static int special_bomb_destruction ();
static int special_bomb_fungus ();
static int special_bomb_renovation ();
static int special_bomb_pyro ();
#endif

static PFI do_special_bomb_function[NUM_BMT] = {
  special_bomb_normal,
  special_bomb_napalm,
  special_bomb_normal,
  special_bomb_normal,
  special_bomb_firecracker,
  special_bomb_firecracker,
  special_bomb_construction,
  special_bomb_threebombs,
  special_bomb_grenade,
  special_bomb_trianglebombs,
  special_bomb_destruction,
  special_bomb_fungus,
  special_bomb_renovation,
  special_bomb_pyro,
  special_bomb_pyro,
  special_bomb_normal,
};

/*
 * public function setup_bombs
 */
#ifdef __STDC__
void
setup_bombs (BMBombData *cur_level)
#else
void
setup_bombs (cur_level)
     BMBombData *cur_level;
#endif
{
  int x, y;

  set_info_bombs(cur_level);

  /* clear list */
  expl_list = NULL;
  expl_end = NULL;
  num_expl = 0;

  do_bomb_click = cur_level->bomb_click;
  do_wall_click = cur_level->wall_click;
  do_player_click = cur_level->player_click;

  initial_bomb_dir = cur_level->bomb_dir;
  cur_bomb_time = fuse_times[cur_level->fuse_time];

  defaultBMT = cur_level->defaultBMT;
  specialBMT = cur_level->buttonBMT;
  evilBMT = cur_level->evilBMT;

  for (x = 0; x < MAZE_W; x++) {
    for (y = 0; y < MAZE_H; y++) {
      bomb_maze[x][y] = NULL;
    }
  }

}


/* local function one_expl_at */
#ifdef __STDC__
static void 
one_expl_at (int x, 
	    int y, 
	    int ra, 
	    int ri)
#else
static void 
one_expl_at (x, y, ra, ri)
     int x, y, ra, ri;
#endif
{
  int i;

  /* right */
  for (i = 0; (i <= ra) && check_maze_open(x+i,y) ; i ++) {
    if (i >= ri) {
      if (i != ri && i != ra) {
	set_expl_block(x+i, y, 0x1a);
      } else if (i != ri) {
	set_expl_block(x+i, y, 0x18);
      } else if (i == ra) {
	set_expl_block(x+i, y, 0x10);
      } else {
	set_expl_block(x+i, y, 0x12);
      }
    }
  }
  if ( (i <= ra) && (check_maze_extra(x+i, y)) ) {
    set_maze_block(x+i, y, BTExtraOpen);
  }

  /* left */
  for (i = 0; (i <= ra) && check_maze_open(x-i,y); i ++) {
    if (i >= ri) {
      if (i != ri && i != ra) {
	set_expl_block(x-i, y, 0x1a);
      } else if (i != ri) {
	set_expl_block(x-i, y, 0x12);
      } else if (i == ra) {
	set_expl_block(x-i, y, 0x10);
      } else {
	set_expl_block(x-i, y, 0x18);
      }
    }
  }
  if ( (i <= ra ) && (check_maze_extra(x-i, y)) ) {
    set_maze_block(x-i, y, BTExtraOpen);
  }


  /* up */
  for (i = 0; (i <= ra) && check_maze_open(x,y-i); i ++) {
    if (i >= ri) {
      if (i != ri && i != ra) {
	set_expl_block(x, y-i, 0x15);
      } else if (i != ri) {
	set_expl_block(x, y-i, 0x14);
      } else if (i == ra) {
	set_expl_block(x, y-i, 0x10);
      } else {
	set_expl_block(x, y-i, 0x11);
      }
    }
  }
  if ( (i <= ra) && ( check_maze_extra(x, y-i)) ) {
    set_maze_block(x, y-i, BTExtraOpen);
  }

  /* down */
  for (i = 0; (i <= ra) && check_maze_open(x,y+i); i ++) {
    if (i >= ri) {
      if (i != ri && i != ra) {
	set_expl_block(x, y+i, 0x15);
      } else if (i != ri) {
	set_expl_block(x, y+i, 0x11);
      } else if (i == ra) {
	set_expl_block(x, y+i, 0x10);
      } else {
	set_expl_block(x, y+i, 0x14);
      }
    }
  }
  if ( (i <= ra) && ( check_maze_extra(x, y+i)) ) {
    set_maze_block(x, y+i, BTExtraOpen);
  }
}


/* local function del_explosion */
#ifdef __STDC__
static void 
del_explosion (Explosion *ptr)
#else
static void 
del_explosion (ptr)
     Explosion *ptr;
#endif
{
  Explosion *hilf;
  int i,x,y,r;

#ifdef DEBUG
  if (ptr == NULL) {
    fprintf (stderr, "Warning null pointer given to del_explosion\n");
    return;
  }
#endif

  /* give bomb back to  player */
  if (ptr->player != NULL) {
    ptr->player->bombs++;
  }

  x = ptr->x;
  y = ptr->y;
  r = ptr->range;
  bomb_maze[x][y]=NULL;

  num_expl --;

  /* rechts */
  for (i=0; i<=r && (check_maze_free(x+i, y)); i++);
  if (i <= r) {
    blast_extra_block(x+i,y);
  }

  /* links */
  for (i=0; i<=r && (check_maze_free(x-i, y)); i++);
  if (i <= r) {
    blast_extra_block(x-i,y);
  }

  /* unten */
  for (i=0; i<=r && (check_maze_free(x, y+i)); i++);
  if (i <= r) {
    blast_extra_block(x,y+i);
  }
  /* oben */
  for (i=0; i<=r && (check_maze_free(x, y-i)); i++);
  if (i <= r) {
    blast_extra_block(x,y-i);
  }

  /* aus liste aushaengen */
  if (ptr == expl_list) {
    expl_list = ptr->next;
    if (expl_list == NULL) {
      expl_end = NULL;
    }
    free(ptr);
  } else {
    for (hilf = expl_list; (hilf->next != ptr) && (hilf->next != NULL);
	 hilf = hilf->next);
    
    if (hilf->next != NULL) {
      if (expl_end == ptr) {
	expl_end = hilf;
      }
      hilf->next = hilf->next->next;
      free(ptr);
    }
  }
}



/* Player, wall, and bomb click functions (Garth Denley) */


/*
 * bomb click functions
 */

/* no bomb click, just stop */
#ifdef __STDC__
void
bomb_click_none (Explosion *bomb)
#else
void
bomb_click_none (bomb)
     Explosion *bomb;
#endif
{
  bomb->dir = GoStop;
  bomb->dx = 0;
  bomb->dy = 0;
}

/* bomb goes on with initial bomb dir */
#ifdef __STDC__
void
bomb_click_initial (Explosion *bomb)
#else
void
bomb_click_initial (bomb)
     Explosion *bomb;
#endif
{
  switch(initial_bomb_dir) {
  case GoStop:
    bomb->dx = 0;
    bomb->dy = 0;
    break;
  case GoRight:
    if (check_maze_free(bomb->x+1, bomb->y)) {
      bomb->dx = BOMB_VX;
    }
    bomb->dy = 0;
    break;
  case GoLeft:
    if (check_maze_free(bomb->x-1, bomb->y)) {
      bomb->dx = -BOMB_VX;
    }
    bomb->dy = 0;
    break;
  case GoDown:
    bomb->dx = 0;
    if (check_maze_free(bomb->x, bomb->y+1)) {
      bomb->dy = BOMB_VY;
    }
    break;
  case GoUp:
    bomb->dx = 0;
    if (check_maze_free(bomb->x, bomb->y-1)) {
      bomb->dy = -BOMB_VY;
    }
    break;
  }
  bomb->dir = initial_bomb_dir;
}

/* bomb goes thru */
#ifdef __STDC__
void
bomb_click_thru (Explosion *bomb)
#else
void
bomb_click_thru (bomb)
     Explosion *bomb;
#endif
{
}

/* snooker bombs  */
#ifdef __STDC__
void
bomb_click_snooker (Explosion *bomb)
#else
void
bomb_click_snooker (bomb)
     Explosion *bomb;
#endif
{
  int dir;

  dir = bomb-> dir;

  bomb->dir = GoStop;
  bomb->dx = 0;
  bomb->dy = 0;

  switch (dir) {
  case GoUp:
    move_bomb(bomb->x,bomb->y-1,dir);
    break;
  case GoLeft:
    move_bomb(bomb->x-1,bomb->y,dir);
    break;
  case GoDown:
    move_bomb(bomb->x,bomb->y+1,dir);
    break;
  case GoRight:
    move_bomb(bomb->x+1,bomb->y,dir);
    break;
  }
}

/* contact bombs */
#ifdef __STDC__
void 
bomb_click_contact (Explosion *bomb)
#else
void 
bomb_click_contact (bomb)
     Explosion *bomb;
#endif
{
  bomb->dir = GoStop;
  bomb->dx = 0;
  bomb->dy = 0;
  bomb->count = 0;
}

static BMDirection turn_clockwise[MAX_DIR] = {
  GoStop, GoRight, GoUp, GoLeft, GoDown, GoDefault
};

/* clockwise bombs */
#ifdef __STDC__
void 
bomb_click_clockwise (Explosion *bomb)
#else
void 
bomb_click_clockwise (bomb)
     Explosion *bomb;
#endif
{
  bomb->dx = 0;
  bomb->dy = 0;
  bomb->dir = turn_clockwise[bomb->dir];
}

static BMDirection turn_anticlockwise[MAX_DIR] = {
  GoStop, GoLeft, GoDown, GoRight, GoUp, GoDefault
};

/* anticlockwise bombs */
#ifdef __STDC__
void 
bomb_click_anticlockwise (Explosion *bomb)
#else
void 
bomb_click_anticlockwise (bomb)
     Explosion *bomb;
#endif
{
  bomb->dx = 0;
  bomb->dy = 0;
  bomb->dir = turn_anticlockwise[bomb->dir];
}

/* randomdir bombs */
#ifdef __STDC__
void 
bomb_click_randomdir (Explosion *bomb)
#else
void 
bomb_click_randomdir (bomb)
     Explosion *bomb;
#endif
{
  bomb->dx = 0;
  bomb->dy = 0;
  bomb->dir = (BMDirection) (random_number(4) + 1);
}

static BMDirection turn_opposite[MAX_DIR] = {
  GoStop, GoDown, GoRight, GoUp, GoLeft, GoDefault
};

/* rebound bombs */
#ifdef __STDC__
void 
bomb_click_rebound (Explosion *bomb)
#else
void 
bomb_click_rebound (bomb)
     Explosion *bomb;
#endif
{
  bomb->dir = turn_opposite[bomb->dir];
}



/* public function do_bombs() */
#ifdef __STDC__
void 
do_bombs (void)
#else
void 
do_bombs ()
#endif
{
  Explosion *ptr;
  int draw_flag;

  for (ptr = expl_list; ptr != NULL; ptr = ptr->next) {
    if (ptr->count < 0) {
      draw_flag = TRUE;

      switch(ptr->dir) {

      case GoStop:
	draw_flag = FALSE;
	break;

      case GoUp:
	if  ( (ptr->dy == 0) && !check_maze_free(ptr->x, ptr->y-1) )  {
	  (*do_wall_click)(ptr);
	} else if ( (ptr->dy <= 0) && check_bomb(ptr->x,ptr->y-1) ) {
	  (*do_bomb_click)(ptr);
	} else {
	  ptr->dy -= BOMB_VY;
	  if (ptr->dy <= -BLOCK_HEIGHT/2) {
#if defined(XBLAST_SOUND)
	    play_sound(SND_SLIDE, (ptr->x * BLOCK_WIDTH) /
		       (PIXW / MAX_SOUND_POSITION));
#endif
	    bomb_maze[ptr->x][ptr->y] = NULL;
	    ptr->dy += BLOCK_HEIGHT;
	    ptr->y -= 1;
	    bomb_maze[ptr->x][ptr->y] = ptr;
	  }
	}
	break;

      case GoDown:
	if ( (ptr->dy == 0) && !check_maze_free(ptr->x, ptr->y+1) ) {
	  (*do_wall_click)(ptr);
	} else if ( (ptr->dy >= 0) && check_bomb(ptr->x,ptr->y+1)) {
	  (*do_bomb_click)(ptr);
	} else {
	  ptr->dy += BOMB_VY;
	  if (ptr->dy >= BLOCK_HEIGHT/2) {
#if defined(XBLAST_SOUND)
	    play_sound(SND_SLIDE, (ptr->x * BLOCK_WIDTH) /
		       (PIXW / MAX_SOUND_POSITION));
#endif
	    bomb_maze[ptr->x][ptr->y] = NULL;
	    ptr->dy -= BLOCK_HEIGHT;
	    ptr->y += 1;
	    bomb_maze[ptr->x][ptr->y] = ptr;
	  }
	}
	break;

      case GoRight:
	if ( (ptr->dx == 0) && !check_maze_free(ptr->x+1, ptr->y) ) {
	  (*do_wall_click)(ptr);
	} else if ( (ptr->dx >= 0) && check_bomb(ptr->x+1,ptr->y)) {
	  (*do_bomb_click)(ptr);
	} else {
	  ptr->dx += BOMB_VX;
	  if (ptr->dx >= BLOCK_WIDTH/2) {
#if defined(XBLAST_SOUND)
	    play_sound(SND_SLIDE, (ptr->x * BLOCK_WIDTH) /
		       (PIXW / MAX_SOUND_POSITION));
#endif
	    bomb_maze[ptr->x][ptr->y] = NULL;
	    ptr->dx -= BLOCK_WIDTH;
	    ptr->x += 1;
	    bomb_maze[ptr->x][ptr->y] = ptr;
	  }
	}
	break;

      case GoLeft:
	if ( (ptr->dx == 0) && !check_maze_free(ptr->x-1, ptr->y) ) {
	  (*do_wall_click)(ptr);
	} else if ( (ptr->dx <= 0) && check_bomb(ptr->x-1,ptr->y)) {
	  (*do_bomb_click)(ptr);
	} else {
	  ptr->dx -= BOMB_VX;
	  if (ptr->dx <= -BLOCK_WIDTH/2) {
#if defined(XBLAST_SOUND)
	    play_sound(SND_SLIDE, (ptr->x * BLOCK_WIDTH) /
		       (PIXW / MAX_SOUND_POSITION));
#endif
	    bomb_maze[ptr->x][ptr->y] = NULL;
	    ptr->dx += BLOCK_WIDTH;
	    ptr->x -= 1;
	    bomb_maze[ptr->x][ptr->y] = ptr;
	  }
	}
	break;

      default:
	break;
      }

      /* draw a bomb */
      if ( (ptr->dir == GoUp) || (ptr->dir == GoDown) ) {
	if (ptr->dy <= 0) {
	  mark_maze_tile(ptr->x, ptr->y-1);
	}
	if (ptr->dy >= 0) {
	  mark_maze_tile(ptr->x, ptr->y+1);
	}
      }
	  
      if ( (ptr->dir == GoLeft) || (ptr->dir == GoRight) ) {
	if (ptr->dx < 0) {
	  mark_maze_tile(ptr->x-1, ptr->y);
	} else {
	  mark_maze_tile(ptr->x+1, ptr->y);
	}
      }

      if (check_explosion(ptr->x, ptr->y)) {
	ptr->count = 0;
      }

      if (ptr->blink + ptr->count == 0) {
	mark_maze_tile(ptr->x, ptr->y);
	add_bomb_to_sprite_list(ptr->x*BLOCK_WIDTH + ptr->dx, 
				ptr->y*BLOCK_HEIGHT + ptr->dy,
				((ptr->range == 1) ? BB_MINI : BB_NORMAL),
				SPM_ALL_DISPLAYS | SPM_MASK_ONLY);
	ptr->blink = ptr->blink >> 1;
      } else {
	if ( draw_flag || ( (ptr->blink<<1) + ptr->count == 1) ) {
	  mark_maze_tile(ptr->x, ptr->y);
	}
	add_bomb_to_sprite_list(ptr->x*BLOCK_WIDTH + ptr->dx, 
				ptr->y*BLOCK_HEIGHT + ptr->dy,
				((ptr->range == 1) ? BB_MINI : BB_NORMAL),
				SPM_ALL_DISPLAYS);
      }

      /* Bomb malfunction, random or illness */
      if ( (ptr->count == -3) 
	  && (ptr->malfunction || (random_number(BOMB_ERROR_PROB) == 0) ) )
	{
	  ptr->malfunction = 0;
	  ptr->count = -BOMB_TIME *(2+random_number(BOMB_DELAY));
	  ptr->blink = (BOMB_TIME >>1);
	}
    }
  }
}



/* 
 * public function ignite_players_bombs 
 */
#ifdef __STDC__
int 
ignite_players_bombs (BMPlayer *ps)
#else
int 
ignite_players_bombs (ps)
     BMPlayer *ps;
#endif
{
  Explosion *ptr;
  int number_of_bombs = 0;

  for (ptr = expl_list; ptr != NULL; ptr = ptr->next) {
    if (ptr->count < 0) {
      /* draw a bomb */
      if ( ptr->player == ps ) {
	ptr->count = 0;
	number_of_bombs ++;
      }
    }
  }
  return(number_of_bombs);
}



/* 
 * public function ignite_players_bombs 
 */
#ifdef __STDC__
int 
ignite_all_bombs (void)
#else
int 
ignite_all_bombs ()
#endif
{
  Explosion *ptr;
  int number_of_bombs = 0;

  for (ptr = expl_list; ptr != NULL; ptr = ptr->next) {
    if (ptr->count < 0) {
      ptr->count = 0;
      number_of_bombs ++;
    }
  }
  return(number_of_bombs);
}



/* public function ignite_bombs */
#ifdef __STDC_
void 
ignite_bombs (void)
#else
void 
ignite_bombs ()
#endif
{
  Explosion *ptr;

  for (ptr = expl_list; ptr != NULL; ptr = ptr->next) {
    if (ptr->count < 0) {
      /* draw a bomb */
      if (check_explosion(ptr->x, ptr->y)) {
	ptr->count = 0;
      }
    }
  }
}



/* public functions do_explosion */
#ifdef __STDC__
int 
do_explosions (void)
#else
int 
do_explosions ()
#endif
{
  Explosion *ptr, *next_ptr;
  register int hilf;
  int explode_flag = FALSE;

  ptr = expl_list;;
  while (ptr != NULL) {
    next_ptr = ptr->next;
    /* check if bomb is exploding */
    if (ptr->count >= 0) {
      /* check if bomb has burned out */
      if ( ptr->count  >= (2*ptr->range + 2) ) {
	del_explosion(ptr);
	ptr = next_ptr;
	continue;
      } else {
	/* get exploding time */
	if ( (hilf = ptr->count) == 0) {
#if defined(XBLAST_SOUND)
          if (ptr->range == 1 ||
              ptr->type == BMTfirecracker ||
              ptr->type == BMTfirecracker2) {
	    play_sound(SND_MINIBOMB, (ptr->x * BLOCK_WIDTH) /
		       (PIXW / MAX_SOUND_POSITION));
	  } else {
	    play_sound(SND_EXPL, (ptr->x * BLOCK_WIDTH) / 
		       (PIXW / MAX_SOUND_POSITION));
	  }
#else
	  /* this is needed for triggering the bell */
	  explode_flag = TRUE;
#endif
	}
	/* now do the explosion */
	one_expl_at(ptr->x, ptr->y,
		    MIN(ptr->range,hilf), MAX(0,( (hilf) - ptr->range)) );
      }
    }
    if (! (*do_special_bomb_function[ptr->type])(ptr)) {
      ptr->count ++;
    };
    ptr = next_ptr;
  }

  return explode_flag;
}

/* Special bomb code (Garth Denley) */

/* Used to spread an explosion out */
#ifdef __STDC__
static void 
sprex (int lx,
       int ly,
       int range,
       int type,
       int type_extr)
#else
static void 
sprex (lx, ly, range, type, type_extr)
     int lx;
     int ly;
     int range;
     int type;
     int type_extr;
#endif
{
  if ( (lx < MAZE_W) && (lx > -1) && (ly < MAZE_H) && (ly > -1)
       && !check_maze_solid(lx, ly) ) {
    new_explosion(NULL, lx, ly, range, FALSE, FALSE,
                  type, type_extr, GoStop);
  }
}

#if 0
/* Used to spread an explosion out */
#ifdef __STDC__
static void 
sprex_move (int lx, int ly,
	    int range,
	    int type, int type_extr,
	    BMDirection dir)
#else
static void 
sprex_move (lx, ly, range, type, type_extr, dir)
     int lx;
     int ly;
     int range;
     int type;
     int type_extr;
     BMDirection dir;
#endif
{
  if ( (lx < MAZE_W) && (lx > -1) && (ly < MAZE_H) && (ly > -1)
       && !check_maze_solid(lx, ly) ) {
    new_explosion(NULL, lx, ly, range, FALSE, FALSE,
                  type, type_extr, dir);
  }
}
#endif

#ifdef __STDC__
static void
move_block_from_to (int sx,
                    int sy,
                    int dx,
                    int dy)
#else
static void
move_block_from_to (sx, sy, dx, dy)
     int sx;
     int sy;
     int dx;
     int dy;
#endif
{
  if (    (sx > 0) && (sx < MAZE_W-1)
       && (sy > 0) && (sy < MAZE_H-1)
       && (dx > 0) && (dx < MAZE_W-1)
       && (dy > 0) && (dy < MAZE_H-1)
       && check_maze_wall(sx, sy)
       && check_maze_free(dx, dy) ) {

    set_maze_block(sx, sy, BTFree);
    set_maze_block(dx, dy, BTBlock);
    kill_player_at(dx, dy);
    delete_bomb_at(dx,dy);

  }
}

/*
 * special bomb functions
 */

#ifdef __STDC__
static int
special_bomb_normal (Explosion *ptr)
#else
static int
special_bomb_normal (ptr)
     Explosion *ptr;
#endif
{
  return 0;
}

/* napalm bomb */
#ifdef __STDC__
static int
special_bomb_napalm (Explosion *ptr)
#else
static int
special_bomb_napalm (ptr)
     Explosion *ptr;
#endif
{
  int i;

  if (ptr->count == -1) {
    ptr->type = BMTnormal;
    for (i= -2; i<=2; i++) {
      sprex (ptr->x+i, ptr->y, ptr->range/(ABS(i)+1), BMTblastnow, 0);
      sprex (ptr->x, ptr->y+i, ptr->range/(ABS(i)+1), BMTblastnow, 0);
    }
  }

  return 0;
}

/* firecracker */
#ifdef __STDC__
static int
special_bomb_firecracker (Explosion *ptr)
#else
static int
special_bomb_firecracker (ptr)
     Explosion *ptr;
#endif
{
  int i;
  int nasty;

  if (ptr->count >= 1) {
    if ((ptr->type == BMTfirecracker) && (random_number(10) == 0)) {
      ptr->type_extr = -5;
    }
    nasty = ptr->type_extr;
    for (i= -1; i<=1; i++) {
      if (nasty<2 || (0 == random_number(1 + nasty))) {
	sprex (ptr->x+i, ptr->y, 1, BMTfirecracker2, nasty + 1);
      }
      if (nasty<2 || (0 == random_number(1 + nasty))) {
	sprex (ptr->x, ptr->y+i, 1, BMTfirecracker2, nasty + 1);
      }
    }
    ptr->type = BMTnormal;
  }

  return 0;
}

/* construction */
#ifdef __STDC__
static int
special_bomb_construction (Explosion *ptr)
#else
static int
special_bomb_construction (ptr)
     Explosion *ptr;
#endif
{
  int x, y;

  if (ptr->count == 1) {
    x = ptr->x;
    y = ptr->y;
    del_explosion(ptr);
    if (check_b_near(x,y) == 1) {
      set_maze_block(x,y,BTExtra);
      set_block_extra(x,y,BTFree);
    }
    return 1;
  }

  return 0;
}

/* fungus */
#ifdef __STDC__
static int
special_bomb_fungus (Explosion *ptr)
#else
static int
special_bomb_fungus (ptr)
     Explosion *ptr;
#endif
{
  int i, x, y;

  x = ptr->x;
  y = ptr->y;
  if (ptr->count == (-cur_bomb_time)*3/5) {
    for (i = -1; i<=1; i++) {
        sprex(x+i,y,1,BMTfungus,0);
        sprex(x,y+i,1,BMTfungus,0);
      }
  }

  return 0;
}

/* threebombs */
#ifdef __STDC__
static int
special_bomb_threebombs (Explosion *ptr)
#else
static int
special_bomb_threebombs (ptr)
     Explosion *ptr;
#endif
{
  if (ptr->count == -cur_bomb_time) {
    sprex(ptr->x-2,ptr->y,ptr->range,defaultBMT,0);
    sprex(ptr->x+2,ptr->y,ptr->range,defaultBMT,0);
  }

  return 0;
}

/* grenade */
#ifdef __STDC__
static int
special_bomb_grenade (Explosion *ptr)
#else
static int
special_bomb_grenade (ptr)
     Explosion *ptr;
#endif
{
  int i, j;

  if (ptr->range > 0) {
    if (ptr->count == -1) {
      if (ptr->range == 1) {
	sprex(ptr->x-1, ptr->y-1, 0, BMTblastnow, 0);
	sprex(ptr->x+1, ptr->y-1, 0, BMTblastnow, 0);
	sprex(ptr->x-1, ptr->y+1, 0, BMTblastnow, 0);
	sprex(ptr->x+1, ptr->y+1, 0, BMTblastnow, 0);
      } else {
	for (i = -((ptr->range)-1); i<=((ptr->range)-1); i++) {
	  for (j = -((ptr->range)-1); j<=((ptr->range)-1); j++) {
	    sprex (ptr->x+i, ptr->y+j, 1, BMTblastnow, 0);
	  }
	}
      }
    }
  }

  return 0;
}

/* trianglebombs */
#ifdef __STDC__
static int
special_bomb_trianglebombs (Explosion *ptr)
#else
static int
special_bomb_trianglebombs (ptr)
     Explosion *ptr;
#endif
{
  if (ptr->count == -cur_bomb_time + 2) {
    int i,j;
    
    i = random_number(2) * 4 - 2;
    j = random_number(2) * 4 - 2;
    sprex(ptr->x+i, ptr->y,   ptr->range, BMTnormal, 0);
    sprex(ptr->x,   ptr->y+j, ptr->range, BMTnormal, 0);
    ptr->type = BMTnormal;
  }

  return 0;
}

/* desctruction */
#ifdef __STDC__
static int
special_bomb_destruction (Explosion *ptr)
#else
static int
special_bomb_destruction (ptr)
     Explosion *ptr;
#endif
{
  int i, x, y;

  x = ptr->x;
  y = ptr->y;
  if (ptr->count == 1) {
    del_explosion(ptr);
    for (i = -1; i<=1; i++) {
      if ( (x+i < (MAZE_W-1)) && check_maze(x+i,y) && (x+i > 0) )
	set_maze_block(x+i,y,BTFree);
      if ( (y+i < (MAZE_H-1)) && check_maze(x,y+i) && (y+i > 0) )
	set_maze_block(x,y+i,BTFree);
    }
    return 1;
  }

  return 0;
}

/* renovation */
#ifdef __STDC__
static int
special_bomb_renovation (Explosion *ptr)
#else
static int
special_bomb_renovation (ptr)
     Explosion *ptr;
#endif
{
  int x, y;

  x = ptr->x;
  y = ptr->y;
  if (ptr->count == 1) {
    move_block_from_to ( x-1, y,   x-2, y   );
    move_block_from_to ( x+1, y,   x+2, y   );
    move_block_from_to ( x,   y-1, x,   y+1 );
    move_block_from_to ( x-1, y-1, x,   y+2 );
  }

  return 0;
}

/* pyro */
#ifdef __STDC__
static int
special_bomb_pyro (Explosion *ptr)
#else
static int
special_bomb_pyro (ptr)
     Explosion *ptr;
#endif
{
  int x, y, k;

  if (ptr->count == 1) {
    for (k=0;k<5;k++) {
      x = ptr->x + random_number(3) - 1;
      y = ptr->y + random_number(3) - 1;  
      if ( !bomb_maze[x][y] && check_maze_free(x,y) ) {
	sprex(x, y, 1, BMTpyro2, 0);
	break;
      }
    }
  }

  return 0;
}


/* local function new_explosion */
#ifdef __STDC__
static int 
new_explosion (BMPlayer *player, 
	       int x, 
	       int y, 
	       int range, 
	       int remote_controlled, 
	       int malfunction, 
	       BMBombType type, 
	       int type_extr, 
	       BMDirection initialdir)
#else
static int 
new_explosion (player, x, y, range, remote_controlled, malfunction, type, 
	       type_extr, initialdir)
     BMPlayer *player;
     int x, y, range; 
     int remote_controlled, malfunction;
     BMBombType type;
     int type_extr;
     BMDirection initialdir;
#endif
{
  Explosion *new;

  if (NULL != bomb_maze[x][y]) {
    return 0;
  }

  mark_maze_tile(x,y);
  
  num_expl ++;
  
  new = (Explosion *)calloc(1,sizeof(Explosion));
  bomb_maze[x][y]=new;
  
  if (expl_list == NULL) {
    expl_list = new;
  } else {
    expl_end->next = new;
  }
  new->next = NULL;
  
  new->player = player;
  new->x = x;
  new->y = y;
  new->dx = 0;
  new->dy = 0;
  new->malfunction = malfunction;
  
  if (initialdir == GoDefault) {
    new->dir = initial_bomb_dir;
  } else {
    new->dir = initialdir;
  }
  
  /* set type */
  switch (type) {
  case BMTdefault:
    type = defaultBMT;
    break;
  case BMTspecial:
    type = specialBMT;
    break;
  case BMTevil:
    type = evilBMT;
    break;
  default:
    break;
  }
  new->type = type;

  if (new->type == BMTrandom) {
    switch (random_number(5)) {
    case 0: new->type = BMTnapalm;      break;
    case 1: new->type = BMTfirecracker; break;
    case 2: new->type = BMTgrenade;     break;
    case 3: new->type = BMTfungus;      break;
    case 4: new->type = BMTpyro;        break;
    }
  }

  new->type_extr = type_extr;
  
  if (type != BMTclose) {
    switch(new->dir) {
    case GoDown:
      if(check_maze_free(new->x,new->y+1)) {
	new->dy= BOMB_VY;
      }
      break;
      
    case GoUp:
      if(check_maze_free(new->x,new->y-1)) {
	new->dy= -BOMB_VY;
      }
      break;
    case GoLeft:
      if(check_maze_free(new->x-1,new->y)) {
	new->dx= -BOMB_VX;
      }
      break;
      
    case GoRight:
      if(check_maze_free(new->x+1,new->y)) {
	new->dx= BOMB_VX;
      }
      break;

    default:
      break;
    }
  }
  if ( (type == BMTfirecracker) || (type == BMTfungus)
      || (type == BMTpyro) ) {
    new->range = 1;
  } else if (type == BMTconstruction) {
    new->range = 0;
  } else if (type == BMTgrenade) {
    new->range = range / 2;
  } else {
    new->range = range;
  }
  if ((type == BMTblastnow) || (type == BMTfirecracker2)
      || (type == BMTpyro2) ) {
    new->count = 0;
  } else if (remote_controlled) {
    new->count = -GAME_TIME;
  } else {
    new->count = -cur_bomb_time;
  }
  new->blink = BOMB_TIME >>1;
  expl_end = new;
  
  return 1;
}

#ifdef __STDC__
int 
new_player_bomb (BMPlayer *ps,
		 BMBombType type)
#else
int 
new_player_bomb (ps, type)
     BMPlayer *ps;
     BMBombType type;
#endif
{
  return new_explosion (ps, (ps->x + BLOCK_WIDTH/2)/BLOCK_WIDTH,
			(ps->y + BLOCK_HEIGHT + BLOCK_HEIGHT/2)/BLOCK_HEIGHT,
			(ps->illness == IllMini) ? 1 : ps->range,
			(ps->remote_control > 0),
			(ps->illness == IllMalfunction),
			type, 0, GoDefault );
}

#ifdef __STDC__
int 
new_evil_bomb (int x, int y)
#else
int
new_evil_bomb (x, y)
     int x, y; 
#endif
{
  return new_explosion (NULL, x, y, 3, FALSE, FALSE, evilBMT, 0, GoDefault);
}

#ifdef __STDC__
int 
new_nasty_bomb (int x, int y,
		int range,
		BMDirection dir)
#else
int
new_nasty_bomb (x, y, range, dir)
     int x, y;
     int range;
     BMDirection dir;
#endif
{
  return new_explosion (NULL, x, y, range, FALSE, FALSE, BMTclose, 0, dir);
}


/* public function stun_players */
#ifdef __STDC__
void 
stun_players (BMPlayer *ps, 
	      int num_player)
#else
void 
stun_players (ps, num_player)
     BMPlayer *ps;
     int num_player;
#endif
{
  Explosion *ptr;
  int player;
  int click_flags;

  for (ptr = expl_list; ptr != NULL; ptr = ptr->next) {
    if (ptr->dir != GoStop) {
      click_flags = 0;
      for (player = 0; player < num_player; player ++) {
	if ( (ps[player].invincible == 0)
	    && (ABS(ptr->x*BLOCK_WIDTH + ptr->dx - ps[player].x ) 
		< BOMB_STUN_X )
	    && (ABS(ptr->y*BLOCK_HEIGHT + ptr->dy 
		    - ps[player].y - BLOCK_HEIGHT ) < BOMB_STUN_Y ) ) {
	  if (ptr->dx == 0) {
	    if (ptr->dy < 0) {
	      mark_maze_tile(ptr->x, ptr->y-1);
	    }
	    if (ptr->dy > 0) {
	      mark_maze_tile(ptr->x, ptr->y+1);
	    }
	  }
	  if (ptr->dy == 0) {
	    if (ptr->dx < 0) {
	      mark_maze_tile(ptr->x-1, ptr->y-1);
	    }
	    if (ptr->dx > 0) {
	      mark_maze_tile(ptr->x+1, ptr->y-1);
	    }
	  }
	  click_flags |= (1<<player);
	}
      }
      /* do player click after all players are checked */
      if (click_flags) {
	for (player=0; player<num_player; player++) {
	  if (click_flags & (1<<player)) {
	    /* stun player */
	      if (0 == ps[player].stunned) {
		ps[player].stunned = STUN_TIME;
#if defined(XBLAST_SOUND)
		play_sound(SND_STUN, ps[player].x / (PIXW/MAX_SOUND_POSITION) );
#endif
	      }
	  }
	}
	/* do bomb-player click */
	(*do_player_click)(ptr);
      }
    }
  }
}




/* public function check_bomb */
#ifdef __STDC__
int 
check_bomb (int x, 
	    int y)
#else
int 
check_bomb (x,y)
     int x,y;
#endif
{
  return (int)( (bomb_maze[x][y] != NULL) && (bomb_maze[x][y]->count < 0));
}



/* public function number_of_explosions */
#ifdef __STDC__
int 
number_of_explosions (void)
#else
int 
number_of_explosions ()
#endif
{
  return(num_expl);
}


/* public function */
#ifdef __STDC__
void 
delete_bomb_at (int x, int y)
#else
void 
delete_bomb_at (x, y)
     int x,y;
#endif
{
  if (bomb_maze[x][y]) {
    del_explosion(bomb_maze[x][y]);
  }
}



/* public function */
#ifdef __STDC__
void 
move_bomb (int x,
	   int y,
	   int dir)
#else
void 
move_bomb (x,y,dir)
     int x,y,dir;
#endif
{
  Explosion *ptr;
  
  if(NULL != (ptr = bomb_maze[x][y]) ) {
    if (ptr->dir == GoStop) {
      ptr->dir = dir;
      switch(dir) {
      case GoUp:
      case GoDown:
	ptr->dx = 0;
	break;
	
      case GoLeft:
      case GoRight:
	ptr->dy = 0;
	break;
      }
    }
  }
}


#ifdef __STDC__
int 
check_distrib_expl (short dist_extra[MAZE_W][MAZE_H],
		    int free_blocks) 
#else
int 
check_distrib_expl (dist_extra, free_blocks)
     short dist_extra[MAZE_W][MAZE_H];
     int free_blocks;
#endif
{
  Explosion *ptr;
  int x,y,ra;

  /* Go through explosions */
  for (ptr = expl_list; ptr != NULL; ptr = ptr->next) {
    if (ptr->count >=0) {
      ra = MIN(ptr->range, ptr->count + 1);
      
      for (x = ptr->x; (x <= (ptr->x + ra)) && (x <= MAZE_W); x++) {
	if (dist_extra[x][ptr->y]) {
	    free_blocks --;
	  }
	dist_extra[x][ptr->y] = FALSE;
      }
      for (x = ptr->x; (x >= (ptr->x - ra)) && (x >= 0); x--)	{
	if (dist_extra[x][ptr->y]) {
	  free_blocks --;
	}
	dist_extra[x][ptr->y] = FALSE;
      }
      for (y = ptr->y; (y <= (ptr->y + ra)) && (y <= MAZE_H); y++) {
	if (dist_extra[ptr->x][y]) {
	  free_blocks --;
	}
	dist_extra[ptr->x][y] = FALSE;
      }
      for (y = ptr->y; (y >= (ptr->y - ra)) && (y >= 0); y--)	{
	if (dist_extra[ptr->x][y]) {
	  free_blocks --;
	}
	dist_extra[ptr->x][y] = FALSE;
      }
    }
  }
  return free_blocks;
}

/* contributed functions */

/* 
 * public function haunt_kick 
 * prob is an integer >= 6
 * the lower the number, the more vicious the haunted bombs are
 */

#ifdef __STDC__
void 
haunt_kick (int prob)
#else
void 
haunt_kick (prob)
     int prob;
#endif
{
  int bnum;
  Explosion *ptr;
  int t;

  ptr = expl_list;
  bnum = random_number(prob);

  if (bnum < 6)  {
    if (bnum > 0) {
      for (t=0; t<bnum ; t++ ) {
	if (ptr != NULL) {
	  ptr = ptr->next;
	}
      }
    }
    
    if (ptr != NULL) {
      if (ptr->dir == GoStop) {
	if (check_b_near(ptr->x,ptr->y) == 1) {
	  switch (random_number(4)) {
	  case 0:
	    ptr->dir = GoUp;
	    ptr->dx = 0;
	    break;
	  case 1:
	    ptr->dir = GoDown;
	    ptr->dx = 0;
	    break;
	  case 2:
	    ptr->dir = GoLeft;
	    ptr->dy = 0;
	    break;
	  case 3:
	    ptr->dir = GoRight;
	    ptr->dy = 0;
	    break;
	  }
#if defined(XBLAST_SOUND)
	  play_sound(SND_HAUNT, SOUND_MIDDLE_POSITION);
#endif
	}
      }
    }
  }
}

/* public function do_air (Garth Denley)*/

/* Shoots bombs away if within 2 square radius */
/* Direction based on angle from bomb */

#ifdef __STDC__
void 
do_air (BMPlayer *ps)
#else
void 
do_air (ps)
     BMPlayer *ps;
#endif
{
  Explosion *ptr;
  int x,y,ex,ey;

  for (ptr = expl_list; ptr != NULL; ptr = ptr->next) {
    if ((ptr->dir == GoStop))  {
      x = (ptr->x) * BLOCK_WIDTH;
      y = (ptr->y-1) * BLOCK_HEIGHT;
      ex = x - ps->x;
      ey = y - ps->y;
      if ((ABS(ex)<BLOCK_WIDTH*2)&&(ABS(ey)<BLOCK_HEIGHT*2)
	  && ((ex!=0)||(ey!=0))) {
        if (ABS(ex)*BLOCK_HEIGHT >= ABS(ey)*BLOCK_WIDTH)  {
          if (ex<0) {
	    ptr->dir=GoLeft; 
	  } else { 
	    ptr->dir=GoRight;
	  }
        } else {
          if (ey<0) {
	    ptr->dir=GoUp; 
	  } else {
	    ptr->dir=GoDown;
	  }
        }
      }
    }
  }
}

/*
 * end of file bomb.c
 */

