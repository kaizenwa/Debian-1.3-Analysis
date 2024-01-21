/*
 * Program XBLAST V2.1.9 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 25th 1996
 * started August 1993
 *
 * File: shrink.c 
 * shrink & scramble functions
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

#define _SHRINK_C

#include <stdio.h>
#include <stdlib.h>

#include "const.h"
#include "include.h"
#include "mytypes.h"
#include "main.h"
#include "maze.h"
#include "map.h"
#include "bomb.h"
#include "info.h"
#include "shrink.h"
#include "shrinkdat.h"

#if defined(XBLAST_SOUND)
#include "sound.h"

#if 0
#define SHRINK_WARN_OFFSET1   35
#define SHRINK_WARN_OFFSET2   (SHRINK_WARN_OFFSET1 + 45)
#define SHRINK_WARN_OFFSET3   (SHRINK_WARN_OFFSET2 + 45)
#else
#define SHRINK_WARN_OFFSET    55
#endif

static int shrink_sound       = SND_SPIRAL;
static int sound_flipflop     = TRUE;
static int play_shrink        = TRUE;

#endif

/*
 * local variables
 */

static ShrinkGeneric *shrink_ptr = NULL;
static ShrinkGeneric *shrink_data = NULL;
static ScrambleStruct *scr_draw;
static ScrambleStruct *scr_del;



/*
 * local constants
 */

static ShrinkGeneric shrink_data_none[] = {
  /* terminator */
  {2*GAME_TIME , 0, 0, 0},
};

/*
 * public function setup_shrink
 */
#ifdef __STDC__
void
setup_shrink (BMShrinkData *cur_level)
#else
void
setup_shrink (cur_level)
     BMShrinkData *cur_level;
#endif
{
  /* set info */
  set_info_shrink(cur_level);
  /* initialize scrambles */
  scr_draw = &(cur_level->scrdraw);
  scr_del  = &(cur_level->scrdel);
  /* create shrink data */
  (*cur_level->shrink_func)();
}

/*
 * local function cmp_shrink
 */
#ifdef __STDC__
static int
cmp_shrink (void *a, 
	    void *b)
{
  return ( ((ShrinkGeneric *)a)->time - ((ShrinkGeneric *)b)->time);
}
#else
static int
cmp_shrink (a, b)
     ShrinkGeneric *a, *b;
{
  return (a->time - b->time);
}
#endif



/*
 * local function sort_shrink_array
 */
#ifdef __STDC__
static void
sort_shrink_array (ShrinkGeneric *data,
		   int nelem)
#else
static void
sort_shrink_array (data, nelem)
     ShrinkGeneric *data;
     int nelem;
#endif
{
#ifdef __STDC__
  qsort(data, nelem, sizeof(ShrinkGeneric), 
	(int (*)(const void *_a,const void *_b)) cmp_shrink);
#else
  qsort(data, nelem, sizeof(ShrinkGeneric), cmp_shrink);
#endif
}

/*
 * local function do_shrink
 */
#ifdef __STDC__
void
do_shrink (int g_time)
#else
void
do_shrink (g_time)
     int g_time;
#endif
{
#if defined(XBLAST_SOUND)
#if 0
  if (g_time == shrink_ptr->time - SHRINK_WARN_OFFSET1 ||
      g_time == shrink_ptr->time - SHRINK_WARN_OFFSET2 ||
      g_time == shrink_ptr->time - SHRINK_WARN_OFFSET3) {
    play_sound(SND_WARN, SOUND_MIDDLE_POSITION);
  }
#else
  if (g_time == shrink_ptr->time - SHRINK_WARN_OFFSET) {
    play_sound(SND_WARN, SOUND_MIDDLE_POSITION);
  }
#endif
#endif

  while ( (g_time) == shrink_ptr->time ) {
    /* set block */
    set_maze_block(shrink_ptr->x, shrink_ptr->y, shrink_ptr->block);

#if defined(XBLAST_SOUND)
    if (play_shrink == TRUE) {
      if (sound_flipflop == TRUE) {
	play_sound(shrink_sound, (shrink_ptr->x * BLOCK_WIDTH) / 
		   (PIXW / MAX_SOUND_POSITION));
	sound_flipflop = FALSE;
	play_shrink = FALSE;
      } else {
	sound_flipflop = TRUE;
      }
    }
#endif
    
    /* for solid blocks kill players and delete bombs */
    if ( (shrink_ptr->block == BTBlock) || (shrink_ptr->block == BTExtra) ) {
      kill_player_at(shrink_ptr->x, shrink_ptr->y);
      delete_bomb_at(shrink_ptr->x,shrink_ptr->y);
    }
    shrink_ptr ++;
  }
#if defined(XBLAST_SOUND)
  play_shrink = TRUE;
#endif
}

/*
 * scramble block code
 */

/*
 * public function do scramble
 */
#ifdef __STDC__
void
do_scramble2 (int g_time)
#else
void
do_scramble2 (g_time)
     int g_time;
#endif
{
  int i;

  /* check for raising blocks */
  if (g_time == (scr_draw->time - 4)) {
    for (i=0; i < scr_draw->num_blocks; i++) {
      set_maze_block(scr_draw->blocks[i].x, scr_draw->blocks[i].y, BTBlockRise);
      kill_player_at(scr_draw->blocks[i].x, scr_draw->blocks[i].y);
    }
  /* check for drawing blocks */
  } else if (g_time == scr_draw->time) {
    for (i=0; i < scr_draw->num_blocks; i++) {
      set_maze_block(scr_draw->blocks[i].x, scr_draw->blocks[i].y, BTBlock);
      kill_player_at(scr_draw->blocks[i].x, scr_draw->blocks[i].y);
    }
  }

  /* check for deleting blocks */
  if (g_time == scr_del->time) {
    for (i=0; i < scr_del->num_blocks; i++) {
      set_maze_block(scr_del->blocks[i].x, scr_del->blocks[i].y, BTFree);
    }
  }
}

/* Generic shrink function */
#ifdef __STDC__
static void
create_generic_shrink (shri_data *data,
		       shri_xoff_data *xoffdata,
		       int startlevel,
		       int endlevel,
		       int inclevel,
		       int starttime,
		       int levelinctime,
		       int offsetinctime,
		       int flags,
		       shri_style *style)
#else
static void
create_generic_shrink (data, xoffdata, startlevel, endlevel, inclevel,
     starttime, levelinctime, offsetinctime, flags, style)
     shri_data *data;
     shri_xoff_data *xoffdata;
     int startlevel, endlevel, inclevel;
     int starttime, levelinctime, offsetinctime;
     int flags;
     shri_style *style;
#endif
{
  int nelem;
  int i, st;
  ShrinkGeneric *dst;
  int acelm;

  int num_styles;
  int inclevel2;

  int st_offset;
  int st_level;
  int block;
  int offset;
  int xoff;
  int bigxoff;

  int reallevel;
  int dellevel;
  int levelstep;

#ifdef DEBUG_SHRINK
  printf("*GDS* Call\n");
  printf("startlevel: %3d endlevel: %3d inclevel: %3d starttime: %3d",
       startlevel, endlevel, inclevel, starttime);
  printf("levelinctime: %3d offsetinctime: %3d flags: %1d\n",
       levelinctime, offsetinctime, flags);
#endif

  /* free last allocated data block */
  if (NULL != shrink_data) {
    free(shrink_data);
  }

  /* Calculate the space required */
  /* This is the maximum that could be used, without checking for levels
     etc. */
  num_styles = style->num;
  for (nelem=0; data[nelem].x != -1; nelem++) {   /* per style*/
  }
  nelem *= num_styles;     /* Total elements */
  nelem++; /* Plus a terminator */
#ifdef DEBUG_SHRINK
  printf("*GDS* Numstyles = %d nelem = %d\n",num_styles,nelem);
#endif

#ifdef DEBUG_SHRINK
  fprintf(stderr, "A total of %d blocks are needed\n", nelem);
#endif
  
  /* alloc blocks memory*/
  if (NULL == 
      (shrink_data = (ShrinkGeneric *) malloc(nelem*sizeof(ShrinkGeneric)))) {
    fprintf(stderr, "Alloc of shrink data failed\n");
    exit (3);
  }

  dst = shrink_data; /* Pointer to current entry */

  acelm = 0;
  /* To adjust xoff for start level */
  if (flags) {
    bigxoff = xoffdata[startlevel];
  } else {
    bigxoff = 0;
  }

  for (st=0; st<num_styles; st++) {
    st_offset = style->styl[st].offset;
    st_level = style->styl[st].level;
    block = style->styl[st].block;

    for (i=0; data[i].x != -1; i++) {
      offset = data[i].offset;

      reallevel = data[i].level - st_level;

      /* Check if in range */
      if ( ((inclevel>0)
          && (reallevel >= startlevel) && (reallevel <= endlevel))

      || ((inclevel<0)
          && (reallevel <= startlevel) && (reallevel >= endlevel)) ) {

        dellevel = reallevel - startlevel;

        inclevel2 = inclevel;
        if ((dellevel < 0) && (inclevel < 0)) {
          dellevel = -dellevel;
          inclevel2 = inclevel2;
        }
        if ((dellevel >= 0) && (inclevel >= 0) && (!(dellevel % inclevel))) {
          levelstep = dellevel / inclevel;
          if (flags) {
            xoff = xoffdata[reallevel];
#if defined(XBLAST_SOUND)
	    shrink_sound = SND_SPIRAL;
#endif
          } else {
            xoff = 0;
#if defined(XBLAST_SOUND)
	    shrink_sound = SND_COMPOUND;
#endif
          }
          dst->time = starttime + (levelstep * levelinctime)
               + ((offset + xoff - bigxoff)* offsetinctime) + st_offset;
          dst->x = data[i].x;
          dst->y = data[i].y;
          dst->block = block;
#ifdef DEBUG_SHRINK
          printf("*GDS* dst: %9d i: %3d st: %2d Time: %5d X: %3d Y: %3d Block: %2d\n",
               (int)dst, i, st, dst->time, dst->x, dst->y,
               dst->block);
#endif
          dst++; /* Get ready for next one */
          acelm++;
        }
      }
    }
  }
  dst->time = GAME_TIME*2;
  dst->x = 0;
  dst->y = 0;
  dst->block = 0;
  acelm++;
  sort_shrink_array(shrink_data, acelm);
  shrink_ptr = shrink_data;
}

#ifdef __STDC__
static void
create_spiral_shrink (int startlevel,
                      int endlevel,
                      int starttime,
                      int speed,
                      shri_style *style)
#else
static void
create_spiral_shrink (startlevel, endlevel, starttime, speed, style)
     int startlevel, endlevel;
     int starttime, speed;
     shri_style *style;
#endif
{
  create_generic_shrink(spiral_shri_data, spiral_xoff, startlevel, endlevel,
       1, starttime, 0, speed, TRUE, style);
}

#ifdef __STDC__
static void
create_quad_shrink (int startlevel,
                    int endlevel,
                    int starttime,
                    int speed,
                    shri_style *style)
#else
static void
create_quad_shrink (startlevel, endlevel, starttime, speed, style)
     int startlevel, endlevel;
     int starttime, speed;
     shri_style *style;
#endif
{
  create_generic_shrink(quad_shri_data, quad_xoff, startlevel, endlevel,
       1, starttime, 0, speed, TRUE, style);
}

#ifdef __STDC__
static void
create_wave_shrink (int startlevel,
                    int endlevel,
                    int starttime,
                    int speed,
                    shri_style *style)
#else
static void
create_wave_shrink (startlevel, endlevel, starttime, speed, style)
     int startlevel, endlevel;
     int starttime, speed;
     shri_style *style;
#endif
{
  create_generic_shrink(quad_shri_data, quad_xoff, startlevel, endlevel,
       1, starttime, speed, speed, FALSE, style);
}

#ifdef __STDC__
static void
create_compound_shrink (int startlevel,
			int endlevel,
			int starttime,
			int speed,
			shri_style *style)
#else
static void
create_compound_shrink (startlevel, endlevel, starttime, speed, style)
     int startlevel, endlevel;
     int starttime, speed;
     shri_style *style;
#endif
{
  create_generic_shrink(spiral_shri_data, spiral_xoff, startlevel, endlevel,
       1, starttime, speed, 0, FALSE, style);
}

#ifdef __STDC__
static void
create_fancy_compound_shrink (int startlevel,
			      int endlevel,
			      int starttime,
			      int speed,
			      shri_style *style)
#else
static void
create_fancy_compound_shrink (startlevel, endlevel, starttime, speed, style)
     int startlevel, endlevel;
     int starttime, speed;
     shri_style *style;
#endif
{
  create_generic_shrink(quad_shri_data, spiral_xoff, startlevel, endlevel,
       1, starttime, speed, 2, FALSE, style);
}


#if 0
#ifdef __STDC__
static void
create_savage_compound_shrink (int startlevel,
			       int endlevel,
			       int starttime,
			       int speed,
			       shri_style *style)
#else
static void
create_savage_compound_shrink (startlevel, endlevel, starttime, speed, style)
     int startlevel, endlevel;
     int starttime, speed;
     shri_style *style;
#endif
{
  create_generic_shrink(spiral_shri_data, spiral_xoff, startlevel, endlevel,
			2, starttime, speed, 0, FALSE, style);
}
#endif


#ifdef __STDC__
static void
create_vertical_shrink (int startlevel,
			int endlevel,
			int inc,
			int starttime,
			int speed,
			shri_style *style)
#else
static void
create_vertical_shrink (startlevel, endlevel, inc, starttime, speed, style)
     int startlevel, endlevel, inc;
     int starttime, speed;
     shri_style *style;
#endif
{
  create_generic_shrink(vertical_shri_data, vertical_xoff, 
			startlevel, endlevel,
			inc, starttime, speed, 0, FALSE, style);
}



#ifdef __STDC__
static void
create_fancy_vertical_shrink (int startlevel,
			      int endlevel,
			      int inc,
			      int starttime,
			      int speed,
			      shri_style *style)
#else
static void
create_fancy_vertical_shrink (startlevel, endlevel, inc, starttime, speed, 
			      style)
     int startlevel, endlevel, inc;
     int starttime, speed;
     shri_style *style;
#endif
{
  create_generic_shrink(vertical_shri_data, vertical_xoff, 
			startlevel, endlevel,
			inc, starttime, speed, 1, FALSE, style);
}

/* Now the actual shrink functions */

/* no shrink func */
#ifdef __STDC__
void
shrink_void (void)
#else
void
shrink_void ()
#endif
{
  shrink_ptr = shrink_data_none;
}

/* 2 level spiral shrinking */
#ifdef __STDC__
void
shrink_spiral (void)
#else
void
shrink_spiral ()
#endif
{
  create_spiral_shrink(1,2,GAME_TIME/2,4,&style_rise_2);
}

/* 2 level speed spiral shrinking */
#ifdef __STDC__
void
shrink_speed_spiral (void)
#else
void
shrink_speed_spiral ()
#endif
{
  create_spiral_shrink(1,2,GAME_TIME/2,2,&style_rise_2);
}

/* 2 level shrinking (plus) */
#ifdef __STDC__
void
shrink_spiral_plus (void)
#else
void
shrink_spiral_plus ()
#endif
{
  create_spiral_shrink(0,2,GAME_TIME/2,4,&style_rise_2_plus);
}

/* 3 level spiral shrinking */
#ifdef __STDC__
void
shrink_spiral_3 (void)
#else
void
shrink_spiral_3 ()
#endif
{
  create_spiral_shrink(1,3,GAME_TIME/2,4,&style_rise_2);
}

/* An early spiral shrink (2 levels) */
#ifdef __STDC__
void
shrink_early_spiral (void)
#else
void
shrink_early_spiral ()
#endif
{
  create_spiral_shrink(1,2,3*GAME_TIME/8,4,&style_rise_2);
}



/* Full compound shrinking */
#ifdef __STDC__
void
shrink_compound (void)
#else
void
shrink_compound ()
#endif
{
  create_compound_shrink(1,5,GAME_TIME/6,GAME_TIME/6,&style_compound);
}

/* Fancy full compound shrinking */
#ifdef __STDC__
void
shrink_compound_f (void)
#else
void
shrink_compound_f ()
#endif
{
  create_fancy_compound_shrink(1,5,GAME_TIME/6,GAME_TIME/6,&style_compound);
}

/* Fancy 2 level compound shrinking */
#ifdef __STDC__
void
shrink_compound_2_f (void)
#else
void
shrink_compound_2_f ()
#endif
{
  create_fancy_compound_shrink(1,2,GAME_TIME/2,GAME_TIME/6,&style_compound);
}

/* Fancy lazy full compound shrinking */
#ifdef __STDC__
void
shrink_lazy_compound_f (void)
#else
void
shrink_lazy_compound_f ()
#endif
{
  create_fancy_compound_shrink(1,3,GAME_TIME/3,GAME_TIME/3,&style_compound_solid);
}

/* Full solid compound shrinking */
/* (Gives a warning and then rises blocks) */
#ifdef __STDC__
void
shrink_compound_solid (void)
#else
void
shrink_compound_solid ()
#endif
{
  create_compound_shrink(1,5,GAME_TIME/6,GAME_TIME/6,&style_compound_solid);
}

/* Full savage compound shrinking */
/* Two levels at a time */
#ifdef __STDC__
void
shrink_savage_compound (void)
#else
void
shrink_savage_compound ()
#endif
{
  create_compound_shrink(2,5,GAME_TIME/6,GAME_TIME/3,&style_savage_compound);
}

/* Full compound shrinking with extras */
#ifdef __STDC__
void
shrink_compound_extra (void)
#else
void
shrink_compound_extra ()
#endif
{
  create_compound_shrink(2,5,GAME_TIME/6,GAME_TIME/6,&style_compound_extra);
}

/* Full downward shrinking */
#ifdef __STDC__
void
shrink_down (void)
#else
void
shrink_down ()
#endif
{
  create_vertical_shrink(1,11,1,GAME_TIME/11,GAME_TIME/11,&style_compound);
}

/* Fancy full downward shrinking */
#ifdef __STDC__
void
shrink_down_f (void)
#else
void
shrink_down_f ()
#endif
{
  create_fancy_vertical_shrink(1,11,1,GAME_TIME/11,GAME_TIME/11,&style_compound);
}

/* 2 level Quad shrink */
#ifdef __STDC__
void
shrink_quad (void)
#else
void
shrink_quad ()
#endif
{
  create_quad_shrink(1,3,GAME_TIME/2,16,&style_rise_2);
} 

/* A wave of inward moving blocks (3 levels) */
#ifdef __STDC__
void
shrink_constrict_wave (void)
#else
void
shrink_constrict_wave ()
#endif
{
  create_wave_shrink(1,3,GAME_TIME/2,5,&style_rise_2_plus);
} 

/*
 * end of file shrink.c
 */




