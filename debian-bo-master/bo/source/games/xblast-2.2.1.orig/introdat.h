/*
 * Programm XBLAST V2.1.7 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 1st 1996
 * started August 1993
 *
 * File: introdat.h
 * data file for intro.c
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

#ifndef _INTRODAT_H
#define _INTRODAT_H

/*
 * data points for X-polygon
 */
static BMPoint pointx[SIZE_OF_X] = 
{
  { 0., 0. },
  { 0.2, 0.},
  { 0.5, 0.3125},
  { 0.8, 0.},

  { 1.0, 0.},
  { 1.0, 0.1667},
  { 0.68, 0.5 },
  { 1.0, 0.8333},

  { 1.0, 1.0 },
  { 0.8, 1.0 },
  { 0.5, 0.6875},
  { 0.2, 1.0 },

  { 0.0, 1.0 },
  { 0.0, 0.8333},
  { 0.32, 0.5 },
  { 0.0, 0.1667 }
};

/*
 * explosion data for letter animations
 */

/* Letter A */
static int Block_A[CHAR_ANIME][CHARH][CHARW] = {
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x10, 0x00, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x14, 0x00, 0x00 },
    { 0x11, 0x00, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x11, 0x00, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x14, 0x00, 0x00 },
    { 0x17, 0x18, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x11, 0x00, 0x00 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x17, 0x1a, 0x18 },
    { 0x15, 0x00, 0x00 },
    { 0x11, 0x00, 0x00 },
  },
  {
    { 0x16, 0x18, 0x00 },
    { 0x15, 0x00, 0x14 },
    { 0x17, 0x1a, 0x1d },
    { 0x15, 0x00, 0x11 },
    { 0x11, 0x00, 0x00 },
  },
  {
    { 0x16, 0x1a, 0x1c },
    { 0x15, 0x00, 0x15 },
    { 0x17, 0x1a, 0x1d },
    { 0x15, 0x00, 0x15 },
    { 0x11, 0x00, 0x11 },
  },
};

/* letter B */
static int Block_B[CHAR_ANIME][CHARH][CHARW] = {
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x10, 0x00, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x14, 0x00, 0x00 },
    { 0x1b, 0x18, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x1b, 0x1a, 0x18 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x14, 0x00, 0x00 },
    { 0x17, 0x18, 0x00 },
    { 0x15, 0x00, 0x14 },
    { 0x1b, 0x1a, 0x19 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x17, 0x1a, 0x1c },
    { 0x15, 0x00, 0x15 },
    { 0x1b, 0x1a, 0x19 },
  },
  {
    { 0x1e, 0x18, 0x00 },
    { 0x15, 0x00, 0x14 },
    { 0x17, 0x1a, 0x1d },
    { 0x15, 0x00, 0x15 },
    { 0x1b, 0x1a, 0x19 },
  },
  {
    { 0x1e, 0x1a, 0x1c },
    { 0x15, 0x00, 0x15 },
    { 0x17, 0x1a, 0x1d },
    { 0x15, 0x00, 0x15 },
    { 0x1b, 0x1a, 0x19 }
  }
};

/* letter L */
static int Block_L[CHAR_ANIME][CHARH][CHARW] = {
  {
    { 0x10, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x11, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x11, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x11, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x11, 0x00, 0x00 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x13, 0x18, 0x00 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x13, 0x1a, 0x18 },
  },
};

/* Letter S */
int Block_S[CHAR_ANIME][CHARH][CHARW] = {
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x10, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x14, 0x00, 0x00 },
    { 0x13, 0x18, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
  },
  {
    { 0x14, 0x00, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x13, 0x1a, 0x18 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
  },
  {
    { 0x16, 0x18, 0x00 },
    { 0x15, 0x00, 0x00 },
    { 0x13, 0x1a, 0x1c },
    { 0x00, 0x00, 0x11 },
    { 0x00, 0x00, 0x00 },
  },
  {
    { 0x16, 0x1a, 0x18 },
    { 0x15, 0x00, 0x00 },
    { 0x13, 0x1a, 0x1c },
    { 0x00, 0x00, 0x15 },
    { 0x00, 0x00, 0x11 },
  },
  {
    { 0x16, 0x1a, 0x18 },
    { 0x15, 0x00, 0x00 },
    { 0x13, 0x1a, 0x1c },
    { 0x00, 0x00, 0x15 },
    { 0x00, 0x12, 0x19 },
  },
  {
    { 0x16, 0x1a, 0x18 },
    { 0x15, 0x00, 0x00 },
    { 0x13, 0x1a, 0x1c },
    { 0x00, 0x00, 0x15 },
    { 0x12, 0x1a, 0x19 },
  },
};

/* Letter T */
int Block_T[CHAR_ANIME][CHARH][CHARW] = {
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x10, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x14, 0x00 },
    { 0x00, 0x11, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x14, 0x00 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x11, 0x00 },
  },
  {
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x14, 0x00 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x11, 0x00 },
  },
  {
    { 0x00, 0x14, 0x00 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x11, 0x00 },
  },
  {
    { 0x12, 0x1e, 0x18 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x15, 0x00 },
    { 0x00, 0x11, 0x00 },
  },
};

/*
 * coordinates and boxes for intro screen
 */
static BMPosition intro_player_pos[MAX_PLAYER] = {
  { 1*BLOCK_HEIGHT,   4*BLOCK_WIDTH },
  { 1*BLOCK_HEIGHT,  10*BLOCK_WIDTH },
  { 5*BLOCK_HEIGHT,  10*BLOCK_WIDTH },
  { 5*BLOCK_HEIGHT,   4*BLOCK_WIDTH },
  { 3*BLOCK_HEIGHT,   4*BLOCK_WIDTH },
  { 3*BLOCK_HEIGHT,  10*BLOCK_WIDTH },
};

static BMPosition control_pos[MAX_PLAYER][4] = {
  { {2,4},  {1,1},  {1,2},  {1,3} },
  { {2,10}, {1,11}, {1,12}, {1,13} },
  { {6,10}, {5,11}, {5,12}, {5,13} },
  { {6,4},  {5,1},  {5,2},  {5,3} },
  { {4,4},  {3,1},  {3,2},  {3,3} },
  { {4,10}, {3,11}, {3,12}, {3,13} },
};

/*
 * coordinates for text boxes
 */


/* winner name box */
static BMRectangle winner_box = {
  25*BLOCK_WIDTH/4, 49*BLOCK_HEIGHT/6,
  5*BLOCK_WIDTH/2,  2*BLOCK_HEIGHT/3, 
};
static BMRectangle two_winner_box = {
  21*BLOCK_WIDTH/4, 49*BLOCK_HEIGHT/6,
  9*BLOCK_WIDTH/2,  2*BLOCK_HEIGHT/3, 
};

/* boxes for intro text */
static BMRectangle intro_box[] = {
  {
    0,    6*BLOCK_HEIGHT, 
    PIXW, 2*BLOCK_HEIGHT/3,
  },
  {
    0,    7*BLOCK_HEIGHT, 
    PIXW, 2*BLOCK_HEIGHT/3,
  },
  {
    0,    PIXH+8-BLOCK_HEIGHT, 
    PIXW, 2*BLOCK_HEIGHT/3,
  },
};


/* 
 * box for copyright string 
 */
#ifdef XBLAST_SOUND
static BMRectangle xc_box[] = {
  {
    35*BLOCK_WIDTH/8, 58*BLOCK_HEIGHT/8,
    25*BLOCK_WIDTH/4, 22*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 59*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 21*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 59*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 4*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 67*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 3*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 73*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 4*BLOCK_HEIGHT/8,
  },
  {
    18*BLOCK_WIDTH/4, 77*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 3*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 83*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 4*BLOCK_HEIGHT/8,
  },
  {
    18*BLOCK_WIDTH/4, 87*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 3*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 93*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 4*BLOCK_HEIGHT/8,
  },
}; 
#else
static BMRectangle xc_box[] = {
  {
    35*BLOCK_WIDTH/8, 68*BLOCK_HEIGHT/8,
    25*BLOCK_WIDTH/4, 16*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 69*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 15*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 69*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 4*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 77*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 3*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 83*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 4*BLOCK_HEIGHT/8,
  },
  {
    18*BLOCK_WIDTH/4, 87*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 3*BLOCK_HEIGHT/4,
  },
  {
    18*BLOCK_WIDTH/4, 93*BLOCK_HEIGHT/8,
    12*BLOCK_WIDTH/2, 4*BLOCK_HEIGHT/8,
  },
}; 
#endif

static int xc_flags[] = {
  FF_Large | FF_Boxed | FF_Black,
  FF_Large | FF_Boxed | FF_White,
  FF_Large | FF_White,
  FF_Medium| FF_White,
  FF_Small | FF_White,
  FF_Medium| FF_White,
  FF_Small | FF_White,
#ifdef XBLAST_SOUND
  FF_Medium| FF_White,
  FF_Small | FF_White,
#endif
};


/* 
 * boxes for intro player names 
 */
static BMRectangle intro_player_box[MAX_PLAYER] = {
  {
    BLOCK_WIDTH,     2*BLOCK_HEIGHT,
    3*BLOCK_WIDTH-1, 40,
  },
  {
    11*BLOCK_WIDTH,  2*BLOCK_HEIGHT, 
    3*BLOCK_WIDTH-1, 40,
  },
  {
    11*BLOCK_WIDTH,  6*BLOCK_HEIGHT, 
    3*BLOCK_WIDTH-1, 40,
  },
  {
    BLOCK_WIDTH,     6*BLOCK_HEIGHT, 
    3*BLOCK_WIDTH-1, 40,
  },
  {
    BLOCK_WIDTH,     4*BLOCK_HEIGHT,
    3*BLOCK_WIDTH-1, 40,
  },
  {
    11*BLOCK_WIDTH,  4*BLOCK_HEIGHT, 
    3*BLOCK_WIDTH-1, 40,
  },
};

/* 
 * display id box 
 */
static BMRectangle disp_box[MAX_PLAYER] = {
  {
    5*BLOCK_WIDTH/4,   4*BLOCK_HEIGHT/3,
    7*BLOCK_WIDTH/16,  5*BLOCK_HEIGHT/12,
  },
  {
    11*BLOCK_WIDTH+16, 4*BLOCK_HEIGHT/3,
    7*BLOCK_WIDTH/16,  5*BLOCK_HEIGHT/12,
  },
  {
    11*BLOCK_WIDTH+16, 16*BLOCK_HEIGHT/3,
    7*BLOCK_WIDTH/16,  5*BLOCK_HEIGHT/12,
  },
  {
    1*BLOCK_WIDTH+16,  16*BLOCK_HEIGHT/3,
    7*BLOCK_WIDTH/16,  5*BLOCK_HEIGHT/12,
  },
  {
    5*BLOCK_WIDTH/4,   10*BLOCK_HEIGHT/3,
    7*BLOCK_WIDTH/16,  5*BLOCK_HEIGHT/12,
  },
  {
    11*BLOCK_WIDTH+16, 10*BLOCK_HEIGHT/3,
    7*BLOCK_WIDTH/16,  5*BLOCK_HEIGHT/12,
  },
};

/*
 * team tag boxes for title in "two on two" mode
 */
static BMRectangle team_box[] = {
  {
    12*BLOCK_WIDTH/2,  5*BLOCK_HEIGHT/3,
     3*BLOCK_WIDTH,    2*BLOCK_HEIGHT/3
  },
  {
    12*BLOCK_WIDTH/2, 17*BLOCK_HEIGHT/3,
     3*BLOCK_WIDTH,    2*BLOCK_HEIGHT/3
  },
  {
    12*BLOCK_WIDTH/2, 11*BLOCK_HEIGHT/3,
     3*BLOCK_WIDTH,    2*BLOCK_HEIGHT/3
  },
};
static BMRectangle team_box2[] = {
  {
    12*BLOCK_WIDTH/2 -   BASE_X,  5*BLOCK_HEIGHT/3 -   BASE_Y,
     3*BLOCK_WIDTH   + 2*BASE_X,  2*BLOCK_HEIGHT/3 + 2*BASE_Y,
  },
  {
    12*BLOCK_WIDTH/2 -   BASE_X, 17*BLOCK_HEIGHT/3 -   BASE_Y,
     3*BLOCK_WIDTH   + 2*BASE_X,  2*BLOCK_HEIGHT/3 + 2*BASE_Y,
  },
  {
    12*BLOCK_WIDTH/2 -   BASE_X, 11*BLOCK_HEIGHT/3 -   BASE_Y,
     3*BLOCK_WIDTH   + 2*BASE_X,  2*BLOCK_HEIGHT/3 + 2*BASE_Y,
  },
};
static char *team_string[] = {
  "TEAM 1",
  "TEAM 2",
  "TEAM 3",
};

/* 
 * level title boxes 
 */
static BMRectangle title_box[] = {
  {
    13*BLOCK_WIDTH/4, BLOCK_HEIGHT/2-2,
    17*BLOCK_WIDTH/2, BLOCK_HEIGHT+4,
  },
  {
    13*BLOCK_WIDTH/4, BLOCK_HEIGHT/2-2,
    17*BLOCK_WIDTH/2, 2*BLOCK_HEIGHT/3,
  },
  {
    13*BLOCK_WIDTH/4, 7*BLOCK_HEIGHT/6-2,
    17*BLOCK_WIDTH/2, BLOCK_HEIGHT/3,
  },
};

/* 
 * player tags at score board
 */
static BMRectangle score_player_box2[] = {
  {
    BLOCK_WIDTH/4,   25*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   41*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
};
static BMRectangle score_player_box3[] = {
  {
    BLOCK_WIDTH/4,   21*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   33*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   45*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
};
static BMRectangle score_player_box4[] = {
  {
    BLOCK_WIDTH/4,   21*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   29*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   37*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   45*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
};
static BMRectangle score_player_box5[] = {
  {
    BLOCK_WIDTH/4,   17*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   25*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   33*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   41*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   49*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
};
static BMRectangle score_player_box6[] = {
  {
    BLOCK_WIDTH/4,   9*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   17*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   25*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   33*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   41*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   49*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
};
static BMRectangle score_player_box2x2[] = {
  {
    BLOCK_WIDTH/4,   25*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   25*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   41*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   41*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
};
static BMRectangle score_player_box3x2[] = {
  {
    BLOCK_WIDTH/4,   21*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   21*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   33*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   33*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   45*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
  {
    BLOCK_WIDTH/4,   45*BLOCK_HEIGHT/4,
    5*BLOCK_WIDTH/2, 7*BLOCK_HEIGHT/12,
  },
};
static BMRectangle *score_player_box[] = {
  score_player_box2,
  score_player_box3,
  score_player_box4,
  score_player_box5,
  score_player_box6,
  score_player_box2x2,
  score_player_box3x2,
};

/* 
 * player info boxes 
 */
static BMRectangle player_info_frame= {
  3 *BLOCK_WIDTH/8,  5*BLOCK_HEIGHT/2,
  17*BLOCK_WIDTH/4,  83*BLOCK_HEIGHT/12,
};
static BMRectangle player_info_box[MAX_INFO+1] = {
  /* header */
  {
    BLOCK_WIDTH/2,  8*BLOCK_HEIGHT/3,
    4*BLOCK_WIDTH, 2*BLOCK_HEIGHT/3,
  },
  /* info 1-6 */
  {
    BLOCK_WIDTH/2,  15*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH, BLOCK_HEIGHT/2,
  },
  {
    BLOCK_WIDTH/2,  19*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH, BLOCK_HEIGHT/2,
  },
  {
    BLOCK_WIDTH/2,  23*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH, BLOCK_HEIGHT/2,
  },
  {
    BLOCK_WIDTH/2,  27*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH, BLOCK_HEIGHT/2,
  },
  {
    BLOCK_WIDTH/2,  31*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH, BLOCK_HEIGHT/2,
  },
  {
    BLOCK_WIDTH/2,  35*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH, BLOCK_HEIGHT/2,
  },
};


/* 
 * level info boxes 
 */
static BMRectangle level_info_frame= {
  41 *BLOCK_WIDTH/8,  5*BLOCK_HEIGHT/2,
  19*BLOCK_WIDTH/4,  83*BLOCK_HEIGHT/12,
};
static BMRectangle level_info_box[MAX_INFO+1] = {
  /* header */
  {
    21*BLOCK_WIDTH/4, 8*BLOCK_HEIGHT/3,
    9*BLOCK_WIDTH/2,   2*BLOCK_HEIGHT/3,
  },
  /* info 1-6 */
  {
    21*BLOCK_WIDTH/4, 15*BLOCK_HEIGHT/4,
    9*BLOCK_WIDTH/2,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/4, 19*BLOCK_HEIGHT/4,
    9*BLOCK_WIDTH/2,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/4, 23*BLOCK_HEIGHT/4,
    9*BLOCK_WIDTH/2,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/4, 27*BLOCK_HEIGHT/4,
    9*BLOCK_WIDTH/2,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/4, 31*BLOCK_HEIGHT/4,
    9*BLOCK_WIDTH/2,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/4, 35*BLOCK_HEIGHT/4,
    9*BLOCK_WIDTH/2,   BLOCK_HEIGHT/2,
  },
};


/* 
 * extra info boxes 
 */
static BMRectangle extra_info_frame= {
  83 *BLOCK_WIDTH/8,  5*BLOCK_HEIGHT/2,
  17*BLOCK_WIDTH/4,  83*BLOCK_HEIGHT/12,
};
static BMRectangle extra_info_box[MAX_INFO+1] = {
  /* header */
  {
    21*BLOCK_WIDTH/2, 8*BLOCK_HEIGHT/3,
    4*BLOCK_WIDTH,   2*BLOCK_HEIGHT/3,
  },
  /* info 1-6 */
  {
    21*BLOCK_WIDTH/2, 15*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/2, 19*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/2, 23*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/2, 27*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/2, 31*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH,   BLOCK_HEIGHT/2,
  },
  {
    21*BLOCK_WIDTH/2, 35*BLOCK_HEIGHT/4,
    4*BLOCK_WIDTH,   BLOCK_HEIGHT/2,
  },
};


static BMRectangle level_tip_box = {
  5*BLOCK_WIDTH/2, 32*BLOCK_HEIGHT/3,
  10*BLOCK_WIDTH,  2*BLOCK_HEIGHT/3,
};

#endif
/*
 * enf of file introdat.h
 */
