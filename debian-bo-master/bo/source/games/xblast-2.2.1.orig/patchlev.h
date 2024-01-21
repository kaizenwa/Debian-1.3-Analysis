/*
 * Programm XBLAST V2.2.1
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: patchlev.h
 * just the version number 
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
 * Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef _MAIN_C
static char win_string1[] = "XBlast 2.2.1 - %s";
static char win_string2[] = "XBlast 2.2.1 - %s & %s";
#ifdef XBLAST_SOUND
static char c_string[] = 
"XBlast 2.2.1 Sound Copyright (c) 1993-97 Oliver Vogel";
#else
static char c_string[] = 
"XBlast 2.2.1 Copyright (c) 1993-97 Oliver Vogel";
#endif
#endif
#ifdef _INTRO_C
#ifdef XBLAST_SOUND
#define NUM_XC 9
#else
#define NUM_XC 7
#endif
static char *xc_string[NUM_XC] = {
  NULL,
  NULL,
  "XBlast  2.2.1",
  "Copyright \251 1993-97 Oliver Vogel",
  "(vogel@ikp.uni-koeln.de)",
  "Coauthor Garth Denley",
  "(g0denley@teaching.cs.adelaide.edu.au)",
#ifdef XBLAST_SOUND
  "Sound by Norbert Nicolay",
  "(nicolay@ikp.uni-koeln.de)",
#endif
};
#endif

/*
 * end of file patchlev.h
 */
