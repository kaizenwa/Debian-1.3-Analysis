/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

     This file shamelessly stolen from WorkBone or so...

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

#define MAX_NAMELEN 128

/*
 * Structure for a single track.  This is pretty much self-explanatory --
 * one of these exists for each track on the current CD.
 */
struct trackinfo {
	char	songname[MAX_NAMELEN];	/* Name of song */
	int	length;		/* Length of track in seconds or Kbytes */
	int	start;		/* Starting position (f+s*75+m*60*75) */
	int	elapsed;	/* Elapsed time since start, according to
				 * playlist! */
	int	volume;		/* Per-track volume (1-32, 0 to disable) */
	int	track;		/* Physical track number */
	int	section;	/* Section number (0 if track not split) */
	char	contd;		/* Flag: continuation of previous track */
	char	avoid;		/* Flag: don't play this track. */
	char	data;		/* Flag: data track */
};

struct cdinfo {
	char	cdname[MAX_NAMELEN];	/* Disc's name */
	int	ntracks;	/* Number of tracks on the disc */
	int	length;		/* Total running time in seconds */
	int	autoplay;	/* Start playing CD immediately */
	int	playmode;	/* How to play the CD */
	int	volume;		/* Default volume (1-32, 0 for none) */
	struct trackinfo *trk;	/* struct trackinfo[ntracks] */
};

/* The global variable "cd" points to the struct for the CD that's playing. */
extern struct cdinfo *cd;

#define CDNULL  0
#define CDPLAY  1
#define CDPAUSE 3
#define CDSTOP  4
#define CDEJECT 5
#define CDMOUNT 6
#define CDERROR 7
