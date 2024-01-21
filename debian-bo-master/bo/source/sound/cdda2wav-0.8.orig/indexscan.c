/*
 * Copyright: GNU Public License 2 applies
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2, or (at your option)
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * CDDA2WAV (C) Heiko Eissfeldt heiko@colossus.escape.de
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>		/* usleep */
#include <ctype.h>
#include <sys/types.h>
#include <linux/cdrom.h>

#include "interface.h"
#include "cdda2wav.h"

static unsigned char g_track=0xff, g_index=0xff;	/* current track, index */
static unsigned char g_minute=0xff, g_seconds=0xff;	/* curr. minute, second */
static unsigned char g_frame=0xff;			/* current frame */


/* print the track currently read */
static void UpdateTrackData (short p_num)
{
  if (quiet == 0) { 
    fprintf (stderr, "\ntrack: %.2d, ", p_num); fflush(stderr);
  }
  g_track = (char)p_num;
}


/* print the index currently read */
static void UpdateIndexData (short p_num)
{
  if (quiet == 0) { 
    fprintf (stderr, "index: %.2d\n", p_num); fflush(stderr);
  }
  g_index = (char)p_num;
}


/* print the time of track currently read */
static void UpdateTimeData (short p_min, short p_sec, short p_frm)
{
  if (quiet == 0) {
    fprintf (stderr, "time: %.2d:%.2d.%.2d\r", p_min, p_sec, p_frm); 
    fflush(stderr);
  }
  g_minute = (char)p_min;
  g_seconds = (char)p_sec;
  g_frame = (char)p_frm;
}

void AnalyzeQchannel ( unsigned frame )
{
    subq_chnl *sub_ch;

    if (trackindex_disp != 0) {
	sub_ch = ReadSubQ(GET_POSITIONDATA,0);
	/* analyze sub Q-channel data */
	if (sub_ch->track != g_track ||
	    sub_ch->index != g_index) {
	    UpdateTrackData (sub_ch->track);
	    UpdateIndexData (sub_ch->index);
	}
    }
    frame += 150;
    UpdateTimeData (frame / (60*75), 
		    (frame % (60*75)) / 75, 
		    frame % 75);
}

unsigned tracks = 0;

long GetStartSector ( unsigned long p_track )
{
  long i;

  for (i = 0; i < tracks; i++) {
    if (g_toc [i].bTrack == p_track) {
      unsigned long dw = g_toc [i].dwStartSector;
      if ((g_toc [i].bFlags & CDROM_DATA_TRACK) != 0)
	return -1;
      return dw;
    }
  }

  return -1;
}


long GetEndSector ( unsigned long p_track )
{
  long i;

  for ( i = 1; i <= tracks; i++ ) {
    if ( g_toc [i-1].bTrack == p_track ) {
      unsigned long dw = g_toc [i].dwStartSector;
      return dw-1;
    }
  }

  return -1;
}

long FirstTrack ( void )
{
  long i;
  
  for ( i = 0; i < tracks; i++ ) {
    if ( g_toc [i].bTrack != CDROM_LEADOUT &&
	( g_toc [i].bFlags & CDROM_DATA_TRACK ) == 0 )
      return g_toc [i].bTrack;
  }
  return 0;
}

long GetLastSectorOnCd( unsigned long p_track )
{
  unsigned int i;
  long LastSec = 0;
  for (i = p_track; i <= tracks; i++) {
    /* break if a nonaudio track follows */
    if ( (g_toc [i-1].bFlags & CDROM_DATA_TRACK) != 0) break;
    LastSec = GetEndSector ( i ) + 1;
  }
  return LastSec;
}

int GetTrack( unsigned long sector )
{
  int i;
  for (i = 0; i < tracks; i++) {
    if (g_toc[i  ].dwStartSector <= sector &&
	g_toc[i+1].dwStartSector > sector)
      return (g_toc [i].bFlags & CDROM_DATA_TRACK) != 0 ? -1 : i+1;
  }
  return -1;
}


void DisplayToc ( void )
{
  unsigned i;
  unsigned long dw;
  unsigned mins;
  unsigned secnds;
  unsigned centi_secnds;	/* hundreds of a second */


  /* get total time */
  dw = (unsigned long) g_toc[tracks].dwStartSector + 150;
  mins	       =       dw / ( 60*75 );
  secnds       =     ( dw % ( 60*75 ) ) / 75;
  centi_secnds = (4* ( dw %      75   ) +1 ) /3; /* convert from 1/75 to 1/100 */
  /* g_toc [i].bFlags contains two fields:
       bits 7-4 (ADR) : 0 no sub-q-channel information
                      : 1 sub-q-channel contains current position
		      : 2 sub-q-channel contains media catalog number
		      : 3 sub-q-channel contains International Standard
		                                 Recording Code ISRC
		      : other values reserved
       bits 3-0 (Control) :
       bit 3 : when set indicates there are 4 audio channels else 2 channels
       bit 2 : when set indicates this is a data track else an audio track
       bit 1 : when set indicates digital copy is permitted else prohibited
       bit 0 : when set indicates pre-emphasis is present else not present
  */

  if ( (verbose & SHOW_SUMMARY) != 0 ) {

    /* summary */
    fputs( "track pre-emphasis copy-permitted tracktype channels sub-Q-channel\n" , stderr);
    i = 0;
    while ( i < tracks ) {
      int from;

      from = g_toc [i].bTrack;
      while ( i < tracks && g_toc [i].bFlags == g_toc [i+1].bFlags ) i++;
      if (i >= tracks) i--;
      
      fprintf(stderr, "%2d-%2d %12s %14s %18s ",from,g_toc [i].bTrack,
			g_toc [i].bFlags & 1 ? "  " : "no",
			g_toc [i].bFlags & 2 ? "  " : "no",
			g_toc [i].bFlags & 4 ? "data          " : 
			g_toc [i].bFlags & 8 ? "audio        4" :
                                               "audio        2"
                       );
      switch ( g_toc [i].bFlags >> 4 ) {
        case 0:  fputs( "no data\n", stderr); break;
	case 1:  fputs( "position\n", stderr); break;
	case 2:  fputs( "media catalog\n", stderr); break;
	case 3:  fputs( "ISRC\n", stderr); break;
	default: fprintf( stderr, "invalid 0x%2x\n", g_toc [i].bFlags >> 4 );
      }
      i++;
    }
    fprintf ( stderr, 
	     "Table of Contents: total tracks:%u, (total time %u:%02u.%02u)\n",
	     tracks, mins, secnds, centi_secnds );

    for ( i = 0; i < tracks; i++ ) {
      if ( (g_toc [i].bFlags & CDROM_DATA_TRACK) != 0 )
	continue;/* skip nonaudio tracks */
      if ( g_toc [i].bTrack <= MAXTRK ) {
	dw = (unsigned long) (g_toc[i+1].dwStartSector - g_toc[i].dwStartSector /* + 150 - 150 */);
	mins         =         dw / ( 60*75 );
	secnds       =       ( dw % ( 60*75 )) / 75;
	centi_secnds = ( 4 * ( dw %      75 ) + 1 ) / 3;
	fprintf ( stderr, " %2u.(%2u:%02u.%02u)", g_toc [i].bTrack,mins,secnds,centi_secnds );
      }
      if ( i < tracks-1 )
	if ( (i+1) % 5 == 0 )
	  fputs( "\n", stderr );
	else
	  fputc ( ',', stderr );
    }
    if ( (i+1) % 5 != 0 )
      fputs( "\n", stderr );
    if ((verbose & SHOW_STARTPOSITIONS) != 0) {
      fputs ("\nTable of Contents: starting sectors\n", stderr);
      for ( i = 0; i < tracks; i++ ) {
	fprintf ( stderr, " %2u.(%8u)", g_toc [i].bTrack, g_toc[i].dwStartSector);
	if ( (i+1) % 5 == 0 )
	  fputs( "\n", stderr );
	else
	  fputc ( ',', stderr );
      }
      fprintf ( stderr, " lead-out(%8u)", g_toc[i].dwStartSector);
      fputs ("\n", stderr);
    }
    if (have_CD_extra != 0) {
      if (quiet == 0)
	fprintf(stderr, "CD-Extra (CD-Plus) detected\n");
      dump_extra_info();
    }
  }
}

void Read_MCN_ISRC(void)
{
  subq_chnl *sub_ch;
  subq_catalog *subq_cat;
  int i;

  if ((verbose & SHOW_MCN) != 0) {
    /* get and display Media Catalog Number ( one per disc ) */
    fprintf(stderr, "scanning for MCN...");
    
    sub_ch = ReadSubQ(GET_CATALOGNUMBER,0);

#if 1 /* TOSHIBA HACK */
#define Toshiba 0
    if (Toshiba != 0 && quiet == 0 && 
	(sub_ch != 0 || (((subq_catalog *)sub_ch->data)->mc_valid & 0x80))) {
      /* no valid MCN yet. do more searching */
      unsigned long h = g_toc[0].dwStartSector;
    
      while (h <= g_toc[0].dwStartSector + 100) {
	if (Toshiba)
	  ReadCdRom(bufferCdda, h, nsectors);
	sub_ch = ReadSubQ(GET_CATALOGNUMBER,0);
	if (sub_ch != NULL) {
	  subq_cat = (subq_catalog *) sub_ch->data;
	  if ((subq_cat->mc_valid & 0x80) != 0) {
	    break;
	  }
	}
	h += nsectors;
      }
    }
#endif

    if (sub_ch != NULL)
      subq_cat = (subq_catalog *)sub_ch->data;
  
    if (sub_ch != NULL && (subq_cat->mc_valid & 0x80) != 0 && quiet == 0) {

      /* unified format guesser:
       * format MCN all digits in bcd
       *     1                                  13
       * A: ab cd ef gh ij kl m0  0  0  0  0  0  0  Plextor 6x Rel. 1.02
       * B: 0a 0b 0c 0d 0e 0f 0g 0h 0i 0j 0k 0l 0m  Toshiba 3401
       * C: AS AS AS AS AS AS AS AS AS AS AS AS AS  ASCII SCSI-2 Plextor 4.5x and 6x Rel. 1.06
       */
      unsigned char *cp = subq_cat->media_catalog_number;
      if (!(cp[8] | cp[9] | cp[10] | cp[11] | cp[12]) &&
	  ((cp[0] & 0xf0) | (cp[1] & 0xf0) | (cp[2] & 0xf0) | 
	   (cp[3] & 0xf0) | (cp[4] & 0xf0) | (cp[5] & 0xf0) | 
	   (cp[6] & 0xf0))) {
	/* reformat A: to B: */
	cp[12] = cp[6] >> 4;
	cp[11] = cp[5] & 0xf;
	cp[10] = cp[5] >> 4;
	cp[ 9] = cp[4] & 0xf;
	cp[ 8] = cp[4] >> 4;
	cp[ 7] = cp[3] & 0xf;
	cp[ 6] = cp[3] >> 4;
	cp[ 5] = cp[2] & 0xf;
	cp[ 4] = cp[2] >> 4;
	cp[ 3] = cp[1] & 0xf;
	cp[ 2] = cp[1] >> 4;
	cp[ 1] = cp[0] & 0xf;
	cp[ 0] = cp[0] >> 4;
      }
      if (!isdigit(cp[0])) {
	if (memcmp(subq_cat->media_catalog_number,"\0\0\0\0\0\0\0\0\0\0\0\0\0",13) != 0)
	  sprintf((char *) subq_cat->media_catalog_number, 
		  "%1.1X%1.1X%1.1X%1.1X%1.1X%1.1X%1.1X%1.1X%1.1X%1.1X%1.1X%1.1X%1.1X", 
		  subq_cat->media_catalog_number [0],
		  subq_cat->media_catalog_number [1],
		  subq_cat->media_catalog_number [2],
		  subq_cat->media_catalog_number [3],
		  subq_cat->media_catalog_number [4],
		  subq_cat->media_catalog_number [5],
		  subq_cat->media_catalog_number [6],
		  subq_cat->media_catalog_number [7],
		  subq_cat->media_catalog_number [8],
		  subq_cat->media_catalog_number [9],
		  subq_cat->media_catalog_number [10],
		  subq_cat->media_catalog_number [11],
		  subq_cat->media_catalog_number [12]
		);
      }
      if (memcmp(subq_cat->media_catalog_number,"0000000000000",13) != 0) {
	strcpy((char *)MCN, (char *)subq_cat->media_catalog_number);
	fprintf(stderr, "\rMedia catalog number: %13.13s\n", subq_cat->media_catalog_number);
      }
    }
  }

  if ((verbose & SHOW_ISRC) != 0) {
    /* get and display Track International Standard Recording Codes 
       (for each track) */
    for ( i = 0; i < tracks; i++ ) {
      subq_track_isrc * subq_tr;
      int j;

      fprintf(stderr, "\rscanning for ISRCs: %d ...", i+1);

      subq_tr = NULL;
      sub_ch = ReadSubQ(GET_TRACK_ISRC,i+1);

#if 1
      if (Toshiba != 0) {
	j = (g_toc[i].dwStartSector/100 + 1) * 100;
	do {
	  ReadCdRom( bufferCdda, j, nsectors);
	  sub_ch = ReadSubQ(GET_TRACK_ISRC, g_toc[i].bTrack);
	  if (sub_ch != NULL) {
	    subq_tr = (subq_track_isrc *) sub_ch->data;
	    if (subq_tr != NULL && (subq_tr->tc_valid & 0x80) != 0)
	      break;
	  }
	  j += nsectors;
	} while (j < (g_toc[i].dwStartSector/100 + 1) * 100 + 100);
      }
#endif
    
      if (sub_ch != NULL)
	subq_tr = (subq_track_isrc *)sub_ch->data;

      if (sub_ch != NULL && (subq_tr->tc_valid & 0x80) && quiet == 0) {
	unsigned char p_start[16];
	unsigned char *p = p_start;
	unsigned char *cp = subq_tr->track_isrc;

	/* unified format guesser:
	 * there are 60 bits and 15 bytes available.
	 * 5 * 6bit-items + two zero fill bits + 7 * 4bit-items
	 *
	 * A: ab cd ef gh ij kl mn o0 0  0  0  0  0  0  0  Plextor 6x Rel. 1.02
	 * B: 0a 0b 0c 0d 0e 0f 0g 0h 0i 0j 0k 0l 0m 0n 0o Toshiba 3401
	 * C: AS AS AS AS AS AS AS AS AS AS AS AS AS AS AS ASCII SCSI-2
	 * eg 'G''B'' ''A''0''7'' ''6''8'' ''0''0''2''7''0' makes most sense
	 * D: 'G''B''A''0''7''6''8''0''0''2''7''0'0  0  0  Plextor 6x Rel. 1.06 and 4.5x R. 1.01 and 1.04
	 */
	if (!(cp[8] | cp[9] | cp[10] | cp[11] | cp[12] | cp[13] | cp[14]) &&
	    ((cp[0] & 0xf0) | (cp[1] & 0xf0) | (cp[2] & 0xf0) | 
	     (cp[3] & 0xf0) | (cp[4] & 0xf0) | (cp[5] & 0xf0) | 
	     (cp[6] & 0xf0) | (cp[7] & 0xf0))) {
	  /* reformat A: to B: */
	  cp[14] = cp[7] >> 4;
	  cp[13] = cp[6] & 0xf;
	  cp[12] = cp[6] >> 4;
	  cp[11] = cp[5] & 0xf;
	  cp[10] = cp[5] >> 4;
	  cp[ 9] = cp[4] & 0xf;
	  cp[ 8] = cp[4] >> 4;
	  cp[ 7] = cp[3] & 0xf;
	  cp[ 6] = cp[3] >> 4;
	  cp[ 5] = cp[2] & 0xf;
	  cp[ 4] = cp[2] >> 4;
	  cp[ 3] = cp[1] & 0xf;
	  cp[ 2] = cp[1] >> 4;
	  cp[ 1] = cp[0] & 0xf;
	  cp[ 0] = cp[0] >> 4;
	}
      
	/* If not yet in ASCII format, do the conversion */
	if (!isupper(cp[0]) || !isupper(cp[1])) {
	  /* coding table for International Standard Recording Code */
	  static char bin2ISRC[] = 
	    {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	       0,0,0,0,0,0,0,
	       'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
	       'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
	       'W', 'X', 'Y', 'Z'
	   };
	
	  /* build 6-bit vector of coded values */
	  unsigned long ind;
	  int bits;
	
	  ind = (cp[0] << 26) +
	        (cp[1] << 22) +
	        (cp[2] << 18) + 
	        (cp[3] << 14) +
	        (cp[4] << 10) +
	        (cp[5] << 6) +
	        (cp[6] << 2) +
	        (cp[7] >> 2);
	  
	  /* decode ISRC due to IEC 908 */
	  for (bits = 0; bits < 30; bits +=6) {
	    int binval = (ind & (0x3f << (24-bits))) >> (24-bits);
	    *p++ = bin2ISRC[binval];
	  
	    /* insert a space after two country characters for legibility */
	    if (bits == 6)
	      *p++ = ' ';
	  }
	  
	  /* format year and serial number */
	  sprintf ((char *)p, " %.1X%.1X %.1X%.1X%.1X%.1X%.1X",
		   subq_tr->track_isrc [8],
		   subq_tr->track_isrc [9],
		   subq_tr->track_isrc [10],
		   subq_tr->track_isrc [11],
		   subq_tr->track_isrc [12],
		   subq_tr->track_isrc [13],
		   subq_tr->track_isrc [14]
		   ); 
	} else {
	  /* It is really in ASCII, surprise */
	  int ii;
	  for (ii = 0; p - p_start < 16; ii++) {
	    if (cp[ii] == '\0')
	      break;
	    if ((ii == 2 || ii == 5 || ii == 7) && cp[ii] != ' ')
	      *p++ = ' ';
	    *p++ = cp[ii];
	  }
	  if (p - p_start >= 16)
	    fprintf(stderr, "\np grows to high\n");
	  else
	    *p = '\0';
	}
	strncpy((char *)g_toc[i].ISRC, (const char *)p_start, 15);
	fprintf (stderr, "\rT: %2d ISRC: %s\n", 
		 i+1, p_start);
	fflush(stderr); 
      }
    }
  fputs("\n", stderr);
  }
}




static int GetIndexOfSector( unsigned sec, int range )
{
    subq_chnl *sub_ch;

    /* The Toshiba XM3401TA with firmware version 0283 has horrible bugs
     * Random reads with CDDA enabled lead to 'random position errors'
     * or just hang the scsi bus (recovered when timing out).
     * I bet they forgot to calculate with the correct sector size...
     */
#if	1
    if (Toshiba) {
      usleep(700000);
      ReadCdRom( bufferCdda + OFF, 0, 1 ); 
    }
    ReadCdRom( bufferCdda + OFF, sec, 1 ); 
#else
    PlayAudio( sec, 2 );
#endif

    if (Toshiba)
      usleep(700000);

    sub_ch = ReadSubQ(GET_POSITIONDATA,0);
    if (Toshiba)
      usleep(700000);
    return sub_ch?sub_ch->index:-1;
}


static int binary_search(int searchInd, unsigned Start, unsigned End)
{
      int l = Start; int r = End; int x;
      int ind;
      while ( r >= l ) {
	  x = ( l + r ) / 2;
	  /* try to avoid seeking */
	  ind = GetIndexOfSector(x, r-l);
	  if ( searchInd == ind ) {
	      break;
	  } else
	      if ( searchInd < ind ) r = x - 1;
	      else	     	     l = x + 1;
      }
      if ( r >= l ) {
        /* Index found. Now find the first position of this index */
	/* l=LastPos	x=found		r=NextPos */
        r = x;
	while ( l < r-1 ) {
	  x = ( l + r ) / 2;
	  /* try to avoid seeking */
	  ind = GetIndexOfSector(x, r-l);
	  if ( searchInd == ind ) {
	      r = x;
	  } else {
	      l = x;
	  }
        }
	return r;
      }

      return -1;
}

/* experimental code */
/* search for indices (audio mode required) */
unsigned ScanIndices( unsigned track, unsigned cd_index )
{
  /* scan for indices. */
  /* look at last sector of track. */
  /* when the index is not equal 1 scan by bipartition 
   * for offsets of all indices */

  int i; 
  int starttrack, endtrack;
  int startindex, endindex;

  int j;
  int LastIndex=0; 
  int StartSector;
  unsigned retval = 0;

  if (!quiet && !(verbose & SHOW_INDICES))
    fprintf(stderr, "seeking index start ...");
  if (cd_index != 1) {
    starttrack = track; endtrack = track;
  } else {
    starttrack = 1; endtrack = tracks;
  }
  for (i = starttrack; i <= endtrack; i++) {
    if ( verbose & SHOW_INDICES ) { 
	fprintf( stderr, "index scan: %d...\r", i ); 
	fflush (stderr);
    }
    if ( g_toc [i-1].bFlags & CDROM_DATA_TRACK )
	continue;/* skip nonaudio tracks */
    StartSector = GetStartSector(i);
    LastIndex = GetIndexOfSector(GetEndSector(i)-75, 
			         GetEndSector(i)-75 - StartSector);

    if (LastIndex < 2)
      continue;
    if ((verbose & SHOW_INDICES) && LastIndex > 1)
	fprintf(stderr, "track %2d index table (frame offsets are shown in in braces)\n", i);

    if (cd_index != 1) {
	startindex = cd_index; endindex = cd_index;
    } else {
	startindex = 2; endindex = LastIndex;
    }
    for (j = startindex; j <= endindex; j++) {
      unsigned IndexOffset;

      /* this track has indices */

      /* do a binary search */
      IndexOffset = binary_search(j, StartSector, GetEndSector(i));
      if ( IndexOffset == -1 ) {
	  fprintf(stderr, " index %d not found\n",j);
      } else {
	  if (verbose & SHOW_INDICES)
	    fprintf(stderr, "%2d  (%7u)   ",j,
		    IndexOffset-(unsigned)GetStartSector(i));
	  StartSector = IndexOffset;
	  if (track == i && cd_index == j) {
	     retval = IndexOffset-(unsigned)GetStartSector(i);
	  }
      }
    }
    fputs("\n", stderr);
  }
  return retval;
}
