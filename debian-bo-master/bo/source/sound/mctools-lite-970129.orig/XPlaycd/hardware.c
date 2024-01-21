#undef ERR_DEBUG
#undef DBG_EXIT

/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

      (This file shamelessly stolen from WorkBone or so...)

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

/*
 * @(#)hardware.c	1.12	12/17/92
 *
 * Get information about a CD.
 */

#include "../config.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>
#include <McTools/McInfoRequest.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <mntent.h>
#include "xplaycd.h"
#include "struct.h"
#include "hardware.h"

extern char *myname;

extern struct cdinfo thiscd, *cd;
extern char *cdrom_name;

int	cd_fd = -1;

static struct cdinfo *read_toc(void);



/*
 * The minimum volume setting for the Sun and DEC CD-ROMs is 128 but for other
 * machines this might not be the case.
 */
int	min_volume = 128;
int	max_volume = 255;

int checkmount(void) {
  FILE *fp;
  struct mntent *mnt;

  /* check if drive is mounted (from Mark Buckaway's cdplayer code) */
  if ((fp = setmntent (MOUNTED, "r")) == NULL) {
    fprintf (stderr, _("Couldn't open %s: %s\n"), MOUNTED, strerror (errno));
    exit (1);
  }
  while ((mnt = getmntent (fp)) != NULL) {
    if (strcmp (mnt->mnt_fsname, cdrom_name) == 0) {
      endmntent (fp);
      return 1;
    }
  }
  endmntent (fp);
  return 0;
}


extern int cur_track, cur_index,  cur_firsttrack, cur_pos_abs,
	cur_frame, cur_pos_rel, cur_tracklen, cur_cdlen, cur_ntracks,	
	cur_cdmode, cur_stopmode, cur_lasttrack;
/*
 * read_toc()
 *
 * Read the table of contents from the CD.  Return a pointer to a cdinfo
 * struct containing the relevant information (minus artist/cdname/etc.)
 * This is a static struct.  Returns NULL if there was an error.
 *
 * XXX allocates one trackinfo too many.
 */

static struct cdinfo *read_toc(void) {
  struct cdrom_tochdr	hdr;
  struct cdrom_tocentry	entry;
  int			i, pos;

  if (cd_fd < 0)
    return(NULL);

  if (ioctl(cd_fd, CDROMREADTOCHDR, &hdr)) {
#ifdef ANNOYING_MESSAGE
    perror("readtochdr");
#endif
    return (NULL);
  }

  thiscd.cdname[0] = '\0';
  thiscd.length = 0;
  thiscd.autoplay = thiscd.playmode = thiscd.volume = 0;
  thiscd.ntracks = hdr.cdth_trk1;
  
  if (thiscd.trk != NULL)
    free(thiscd.trk);
  
  thiscd.trk = calloc((thiscd.ntracks + 1), sizeof(struct trackinfo));
  if (thiscd.trk == NULL) {
      perror("malloc");
      return (NULL);
    }
  for (i = 0; i <= thiscd.ntracks; i++) {
    if (i == thiscd.ntracks)
      entry.cdte_track = CDROM_LEADOUT;
    else
      entry.cdte_track = i + 1;
    entry.cdte_format = CDROM_MSF;
    if (ioctl(cd_fd, CDROMREADTOCENTRY, &entry)) {
#ifdef ANNOYING_MESSAGE
      perror("tocentry read");
#endif
      return (NULL);
    }

    thiscd.trk[i].data =
      thiscd.trk[i].avoid = entry.cdte_ctrl & CDROM_DATA_TRACK ? 1 : 0;
    thiscd.trk[i].length = entry.cdte_addr.msf.minute * 60 +
      entry.cdte_addr.msf.second;
    thiscd.trk[i].start = thiscd.trk[i].length * 75 +
      entry.cdte_addr.msf.frame;
    *thiscd.trk[i].songname = 0;
    thiscd.trk[i].contd = 0;
    thiscd.trk[i].volume = 0;
    thiscd.trk[i].track = i + 1;
    thiscd.trk[i].section = 0;
  }

/* Now compute actual track lengths. */
  pos = thiscd.trk[0].length;

  for (i = 0; i < thiscd.ntracks; i++) {
    thiscd.trk[i].length = thiscd.trk[i+1].length - pos;
    pos = thiscd.trk[i+1].length;
    if (thiscd.trk[i].avoid)
      strcpy(thiscd.trk[i].songname, _("DATA TRACK"));
  }

  thiscd.length = thiscd.trk[thiscd.ntracks].length;

  return (&thiscd);
}

/*
 * Try to open the cdrom device
 */
static int open_device(void) {
  if (cd_fd < 0) {
    cur_cdmode = 5;
    cur_pos_rel= 0;
    if ((cd_fd = open(cdrom_name, 0)) < 0) {
      McSizedError(app, 400, 0, _("%s: can't open %s: %s"),
		   myname, cdrom_name, strerror(errno));
      return 0;
    }
  }
  return 1;
}

/*
 * cd_status()
 *
 * Return values:
 *
 *	0	No CD in drive.
 *	1	CD in drive.
 *	2	CD has just been inserted (TOC has been read)
 *	3	CD has just been removed
 *	4	CD is mounted
 *	7	Error opening device
 *
 * Updates cur_track, cur_pos_rel, cur_pos_abs and other variables.
 */
int cd_status(void) {
  struct cdrom_subchnl sc;
  int ret = 1;

  if (checkmount()) {
    cur_cdmode = 6;
    cur_pos_rel= 0;
    if (cd_fd>=0) {
      close(cd_fd);
      cd_fd=-1;
    }
    return 4;
  }

  if (!open_device()) {
    cur_cdmode=CDERROR;
    return 7;
  }

  sc.cdsc_format = CDROM_MSF;

  if (ioctl(cd_fd, CDROMSUBCHNL, &sc)) {
    cur_cdmode = 5;
    cur_track = -1;
    cur_cdlen = cur_tracklen = 1;
    cur_pos_abs = cur_pos_rel = cur_frame = 0;

    close(cd_fd);
    cd_fd = -1;
    
#ifdef DBG_EXIT
    fprintf(stderr,"exit 2\n");
#endif
    return (0);
  }

  if (cur_cdmode == 5) { /* CD has been ejected */
    cur_pos_rel = cur_pos_abs = cur_frame = 0;

    if ((cd = read_toc()) == NULL) {
      close(cd_fd);
      cd_fd = -1;

#ifdef DBG_EXIT
      fprintf(stderr,"exit 3\n");
#endif
      return (0);
    }
    cur_ntracks = cd->ntracks;
    cur_cdlen = cd->length;
    cur_cdmode = 4;
    cur_pos_rel= 0;
    ret = 2;
  }

  switch (sc.cdsc_audiostatus) {
  case CDROM_AUDIO_PLAY:
    cur_cdmode = 1;
dopos:
    cur_pos_abs = sc.cdsc_absaddr.msf.minute * 60 +
      sc.cdsc_absaddr.msf.second;
    cur_frame = cur_pos_abs * 75 + sc.cdsc_absaddr.msf.frame;

    /* Only look up the current track number if necessary. */
    if (cur_track < 1 || cur_frame < cd->trk[cur_track-1].start ||
	cur_frame >= (cur_track >= cur_ntracks ?
		      (cur_cdlen + 1) * 75 :
		      cd->trk[cur_track].start)) {
      cur_track = 0;
      while (cur_track < cur_ntracks && cur_frame >=
	     cd->trk[cur_track].start)
	cur_track++;
    }
    if (cur_track >= 1 && sc.cdsc_trk > cd->trk[cur_track-1].track)
      cur_track++;
    
    cur_index = sc.cdsc_ind;
doall:
    if ((cur_cdmode==CDPLAY || cur_cdmode==CDPAUSE) &&
	cur_track >= 1 && cur_track <= cur_ntracks) {
      cur_pos_rel = (cur_frame -
		     cd->trk[cur_track-1].start) / 75;
    } else {
      cur_pos_rel= 0;
    }
    if (cur_pos_abs < 0)
      cur_pos_abs = cur_frame = 0;
    
    if (cur_track < 1)
      cur_tracklen = cd->length;
    else
      cur_tracklen = cd->trk[cur_track-1].length;
    break;

  case CDROM_AUDIO_PAUSED:
    if (cur_cdmode == 1 || cur_cdmode == 3) {
      cur_cdmode = 3;
      goto dopos;
    } else {
      cur_cdmode = 4;
    }
    goto doall;

  case CDROM_AUDIO_COMPLETED:
    cur_cdmode = 0;		/* waiting for next track. */
    cur_pos_rel= 0;
    break;
    
  case CDROM_AUDIO_NO_STATUS:
    cur_cdmode = 4;
    cur_pos_rel= 0;
    cur_firsttrack = -1;
    goto doall;
  }

#ifdef DBG_EXIT
  fprintf(stderr,"exit 3 with value %d\n",ret);
#endif
  return (ret);
}

/*
 * pause_cd()
 *
 * Pause the CD, if it's in play mode.  If it's already paused, go back to
 * play mode.
 */
void pause_cd(void) {
  if (cd_fd < 0)	/* do nothing if there's no CD! */
    return;

  if (checkmount()) {
    cur_cdmode = 6;
    cur_pos_rel= 0;
    return;
  }

  switch (cur_cdmode) {
  case 1:		/* playing */
    cur_cdmode = 3;
    ioctl(cd_fd, CDROMPAUSE);
    break;
  case 3:		/* paused */
    cur_cdmode = 1;
    ioctl(cd_fd, CDROMRESUME);
  }
}

/*
 * stop_cd()
 *
 * Stop the CD if it's not already stopped.
 */
void stop_cd(void) {

  if (cd_fd < 0)
    return;

  if (checkmount()) {
    cur_cdmode = 6;
    cur_pos_rel= 0;
    return;
  }

  if (cur_cdmode != 4) {
    cur_firsttrack = -1;
    cur_cdmode = 4;
    cur_pos_rel= 0;
    ioctl(cd_fd, CDROMSTOP);
  }
}

/*
 * play_chunk(start, end)
 *
 * Play the CD from one position to another (both in frames.)
 */
static int play_chunk(int start, int end) {
  struct cdrom_msf msf;

  if (cd == NULL || cd_fd < 0)
    return -1;

  end--;
  if (start >= end)
    start = end-1;

  msf.cdmsf_min0 = start / (60*75);
  msf.cdmsf_sec0 = (start % (60*75)) / 75;
  msf.cdmsf_frame0 = start % 75;
  msf.cdmsf_min1 = end / (60*75);
  msf.cdmsf_sec1 = (end % (60*75)) / 75;
  msf.cdmsf_frame1 = end % 75;
  
  if ((cur_cdmode!=CDPLAY) && (ioctl(cd_fd, CDROMSTART))) {
#ifdef ERR_DEBUG
    perror("CDROMSTART");
#endif
    return -1;
  }
  if (ioctl(cd_fd, CDROMPLAYMSF, &msf)) {
#ifdef ERR_DEBUG
    printf("play(%d,%d)\n",start,end);
    printf("msf = %d:%d:%d %d:%d:%d\n",
	   msf.cdmsf_min0, msf.cdmsf_sec0, msf.cdmsf_frame0,
	   msf.cdmsf_min1, msf.cdmsf_sec1, msf.cdmsf_frame1);
    perror("CDROMPLAYMSF");
#endif
    if (errno == EINVAL)
      return -2; /* Probably a data track */
    return -1;
  }
  return 0;
}

/*
 * play_cd(starttrack, pos, endtrack)
 *
 * Start playing the CD or jump to a new position.  "pos" is in seconds,
 * relative to start of track.
 */
int play_cd(int start, int pos, int end) {

	if (cd == NULL || cd_fd < 0)
		return 0;

	if (thiscd.trk[start].avoid)
	  return -3;

	cur_firsttrack = start;
	cur_lasttrack = end;

	if (checkmount()) {
	  cur_cdmode = 6;
	  cur_pos_rel= 0;
	  return 0;
	}

	if (start>end)
	  return -4; /* disabled */

	return play_chunk(cd->trk[start-1].start + pos * 75,
			  end >= cur_ntracks ?
			  cur_cdlen * 75 : cd->trk[end].start - 1);
}

/*
 * replay_cd(int end)
 *
 * Set a new end position fo a running play
 */
int replay_cd(int end) {

	if (cd == NULL || cd_fd < 0)
		return 0;

	cur_lasttrack = end;

	if (checkmount()) {
	  cur_cdmode = 6;
	  cur_pos_rel= 0;
	  return 0;
	}

	return play_chunk(cur_frame, end >= cur_ntracks ?
			  cur_cdlen * 75 : cd->trk[end].start - 1);
}

/*
 * Eject the current CD, if there is one, and set the mode to 5.
 *
 * Returns 0 on success, 1 if the CD couldn't be ejected, or 2 if the
 * CD contains a mounted filesystem.
 */
int eject_cd(void) {
	struct stat	stbuf;
	struct ustat	ust;

	if (cur_cdmode == 5) {		/* Already ejected! */
	  return 0;
	}

	if (checkmount()) {
	  cur_cdmode = 6;
	  cur_pos_rel= 0;
	  return 0;
	}

	if (fstat(cd_fd, &stbuf) != 0)
	{
#ifdef ERR_DEBUG
		perror("fstat");
#endif
		return (1);
	}

	/* Is this a mounted filesystem? */
	if (ustat(stbuf.st_rdev, &ust) == 0)
		return (2);

	if (ioctl(cd_fd, CDROMEJECT))
	{
#ifdef ERR_DEBUG
		perror("CDEJECT");
#endif
		return (1);
	}

	cur_track = -1;
	cur_cdlen = cur_tracklen = 1;
	cur_pos_abs = cur_pos_rel = cur_frame = 0;
	cur_cdmode = 5;

	return (0);
}

/*
 * Close the tray-door.
 * Returns 0 on success, 1 on error.
 */
int load_cd(void) {
#ifdef CDROMCLOSETRAY
  if (!open_device()) return 1;
  if (ioctl(cd_fd, CDROMCLOSETRAY))
    {
#ifdef ERR_DEBUG
      perror("CDROMCLOSETRAY");
#endif
      close(cd_fd);
      cd_fd=-1;
      return (1);
    }
#endif
  return 0;
}

#if 0 /* Leave this alone, maybe we need it some day... */
/*
 * find_trkind(track, index)
 *
 * Start playing at a particular track and index, optionally using a particular
 * frame as a starting position.  Returns a frame number near the start of the
 * index mark if successful, 0 if the track/index didn't exist.
 *
 * This is made significantly more tedious (though probably easier to port)
 * by the fact that CDROMPLAYTRKIND doesn't work as advertised.  The routine
 * does a binary search of the track, terminating when the interval gets to
 * around 10 frames or when the next track is encountered, at which point
 * it's a fair bet the index in question doesn't exist.
 */
int find_trkind(int track, int index, int start) {
  int	top = 0, bottom, current, interval, ret = 0, i;

  if (cd == NULL || cd_fd < 0)
    return 0;

  for (i = 0; i < cur_ntracks; i++)
    if (cd->trk[i].track == track)
      break;
  bottom = cd->trk[i].start;

  for (; i < cur_ntracks; i++)
    if (cd->trk[i].track > track)
      break;
  
  top = i == cur_ntracks ? (cd->length - 1) * 75 : cd->trk[i].start;

  if (start > bottom && start < top)
    bottom = start;
  
  current = (top + bottom) / 2;
  interval = (top - bottom) / 4;

  do {
    play_chunk(current, current + 75);

    if (cd_status() != 1)
      return (0);
    while (cur_frame < current)
      if (cd_status() != 1 || cur_cdmode != 1)
	return (0);
      else
	usleep(10);

    if (cd->trk[cur_track - 1].track > track)
      break;
    
    if (cur_index >= index)
      {
	ret = current;
	current -= interval;
      }
    else
      current += interval;
    interval /= 2;
  } while (interval > 2);
  
  return (ret);
}
#endif

#ifdef DEBUG_CODE
/*
 * This is for debugging only, it prints out the trackinfo of one track
 */
void pr_trackinfo(struct McApp *appl, int track) {
  struct trackinfo *t;

  t = &thiscd.trk[track-1];

  McInfo(appl, "Track %d:\n"
	 "&L=Songname    = '%s'\n"
	 "&L=Length      = %d\n"
	 "&L=Start frame = %d (%d sec)\n"
	 "&L=Volume      = %d\n"
	 "&L=Track       = %d\n"
	 "&L=Index       = %d\n"
	 "&L=continue    = %d\n"
	 "&L=avoid       = %d\n"
	 "&L=data        = %d\n",track,
	 t->songname, t->length, t->start, t->start/75,t->volume, t->track,
	 t->section,t->contd, t->avoid, t->data);
}

#endif

/*
 * scale_volume(vol, max)
 *
 * Return a volume value suitable for passing to the CD-ROM drive.  "vol"
 * is a volume slider setting; "max" is the slider's maximum value.
 *
 * On Sun and DEC CD-ROM drives, the amount of sound coming out the jack
 * increases much faster toward the top end of the volume scale than it
 * does at the bottom.  To make up for this, we make the volume scale look
 * sort of logarithmic (actually an upside-down inverse square curve) so
 * that the volume value passed to the drive changes less and less as you
 * approach the maximum slider setting.  The actual formula looks like
 *
 *     (max^2 - (max - vol)^2) * (max_volume - min_volume)
 * v = --------------------------------------------------- + min_volume
 *                           max^2
 *
 * If your system's volume settings aren't broken in this way, something
 * like the following should work:
 *
 *	return ((vol * (max_volume - min_volume)) / max + min_volume);
 */
static int scale_volume(int vol, int max) {
  return ((max * max - (max - vol) * (max - vol)) *
	  (max_volume - min_volume) / (max * max) + min_volume);
}

#if 0 /* Leave this alone, maybe we need it some day... */
/*
 * unscale_volume(cd_vol, max)
 *
 * Given a value between min_volume and max_volume, return the volume slider
 * value needed to achieve that value.
 *
 * Rather than perform floating-point calculations to reverse the above
 * formula, we simply do a binary search of scale_volume()'s return values.
 */
static int unscale_volume(int cd_vol, int max) {
  int	vol, incr, scaled;

  for (vol = max / 2, incr = max / 4 + 1; incr; incr /= 2) {
    scaled = scale_volume(vol, max);
    if (cd_vol == scaled)
      break;
    if (cd_vol < scaled)
      vol -= incr;
    else
      vol += incr;
  }
  
  if (vol < 0)
    vol = 0;
  else if (vol > max)
    vol = max;

  return (vol);
}
#endif

/*
 * cd_volume(vol, bal, max)
 *
 * Set the volume levels.  "left" and "right" are assumed to be 0..100%
 * settings, respectively.
 */
void cd_volume(int left, int right) {
  struct cdrom_volctrl v;

/* Adjust the volume to make up for the CD-ROM drive's weirdness. */
  left = scale_volume(left, 100);
  right = scale_volume(right, 100);

  v.channel0 = left < 0 ? 0 : left > 255 ? 255 : left;
  v.channel1 = right < 0 ? 0 : right > 255 ? 255 : right;
  if (cd_fd >= 0) {
#ifdef DEBUG_CODE
    fprintf(stderr, "set volume to %d:%d --> %d: ", left, right,
	    ioctl(cd_fd, CDROMVOLCTRL, &v));
    perror("");
#else
    ioctl(cd_fd, CDROMVOLCTRL, &v);
#endif
  }

}
