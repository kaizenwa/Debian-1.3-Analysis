#define load() /* nothing */

/*
 * @(#)hardware.c	1.12	12/17/92
 *
 * Get information about a CD.
 */
#ifdef BOZO 
static char *ident = "@(#)hardware.c	1.12 12/17/92";
#endif

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#    ifdef linux
#     include <linux/cdrom.h>
#    else
#     include <sundev/srreg.h>
#    endif /* linux */
#include "struct.h"
#include "workbone.h"


struct cdinfo * read_toc(void);
void play_chunk( int start, int end );
extern struct play *playlist;
extern struct cdinfo thiscd, *cd;

int	cd_fd = -1;

/*
 * The minimum volume setting for the Sun and DEC CD-ROMs is 128 but for other
 * machines this might not be the case.
 */
int	min_volume = 128;
int	max_volume = 255;

# ifdef linux
const char *cd_device = "/dev/cdrom\0";
# else
const char *cd_device = "/dev/rsr0\0";
# endif

extern int cur_track, cur_index, cur_lasttrack, cur_firsttrack, cur_pos_abs,	
	cur_frame, cur_pos_rel, cur_tracklen, cur_cdlen, cur_ntracks,	
	cur_nsections, cur_cdmode, cur_listno, cur_stopmode, exit_on_eject,
	cur_balance;
extern char *cur_artist, *cur_cdname, *cur_trackname;
extern char	cur_contd, cur_avoid;
/*
 * read_toc()
 *
 * Read the table of contents from the CD.  Return a pointer to a cdinfo
 * struct containing the relevant information (minus artist/cdname/etc.)
 * This is a static struct.  Returns NULL if there was an error.
 *
 * XXX allocates one trackinfo too many.
 */

struct cdinfo *
read_toc(void)
{
	struct playlist		*l;
	struct cdrom_tochdr	hdr;
	struct cdrom_tocentry	entry;
	int			i, pos;

	if (cd_fd < 0)
		return(NULL);

	if (ioctl(cd_fd, CDROMREADTOCHDR, &hdr))
	{
		perror("readtochdr");
		return (NULL);
	}

	thiscd.artist[0] = thiscd.cdname[0] = '\0';
	thiscd.whichdb = thiscd.otherrc = thiscd.otherdb = NULL;
	thiscd.length = 0;
	thiscd.autoplay = thiscd.playmode = thiscd.volume = 0;
	thiscd.ntracks = hdr.cdth_trk1;

	if (thiscd.lists != NULL)
	{
		for (l = thiscd.lists; l->name != NULL; l++)
		{
			free(l->name);
			free(l->list);
		}
		free(thiscd.lists);
		thiscd.lists = NULL;
	}

	if (thiscd.trk != NULL)
		free(thiscd.trk);

	thiscd.trk = malloc((thiscd.ntracks + 1) * sizeof(struct trackinfo));
	if (thiscd.trk == NULL)
	{
		perror("malloc");
		return (NULL);
	}
	for (i = 0; i <= thiscd.ntracks; i++)
	{
		if (i == thiscd.ntracks)
			entry.cdte_track = CDROM_LEADOUT;
		else
			entry.cdte_track = i + 1;
		entry.cdte_format = CDROM_MSF;
		if (ioctl(cd_fd, CDROMREADTOCENTRY, &entry))
		{
			perror("tocentry read");
			return (NULL);
		}

		thiscd.trk[i].data =
		thiscd.trk[i].avoid = entry.cdte_ctrl & CDROM_DATA_TRACK ?
			1 : 0;
		thiscd.trk[i].length = entry.cdte_addr.msf.minute * 60 +
				entry.cdte_addr.msf.second;
		thiscd.trk[i].start = thiscd.trk[i].length * 75 +
				entry.cdte_addr.msf.frame;
		thiscd.trk[i].songname = thiscd.trk[i].otherrc =
		thiscd.trk[i].otherdb = NULL;
		thiscd.trk[i].contd = 0;
		thiscd.trk[i].volume = 0;
		thiscd.trk[i].track = i + 1;
		thiscd.trk[i].section = 0;
	}

/* Now compute actual track lengths. */
	pos = thiscd.trk[0].length;

	for (i = 0; i < thiscd.ntracks; i++)
	{
		thiscd.trk[i].length = thiscd.trk[i+1].length - pos;
		pos = thiscd.trk[i+1].length;
		if (thiscd.trk[i].data)
			thiscd.trk[i].length = (thiscd.trk[i + 1].start -
				thiscd.trk[i].start) * 2;
		if (thiscd.trk[i].avoid)
			strmcpy(&thiscd.trk[i].songname, "DATA TRACK");
	}

	thiscd.length = thiscd.trk[thiscd.ntracks].length;

	return (&thiscd);
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
 *
 * Updates cur_track, cur_pos_rel, cur_pos_abs and other variables.
 */
int
cd_status()
{
	char				realname[MAXPATHLEN];
	static int			warned = 0;
	struct cdrom_subchnl		sc;

	int				ret = 1;

	if (cd_fd < 0)
	{

		if ((cd_fd = open(cd_device, 0)) < 0)
		{

			if (errno == EACCES)
			{
				if (!warned)
				{
					strcpy(realname, cd_device);

					fprintf(stderr,
		"As root, please run\n\nchmod 666 %s\n\n%s\n", realname,
		"to give yourself permission to access the CD-ROM device.");
					warned++;
				}
			}
			else if (errno != ENXIO)
			{
				perror(cd_device);
				if (thiscd.trk != NULL)
					free(thiscd.trk);
				exit(1);
			}
			return (0);
		}
		cur_cdmode = 5;
	}

	if (warned)
	{
		warned = 0;
		fprintf(stderr, "Thank you.\n");
	}

	sc.cdsc_format = CDROM_MSF;

	if (ioctl(cd_fd, CDROMSUBCHNL, &sc))
	{
		cur_cdmode = 5;
		cur_track = -1;
		cur_cdlen = cur_tracklen = 1;
		cur_pos_abs = cur_pos_rel = cur_frame = 0;

		if (exit_on_eject)
			exit(0);

		return (0);
	}

	if (cur_cdmode == 5)	/* CD has been ejected */
	{
		cur_pos_rel = cur_pos_abs = cur_frame = 0;

		if ((cd = read_toc()) == NULL) 
		{
			close(cd_fd);
			cd_fd = -1;
			if (exit_on_eject)
				exit(0);
			else
				return (0);
		}
		cur_nsections = 0;
		cur_ntracks = cd->ntracks;
		cur_cdlen = cd->length;
		load();
		cur_artist = cd->artist;
		cur_cdname = cd->cdname;
		cur_cdmode = 4;
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
				cd->trk[cur_track].start))
		{
			cur_track = 0;
			while (cur_track < cur_ntracks && cur_frame >=
					cd->trk[cur_track].start)
				cur_track++;
		}
		if (cur_track >= 1 && sc.cdsc_trk > cd->trk[cur_track-1].track)
			cur_track++;

		cur_index = sc.cdsc_ind;
doall:
		if (cur_track >= 1 && cur_track <= cur_ntracks)
		{
			cur_trackname = cd->trk[cur_track-1].songname;
			cur_avoid = cd->trk[cur_track-1].avoid;
			cur_contd = cd->trk[cur_track-1].contd;
			cur_pos_rel = (cur_frame -
				cd->trk[cur_track-1].start) / 75;
			if (cur_pos_rel < 0)
				cur_pos_rel = -cur_pos_rel;
		}

		/* note: workbone requires playlist == NULL always! */		
		if (playlist != NULL && playlist[0].start)
		{
			cur_pos_abs -= cd->trk[playlist[cur_listno-1].
				start - 1].start / 75;
			cur_pos_abs += playlist[cur_listno-1].starttime;
		}
		if (cur_pos_abs < 0)
			cur_pos_abs = cur_frame = 0;

		if (cur_track < 1)
			cur_tracklen = cd->length;
		else
			cur_tracklen = cd->trk[cur_track-1].length;
		break;

	case CDROM_AUDIO_PAUSED:
		if (cur_cdmode == 1 || cur_cdmode == 3)
		{
			cur_cdmode = 3;
			goto dopos;
		}
		else
			cur_cdmode = 4;
		goto doall;

	case CDROM_AUDIO_COMPLETED:
		cur_cdmode = 0;		/* waiting for next track. */
		break;

	case CDROM_AUDIO_NO_STATUS:
		cur_cdmode = 4;
		cur_lasttrack = cur_firsttrack = -1;
		goto doall;
	}
	return (ret);
}
#ifdef BOZO
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
scale_volume(vol, max)
	int	vol, max;
{
	return ((max * max - (max - vol) * (max - vol)) *
		(max_volume - min_volume) / (max * max) + min_volume);
}

/*
 * unscale_volume(cd_vol, max)
 *
 * Given a value between min_volume and max_volume, return the volume slider
 * value needed to achieve that value.
 *
 * Rather than perform floating-point calculations to reverse the above
 * formula, we simply do a binary search of scale_volume()'s return values.
 */
unscale_volume(cd_vol, max)
	int	cd_vol, max;
{
	int	vol, incr, scaled;

	for (vol = max / 2, incr = max / 4 + 1; incr; incr /= 2)
	{
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

/*
 * cd_volume(vol, bal, max)
 *
 * Set the volume levels.  "vol" and "bal" are the volume and balance knob
 * settings, respectively.  "max" is the maximum value of the volume knob
 * (the balance knob is assumed to always go from 0 to 20.)
 */
void
cd_volume(vol, bal, max)
	int	vol, bal, max;
{
	int	left, right;
	struct cdrom_volctrl v;

/*
 * Set "left" and "right" to volume-slider values accounting for the
 * balance setting.
 *
 * XXX - the maximum volume setting is assumed to be in the 20-30 range.
 */
	if (bal < 9)
		right = vol - (9 - bal) * 2;
	else
		right = vol;
	if (bal > 11)
		left = vol - (bal - 11) * 2;
	else
		left = vol;

/* Adjust the volume to make up for the CD-ROM drive's weirdness. */
	left = scale_volume(left, max);
	right = scale_volume(right, max);

	v.channel0 = left < 0 ? 0 : left > 255 ? 255 : left;
	v.channel1 = right < 0 ? 0 : right > 255 ? 255 : right;
	if (cd_fd >= 0)
		(void) ioctl(cd_fd, CDROMVOLCTRL, &v);
}
#endif /* BOZO */
/*
 * pause_cd()
 *
 * Pause the CD, if it's in play mode.  If it's already paused, go back to
 * play mode.
 */
void
pause_cd()
{
	if (cd_fd < 0)	/* do nothing if there's no CD! */
		return;

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
void
stop_cd()
{
	if (cd_fd < 0)
		return;

	if (cur_cdmode != 4)
	{
		cur_lasttrack = cur_firsttrack = -1;
		cur_cdmode = 4;
		ioctl(cd_fd, CDROMSTOP);
		cur_track = 1;
	}
}

/*
 * play_chunk(start, end)
 *
 * Play the CD from one position to another (both in frames.)
 */
void
play_chunk(start, end)
	int start, end;
{
	struct cdrom_msf		msf;

	if (cd == NULL || cd_fd < 0)
		return;

	end--;
	if (start >= end)
		start = end-1;

	msf.cdmsf_min0 = start / (60*75);
	msf.cdmsf_sec0 = (start % (60*75)) / 75;
	msf.cdmsf_frame0 = start % 75;
	msf.cdmsf_min1 = end / (60*75);
	msf.cdmsf_sec1 = (end % (60*75)) / 75;
	msf.cdmsf_frame1 = end % 75;

	if (ioctl(cd_fd, CDROMSTART))
	{
		perror("CDROMSTART");
		return;
	}
	if (ioctl(cd_fd, CDROMPLAYMSF, &msf))
	{
		printf("play(%d,%d)\n",start,end);
		printf("msf = %d:%d:%d %d:%d:%d\n",
			msf.cdmsf_min0, msf.cdmsf_sec0, msf.cdmsf_frame0,
			msf.cdmsf_min1, msf.cdmsf_sec1, msf.cdmsf_frame1);
		perror("CDROMPLAYMSF");
		return;
	}
}

/*
 * play_cd(starttrack, pos, endtrack)
 *
 * Start playing the CD or jump to a new position.  "pos" is in seconds,
 * relative to start of track.
 */
void
play_cd(start, pos, end)
int start, pos, end;
{

	if (cd == NULL || cd_fd < 0)
		return;

	cur_firsttrack = start;
	start--;
	end--;
	cur_lasttrack = end;

	play_chunk(cd->trk[start].start + pos * 75, end >= cur_ntracks ?
		cur_cdlen * 75 : cd->trk[end].start - 1);
}

#ifdef BOZO
/*
 * Set the offset into the current track and play.  -1 means end of track
 * (i.e., go to next track.)
 */
void
play_from_pos(pos)
	int	pos;
{
	if (pos == -1)
		if (cd)
			pos = cd->trk[cur_track - 1].length - 1;
	if (cur_cdmode == 1)
		play_cd(cur_track, pos, playlist[cur_listno-1].end);
}
#endif

/*
 * Eject the current CD, if there is one, and set the mode to 5.
 *
 * Returns 0 on success, 1 if the CD couldn't be ejected, or 2 if the
 * CD contains a mounted filesystem.
 */
int 
eject_cd()
{
	struct stat	stbuf;
	struct ustat	ust;

	if (cur_cdmode == 5)		/* Already ejected! */
		return (0);

	if (fstat(cd_fd, &stbuf) != 0)
	{
		perror("fstat");
		return (1);
	}

	/* Is this a mounted filesystem? */
	if (ustat(stbuf.st_rdev, &ust) == 0)
		return (2);

	if (ioctl(cd_fd, CDROMEJECT))
	{
		perror("CDEJECT");
		return (1);
	}

	if (exit_on_eject)
		exit(0);

	cur_track = -1;
	cur_cdlen = cur_tracklen = 1;
	cur_pos_abs = cur_pos_rel = cur_frame = 0;
	cur_cdmode = 5;

	return (0);
}
#ifdef BOZO
/* Try to keep the CD open all the time.  This is run in a subprocess. */
void
keep_cd_open()
{
	int	fd;
	struct flock	fl;
	extern	end;

	for (fd = 0; fd < 256; fd++)
		close(fd);

	if (fork())
		exit(0);

	if ((fd = open("/tmp/cd.lock", O_RDWR | O_CREAT, 0666)) < 0)
		exit(0);
	fl.l_type = F_WRLCK;
	fl.l_whence = 0;
	fl.l_start = 0;
	fl.l_len = 0;
	if (fcntl(fd, F_SETLK, &fl) < 0)
		exit(0);

	if (open(cd_device, 0) >= 0)
	{
		brk(&end);
		pause();
	}

	exit(0);
}

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
find_trkind(track, index, start)
	int	track, index, start;
{
	int	top = 0, bottom, current, interval, ret = 0, i;

	if (cd == NULL || cd_fd < 0)
		return;

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
				susleep(1);

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

/*
 * Simulate usleep() using select().
 */
susleep(usec)
	int	usec;
{
	struct timeval	tv;

	timerclear(&tv);
	tv.tv_sec = usec / 1000000;
	tv.tv_usec = usec % 1000000;
	return (select(0, NULL, NULL, NULL, &tv));
}
/*
 * Read the initial volume from the drive, if available.  Set cur_balance to
 * the balance level (0-20, 10=centered) and return the proper setting for
 * the volume knob.
 *
 * "max" is the maximum value of the volume knob.
 */
read_initial_volume(max)
	int max;
{
	int	left, right;

	/* Suns can't read the volume; oh well */
	left = right = 255;

	left = unscale_volume(left, max);
	right = unscale_volume(right, max);

	if (left < right)
	{
		cur_balance = (right - left) / 2 + 11;
		if (cur_balance > 20)
			cur_balance = 20;

		return (right);
	}
	else if (left == right)
	{
		cur_balance = 10;
		return (left);
	}
	else
	{
		cur_balance = (right - left) / 2 + 9;
		if (cur_balance < 0)
			cur_balance = 0;

		return (left);
	}
}
#endif
