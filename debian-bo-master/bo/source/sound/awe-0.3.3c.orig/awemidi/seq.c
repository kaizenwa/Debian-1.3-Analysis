/*
 */

#include <stdio.h>
#include <math.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <math.h>
#include <fcntl.h>
#ifdef linux
#include <sys/soundcard.h>
#include <linux/awe_voice.h>
#elif defined(__FreeBSD__)
#include <machine/soundcard.h>
#include <awe_voice.h>
#endif
#include "seq.h"
#include "channel.h"
#include "controls.h"


#if 1
#define DEB(x)  /**/
#else
#define DEB(x) x
#endif

#define SEQUENCER_DEV	"/dev/sequencer"
SEQ_DEFINEBUF(1280);
int seqfd;

void seqbuf_dump()
{
	if (_seqbufptr)
		if (write(seqfd, _seqbuf, _seqbufptr) == -1) {
			perror("write " SEQUENCER_DEV);
			if (ctl) ctl->close();
			exit(-1);
		}
	_seqbufptr = 0;
}


#define MAX_CARDS	16
int nrsynths;
struct synth_info card_info[MAX_CARDS];
int awe_dev;

int seq_init(MidiInfo *mp)
{
	int i;

	if ((seqfd = open(SEQUENCER_DEV, O_WRONLY, 0)) < 0) {
		if (ctl == NULL) {
			perror("sequencer " SEQUENCER_DEV);
			if (ctl) ctl->close();
			exit(1);
		}
		return 1;
	}

	if (ioctl(seqfd, SNDCTL_SEQ_NRSYNTHS, &nrsynths) == -1) {
		fprintf(stderr, "there is no soundcard\n");
		if (ctl) ctl->close();
		exit(1);
	}
	awe_dev = -1;
	for (i = 0; i < nrsynths; i++) {
		card_info[i].device = i;
		if (ioctl(seqfd, SNDCTL_SYNTH_INFO, &card_info[i]) == -1) {
			fprintf(stderr, "cannot get info on soundcard\n");
			perror(SEQUENCER_DEV);
			if (ctl) ctl->close();
			exit(1);
		}
		card_info[i].device = i;
		if (card_info[i].synth_type == SYNTH_TYPE_SAMPLE
		    && card_info[i].synth_subtype == SAMPLE_TYPE_AWE32)
			awe_dev = i;
	}

	if (awe_dev < 0) {
		fprintf(stderr, "No AWE synth device is found\n");
		if (ctl) ctl->close();
		exit(1);
	}

	AWE_SET_CHANNEL_MODE(awe_dev, 1);
	if (mp)
		AWE_DRUM_CHANNELS(awe_dev, mp->drumflag);

	return 0;
}

void seq_end()
{
	DEB(fprintf(stderr, "wait end.\n"));
	SEQ_DUMPBUF();
	ioctl(seqfd, SNDCTL_SEQ_SYNC);
	close(seqfd);
}

/*----------------------------------------------------------------*/

static int start_csec;
static struct timeval start_time;

void seq_clear(int csec)
{
	SEQ_START_TIMER();
	SEQ_DUMPBUF();
	gettimeofday(&start_time, NULL);
	start_csec = csec;
}

int seq_curtime()
{
	int d1, d2;
	struct timeval now_time;

	gettimeofday(&now_time, NULL);
	d1 = now_time.tv_sec - start_time.tv_sec;
	d2 = now_time.tv_usec - start_time.tv_usec;
	if (d2 < 0) {
		d2 += 1000000;
		d1--;
	}
	d2 /= 10000;
	return (d2 + d1 * 100 + start_csec);
}

void seq_wait(int time)
{
	/*DEB(fprintf(stderr, "(--) wait %d\n", time));*/
	SEQ_DELTA_TIME(time);
	SEQ_DUMPBUF();
}

void seq_sync(int cs)
{
	int new_cs;
	SEQ_DUMPBUF();
	/* sync for waiting */
	ioctl(seqfd, SNDCTL_SEQ_SYNC);
	new_cs = seq_curtime();
	start_csec += cs - new_cs;
}

void seq_wait_abs(int time)
{
	DEB(fprintf(stderr, "(--) wait %d\n", time));
	SEQ_WAIT_TIME(time);
	SEQ_DUMPBUF();
}

/*----------------------------------------------------------------*/

void seq_set_reverb(int mode)
{
	AWE_REVERB_MODE(awe_dev, mode);
	SEQ_DUMPBUF();
}

void seq_set_chorus(int mode)
{
	AWE_CHORUS_MODE(awe_dev, mode);
	SEQ_DUMPBUF();
}

/*----------------------------------------------------------------*/

void seq_send_effect(int v, int type, int val)
{
	DEB(fprintf(stderr, "(%2d) effect %d %d\n", v, type, val));
	AWE_SEND_EFFECT(awe_dev, v, type, val);
}

/*----------------------------------------------------------------*/

void seq_set_bank(int v, int bank)
{
	DEB(fprintf(stderr, "(%2d) bank %d\n", v, bank));
	SEQ_CONTROL(awe_dev, v, CTL_BANK_SELECT, bank);
}

void seq_sustain(int v, int val)
{
	SEQ_CONTROL(awe_dev, v, CTL_SUSTAIN, val);
}

void seq_reset_control(void)
{
}

void seq_note_off_all(void)
{
#ifdef AWE_NOTEOFF_ALL
	AWE_NOTEOFF_ALL(awe_dev);
#else
	SEQ_CONTROL(awe_dev, 0, 123, 0);
#endif
}

void seq_sound_off_all(void)
{
#ifdef AWE_RELEASE_ALL
	AWE_RELEASE_ALL(awe_dev);
#else
	SEQ_CONTROL(awe_dev, 0, 120, 0);
#endif
}

void seq_set_program(int v, int pgm)
{
	DEB(fprintf(stderr, "(%2d) prog %d\n", v, pgm));
	SEQ_SET_PATCH(awe_dev, v, pgm);
}

void seq_start_note(int v, int note, int vel)
{
	DEB(fprintf(stderr, "(%2d) on   %d %d\n", v, note, vel));
	SEQ_START_NOTE(awe_dev, v, note, vel);
}

void seq_stop_note(int v, int note, int vel)
{
	DEB(fprintf(stderr, "(%2d) off\n", v));
	SEQ_STOP_NOTE(awe_dev, v, note, vel);
}

void seq_stop_all(void)
{
	int i;
	DEB(fprintf(stderr, "stop all\n"));
	for (i = 0; i < MAX_MIDI_CHANNELS; i++)
		AWE_TERMINATE_CHANNEL(awe_dev, i);
	SEQ_DUMPBUF();
}	

void seq_terminate_all(void)
{
	DEB(fprintf(stderr, "terminate all\n"));
	ioctl(seqfd, SNDCTL_SEQ_RESET);
}	

void seq_pitchsense(int v, int val)
{
	val *= 100;
	DEB(fprintf(stderr, "(%2d) sens %d\n", v, val));
	SEQ_BENDER_RANGE(awe_dev, v, val);
}

void seq_pitchbend(int v, int val)
{
	DEB(fprintf(stderr, "(%2d) bend %d\n", v, val));
	SEQ_BENDER(awe_dev, v, val);
}

void seq_panning(int v, int val)
{
	DEB(fprintf(stderr, "(%2d) pan  %d\n", v, val));
	SEQ_CONTROL(awe_dev, v, CTL_PAN, val);
}

void seq_expression(int v, int val)
{
	DEB(fprintf(stderr, "(%2d) expr %d\n", v, val));
	SEQ_CONTROL(awe_dev, v, CTL_EXPRESSION, val);
}

void seq_mainvolume(int v, int val)
{
	DEB(fprintf(stderr, "(%2d) vol  %d\n", v, val));
	SEQ_CONTROL(awe_dev, v, CTL_MAIN_VOLUME, val);
}

void seq_aftertouch(int v, int note, int vel)
{
	DEB(fprintf(stderr, "(%2d) velc %d %d\n", v, note, vel));
	AWE_KEY_PRESSURE(awe_dev, v, note, vel);
}

void seq_change_volume(int vol)
{
	int atten;
	if (vol <= 0)
		atten = 0xff;
	else {
		atten = (int)(32 - log10((double)vol / 100.0) * 20 * 8 / 3);
		if (atten < 0) atten = 0;
		else if (atten > 0xff) atten = 0xff;
	}
	DEB(fprintf(stderr, "vol %d / atten %d\n", vol, atten));
	AWE_INITIAL_ATTEN(awe_dev, atten);
	SEQ_DUMPBUF();
}

void seq_set_realtime_pan(int mode)
{
#ifdef AWE_REALTIME_PAN
	DEB(fprintf(stderr, "realtime pan mode = %d\n", mode));
	AWE_REALTIME_PAN(awe_dev, mode);
#endif
}

void seq_set_drumchannels(int channels)
{
	AWE_DRUM_CHANNELS(awe_dev, channels);
}

int seq_get_sf_id(void)
{
	return 1;
}

void seq_equalizer(int bass, int treble)
{
#ifdef AWE_EQUALIZER
	AWE_EQUALIZER(awe_dev, bass, treble);
#endif
}
