/*================================================================
 * txtplay
 *	play textized midi event
 *================================================================*/

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include "channel.h"
#include "seq.h"
#include "midievent.h"
#include "util.h"
#include "controls.h"

/*----------------------------------------------------------------*/

#if 0
#define DBG(X)  X
#else
#define DBG(X)  /**/
#endif

/*----------------------------------------------------------------*/

/*
 * prototypes
 */
static void clean_up(int sig);
static void cvt_cb(MidiEvent *ev, int ch, int v1, int v2);
static void cvt_cw(MidiEvent *ev, int ch, int v1, int v2);
static void cvt_cd(MidiEvent *ev, int ch, int v1, int v2);
static void cvt_nrpn(MidiEvent *ev, int ch, int v1, int v2);
static void cvt_tag(MidiEvent *ev, int ch, int v1, int v2);
static void cvt_tempo(MidiEvent *ev, int ch, int v1, int v2);
static int parse_text_file(char *buf, MidiEvent *ev);

MidiInfo glinfo;

extern ControlMode dumb_control_mode;
ControlMode *ctl = &dumb_control_mode;

#define numberof(array)	(sizeof(array)/sizeof(array[0]))

void main(int argc, char **argv)
{
	char buf[256];
	MidiEvent ev;
	FILE *fp;
	int cp, mode, verbose = 0;

	glinfo.chorus = 2;
	glinfo.reverb = 4;
	glinfo.master_volume = 100;
	glinfo.bass_level = 5;
	glinfo.treble_level = 9;
	glinfo.realtime_pan = 1;
	glinfo.multi_part = 0;
	glinfo.check_same_csec = 1;
	glinfo.check_gs_macro = 1;
	glinfo.drumflag = (1<<9)|(1<<25); /* default drum is channel 10 & 26 */
	glinfo.accept_all_off = 0;
	glinfo.track_nums = 16;

	while ((cp = getopt(argc, argv, "vtD:i:c:Or:PRV:XC:")) != -1) {
		switch (cp) {
		case 'v':
			verbose++;
			break;
		case 'c':
			glinfo.chorus = atoi(optarg);
			break;
		case 'r':
			glinfo.reverb = atoi(optarg);
			break;
		case 'D':
			mode = atoi(optarg);
			if (mode < 0)
				glinfo.drumflag &= ~(1 << (-mode - 1));
			else
				glinfo.drumflag |= (1<< (mode - 1));
			break;
		case 'O':
			glinfo.accept_all_off = !glinfo.accept_all_off;
			break;
		case 'C':
			glinfo.check_same_csec = !glinfo.check_same_csec;
			break;
		case 'G':
			glinfo.check_gs_macro = !glinfo.check_gs_macro;
			break;
		case 'T':
			glinfo.track_nums = atoi(optarg);
			break;
		default:
			printf("illegal options\n");
			exit(1);
		}
	}

	seq_init(&glinfo);
	signal(SIGTERM, clean_up);
	signal(SIGINT, clean_up);
	signal(SIGQUIT, clean_up);

	seq_set_chorus(glinfo.chorus);
	seq_set_reverb(glinfo.reverb);
	seq_equalizer(glinfo.bass_level, glinfo.treble_level);
	seq_change_volume(glinfo.master_volume);

	channel_init(&glinfo);

	glinfo.curt = 0;
	glinfo.curcs = 0;
	glinfo.tempo = MIDI_DEFAULT_TEMPO;
	seq_clear(0);

	fp = stdin;
	while (fgets(buf, sizeof(buf), fp)) {
		if (parse_text_file(buf, &ev) == -1)
			continue;
		if (ev.time > glinfo.curt) {
			/* stop if there's more than 40 seconds of nothing */
			if (ev.csec - glinfo.curcs > 4096)
				break;
			if (ev.csec > glinfo.curcs) {
				seq_wait(ev.csec - glinfo.curcs);
				glinfo.curcs = ev.csec;
				ctl->current_time(glinfo.curcs);
			}
		}
		glinfo.curt = ev.time;
		if (do_midi_event(&ev, &glinfo, 0))
			break;
		seq_sync(ev.csec);
	}

	exit(0);
}

int play_midi_file(MidiInfo *mp)
{
	return RC_NONE;
}

static void clean_up(int sig)
{
	fprintf(stderr, "\nterminating..");
	seq_terminate_all();
	seq_end();
	exit(0);
}


typedef struct _CvtFmt {
	char *tag;
	int type;
	void (*convert)(MidiEvent *ev, int chan, int v1, int v2);
} CvtFmt;

static CvtFmt formats[] = {
	{"note_on", ME_NOTEON, cvt_cd},
	{"note_off", ME_NOTEOFF, cvt_cd},
	{"key_press", ME_KEYPRESSURE, cvt_cd},
	{"pitch_sens", ME_PITCH_SENS, cvt_cb},
	{"pitch_wheel", ME_PITCHWHEEL, cvt_cw},
	{"main_volume", ME_MAINVOLUME, cvt_cb},
	{"pan", ME_PAN, cvt_cb},
	{"expression", ME_EXPRESSION, cvt_cb},
	{"program", ME_PROGRAM, cvt_cb},
	{"sustain", ME_SUSTAIN, cvt_cb},
	{"reset_controllers", ME_RESET_CONTROLLERS, cvt_tag},
	{"all_notes_off", ME_ALL_NOTES_OFF, cvt_tag},
	{"all_sounds_off", ME_ALL_SOUNDS_OFF, cvt_tag},
	{"tone_bank", ME_TONE_BANK, cvt_cb},
	{"chorus", ME_CHORUS, cvt_cb},
	{"reverb", ME_REVERB, cvt_cb},
	{"tempo", ME_TEMPO, cvt_tempo},
	{"lyric", ME_NONE, cvt_tag},
	{"chorus_mode", ME_SET_CHORUS_MODE, cvt_cb},
	{"reverb_mode", ME_SET_CHORUS_MODE, cvt_cb},
	{"end_of_tune", ME_EOT, cvt_tag},
	{"e1_delay", ME_ENV1_DELAY, cvt_nrpn},
	{"e1_attack", ME_ENV1_ATTACK, cvt_nrpn},
	{"e1_hold", ME_ENV1_HOLD, cvt_nrpn},
	{"e1_decay", ME_ENV1_DECAY, cvt_nrpn},
	{"e1_release", ME_ENV1_RELEASE, cvt_nrpn},
	{"e1_sustain", ME_ENV1_SUSTAIN, cvt_nrpn},
	{"e1_pitch", ME_ENV1_PITCH, cvt_nrpn},
	{"e1_cutoff", ME_ENV1_CUTOFF, cvt_nrpn},
	{"e2_delay", ME_ENV2_DELAY, cvt_nrpn},
	{"e2_attack", ME_ENV2_ATTACK, cvt_nrpn},
	{"e2_hold", ME_ENV2_HOLD, cvt_nrpn},
	{"e2_decay", ME_ENV2_DECAY, cvt_nrpn},
	{"e2_release", ME_ENV2_RELEASE, cvt_nrpn},
	{"e2_sustain", ME_ENV2_SUSTAIN, cvt_nrpn},
	{"L1_delay", ME_LFO1_DELAY, cvt_nrpn},
	{"L1_freq", ME_LFO1_FREQ, cvt_nrpn},
	{"L1_volume", ME_LFO1_VOLUME, cvt_nrpn},
	{"L1_pitch", ME_LFO1_PITCH, cvt_nrpn},
	{"L1_cutoff", ME_LFO1_CUTOFF, cvt_nrpn},
	{"L2_delay", ME_LFO2_DELAY, cvt_nrpn},
	{"L2_freq", ME_LFO2_FREQ, cvt_nrpn},
	{"L2_pitch", ME_LFO2_PITCH, cvt_nrpn},
	{"fx_initpitch", ME_INIT_PITCH, cvt_nrpn},
	{"fx_chorus", ME_FX_CHORUS, cvt_nrpn},
	{"fx_reverb", ME_FX_REVERB, cvt_nrpn},
	{"fx_cutoff", ME_CUTOFF, cvt_nrpn},
	{"fx_filterQ", ME_FILTERQ, cvt_nrpn},
};


static void cvt_cb(MidiEvent *ev, int ch, int v1, int v2)
{
	ev->channel = ch;
	ev->a = v1;
	DBG(fprintf(stderr, "(%d) %d\n", ev->channel, ev->a));
}

static void cvt_cw(MidiEvent *ev, int ch, int v1, int v2)
{
	ev->channel = ch;
	ev->a = v1 % 128;
	ev->b = v1 / 128;
	DBG(fprintf(stderr, "(%d) %d %d\n", ev->channel, ev->a, ev->b));
}

static void cvt_cd(MidiEvent *ev, int ch, int v1, int v2)
{
	ev->channel = ch;
	ev->a = v1;
	ev->b = v2;
	DBG(fprintf(stderr, "(%d) %d %d\n", ev->channel, ev->a, ev->b));
}

static void cvt_nrpn(MidiEvent *ev, int ch, int v1, int v2)
{
	ev->channel = ch;
	ev->a = v1;
	ev->b = v2;
	DBG(fprintf(stderr, "(%d) %d %d\n", ev->channel, ev->a, ev->b));
}

static void cvt_tag(MidiEvent *ev, int ch, int v1, int v2)
{
	DBG(fprintf(stderr, "\n"));
}

static void cvt_tempo(MidiEvent *ev, int ch, int v1, int v2)
{
	ev->a = ch / 65536;
	ch -= ev->a * 65536;
	ev->b = ch / 256;
	ch -= ev->b * 256;
	ev->channel = ch;
	DBG(fprintf(stderr, "(%d) %d %d\n", ev->channel, ev->a, ev->b));
}


static int parse_text_file(char *buf, MidiEvent *ev)
{
	int i;
	char *time, *cmd, *channel, *val1, *val2;
	int chn_val, val1_val, val2_val;

	if (*buf == 'm')
		return -1;

	chn_val = 0;
	val1_val = 0;
	val2_val = 0;

	time = strtok(buf, " \n");
	if (time)
		ev->csec = atoi(time);
	time = strtok(NULL, " \n");
	if (time)
		ev->time = atoi(time);
	cmd = strtok(NULL, " \n");
	channel = strtok(NULL, " \n");
	if (channel) {
		if (*channel == '(') {
			if (channel[1] == '0')
				chn_val = channel[2] - '0';
			else
				chn_val = channel[2] - '0' + 10;
		}
		else /* tempo */
			chn_val = atoi(channel);
	}
	val1 = strtok(NULL, " \n");
	if (val1)
		val1_val = atoi(val1);
	val2 = strtok(NULL, " \n");
	if (val2)
		val2_val = atoi(val2);

	if (strcmp(cmd, "unknown") == 0)
		return -1;

	for (i = 0; i < numberof(formats); i++) {
		if (strcmp(cmd, formats[i].tag) == 0) {
			ev->type = formats[i].type;
			DBG(fprintf(stderr, "%s: ", cmd));
			formats[i].convert(ev, chn_val, val1_val, val2_val);
			return 0;
		}
	}
	fprintf(stderr, "unknown cmd=%s\n", cmd);
	return -1;
}
