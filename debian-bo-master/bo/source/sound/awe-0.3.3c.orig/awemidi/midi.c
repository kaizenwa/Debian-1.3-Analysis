/*================================================================
 * midi.c
 *	midi player program for AWE32 sound driver
 *
 * Copyright (C) 1996,1997 Takashi Iwai
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *================================================================*/

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "channel.h"
#include "seq.h"
#include "midievent.h"
#include "util.h"
#include "controls.h"

/*
 * prototypes
 */
static int play_event(MidiEvent *ev, MidiInfo *mp);
static void print_usage(void);
static void clean_up(int sig);
static void reset_status(MidiInfo *mp);
int find_midi_index(MidiInfo *mp);
int read_midi_index(char *name, MidiInfo *mp);
static void load_sub_sf(char *args, MidiInfo *mp);

static void push_clear(void);
static int push_event(MidiEvent *ev, MidiInfo *mp, int csec);
static int pop_event(MidiInfo *mp, int csec);
static int check_same_csec(MidiEvent *ev, MidiInfo *mp, int i);
static int wait_and_event(MidiEvent *ev, MidiInfo *mp, int i);
static int do_control(MidiInfo *mp);

#ifdef INCLUDE_TK_MODE
extern ControlMode tk_control_mode;
#endif
#ifdef INCLUDE_NCURSES_MODE
extern ControlMode ncurses_control_mode;
#endif
extern ControlMode dumb_control_mode;
extern ControlMode pipe_control_mode;

ControlMode *ctl_list[] = {
#ifdef INCLUDE_TK_MODE
	&tk_control_mode,
#endif
	&dumb_control_mode,
	&pipe_control_mode,
#ifdef INCLUDE_NCURSES_MODE
	&ncurses_control_mode,
#endif
};

#ifdef INCLUDE_TK_MODE
extern char *tk_display, *tk_geometry;
#endif

ControlMode *ctl;

#define numberof(array)	(sizeof(array)/sizeof(array[0]))

MidiInfo glinfo;

void main(int argc, char **argv)
{
	int cp;
	int i, mode;
	int verbose = 0;
	int playing_mode = 0;
	char *p;

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

	if (!getenv("DISPLAY"))
		ctl = &dumb_control_mode;
	else
		ctl = ctl_list[0];
	while ((cp = getopt(argc, argv,
			    "vc:r:D:i:V:POT:CGtm:"
#ifdef INCLUDE_TK_MODE
			    "d:g:"
#endif
		)) != -1) {
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
		case 'i':
			mode = optarg[0];
			for (i = 0; i < numberof(ctl_list); i++) {
				if (ctl_list[i]->id_character == mode) {
					ctl = ctl_list[i];
					break;
				}
			}
			if (i >= numberof(ctl_list)) {
		                print_usage();
				exit(1);
			}  
			break;
		case 'V':
			glinfo.master_volume = atoi(optarg);
			break;
		case 'P':
			glinfo.realtime_pan = !glinfo.realtime_pan;
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
		case 't':
			playing_mode |= PLAY_TRACE;
			break;
		case 'm':
			for (p = optarg; *p; p++) {
				switch (*p) {
				case 't':
					playing_mode |= PLAY_TRACE; break;
				case 's':
					playing_mode &= ~PLAY_NORMAL;
					playing_mode |= PLAY_SHUFFLE; break;
				case 'n':
					playing_mode &= ~PLAY_SHUFFLE;
					playing_mode |= PLAY_NORMAL; break;
				case 'p':
					playing_mode |= PLAY_AUTOSTART; break;
				case 'q':
					playing_mode |= PLAY_AUTOEXIT;
					playing_mode &= ~PLAY_REPEAT; break;
				case 'r':
					playing_mode |= PLAY_REPEAT;
					playing_mode &= ~PLAY_AUTOEXIT; break;
				default:
					print_usage();
					exit(1);
				}
			}
			break;
#ifdef INCLUDE_TK_MODE
		case 'g':
			tk_geometry = optarg;
			break;
		case 'd':
			tk_display = optarg;
			break;
#endif
		default:
			print_usage();
			exit(1);
		}
	}
	if (optind >= argc && ctl->need_args) {
		print_usage();
		exit(1);
	}

	if (! seq_init(NULL)) {
		if (seq_get_sf_id() < 1) {
			ctl->cmsg(CMSG_ERROR, -1, "No SoundFont is loaded yet");
			exit(1);
		}
		seq_end();
	}

	ctl->verbosity = verbose;
	ctl->playing_mode = playing_mode;

	if (argc > optind && *argv[optind] == '-')
		ctl->open(1, 0);
	else
		ctl->open(0, 0);
	ctl->pass_playing_list(argc - optind, argv + optind);
	ctl->close();

	exit(0);
}

static void print_usage()
{
	int i;
	printf("drvmidi ver.0.3.3f  MIDI player for AWE sound driver\n");
	printf("     copyright (c) 1996,1997 by Takashi Iwai\n");
	printf("usage: drvmidi [-options] midifiles[.gz] ...\n");
	printf("options are:\n");
	printf("  -i interface: available interfaces are\n");
	for (i = 0; i < numberof(ctl_list); i++) {
		printf("        %c : %s", ctl_list[i]->id_character,
		       ctl_list[i]->id_name);
		if (ctl == ctl_list[i])
			printf("  (default)");
		printf("\n");
	}
	printf("  -D channel: turn on (>0) or off (<0) drum channel\n");
	printf("  -c mode: set chorus mode (0-7)\n");
	printf("      0=chorus1, 1=chorus2, 2=chorus3, 3=chorus4,\n");
	printf("      4=feedback, 5=flanger, 6=short delay, 7=short delay2\n");
	printf("      default : %d\n", glinfo.chorus);
	printf("  -r mode: set reverb mode (0-7)\n");
	printf("      0=room1, 1=room2, 2=room3, 3=hall1,\n");
	printf("      4=hall2, 5=plate, 6=delay, 7=panning delay,\n");
	printf("      default : %d\n", glinfo.reverb);
	printf("  -V volume: set the master volume (0-100%%)\n");
	printf("  -O : accept all notes/sounds off controls\n");
	printf("  -P : change panning position in real time\n");
	printf("  -C : do not check short note-on/off events\n");
	printf("  -G : do not check GS part sysex macro\n");
	printf("  -t : turn on tracing mode (equivalent with -mt)\n");
	printf("  -m mode(s): set playing mode\n");
	printf("      n: normal play, s: shffule play, r: repeat play\n");
#if defined(INCLUDE_TK_MODE) || defined(INCLUDE_NCURSES_MODE)
	printf("      t: trace on,\n");
#endif
#ifdef INCLUDE_TK_MODE
	printf("      p: auto start, q: auto exit\n");
	printf("  -d display: set window display\n");
	printf("  -g geometry: set window geometry\n");
#endif
}

static int seq_opened = 0;

int midi_open(MidiInfo *mp)
{
	int chorus, reverb, volume;

	if (! seq_opened) {
		if (seq_init(mp)) {
			ctl->cmsg(CMSG_ERROR, -1, "can't open sequencer device");
			return 1;
		}
		add_signal(SIGTERM, clean_up, 1);
		add_signal(SIGINT, clean_up, 1);
		add_signal(SIGQUIT, clean_up, 1);
	}

	if (mp->chorus >= 0 && mp->chorus <= 7)
		chorus = mp->chorus;
	else
		chorus = glinfo.chorus;
	ctl->chorus_mode(chorus);
	seq_set_chorus(chorus);

	if (mp->reverb >= 0 && mp->reverb <= 7)
		reverb = mp->reverb;
	else
		reverb = glinfo.reverb;
	ctl->reverb_mode(reverb);
	seq_set_reverb(reverb);

	if (mp != &glinfo && mp->master_volume >= 0)
		volume = mp->master_volume;
	else
		volume = glinfo.master_volume;
	seq_change_volume(volume);
	seq_equalizer(mp->bass_level, mp->treble_level);
	ctl->master_volume(volume);

	seq_opened = 1;
	return 0;
}

void midi_close(MidiInfo *mp)
{
	if (seq_opened) {
		seq_terminate_all();
		seq_end();
		seq_opened = 0;
	}
}

static void clean_up(int sig)
{
	ctl->cmsg(CMSG_INFO, 0, "\nterminating..");
	seq_terminate_all();
	seq_end();
	ctl->close();
}

/*================================================================*/

static int jump_to(MidiInfo *mp, int csec)
{
	int i;
	MidiEvent *ev;

	seq_terminate_all();
	channel_init(mp);
	push_clear();

	ev = mp->list;
	for (i = 0; i < mp->nlists; i++, ev++) {
		if (ev->csec >= csec)
			break;
		do_midi_event(ev, mp, 1);
	}

	ctl->reset(mp);
	channel_set();

	if (i >= mp->nlists) {
		mp->curt = mp->list[mp->nlists-1].time;
		mp->curcs = mp->list[mp->nlists-1].csec;
		mp->curev = i;
		return i;
	}

	mp->curt = ev->time;
	mp->prevcs = mp->curcs = ev->csec;
	seq_clear(mp->curcs);

	mp->curev = i - 1;
	return mp->curev;
}

static void reset_status(MidiInfo *mp)
{
	seq_terminate_all();
	channel_init(mp);
	push_clear();

	ctl->reset(mp);
	ctl->current_time(0);

	mp->curt = 0;
	mp->prevcs = 0; mp->curcs = 0;
	mp->tempo = MIDI_DEFAULT_TEMPO;
	seq_clear(0);
	seq_set_drumchannels(mp->drumflag);
	channel_set();
}


int play_midi_file(MidiInfo *mp)
{
	int piped;
	MidiEvent *ev;
	FILE *fp;
	char *p;
	int cmd;

	if ((fp = CmpOpenFile(mp->filename, &piped)) == NULL) {
		ctl->cmsg(CMSG_ERROR, -1, "can't open MIDI file %s",
			  mp->filename);
		return RC_NEXT;
	}
	ctl->cmsg(CMSG_INFO, 0, "reading midi %s:", mp->filename);
	if ((p = strrchr(mp->filename, '/')) != NULL)
		ctl->file_name(p + 1);
	else
		ctl->file_name(mp->filename);

	find_midi_index(mp);
	ev = ReadMidiFile(fp, mp);
	CmpCloseFile(fp, piped);

	if (ev == NULL)
		return RC_NEXT;

	if (midi_open(mp))
		return RC_NONE;

	ctl->total_time(ev[mp->nlists-1].csec);

	for (;;) {
		cmd = play_event(ev, mp);
		if (cmd != RC_TUNE_END || !ctl->repeated)
			break;
	}
	if (cmd != RC_KILL)
		usleep(500000);
	seq_terminate_all();
	FreeMidiFile(mp); /* free event list */

	if (cmd == RC_TUNE_END)
		return RC_NEXT;
	if (cmd == RC_QUIT || cmd == RC_KILL)
		midi_close(mp);
	return cmd;
}


/*----------------------------------------------------------------
 * trick to avoid send on/off at the same time
 *----------------------------------------------------------------*/

/* temporary event-list handlers */

#define MAX_PUSHLIST		32

static int start_push, last_push, num_pushed;
static MidiEvent push_list[MAX_PUSHLIST];

static void push_clear(void)
{
	start_push = last_push = 0;
	num_pushed = 0;
}

static int push_event(MidiEvent *ev, MidiInfo *mp, int csec)
{
	if (num_pushed >= MAX_PUSHLIST)
		return do_midi_event(ev, mp, 0);
	memcpy(&push_list[last_push], ev, sizeof(MidiEvent));
	push_list[last_push].csec = csec;
	num_pushed++;
	last_push++;
	if (last_push >= MAX_PUSHLIST)
		last_push = 0;
	return RC_NONE;
}

static int pop_event(MidiInfo *mp, int csec)
{
	int rc;

	while (num_pushed > 0) {
		if (push_list[start_push].csec < csec &&
		    push_list[start_push].csec > mp->curcs) {
			seq_wait(push_list[start_push].csec - mp->curcs);
			mp->curcs = push_list[start_push].csec;
		}
		if (push_list[start_push].csec > csec)
			break;
		rc = do_midi_event(&push_list[start_push], mp, 0);
		num_pushed--;
		start_push++;
		if (start_push >= MAX_PUSHLIST)
			start_push = 0;
		if (rc != RC_NONE)
			return rc;
	}
	return RC_NONE;
}

/*----------------------------------------------------------------*/

/* check the note-on event was done in the same csec */
static int check_same_csec(MidiEvent *ev, MidiInfo *mp, int i)
{
	int j, cs = ev[i].csec + 1;

	/* check only note-off events */
	if ((ev[i].type != ME_NOTEON || ev[i].b != 0) &&
	    ev[i].type != ME_NOTEOFF)
		return 0;

	/* search forward the note-on event;
	 * if found, send this event as usual.
	 */
	for (j = i + 1; j < mp->nlists; j++) {
		if (ev[j].csec > cs)
			break;
		if (ev[j].type == ME_NOTEON &&
		    ev[j].channel == ev[i].channel &&
		    ev[j].a == ev[i].a)
			return 0;
	}
	/* search backward the note-on event;
	 * if found in the same csec, do this event later.
	 */
	for (j = i - 1; j >= 0; j--) {
		if (ev[j].csec < ev[i].csec)
			return 0;
		if (ev[j].type == ME_NOTEON &&
		    ev[j].channel == ev[i].channel &&
		    ev[j].a == ev[i].a)
			return 1;
	}
	return 0;
}

#define UPDATE_DELTA	10

/* send wait and do the event */
static int wait_and_event(MidiEvent *ev, MidiInfo *mp, int i)
{
	int rc;
	if (num_pushed) {
		if ((rc = pop_event(mp, ev[i].csec)) != RC_NONE)
			return rc;
	}

	/* check note on/off */
	if (mp->check_same_csec) {
		if (check_same_csec(ev, mp, i))
			return push_event(ev + i, mp, ev[i].csec + 1);
	}

	if (ev[i].csec > mp->curcs) {
		/* stop if there's more than 40 seconds of nothing */
		if (ev[i].csec - mp->curcs > 4096)
			return RC_TUNE_END;
		seq_wait(ev[i].csec - mp->curcs);
	}
	if ((rc = do_midi_event(ev + i, mp, 0)) != RC_NONE)
		return rc;

	mp->curt = ev[i].time;
	if (ev[i].csec > mp->curcs) {
		if (! ctl->need_sync) {
			mp->curcs = ev[i].csec;
			return RC_NONE;
		}
			
		for (;;) {
			int cs = seq_curtime();
			if (cs >= mp->prevcs + UPDATE_DELTA) {
				mp->curcs = cs;
				ctl->update(mp);
				mp->prevcs = cs;
			}
			ctl->current_time(mp->curcs);
			if ((rc = do_control(mp)) == RC_JUMP)
				return RC_NONE;
			else if (rc != RC_NONE)
				return rc;

			cs = seq_curtime();
			if (ev[i].csec - cs < 15)
				break;
			usleep(100000);
		}
		seq_sync(ev[i].csec);
		mp->curcs = ev[i].csec;
	}
	return RC_NONE;
}

/*----------------------------------------------------------------
 * get input message from control panel
 *----------------------------------------------------------------*/

static int do_control(MidiInfo *mp)
{
	int cmd, val;

	val = 0;
	cmd = ctl->read(&val);
	if (cmd == RC_NONE) return RC_NONE;

	switch (cmd) {
	case RC_PAUSE:
		seq_stop_all();
		for (;;) {
			val = 1;
			cmd = ctl->read(&val);
			if (cmd == RC_CONTINUE ||
			    cmd == RC_RESUME)
				break;
			else if (cmd == RC_KILL) {
				seq_terminate_all();
				return cmd;
			}
		}
		/* no break */
	case RC_RESUME:
		jump_to(mp, mp->curcs);
		return RC_JUMP;
	case RC_CHANGE_CHORUS:
		mp->chorus = val;
		seq_set_chorus(val);
		return RC_NONE;
	case RC_CHANGE_REVERB:
		mp->reverb = val;
		seq_set_reverb(val);
		return RC_NONE;
	case RC_CHANGE_VOLUME:
		glinfo.master_volume = val;
		seq_change_volume(val);
		ctl->master_volume(val);
		return RC_NONE;
	case RC_CHANGE_BASS:
		glinfo.bass_level = val;
		seq_equalizer(glinfo.bass_level, glinfo.treble_level);
		return RC_NONE;
	case RC_CHANGE_TREBLE:
		glinfo.treble_level = val;
		seq_equalizer(glinfo.bass_level, glinfo.treble_level);
		return RC_NONE;
	case RC_RESTART:
		mp->curev = -1;
		reset_status(mp);
		return RC_JUMP;
	case RC_JUMP:
		jump_to(mp, val);
		return RC_JUMP;
	case RC_FORWARD:
		jump_to(mp, mp->curcs + 200);
		return RC_JUMP;
	case RC_BACK:
		jump_to(mp, mp->curcs - 200);
		return RC_JUMP;
	case RC_PREVIOUS:
		if (!ctl->fuzzy_prev || mp->curcs > 500) {
			mp->curev = -1;
			reset_status(mp);
			return RC_JUMP;
		}
		cmd = RC_REALLY_PREVIOUS;
		/* continue */
	case RC_KILL:
	case RC_NEXT:
	case RC_REALLY_PREVIOUS:
	case RC_LOAD_FILE:
	case RC_QUIT:
		seq_terminate_all();
		return cmd;
	}
	return RC_NONE;
}


/*----------------------------------------------------------------
 * play event list
 *----------------------------------------------------------------*/

static int play_event(MidiEvent *ev, MidiInfo *mp)
{
	int cmd;
	reset_status(mp);
	for (mp->curev = 0; mp->curev < mp->nlists; mp->curev++) {
		if ((cmd = wait_and_event(ev, mp, mp->curev)) != RC_NONE)
			return cmd;
	}
	seq_sync(mp->curcs);
	return RC_TUNE_END;
}


/*================================================================
 * read index file
 *================================================================*/

#define MIDI_INDEX_SUFFIX	".id"

int find_midi_index(MidiInfo *mp)
{
	char name[256], *ext;

	strcpy(name, mp->filename);
	if (CmpSearchFile(name) >= 0) {
		ext = strchr(name, '.');
		if (ext) *ext = 0;
	}
	ext = strrchr(name, '.');
	if (ext)
		strcpy(ext, MIDI_INDEX_SUFFIX);
	else
		strcat(name, MIDI_INDEX_SUFFIX);

	return read_midi_index(name, mp);
}


int read_midi_index(char *name, MidiInfo *mp)
{
	FILE *fp;
	char line[256];
	int ival;

	/*fprintf(stderr, "searching %s\n", name);*/
	if ((fp = fopen(name, "r")) ==NULL)
		return 0;

	while (fgets(line, sizeof(line), fp)) {
		char *tok, *val, *p;
		/* skip white spaces at the beginning */
		for (tok = line; *tok && isspace(*tok); tok++)
			;
		if (*tok == '#') /* skip comment line */
			continue;
		/* search the value string */
		for (val = tok; *val && !isspace(*val); val++)
			;
		if (*val) {
			*val = 0;
			val++;
		}
		for (; isspace(*val); val++)
			;

		/* delete linefeed code */
		for (p = val; *p && *p != '\n'; p++)
			;
		*p = 0;

		ival = (int)strtol(val, NULL, 0);

		if (strcmp(tok, "chorus") == 0) {
			mp->chorus = ival;
			ctl->cmsg(CMSG_INFO, 0, "set chorus mode %d", ival);
		} else if (strcmp(tok, "reverb") == 0) {
			mp->reverb = ival;
			ctl->cmsg(CMSG_INFO, 0, "set reverb mode %d", ival);
		} else if (strcmp(tok, "volume") == 0) {
			mp->master_volume = ival;
			ctl->cmsg(CMSG_INFO, 0, "set volume %d%%", ival);
		} else if (strcmp(tok, "realpan") == 0) {
			mp->realtime_pan = ival;
			ctl->cmsg(CMSG_INFO, 0, "set realtime pan %d", ival);
		} else if (strcmp(tok, "samecsec") == 0) {
			mp->check_same_csec = ival;
			ctl->cmsg(CMSG_INFO, 0, "set same csec %d", ival);
		} else if (strcmp(tok, "gsmacro") == 0) {
			mp->check_gs_macro = ival;
			ctl->cmsg(CMSG_INFO, 0, "set gs macro %d", ival);
		} else if (strcmp(tok, "drumflag") == 0) {
			mp->drumflag = ival;
			ctl->cmsg(CMSG_INFO, 0, "set drum channels 0x%x", ival);
		} else if (strcmp(tok, "acceptall") == 0) {
			mp->accept_all_off = ival;
			ctl->cmsg(CMSG_INFO, 0, "set all notes access %d", ival);
		} else if (strcmp(tok, "subsf") == 0) {
			if (val && *val) {
				load_sub_sf(val, mp);
				ctl->cmsg(CMSG_INFO, 0, "loading subsf %s", val);
			}
		} else if (strcmp(tok, "tracks") == 0) {
			mp->track_nums = ival;
			ctl->cmsg(CMSG_INFO, 0, "track numbers %d", ival);
		} else if (strcmp(tok, "title") == 0) {
			ctl->file_name(val);
		}
	}
	fclose(fp);
	return 1;
}

/*----------------------------------------------------------------*/

#define LOAD_CMD	"sfxload"
#define LOAD_DEFARGS	"-I -x -s -b1"

static void load_sub_sf(char *args, MidiInfo *mp)
{
	char cmdline[256];
	midi_close(mp);
	sprintf(cmdline, "%s %s %s", LOAD_CMD, LOAD_DEFARGS, args);
	system(cmdline);
}
