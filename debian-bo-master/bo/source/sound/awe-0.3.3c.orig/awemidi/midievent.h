/*----------------------------------------------------------------
 * midievent.h
 *	read midi file and parse events
 *----------------------------------------------------------------*/

#ifndef MIDIEVENT_H_DEF
#define MIDIEVENT_H_DEF

#ifdef linux
#include <asm/byteorder.h>
#endif

typedef struct _MidiEvent {
	int time;	/* tempo counter */
	int csec;	/* realtime csec counter */
	unsigned char channel, type, a, b; /* channel, type, parameters */
	void *misc;	/* misc. parameters */
} MidiEvent;

typedef struct _MidiInfo {
	int format;
	int tracks;
	int divisions;

	/* variable data */
	int curt;
	int prevcs, curcs;
	int curev;
	int tempo;

	/* midi data */
	int nlists;
	MidiEvent *list;

	/* other control parameters */
	int chorus, reverb;
	int master_volume;
	int bass_level, treble_level;
	int accept_all_off;
	int realtime_pan;
	int multi_part;
	int check_same_csec;
	int check_gs_macro;
	unsigned long drumflag;
	int track_nums;

	char *filename;
} MidiInfo;

#define MidiByte(ep)	((ep)->a)
#define MidiWord(ep)	((ep)->a + (ep)->b * 128)
#define MidiTempo(ep)	((ep)->channel + (ep)->b * 256 + (ep)->a * 65536)


/* Midi events */
enum {
	ME_NONE, ME_NOTEON, ME_NOTEOFF, ME_KEYPRESSURE,
	ME_MAINVOLUME, ME_PAN, ME_SUSTAIN, ME_EXPRESSION,
	ME_PITCHWHEEL, ME_PROGRAM, ME_TEMPO, ME_PITCH_SENS,

	ME_ALL_SOUNDS_OFF, ME_RESET_CONTROLLERS, ME_ALL_NOTES_OFF,
	ME_TONE_BANK,

	ME_LYRIC,

	ME_REVERB, ME_CHORUS,

	ME_SET_REVERB_MODE, ME_SET_CHORUS_MODE,

	ME_ENV1_DELAY, ME_ENV1_ATTACK, ME_ENV1_HOLD, ME_ENV1_DECAY,
	ME_ENV1_RELEASE, ME_ENV1_SUSTAIN, ME_ENV1_PITCH, ME_ENV1_CUTOFF,

	ME_ENV2_DELAY, ME_ENV2_ATTACK, ME_ENV2_HOLD, ME_ENV2_DECAY,
	ME_ENV2_RELEASE, ME_ENV2_SUSTAIN,

	ME_LFO1_DELAY, ME_LFO1_FREQ, ME_LFO1_VOLUME,
	ME_LFO1_PITCH, ME_LFO1_CUTOFF,

	ME_LFO2_DELAY, ME_LFO2_FREQ, ME_LFO2_PITCH,
	ME_INIT_PITCH, ME_FX_CHORUS, ME_FX_REVERB, ME_CUTOFF, ME_FILTERQ,

	ME_EOT = 99,
};

/* default values */
#define MIDI_DEFAULT_TEMPO	500000

/* prototypes */
MidiEvent *ReadMidiFile(FILE *fp, MidiInfo *infop);
void FreeMidiFile(MidiInfo *infop);

int do_midi_event(MidiEvent *ev, MidiInfo *minfo, int no_ctl);

extern MidiInfo glinfo;
int play_midi_file(MidiInfo *minfo);
int midi_open(MidiInfo *mp);
void midi_close(MidiInfo *mp);


#endif
