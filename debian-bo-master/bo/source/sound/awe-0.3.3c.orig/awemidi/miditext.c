/*================================================================
 * textize MIDI data
 *================================================================*/

#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include "midievent.h"
#include "util.h"
#include "controls.h"
#ifdef linux
#include <linux/awe_voice.h>
#else
#include <awe_voice.h>
#endif
#include "awe_effect.h"

static int cmsg(int type, int verbosity_level, char *fmt, ...);

static ControlMode dummy_ctl = 
{
  "dumb interface", 'd',
  FALSE,
  FALSE,
  FALSE,
  0,0,0,0,
  NULL,NULL,NULL,NULL,cmsg,
  NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,
};

ControlMode *ctl = &dummy_ctl;

static int cmsg(int type, int verbosity_level, char *fmt, ...)
{
	va_list ap;
	if (ctl->verbosity <= verbosity_level) return 0;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	printf("\n");
	va_end(ap);
	return 0;
}

static void PrintCD(char *msg, MidiEvent *e)
{
	printf("%-16s (%02d) %d %d\n", msg, e->channel, e->a, e->b);
}

static void PrintCB(char *msg, MidiEvent *e)
{
	printf("%-16s (%02d) %d\n", msg, e->channel, e->a);
}

static void PrintCW(char *msg, MidiEvent *e)
{
	printf("%-16s (%02d) %d [%04x]\n", msg, e->channel,
	       MidiWord(e), MidiWord(e));
}

static struct set_effect_rec {
	int type;
	int awe_effect;
	unsigned short (*convert)(int);
} effect_rec[] = {
	{ME_ENV1_DELAY,		AWE_FX_ENV1_DELAY,	fx_env1_delay},
	{ME_ENV1_ATTACK,	AWE_FX_ENV1_ATTACK,	fx_env1_attack},
	{ME_ENV1_HOLD,		AWE_FX_ENV1_HOLD,	fx_env1_hold},
	{ME_ENV1_DECAY,		AWE_FX_ENV1_DECAY,	fx_env1_decay},
	{ME_ENV1_RELEASE,	AWE_FX_ENV1_RELEASE,	fx_env1_release},
	{ME_ENV1_SUSTAIN,	AWE_FX_ENV1_SUSTAIN,	fx_env1_sustain},
	{ME_ENV1_PITCH,		AWE_FX_ENV1_PITCH,	fx_env1_pitch},
	{ME_ENV1_CUTOFF,	AWE_FX_ENV1_CUTOFF,	fx_env1_cutoff},

	{ME_ENV2_DELAY,		AWE_FX_ENV2_DELAY,	fx_env2_delay},
	{ME_ENV2_ATTACK,	AWE_FX_ENV2_ATTACK,	fx_env2_attack},
	{ME_ENV2_HOLD,		AWE_FX_ENV2_HOLD,	fx_env2_hold},
	{ME_ENV2_DECAY,		AWE_FX_ENV2_DECAY,	fx_env2_decay},
	{ME_ENV2_RELEASE,	AWE_FX_ENV2_RELEASE,	fx_env2_release},
	{ME_ENV2_SUSTAIN,	AWE_FX_ENV2_SUSTAIN,	fx_env2_sustain},

	{ME_LFO1_DELAY,		AWE_FX_LFO1_DELAY,	fx_lfo1_delay},
	{ME_LFO1_FREQ,		AWE_FX_LFO1_FREQ,	fx_lfo1_freq},
	{ME_LFO1_VOLUME,	AWE_FX_LFO1_VOLUME,	fx_lfo1_volume},
	{ME_LFO1_PITCH,		AWE_FX_LFO1_PITCH,	fx_lfo1_pitch},
	{ME_LFO1_CUTOFF,	AWE_FX_LFO1_CUTOFF,	fx_lfo1_cutoff},

	{ME_LFO2_DELAY,		AWE_FX_LFO2_DELAY,	fx_lfo2_delay},
	{ME_LFO2_FREQ,		AWE_FX_LFO2_FREQ,	fx_lfo2_freq},
	{ME_LFO2_PITCH,		AWE_FX_LFO2_PITCH,	fx_lfo2_pitch},

	{ME_INIT_PITCH,		AWE_FX_INIT_PITCH,	fx_init_pitch},
	{ME_FX_CHORUS,		AWE_FX_CHORUS,		fx_chorus},
	{ME_FX_REVERB,		AWE_FX_REVERB,		fx_reverb},
	{ME_CUTOFF,		AWE_FX_CUTOFF,		fx_cutoff},
	{ME_FILTERQ,		AWE_FX_FILTERQ,		fx_filterQ},
};

static char *fx_name[] = {
	"e1_delay", "e1_attack", "e1_hold", "e1_decay",
	"e1_release", "e1_sustain", "e1_pitch", "e1_cutoff",
	"e2_delay", "e2_attack", "e2_hold", "e2_decay",
	"e2_release", "e2_sustain",
	"L1_delay", "L1_freq", "L1_volume", "L1_pitch", "L1_cutoff",
	"L2_delay", "L2_freq", "L2_pitch",
	"fx_initpitch", "fx_chorus", "fx_reverb", "fx_cutoff", "fx_filterQ",
	"fx_sample", "fx_loop_start", "fx_loop_end",
};

#define numberof(ary)	(sizeof(ary)/sizeof(ary[0]))

static void NRPNEvent(MidiEvent *ev)
{
	int i, val;
	/* LSB:a, MSB:b */
	val = (ev->a + ev->b * 128) - 8192;

	for (i = 0; i < numberof(effect_rec); i++) {
		if (effect_rec[i].type == ev->type) {
			short cval = effect_rec[i].convert(val);
			printf("%-16s (%02d) %d [%x:%x] --> %x\n",
			       fx_name[effect_rec[i].awe_effect],
			       ev->channel,
			       val, ev->a, ev->b, (unsigned short)cval);
			return;
		}
	}
	printf("%-16s (%02d) %d %d\n", "unknown", ev->channel, ev->a, ev->b);
}

static void usage()
{
	fprintf(stderr, "usage: miditext [-v] midifile\n");
}

void main(int argc, char **argv)
{
	FILE *fp;
	MidiInfo minfo;
	MidiEvent *ev;
	int piped;
	int i, cp;

	while ((cp = getopt(argc, argv, "v")) != -1) {
		switch (cp) {
		case 'v':
			ctl->verbosity++;
			break;
		default:
			usage();
			exit(1);
		}
	}
	if (optind >= argc) {
		usage();
		exit(1);
	}

	if ((fp = CmpOpenFile(argv[optind], &piped)) == NULL) {
		fprintf(stderr, "can't open midi file %s\n", argv[optind]);
		exit(1);
	}

	ev = ReadMidiFile(fp, &minfo);
	if (ev == NULL)
		exit(0);

	printf("midi events = %d\n", minfo.nlists);
	for (i = 0; i < minfo.nlists; i++) {
		printf("%-15d %-15d ", ev[i].csec, ev[i].time);
		switch (ev[i].type) {
		case ME_NOTEON:
			PrintCD("note_on", ev + i); break;
		case ME_NOTEOFF:
			PrintCD("note_off", ev + i); break;
		case ME_KEYPRESSURE:
			PrintCD("key_press", ev + i); break;
		case ME_PITCH_SENS:
			PrintCB("pitch_sens", ev + i); break;
		case ME_PITCHWHEEL:
			PrintCW("pitch_wheel", ev + i); break;
		case ME_MAINVOLUME:
			PrintCB("main_volume", ev + i); break;
		case ME_PAN:
			PrintCB("pan", ev + i); break;
		case ME_EXPRESSION:
			PrintCB("expression", ev + i); break;
		case ME_PROGRAM:
			PrintCB("program", ev + i); break;
		case ME_SUSTAIN:
			PrintCB("sustain", ev + i); break;
		case ME_RESET_CONTROLLERS:
			printf("reset_controllers \n"); break;
		case ME_ALL_NOTES_OFF:
			printf("all_notes_off \n"); break;
		case ME_ALL_SOUNDS_OFF:
			printf("all_sounds_off \n"); break;
		case ME_TONE_BANK:
			PrintCB("tone_bank", ev + i); break;
		case ME_CHORUS:
			PrintCB("chorus", ev + i); break;
		case ME_REVERB:
			PrintCB("reverb", ev + i); break;
		case ME_TEMPO:
			{ int tempo = ev[i].channel + ev[i].b * 256 + ev[i].a * 65536;
			printf("%-16s %d\n", "tempo", tempo);
			}
			break;
		case ME_LYRIC:
			printf("%-16s\n", "lyric");
			break;
		case ME_SET_CHORUS_MODE:
			PrintCB("chorus_mode", ev + i);
			break;
		case ME_SET_REVERB_MODE:
			PrintCB("reverb_mode", ev + i);
			break;
		case ME_EOT:
			printf("end_of_tune \n");
			exit(0);
		default:
			NRPNEvent(ev + i);
			break;
		}
	}
}
