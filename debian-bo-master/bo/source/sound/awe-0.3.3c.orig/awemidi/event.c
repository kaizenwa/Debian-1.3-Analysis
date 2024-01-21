/*================================================================
 * event.c
 *	MIDI event processing routine
 *================================================================*/

#include <stdio.h>
#include "channel.h"
#include "midievent.h"
#include "controls.h"
#include "util.h"
#include "seq.h"
#include "awe_effect.h"

static void Nothing(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void NoteOn(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void NoteOff(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void KeyPressure(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void PitchSens(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void PitchWheel(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void MainVolume(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void Pan(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void Expression(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void Program(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void Sustain(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void ResetControl(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void AllNotesOff(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void AllSoundsOff(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void ToneBank(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void PrintLyric(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void Tempo(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void Chorus(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void Reverb(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void ChorusMode(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void ReverbMode(MidiEvent *ev, MidiInfo *minfo, int no_ctl);
static void NRPNEvent(MidiEvent *ev, MidiInfo *minfo, int no_ctl);

/*----------------------------------------------------------------*/

ChannelStat channels[MAX_MIDI_CHANNELS];

/*----------------------------------------------------------------*/
  
typedef void (*EventFunc)(MidiEvent *ev, MidiInfo *minfo, int no_ctl);

static EventFunc midi_events[] = {
	Nothing,	/* ME_NONE */
	NoteOn,		/* ME_NOTEON */
	NoteOff,	/* ME_NOTEOFF */
	KeyPressure,	/* ME_KEYPRESSURE */
	MainVolume,	/* ME_MAINVOLUME */
	Pan,		/* ME_PAN */
	Sustain,	/* ME_SUSTAIN */
	Expression,	/* ME_EXPRESSION */
	PitchWheel,	/* ME_PITCHWHEEL */
	Program,	/* ME_PROGRAM */
	Tempo,		/* ME_TEMPO */
	PitchSens,	/* ME_PITCH_SENS */
	AllSoundsOff,	/* ME_ALL_SOUNDS_OFF */
	ResetControl,	/* ME_RESET_CONTROLLERS */
	AllNotesOff,	/* ME_ALL_NOTES_OFF */
	ToneBank,	/* ME_TONE_BANK */
	PrintLyric,	/* ME_LYRIC */
	Reverb,		/* ME_REVERB */
	Chorus,		/* ME_CHORUS */
	ReverbMode,	/* ME_SET_REVERB_MODE */
	ChorusMode,	/* ME_SET_CHORUS_MODE */
};


/*
 */
int do_midi_event(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (ev->type == ME_EOT)
		return RC_TUNE_END;
	else if (ev->type <= ME_SET_CHORUS_MODE)
		midi_events[ev->type](ev, minfo, no_ctl);
	else if (ev->type >= ME_ENV1_DELAY && ev->type <= ME_FILTERQ)
		NRPNEvent(ev, minfo, no_ctl);
	else {
		ctl->cmsg(CMSG_INFO, 0, "?? event=%d ch=%d a=%d b=%d",
			  ev->type, ev->channel, ev->a, ev->b);
	}
	return RC_NONE;
}


/*
 */
void channel_init(MidiInfo *mp)
{
	int i;
	for (i = 0; i < MAX_MIDI_CHANNELS; i++) {
		channels[i].isdrum = mp->drumflag & (1 << i);
		channel_reset(i);
	}
}

void channel_reset(int i)
{
	channels[i].preset = 0;
	if (channels[i].isdrum)
		channels[i].bank = 128;
	else
		channels[i].bank = 0;
	channels[i].pan = 63;
	channels[i].volume = 127;
	channels[i].pitchsense = 2;
	channels[i].pitchbend = 0x2000;
	channels[i].expression = 127;
	channels[i].chorus = 0;
	channels[i].reverb = 0;
	channels[i].sustain = 0;
}

/*
 */
void channel_set(void)
{
	int i;
	for (i = 0; i < MAX_MIDI_CHANNELS; i++) {
		seq_set_bank(i, channels[i].bank);
		seq_set_program(i, channels[i].preset);
		seq_panning(i, channels[i].pan);
		seq_mainvolume(i, channels[i].volume);
		seq_expression(i, channels[i].expression);
		seq_sustain(i, channels[i].sustain);
		seq_pitchsense(i, channels[i].pitchsense);
		seq_pitchbend(i, channels[i].pitchbend);
		seq_send_effect(i, AWE_FX_CHORUS, channels[i].chorus * 2);
		seq_send_effect(i, AWE_FX_REVERB, channels[i].reverb * 2);
	}
}

/*----------------------------------------------------------------*/

static void Nothing(MidiEvent *ev, MidiInfo *minfo, int no_ctl) {}

static void NoteOn(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (no_ctl) return;
	if (ev->b == 0) {
		NoteOff(ev, minfo, no_ctl);
		return;
	}
	seq_start_note(ev->channel, ev->a, ev->b);
	ctl->note(ev->channel, ev->a, ev->b);
}

static void NoteOff(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (no_ctl) return;
	seq_stop_note(ev->channel, ev->a, ev->b);
	ctl->note(ev->channel, ev->a, 0);
}

static void KeyPressure(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (no_ctl) return;
	seq_aftertouch(ev->channel, ev->a, ev->b);
	ctl->note(ev->channel, ev->a, ev->b);
}

static void PitchSens(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].pitchsense = MidiByte(ev);
	if (no_ctl) return;
	seq_pitchsense(ev->channel, MidiByte(ev));
	ctl->pitch_sense(ev->channel, MidiByte(ev));
}	

static void PitchWheel(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].pitchbend = MidiWord(ev);
	if (no_ctl) return;
	seq_pitchbend(ev->channel, MidiWord(ev));
	ctl->pitch_bend(ev->channel, MidiWord(ev));
}

static void MainVolume(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].volume = MidiByte(ev);
	if (no_ctl) return;
	seq_mainvolume(ev->channel, MidiByte(ev));
	ctl->volume(ev->channel, MidiByte(ev));
}

static void Pan(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].pan = MidiByte(ev);
	if (no_ctl) return;
	seq_panning(ev->channel, MidiByte(ev));
	ctl->panning(ev->channel, MidiByte(ev));
}

static void Expression(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].expression = MidiByte(ev);
	if (no_ctl) return;
	seq_expression(ev->channel, MidiByte(ev));
	ctl->expression(ev->channel, MidiByte(ev));
}

static void Program(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].preset = MidiByte(ev);
	if (no_ctl) return;
	seq_set_program(ev->channel, MidiByte(ev));
	ctl->program(ev->channel, MidiByte(ev));
}

static void Sustain(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].sustain = MidiByte(ev);
	if (no_ctl) return;
	seq_sustain(ev->channel, MidiByte(ev));
	ctl->sustain(ev->channel, MidiByte(ev));
}

static void ResetControl(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
}

static void AllNotesOff(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (!no_ctl && minfo->accept_all_off)
		seq_note_off_all();
}

static void AllSoundsOff(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (!no_ctl && minfo->accept_all_off)
		seq_sound_off_all();
}

static void ToneBank(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (! channels[ev->channel].isdrum) {
		channels[ev->channel].bank = MidiByte(ev);
		if (no_ctl) return;
		seq_set_bank(ev->channel, MidiByte(ev));
		ctl->bank(ev->channel, MidiByte(ev));
	}
}

static void PrintLyric(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (no_ctl) return;
	if (ev->misc)
		ctl->cmsg(CMSG_INFO, -1, "%s", ev->misc);
}

static void Tempo(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	minfo->tempo = MidiTempo(ev);
}

static void Chorus(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].chorus = MidiByte(ev);
	if (no_ctl) return;
	seq_send_effect(ev->channel, AWE_FX_CHORUS, MidiByte(ev) * 2);
	ctl->effects_change(ev->channel, ME_FX_CHORUS, MidiByte(ev) * 2);
}

static void Reverb(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	channels[ev->channel].reverb = MidiByte(ev);
	if (no_ctl) return;
	seq_send_effect(ev->channel, AWE_FX_REVERB, MidiByte(ev) * 2);
	ctl->effects_change(ev->channel, ME_FX_REVERB, MidiByte(ev) * 2);
}

static void ChorusMode(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (no_ctl) return;
	seq_set_chorus(MidiByte(ev));
	ctl->chorus_mode(MidiByte(ev));
}

static void ReverbMode(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	if (no_ctl) return;
	seq_set_reverb(MidiByte(ev));
	ctl->reverb_mode(MidiByte(ev));
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
	"init_pitch", "chorus", "reverb", "cutoff", "filterQ",
	"sample", "loop_start", "loop_end",
};

#define numberof(ary)	(sizeof(ary)/sizeof(ary[0]))

static void NRPNEvent(MidiEvent *ev, MidiInfo *minfo, int no_ctl)
{
	int i, val;
	val = (ev->a + ev->b * 128) - 8192;

	if (no_ctl) return;
	for (i = 0; i < numberof(effect_rec); i++) {
		if (effect_rec[i].type == ev->type) {
			short cval = effect_rec[i].convert(val);
			ctl->cmsg(CMSG_INFO, 2, "[nrpn ch=%d %s: %d -> %x]",
				  ev->channel,
				  fx_name[effect_rec[i].awe_effect],
				  val, (unsigned short)cval);
			seq_send_effect(ev->channel, effect_rec[i].awe_effect, cval);
			ctl->effects_change(ev->channel, ev->type, val);
			return;
		}
	}
	if (no_ctl) return;
	if (i >= numberof(effect_rec)) {
		ctl->cmsg(CMSG_INFO, 2, "[nrpn unknown ch=%d type=%d val=%d]",
			  ev->channel, ev->type, val);
	}
}
