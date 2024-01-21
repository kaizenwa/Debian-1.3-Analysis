/*
 */

#ifndef CHANNEL_H_DEF
#define CHANNEL_H_DEF

#include "midievent.h"

#define MAX_MIDI_CHANNELS	32

#ifdef linux
#include <linux/awe_voice.h>
#else
#include <awe_voice.h>
#endif

typedef struct _ChannelStat {
	int bank;	/* bank */
	int preset;	/* preset */
	int isdrum;	/* drum set flag */

	/* parameters */
	int pan;	/* 0 - 127 */
	int volume;	/* 0 - 127 (127 = max) */
	int sustain;	/* 0 - 127 */
	int pitchsense;	/* usually 2 */
	int pitchbend;	/* 0 - 0x3fff (0x2000 = no bend) */
	int expression;	/* 0 - 127 (usually 127) */

	int chorus; /* 0-127, -1 not set */
	int reverb; /* 0-127, -1 not set */

} ChannelStat;

extern ChannelStat channels[MAX_MIDI_CHANNELS];

void channel_init(MidiInfo *mp);
void channel_reset(int ch);
void channel_set(void);

#endif	/* CHANNEL_H_DEF */
