/*================================================================
 * layer.h
 *	SoundFont layer structure
 *================================================================*/

#ifndef LAYER_H_DEF
#define LAYER_H_DEF

enum {
	SF_startAddrs,         /* sample start address -4 (0 to * 0xffffff) */
        SF_endAddrs,
        SF_startloopAddrs,     /* loop start address -4 (0 to * 0xffffff) */
        SF_endloopAddrs,       /* loop end address -3 (0 to * 0xffffff) */
        SF_startAddrsHi,       /* high word of startAddrs */
        SF_lfo1ToPitch,        /* main fm: lfo1-> pitch */
        SF_lfo2ToPitch,        /* aux fm:  lfo2-> pitch */
        SF_env1ToPitch,        /* pitch env: env1(aux)-> pitch */
        SF_initialFilterFc,    /* initial filter cutoff */
        SF_initialFilterQ,     /* filter Q */
        SF_lfo1ToFilterFc,     /* filter modulation: lfo1 -> filter * cutoff */
        SF_env1ToFilterFc,     /* filter env: env1(aux)-> filter * cutoff */
        SF_endAddrsHi,         /* high word of endAddrs */
        SF_lfo1ToVolume,       /* tremolo: lfo1-> volume */
        SF_env2ToVolume,       /* Env2Depth: env2-> volume */
        SF_chorusEffectsSend,  /* chorus */
        SF_reverbEffectsSend,  /* reverb */
        SF_panEffectsSend,     /* pan */
        SF_auxEffectsSend,     /* pan auxdata (internal) */
        SF_sampleVolume,       /* used internally */
        SF_unused3,
        SF_delayLfo1,          /* delay 0x8000-n*(725us) */
        SF_freqLfo1,           /* frequency */
        SF_delayLfo2,          /* delay 0x8000-n*(725us) */
        SF_freqLfo2,           /* frequency */
        SF_delayEnv1,          /* delay 0x8000 - n(725us) */
        SF_attackEnv1,         /* attack */
        SF_holdEnv1,             /* hold */
        SF_decayEnv1,            /* decay */
        SF_sustainEnv1,          /* sustain */
        SF_releaseEnv1,          /* release */
        SF_autoHoldEnv1,
        SF_autoDecayEnv1,
        SF_delayEnv2,            /* delay 0x8000 - n(725us) */
        SF_attackEnv2,           /* attack */
        SF_holdEnv2,             /* hold */
        SF_decayEnv2,            /* decay */
        SF_sustainEnv2,          /* sustain */
        SF_releaseEnv2,          /* release */
        SF_autoHoldEnv2,
        SF_autoDecayEnv2,
        SF_instrument,           /* */
        SF_nop,
        SF_keyRange,             /* */
        SF_velRange,             /* */
        SF_startloopAddrsHi,     /* high word of startloopAddrs */
        SF_keynum,               /* */
        SF_velocity,             /* */
        SF_instVol,              /* */
        SF_keyTuning,
        SF_endloopAddrsHi,       /* high word of endloopAddrs */
        SF_coarseTune,
        SF_fineTune,
        SF_sampleId,
        SF_sampleFlags,
        SF_samplePitch,          /* SF1 only */
        SF_scaleTuning,
        SF_keyExclusiveClass,
        SF_rootKey,
	SF_EOF,
};

#define PARM_SIZE	SF_EOF

typedef struct _LayerValue {
	char *str;
	struct _hilov {
		unsigned char lo, hi;
	} r;
	int val;
} LayerValue;


typedef struct _Layer {
	LayerValue val[PARM_SIZE];
	char set[PARM_SIZE];
	struct _Layer *next;
} Layer;


typedef struct _Preset {
	char name[20];
	int bank, preset;
	int layers;
	int choose;
	Layer *layer;
	struct _Preset *next;
} Preset;

typedef struct _Inst {
	char name[20];
	int layers;
	Layer *layer;
	struct _Inst *next;
} Inst;

typedef struct _Sample {
	char name[20];
	long start, end, loopstart, loopend;
	int samplerate, pitch, correction;
	int link, mode;
	struct _Sample *next;
} Sample;


/*----------------------------------------------------------------
 *----------------------------------------------------------------*/

typedef struct {
	int version, minorversion;
	long samplepos, samplesize;

	int npresets;
	Preset *ptable;

	int ninsts;
	Inst *itable;

	int nsamples;
	Sample *sample;

	char *samplefile;
} LayInfo;


char *gettag(char *line);
int text_to_layer(Layer *lp, char *text);

void load_sbk_text(LayInfo *lp, FILE *fp);

typedef struct _VoiceInfo VoiceInfo;
void awe_convert_layer(VoiceInfo *vp, LayInfo *lp, int drum_mode);

extern int default_choose_mode;
extern int auto_add_blank;

#endif
