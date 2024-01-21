/*================================================================
 * sf2sfx.c
 *	convert Layer information to AWE voice parameters
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
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "layer.h"
#include "sfx.h"
#include "awe_parm.h"
#include "debugmsg.h"

/*----------------------------------------------------------------
 * prototypes
 *----------------------------------------------------------------*/

static int find_inst(LayInfo *ip, Preset *preset, Layer *lp);
static int next_inst(LayInfo *ip, Preset *preset, Layer *lp);
static Inst *to_inst(LayInfo *ip, int inst_no);
static int search_inst_num(Inst *ip);
static void alloc_voice(awe_voice_rec **vrec, int nv, int bank, int preset);
static awe_voice_rec **convert_preset(awe_voice_rec **vrec, LayInfo *ip, Preset *preset, int inst, Layer *gl);
static void add_to_layer(Layer *dst, Layer *src, int version);
static void clear_layer(Layer *dst, int version);
static void copy_layer(Layer *dst, Layer *src);
static void layer_to_voice(LayInfo *ip, awe_voice_info *vp, Layer *lp);

static char calc_pan(LayInfo *ip, Layer *lp);
static unsigned short calc_attenuation(LayInfo *ip, Layer *lp);
static short calc_scaleTuning(LayInfo *ip, Layer *lp);
static unsigned char calc_chorus(LayInfo *ip, Layer *lp);
static unsigned char calc_reverb(LayInfo *ip, Layer *lp);
static unsigned char calc_pitch_shift(LayInfo *ip, Layer *lp, int idx);
static unsigned char calc_cutoff_shift(LayInfo *ip, Layer *lp, int idx, int ratio);
static unsigned char calc_tremolo_vol(LayInfo *ip, Layer *lp);
static unsigned char calc_freq(LayInfo *ip, Layer *lp, int idx);
static unsigned char calc_cutoff(LayInfo *ip, Layer *lp);
static unsigned char calc_filterQ(LayInfo *ip, Layer *lp);
static unsigned short calc_delay(LayInfo *ip, Layer *lp, int idx);
static unsigned short calc_atkhld(LayInfo *ip, Layer *lp, int atk, int hld);
static unsigned char calc_sustain(LayInfo *ip, Layer *lp);
static unsigned char calc_mod_sustain(LayInfo *ip, Layer *lp);
static unsigned char calc_decay(LayInfo *ip, Layer *lp, int decay);
static short calc_keychange(LayInfo *ip, Layer *lp, int idx);

#define calc_word(hi,lo) (((unsigned short)(hi) << 8) | (unsigned short)(lo))

int auto_norelease = 0;
int default_choose_mode = 2;
int auto_add_blank = 0;


/*----------------------------------------------------------------
 * convert Layer to SFX data
 *----------------------------------------------------------------*/

void awe_convert_layer(VoiceInfo *vp, LayInfo *lp, int dummy)
{
	int i, inst, maxvoices;
	Preset *pp;
	Sample *sp;
	Layer glayer;
	long prev_end;
	awe_voice_rec **vrec;
	int def_drum_inst;

	vp->sfx_version = 1;

	DEBUG(0,fprintf(stderr, "version = %d.%d\n", lp->version, lp->minorversion));
	DEBUG(0,fprintf(stderr, "sampledata = %d %d\n",
		(int)lp->samplepos, (int)lp->samplesize));
	DEBUG(0,fprintf(stderr, "presets = %d\n", lp->npresets));
	DEBUG(0,fprintf(stderr, "instruments = %d\n", lp->ninsts));
	DEBUG(0,fprintf(stderr, "samples = %d\n", lp->nsamples));
	DEBUG(1,fprintf(stderr, "converting..\n"));
	vp->samplepos = lp->samplepos;
	vp->samplesize = lp->samplesize;

	def_drum_inst = -1;
	for (pp = lp->ptable; pp; pp = pp->next) {
		if (pp->bank == 128 && pp->preset == 0) {
			def_drum_inst = find_inst(lp, pp, NULL);
			DEBUG(1,fprintf(stderr, "def drum inst = %d\n", def_drum_inst));
			break;
		}
	}
		
	/* sum up the number of instruments */
	DEBUG(1,fprintf(stderr, "count up voices..\n"));
	vp->nvoices = 0;
	maxvoices = 0;
	for (pp = lp->ptable; pp; pp = pp->next) {
		for (inst = find_inst(lp, pp, NULL);
		     inst >= 0; inst = next_inst(lp, pp, NULL)) {
			Inst *this_inst = to_inst(lp, inst);
			if (this_inst == NULL) continue;
			if (pp->bank == 128) { /* drum bank */
				if (pp->preset == 0 || inst != def_drum_inst) {
					maxvoices += search_inst_num(this_inst);
					vp->nvoices++;
				}
			} else {
				maxvoices += search_inst_num(this_inst);
				vp->nvoices++;
			}
		}
	}

	DEBUG(0,fprintf(stderr, "voices=%d\n", vp->nvoices));
	DEBUG(0,fprintf(stderr, "max infos=%d\n", maxvoices));

	DEBUG(1,fprintf(stderr, "layer to voice (to %d)\n", lp->npresets));

	vp->voice = (awe_voice_rec**)calloc
		(vp->nvoices, sizeof(awe_voice_rec*));

	vrec = vp->voice;
	for (pp = lp->ptable; pp; pp = pp->next) {
		clear_layer(&glayer, lp->version);
		for (inst = find_inst(lp, pp, &glayer);
		     inst >= 0; inst = next_inst(lp, pp, &glayer)) {
			Inst *this_inst = to_inst(lp, inst);
			if (this_inst == NULL) continue;
			if (pp->bank == 128) {
				if (pp->preset == 0 || inst != def_drum_inst)
					vrec = convert_preset(vrec, lp, pp, inst, &glayer);
			}
			else
				vrec = convert_preset(vrec, lp, pp, inst, &glayer);
		}
	}
	
	DEBUG(1,fprintf(stderr, "convert waves\n"));
	/* all waves */
	vp->nwaves = lp->nsamples;
	vp->sample = (awe_sample_info*)calloc
		(vp->nwaves, AWE_SAMPLE_INFO_SIZE);
	prev_end = 0;
	for (i = 0, sp = lp->sample; sp; sp = sp->next, i++) {
		vp->sample[i].sf_id = 0;
		vp->sample[i].sample = i;
		vp->sample[i].start = sp->start;
		vp->sample[i].end = sp->end;
		vp->sample[i].loopstart = sp->loopstart;
		vp->sample[i].loopend = sp->loopend;
		if (lp->sample[i].mode & 0x8000)
			vp->sample[i].size = 0;
		else if (sp->start < prev_end && sp->start != 0)
			vp->sample[i].size = 0;
		else {
			vp->sample[i].size = -1;
			if (sp->next && !auto_add_blank)
				vp->sample[i].size = sp->next->start - sp->start;
			if (vp->sample[i].size < 0)
				vp->sample[i].size = sp->end - sp->start + 48;
		}
		prev_end = sp->end;
	}
	DEBUG(1,fprintf(stderr, "convert done\n"));
}


/*----------------------------------------------------------------
 *----------------------------------------------------------------*/

static Layer *cur_layp;

/* find the instrument used in the preset */
static int find_inst(LayInfo *ip, Preset *preset, Layer *globalp)
{
	cur_layp = preset->layer;
	return next_inst(ip, preset, globalp);
}

static int next_inst(LayInfo *ip, Preset *preset, Layer *globalp)
{
	int inst = -1;
	for (; cur_layp; cur_layp = cur_layp->next) {
		if (globalp)
			copy_layer(globalp, cur_layp);
		/* use first instrument */
		if (cur_layp->set[SF_instrument]) {
			inst =  cur_layp->val[SF_instrument].val;
			if (preset->choose == 0) {
				cur_layp = NULL;
				break;
			} else if (preset->choose != 1) {
				cur_layp = cur_layp->next;
				break;
			}
		}
	}
	return inst;
}

static Inst *to_inst(LayInfo *lp, int inst)
{
	Inst *curi;
	for (curi = lp->itable; inst > 0 && curi; curi = curi->next, inst--)
		;
	return curi;
}

/* search number of instruments in the instrument */
static int search_inst_num(Inst *ip)
{
	Layer *curl;
	int num = 0;
	for (curl = ip->layer; curl; curl = curl->next) {
		if (curl->set[SF_sampleId])
			num++;
	}
	return num;
}

/* allocate voice information record */
static void alloc_voice(awe_voice_rec **vrec, int nv, int bank, int preset)
{
	awe_voice_rec *vp;
	vp = (awe_voice_rec*)calloc(AWE_VOICE_REC_SIZE +
				    AWE_VOICE_INFO_SIZE * nv, 1);
	vp->bank = (unsigned char)bank;
	vp->instr = (unsigned char)preset;
	vp->nvoices = (short)nv;
	*vrec = vp;
}

/* convert instrument preset */
static awe_voice_rec **
convert_preset(awe_voice_rec **vrec, LayInfo *linfo, Preset *preset, int inst_no, Layer *gl)
{
	Layer glayer, *lp;
	Inst *inst;
	int v, nv;

	inst = to_inst(linfo, inst_no);
	if (inst == NULL) return vrec;
	nv = search_inst_num(inst);
	alloc_voice(vrec, nv, preset->bank, preset->preset);

	clear_layer(&glayer, linfo->version);
	v = 0;
	for (lp = inst->layer; lp; lp = lp->next) {
		if (!lp->set[SF_sampleId])
			copy_layer(&glayer, lp);
		else {
			Layer tlayer;
			memcpy(&tlayer, &glayer, sizeof(glayer));
			copy_layer(&tlayer, lp);
			add_to_layer(&tlayer, gl, linfo->version);
			layer_to_voice(linfo, &(*vrec)->info[v], &tlayer);
			v++;
		}
	}

	vrec++;
	return vrec;
}


/* intiaizlie layer */
static void clear_layer(Layer *dst, int version)
{
	memset(dst, 0, sizeof(Layer));
	/* set only non-zero default values (not flags yet) */
	dst->val[SF_rootKey].val = 60;
	if (version == 2)
		dst->val[SF_scaleTuning].val = 100;
	dst->val[SF_keyRange].r.hi = 127;
	dst->val[SF_velRange].r.hi = 127;
}

/* copy global layer to the temporary layer */
static void copy_layer(Layer *dst, Layer *src)
{
	int i;
	for (i = 0; i < PARM_SIZE; i++) {
		if (src->set[i]) {
			dst->val[i] = src->val[i];
			dst->set[i] = 1;
		}
	}
}

static void add_to_layer(Layer *dst, Layer *src, int version)
{
	int i;
	for (i = 0; i < PARM_SIZE; i++) {
		if (src->set[i]) {
			if (version == 1 && i == SF_instVol)
				dst->val[i].val = (src->val[i].val * 127) / 127;
			else if (i == SF_keyRange || i == SF_velRange) {
				if (dst->val[i].r.hi > src->val[i].r.hi)
					dst->val[i].r.hi = src->val[i].r.hi;
				if (dst->val[i].r.lo < src->val[i].r.lo)
					dst->val[i].r.lo = src->val[i].r.lo;
			} else
				dst->val[i].val += src->val[i].val;
			dst->set[i] = 1;
		}
	}
}


/* convert layer information to AWE driver parameters */
static void layer_to_voice(LayInfo *ip, awe_voice_info *vp, Layer *lp)
{
	Sample *sp;

	awe_init_voice(vp);

	vp->sf_id = 0;
	vp->sample = lp->val[SF_sampleId].val;
	sp = &ip->sample[vp->sample];

	vp->start = (lp->val[SF_startAddrsHi].val << 16)
		+ lp->val[SF_startAddrs].val;
	vp->end = (lp->val[SF_endAddrsHi].val << 16)
		+ lp->val[SF_endAddrs].val;
	vp->loopstart = (lp->val[SF_startloopAddrsHi].val << 16)
		+ lp->val[SF_startloopAddrs].val;
	vp->loopend = (lp->val[SF_endloopAddrsHi].val << 16)
		+ lp->val[SF_endloopAddrs].val;

	vp->rate_offset = awe_calc_rate_offset(sp->samplerate);

	/* sample mode */
	vp->mode = 0;
	if (sp->mode & 0x8000)
		vp->mode |= AWE_MODE_ROMSOUND;
	if (lp->val[SF_sampleFlags].val == 1 ||
	    lp->val[SF_sampleFlags].val == 3) /* looping */
		vp->mode |= AWE_MODE_LOOPING;
	else {
		/* change loop position for non-looping */
		int loopsize = 48;
		if (sp->next && !auto_add_blank) {
			loopsize = sp->next->start - sp->end;
			if (loopsize < 0 || loopsize > 48) loopsize = 48;
		}
		if (loopsize > 8) {
			vp->loopstart = sp->end + 8 - sp->loopstart;
			vp->loopend = sp->end + loopsize - 8 - sp->loopend;
		} else {
			fprintf(stderr, "loop size is too short: %d\n", loopsize);
			exit(1);
		}

		if (lp->val[SF_sampleFlags].val == 4 || auto_norelease)
			vp->mode |= AWE_MODE_NORELEASE;
	}

	/* scale tuning: 0  - 100 */
	vp->scaleTuning = 100;
	if (lp->set[SF_scaleTuning])
		vp->scaleTuning = calc_scaleTuning(ip, lp);

	/* root pitch */
	vp->root = sp->pitch;
	vp->tune = sp->correction;
	if (ip->version == 1) {
		if (lp->set[SF_samplePitch]) {
			vp->root = lp->val[SF_samplePitch].val / 100;
			vp->tune = -lp->val[SF_samplePitch].val % 100;
			if (vp->tune <= -50) {
				vp->root++;
				vp->tune = 100 + vp->tune;
			}
			if (vp->scaleTuning == 50)
				vp->tune /= 2;
		}
		/* orverride root key */
		if (lp->set[SF_rootKey])
			vp->root += lp->val[SF_rootKey].val - 60;
		/* tuning */
		vp->tune += lp->val[SF_coarseTune].val * vp->scaleTuning +
			lp->val[SF_fineTune].val * vp->scaleTuning / 100;
	} else {
		/* orverride root key */
		if (lp->set[SF_rootKey])
			vp->root = lp->val[SF_rootKey].val;
		/* tuning */
		vp->tune += lp->val[SF_coarseTune].val * 100
			+ lp->val[SF_fineTune].val;
	}

	/* key range */
	vp->low = 0;
	vp->high = 127;
	if (lp->set[SF_keyRange]) {
		vp->low = (char)lp->val[SF_keyRange].r.lo;
		vp->high = (char)lp->val[SF_keyRange].r.hi;
	}

	/* it's too high.. */
	if (vp->root >= vp->high + 60)
		vp->root -= 60;

	/* velocity range */
	vp->vellow = 0;
	vp->velhigh = 127;
	if (lp->set[SF_velRange]) {
		vp->vellow = (char)lp->val[SF_velRange].r.lo;
		vp->velhigh = (char)lp->val[SF_velRange].r.hi;
	}

	/* fixed key & velocity */
	vp->fixkey = -1;
	if (lp->set[SF_keynum])
		vp->fixkey = (char)lp->val[SF_keynum].val;
	vp->fixvel = -1;
	if (lp->set[SF_velocity])
		vp->fixvel = (char)lp->val[SF_velocity].val;

	/* fixed pan */
	vp->pan = -1;
	vp->fixpan = -1;
	if (lp->set[SF_panEffectsSend])
		vp->pan = calc_pan(ip, lp);

	vp->exclusiveClass = 0;
	if (lp->set[SF_keyExclusiveClass])
		vp->exclusiveClass = (short)lp->val[SF_keyExclusiveClass].val;

	/* volume control */
	vp->amplitude = 127;  /* this value is overwritten by sfxload */
	vp->attenuation = 0;
	if (lp->set[SF_instVol])
		vp->attenuation = calc_attenuation(ip, lp);

	/* chorus & reverb: 0 - 255 */
	vp->parm.chorus = 0;
	if (lp->set[SF_chorusEffectsSend])
		vp->parm.chorus = calc_chorus(ip, lp);

	vp->parm.reverb = 0;
	if (lp->set[SF_reverbEffectsSend])
		vp->parm.reverb = calc_reverb(ip, lp);

	/* modulation envelope parameters */
	vp->parm.moddelay = calc_delay(ip, lp, SF_delayEnv1);
	vp->parm.modatkhld = calc_atkhld(ip, lp, SF_attackEnv1, SF_holdEnv1);
	vp->parm.moddcysus = calc_word(calc_mod_sustain(ip, lp),
				       calc_decay(ip, lp, SF_decayEnv1));
	vp->parm.modrelease = calc_word(0x80, calc_decay(ip, lp, SF_releaseEnv1));
	vp->parm.modkeyhold = calc_keychange(ip, lp, SF_autoHoldEnv1);
	vp->parm.modkeydecay = calc_keychange(ip, lp, SF_autoDecayEnv1);
	vp->parm.pefe = calc_word(calc_pitch_shift(ip, lp, SF_env1ToPitch),
				  calc_cutoff_shift(ip, lp, SF_env1ToFilterFc, 6));

	/* volume envelope parameters */
	vp->parm.voldelay = calc_delay(ip, lp, SF_delayEnv2);
	vp->parm.volatkhld = calc_atkhld(ip, lp, SF_attackEnv2, SF_holdEnv2);
	vp->parm.voldcysus = calc_word(calc_sustain(ip, lp),
				       calc_decay(ip, lp, SF_decayEnv2));
	vp->parm.volrelease = calc_word(0x80, calc_decay(ip, lp, SF_releaseEnv2));
	vp->parm.volkeyhold = calc_keychange(ip, lp, SF_autoHoldEnv2);
	vp->parm.volkeydecay = calc_keychange(ip, lp, SF_autoDecayEnv2);
	
	/* lfo1 parameters (tremolo & vibrato) */
	vp->parm.lfo1delay = calc_delay(ip, lp, SF_delayLfo1);
	vp->parm.fmmod = calc_word(calc_pitch_shift(ip, lp, SF_lfo1ToPitch),
				   calc_cutoff_shift(ip, lp, SF_lfo1ToFilterFc, 3));
	vp->parm.tremfrq = calc_word(calc_tremolo_vol(ip, lp),
				     calc_freq(ip, lp, SF_freqLfo1));

	/* lfo2 parameters (vibrato only) */
	vp->parm.lfo2delay = calc_delay(ip, lp, SF_delayLfo2);
	vp->parm.fm2frq2 = calc_word(calc_pitch_shift(ip, lp, SF_lfo2ToPitch),
				     calc_freq(ip, lp, SF_freqLfo2));

	/* initial cutoff filter freq & Q */
	vp->parm.cutoff = calc_cutoff(ip, lp);
	vp->parm.filterQ = calc_filterQ(ip, lp);

	memset(vp->parm.reserved, 0, sizeof(vp->parm.reserved));
}


/* convert panning position */
static char calc_pan(LayInfo *ip, Layer *lp)
{
	if (ip->version == 1)
		return (char)lp->val[SF_panEffectsSend].val;
	else
		return awe_calc_pan(lp->val[SF_panEffectsSend].val);
}


/* convert initial attenuation */
static unsigned short calc_attenuation(LayInfo *ip, Layer *lp)
{	
	int atten;
	if (ip->version == 1)
		atten = (int)(-200.0 * log10(lp->val[SF_instVol].val / 127.0));
	else /* this is not a centibel? */
		atten = lp->val[SF_instVol].val / 10;
	return awe_calc_attenuation(atten);
}


/* convert scale tuning */
static short calc_scaleTuning(LayInfo *ip, Layer *lp)
{
	if (ip->version == 1) {
		if (lp->val[SF_scaleTuning].val)
			return 50;
		else
			return 100;
	} else
		return lp->val[SF_scaleTuning].val;
}


/* convert chorus effects send */
static unsigned char calc_chorus(LayInfo *ip, Layer *lp)
{
	if (ip->version == 1)
		return (unsigned char)(lp->val[SF_chorusEffectsSend].val * 2);
	else
		return awe_calc_chorus(lp->val[SF_chorusEffectsSend].val);
}


/* convert reverb effects send */
static unsigned char calc_reverb(LayInfo *ip, Layer *lp)
{
	if (ip->version == 1)
		return (unsigned char)(lp->val[SF_reverbEffectsSend].val * 2);
	else
		return awe_calc_reverb(lp->val[SF_reverbEffectsSend].val);
}


/* convert pitch shift height */
static unsigned char calc_pitch_shift(LayInfo *ip, Layer *lp, int idx)
{
	int shift = lp->val[idx].val;
	if (ip->version == 1)
		shift = (1200 * shift / 64 + 1) / 2;
	return awe_calc_pitch_height(shift);
}


/* convert cutoff freq shift */
static unsigned char calc_cutoff_shift(LayInfo *ip, Layer *lp, int idx, int ratio)
{
	int shift = lp->val[idx].val;
	if (ip->version == 1)
		shift = (1200 * ratio * shift) / 64;
	return awe_calc_env_height(shift, ratio);
}


/* convert tremolo volume */
static unsigned char calc_tremolo_vol(LayInfo *ip, Layer *lp)
{
	int level = lp->val[SF_lfo1ToVolume].val;
	if (ip->version == 1)
		level = (120 * level) / 64;
	return awe_calc_tremolo_vol(level);
}


/* convert envelope/lfo frequency */
static unsigned char calc_freq(LayInfo *ip, Layer *lp, int idx)
{
	int val = lp->val[idx].val;
	if (lp->set[idx]) {
		if (val > 0 && ip->version == 1)
			val = (int)(3986.0 * log10((double)val) - 7925.0);
	} else {
		if (ip->version == 1) {
			if (idx == SF_freqLfo1)
				val = -725;
			else
				val = -15600;
		} else
			val = awe_mHz_to_abscent(8176);
	}
	return awe_calc_env_freq(val);
}


/* convert intitial cutoff freq */
static unsigned char calc_cutoff(LayInfo *ip, Layer *lp)
{
	int val = lp->val[SF_initialFilterFc].val;
	if (ip->version == 1) {
		if (val == 127)
			val = 14400;
		else if (val > 0)
			val = 59 * val + 4366;
	}
	return awe_calc_cutoff(val);
}


/* convert intial filter Q */
static unsigned char calc_filterQ(LayInfo *ip, Layer *lp)
{
	int val = lp->val[SF_initialFilterQ].val;
	if (ip->version == 1)
		val = val * 3 / 2;
	return awe_calc_filterQ(val);
}


/* convert delay time */
static unsigned short calc_delay(LayInfo *ip, Layer* lp, int idx)
{
	int msec;
	if (!lp->set[idx])
		return 0x8000;
	if (ip->version == 1)
		msec = lp->val[idx].val;
	else
		msec = awe_timecent_to_msec(lp->val[idx].val);
	return awe_calc_delay(msec);
}


/* convert attack and hold time */
static unsigned short calc_atkhld(LayInfo *ip, Layer *lp, int atk, int hld)
{
	int atkmsec = 0, hldmsec = 0;
	if (ip->version == 1) {
		if (lp->set[hld])
			hldmsec = lp->val[hld].val;
		else
			hldmsec = awe_timecent_to_msec(-12000);
		if (lp->set[atk])
			atkmsec = lp->val[atk].val;
		else
			atkmsec = awe_timecent_to_msec(-8800);
	} else {
		if (lp->set[hld])
			hldmsec = awe_timecent_to_msec(lp->val[hld].val);
		if (lp->set[atk])
			atkmsec = awe_timecent_to_msec(lp->val[atk].val);
	}
	return awe_calc_atkhld(atkmsec, hldmsec);
}


/* convert volume sustain level */
static unsigned char calc_sustain(LayInfo *ip, Layer *lp)
{
	int level = lp->val[SF_sustainEnv2].val;
	if (ip->version == 1) {
		if (!lp->set[SF_sustainEnv2])
			level = 1000;
		else {
			if (level < 96)
				level = 1000 * (96 - level) / 96;
			else
				level = 0;
		}
	} else {
		if (!lp->set[SF_sustainEnv2])
			return 0x7f;
	}
	return awe_calc_sustain(level);
}


/* convert modulation sustain level */
static unsigned char calc_mod_sustain(LayInfo *ip, Layer *lp)
{
	int level = lp->val[SF_sustainEnv1].val;
	if (ip->version == 1) {
		if (!lp->set[SF_sustainEnv1])
			level = 1000;
		else {
			if (level < 96)
				level = 1000 * (96 - level) / 96;
			else
				level = 0;
		}
	} else {
		if (!lp->set[SF_sustainEnv1])
			return 0x7f;
	}

	return awe_calc_mod_sustain(level);
}


/* convert decay time */ 
static unsigned char calc_decay(LayInfo *ip, Layer *lp, int decay)
{
	int dcymsec = 0;
	if (ip->version == 1) {
		if (lp->set[decay])
			dcymsec = lp->val[decay].val;
		else
			dcymsec = awe_timecent_to_msec(-12000);
	} else {
		if (lp->set[decay])
			dcymsec = awe_timecent_to_msec(lp->val[decay].val);
	}
	return awe_calc_decay(dcymsec);
}


/* convert envelope time change per key */
static short calc_keychange(LayInfo *ip, Layer *lp, int idx)
{
	if (ip->version == 1)
		return (int)(5.55 * lp->val[idx].val);
	else
		return lp->val[idx].val;
}
