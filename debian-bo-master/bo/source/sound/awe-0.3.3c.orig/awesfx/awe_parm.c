/*================================================================
 * awe_parm.c
 *	convert Emu8000 parameters
 *================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "awe_parm.h"


/*================================================================
 * unit conversion
 *================================================================*/

/*
 * convert timecents to msec
 */
int awe_timecent_to_msec(int timecent)
{
	return (int)(1000 * pow(2.0, (double)timecent / 1200.0));
}


/*
 * convert msec to timecents
 */
int awe_msec_to_timecent(int msec)
{
	return (int)(log((double)msec / 1000.0) / log(2.0) * 1200.0);
}


/*
 * convert abstract cents to mHz
 */
int awe_abscent_to_mHz(int abscents)
{
	return (int)(8176.0 * pow(2.0, (double)abscents / 1200.0));
}


/*
 * convert from mHz to abstract cents
 */
int awe_mHz_to_abscent(int mHz)
{
	return (int)(log((double)mHz / 8176.0) / log(2.0) * 1200.0);
}


/*
 * convert abstract cents to Hz
 */
int awe_abscent_to_Hz(int abscents)
{
	return (int)(8.176 * pow(2.0, (double)abscents / 1200.0));
}


/*
 * convert from Hz to abstract cents
 */
int awe_Hz_to_abscent(int Hz)
{
	return (int)(log((double)Hz / 8.176) / log(2.0) * 1200.0);
}


/*================================================================
 * Emu8000 pitch offset conversion
 *================================================================*/

/*
 * Sample pitch offset for the specified sample rate
 * rate=44100 is no offset, each 4096 is 1 octave (twice).
 * eg, when rate is 22050, this offset becomes -4096.
 */
short awe_calc_rate_offset(int Hz)
{
	if (Hz == 44100) return 0;
	return (short)(log((double)Hz / 44100) / log(2.0) * 4096.0);
}


/*================================================================
 * initialize Emu8000 parameters
 *================================================================*/

void awe_init_voice(awe_voice_info *vp)
{
	vp->sf_id = 0;
	vp->sample = 0;
	vp->start = 0;
	vp->end = 0;
	vp->loopstart = 0;
	vp->loopend = 0;
	vp->rate_offset = 0;

	vp->mode = 0;
	vp->root = 60;
	vp->tune = 0;

	vp->low = 0;
	vp->high = 127;
	vp->vellow = 0;
	vp->velhigh = 127;

	vp->fixkey = -1;
	vp->fixvel = -1;
	vp->fixpan = -1;
	vp->pan = -1;

	vp->exclusiveClass = 0;
	vp->amplitude = 127;
	vp->attenuation = 0;
	vp->scaleTuning = 100;

	awe_init_parm(&vp->parm);
}


void awe_init_parm(awe_voice_parm *pp)
{
	pp->moddelay = 0x8000;
	pp->modatkhld = 0x7f7f;
	pp->moddcysus = 0x7f7f;
	pp->modrelease = 0x807f;
	pp->modkeyhold = 0;
	pp->modkeydecay = 0;

	pp->voldelay = 0x8000;
	pp->volatkhld = 0x7f7f;
	pp->voldcysus = 0x7f7f;
	pp->volrelease = 0x807f;
	pp->volkeyhold = 0;
	pp->volkeydecay = 0;

	pp->lfo1delay = 0x8000;
	pp->lfo2delay = 0x8000;
	pp->pefe = 0;

	pp->fmmod = 0;
	pp->tremfrq = 0;
	pp->fm2frq2 = 0;

	pp->cutoff = 0xff;
	pp->filterQ = 0;

	pp->chorus = 0;
	pp->reverb = 0;
}


/*================================================================
 * Emu8000 parameters conversion
 *================================================================*/

/*
 * Delay time
 * sf: timecents
 * parm: 0x8000 - msec * 0.725
 */
unsigned short awe_calc_delay(int msec)
{
	return (unsigned short)(0x8000 - msec * 1000 / 725);
}


/*
 * Attack and Hold time
 * This parameter is difficult...
 *
 * ADIP says:
 * bit15 = always 0
 * upper byte = 0x7f - hold time / 82, max 11.68sec at 0, no hold at 0x7f.
 * bit7 = always 0
 * lower byte = encoded attack time, 0 = never attack,
 *      1 = max 11.68sec, 0x7f = min 6msec.
 *
 * In VVSG, 
 *        if AttackTime_ms >= 360ms:
 *          RegisterValue = 11878/AttackTime_ms - 1
 *        if AttackTime_ms < 360ms and AttackTime != 0:
 *          RegisterValue = 32 + [16/log(1/2)] * log(360_ms/AttackTime_ms)
 *        if AttackTime_ms == 0
 *          RegisterValue = 0x7F
 */
unsigned short awe_calc_atkhld(int atkmsec, int hldmsec)
{
	int up, lw;

	up = 0x7f - (unsigned char)(hldmsec / 92);
	if (up < 1) up = 1;
	if (up > 127) up = 127;

	if (atkmsec == 0)
		lw = 0x7f;
	else if (atkmsec >= 360)
		lw = (unsigned char)(11878 / atkmsec);
	else if (atkmsec < 360)
		lw = (unsigned char)(32 + 53.426 * log10(360.0/atkmsec));
	else
		lw = 0x7f;
	if (lw < 1) lw = 1;
	if (lw > 127) lw = 127;

	return (unsigned short)((up << 8) | lw);
}


/*
 * Sustain level
 * sf: centibels
 * parm: 0x7f - sustain_level(dB) * 0.75
 */
unsigned char awe_calc_sustain(int sust_cB)
{
	int up;

	/* sustain level */
	up = 0x7f - sust_cB * 3 / 40;
	/* sustain level must be greater than zero to be audible.. */
	if (up < 1)
		up = 1;
	return up;
}

/*
 * Modulation sustain level
 * sf: 0.1%
 * parm: 0x7f - sustain_level(dB) * 0.75
 */
unsigned char awe_calc_mod_sustain(int tenth_percent)
{
	int up;

	if (tenth_percent < 0) tenth_percent = 0;
	else if (tenth_percent > 1000) tenth_percent = 1000;
	if (tenth_percent == 1000)
		up = 1;
	else {
		up = 0x7f + (int)(20.0 * 0.75 * log10((double)(1000 - tenth_percent) / 1000.0));
		if (up < 1) up = 1;
	}
	return up;
}

/*
 * This parameter is also difficult to understand...
 *
 * ADIP says the value means decay rate, 0x7f minimum time is of 240usec/dB,
 * 0x01 being the max time of 470msec/dB, and 0 begin no decay.
 *
 * In VVSG, 2 * log(0.5) * log(23756/[ms]) (0x7F...0ms), but this is
 * obviously incorrect. But, the max time 23756 seems to be correct.
 * (actually, in NRPN control, decay time is within 0.023 and 23.7 secs.)
 * 
 */ 

unsigned char awe_calc_decay(int dcymsec)
{
	int lw;

	/* decay time */
	if (dcymsec == 0)
		lw = 127;
	else {
		/*lw = 0x7f - (int)((double)0x7f / 2.2 * log10(dcymsec / 23.04));*/
		lw = 0x7f - 54.8 * log10(dcymsec / 23.04);
	}
	if (lw < 1) lw = 1;
	if (lw > 127) lw = 127;
	return (unsigned char)lw;
}


/*
 * Cutoff frequency; return (0-255)
 * sf: abs cents (cents above 8.176Hz)
 * parm: quarter semitone; 0 = 125Hz, 0xff=8kHz?
 * (in VVS, cutoff(Hz) = value * 31.25 + 100)
 */
unsigned char awe_calc_cutoff(int abscents)
{
	int cutoff;
	if (abscents == 0) /* no cutoff */
		return 0xff;

	cutoff = abscents / 25 - 189;
	if (cutoff < 0) cutoff = 0;
	if (cutoff > 255) cutoff = 255;
	return (unsigned char)cutoff;
}


/*
 * Initial filter Q; return (0-15)
 * sf: centibels above DC gain.
 * parm: 0 = no attenuation, 15 = 24dB
 */
unsigned char awe_calc_filterQ(int gain_cB)
{
	int Q;
	Q = gain_cB * 2 / 30;
	if (Q < 0)
		Q = 0;
	else if (Q > 15)
		Q = 15;
	return (unsigned char)Q;
}

/*
 * Pitch modulation height (0-255)
 * sf: cents, 100 = 1 semitone
 * parm: signed char, 0x80 = 1 octave
 */
unsigned char awe_calc_pitch_height(int cents)
{
	int val;
	val = cents * 0x80 / 1200;
	if (val < -128) val = -128;
	if (val > 127) val = 127;
	if (val < 0)
		return (unsigned char)(0x100 + val);
	else
		return (unsigned char)val;
}

/*
 * Filter cutoff modulation height (0-255)
 * sf: 1200 = +1 octave
 * par: 0x80 = +6 octave
 */
unsigned char awe_calc_env_height(int cents, int octave_shift)
{
	int val;
	val = (int)cents * 0x80 / (octave_shift * 1200);
	if (val < -128) val = -128;
	if (val > 127) val = 127;
	if (val < 0)
		return (unsigned char)(0x100 + val);
	else
		return (unsigned char)val;
}


/*
 * Tremolo volume (0-255)
 * sf: cB, 10 = 1dB
 * parm: 0x7f = +/-12dB, 0x80 = -/+12dB
 */
unsigned short awe_calc_tremolo_vol(int vol_cB)
{
	int val;
	val = (int)vol_cB * 0x80 / 120;
	if (val < -128) val = -128;
	if (val > 127) val = 127;
	if (val < 0)
		val = 0x100 + val;
	return (unsigned char)val;
}

/*
 * Envelope frequency (0-255)
 * sf: cents 
 * parm: mHz / 42
 */
unsigned char awe_calc_env_freq(int abscents)
{
	int mHz, val;

	mHz = awe_abscent_to_mHz(abscents);
	val = mHz / 42;
	if (val < 0) val = 0;
	if (val > 255) val = 255;
	return (unsigned char)val;
}

/*
 * Panning position (0-127)
 * sf: (left) -500 - 500 (right) (0=center)
 * parm: (left) 0 - 127 (right), as same as MIDI parameter.
 *
 * NOTE:
 *   The value above is converted in the driver to the actual emu8000
 *   parameter, 8bit, 0 (right) - 0xff (left).
 */
char awe_calc_pan(int val)
{
	return (char)((val + 500) * 127 / 1000);
}

/*
 * Chorus strength
 * sf: 0 - 1000 (max)
 * parm: 0 - 255 (max)
 */
unsigned char awe_calc_chorus(int val)
{
	return (unsigned char)(val * 255 / 1000);
}

/*
 * Reverb strength
 * sf: 0 - 1000 (max)
 * parm: 0 - 255 (max)
 */
unsigned char awe_calc_reverb(int val)
{
	return (unsigned char)(val * 255 / 1000);
}

/*
 * Initial volume attenuation (0-255)
 * sf: centibels, eg. 60 = 6dB below from full scale
 * parm: dB * 8 / 3
 */
unsigned char awe_calc_attenuation(int att_cB)
{
	return (unsigned char)(att_cB * 8 / 30);
}
