/*================================================================
 * awesetup.c
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
#include "awe_parm.h"
#include "awe_setup.h"

void awe_set_lfo1_delay(awe_voice_info *vp, int msec)
{
	vp->parm.lfo1delay = awe_calc_delay(msec);
}

void awe_set_lfo1_freq(awe_voice_info *vp, int abscents)
{
	vp->parm.tremfrq &= 0xff00;
	vp->parm.tremfrq |= awe_calc_env_freq(abscents) & 0xff;
}

void awe_set_lfo1_to_vol(awe_voice_info *vp, int centibels)
{
	vp->parm.tremfrq &= 0x00ff;
	vp->parm.tremfrq |= (awe_calc_tremolo_vol(centibels) << 8) & 0xff00;
}

void awe_set_lfo2_delay(awe_voice_info *vp, int msec)
{
	vp->parm.lfo2delay = awe_calc_delay(msec);
}

void awe_set_lfo2_freq(awe_voice_info *vp, int abscents)
{
	vp->parm.fm2frq2 &= 0xff00;
	vp->parm.fm2frq2 |= awe_calc_env_freq(abscents) & 0xff;
}

void awe_set_lfo2_to_pitch(awe_voice_info *vp, int cents)
{
	vp->parm.fm2frq2 &= 0x00ff;
	vp->parm.fm2frq2 |= (awe_calc_pitch_height(cents) << 8) & 0xff00;
}

void awe_set_mod_delay(awe_voice_info *vp, int msec)
{
	vp->parm.moddelay = awe_calc_delay(msec);
}

void awe_set_mod_attack(awe_voice_info *vp, int msec)
{
	vp->parm.modatkhld &= 0x00ff;
	vp->parm.modatkhld |= awe_calc_atkhld(msec, 0) & 0xff00;
}

void awe_set_mod_hold(awe_voice_info *vp, int msec)
{
	vp->parm.modatkhld &= 0xff00;
	vp->parm.modatkhld |= awe_calc_atkhld(msec, 0) & 0x00ff;
}
	
void awe_set_mod_decay(awe_voice_info *vp, int msec)
{
	vp->parm.moddcysus &= 0xff00;
	vp->parm.moddcysus |= awe_calc_decay(msec) & 0xff;
}

void awe_set_mod_sustain(awe_voice_info *vp, int centibels)
{
	vp->parm.moddcysus &= 0x00ff;
	vp->parm.moddcysus |= (awe_calc_sustain(centibels) << 8) & 0xff00;
}

void awe_set_mod_release(awe_voice_info *vp, int msec)
{
	vp->parm.modrelease &= 0xff00;
	vp->parm.modrelease |= awe_calc_decay(msec) & 0xff;
}


void awe_set_vol_delay(awe_voice_info *vp, int msec)
{
	vp->parm.voldelay = awe_calc_delay(msec);
}

void awe_set_vol_attack(awe_voice_info *vp, int msec)
{
	vp->parm.volatkhld &= 0x00ff;
	vp->parm.volatkhld |= awe_calc_atkhld(msec, 0) & 0xff00;
}

void awe_set_vol_hold(awe_voice_info *vp, int msec)
{
	vp->parm.volatkhld &= 0xff00;
	vp->parm.volatkhld |= awe_calc_atkhld(msec, 0) & 0x00ff;
}
	
void awe_set_vol_decay(awe_voice_info *vp, int msec)
{
	vp->parm.voldcysus &= 0xff00;
	vp->parm.voldcysus |= awe_calc_decay(msec) & 0xff;
}

void awe_set_vol_sustain(awe_voice_info *vp, int centibels)
{
	vp->parm.voldcysus &= 0x00ff;
	vp->parm.voldcysus |= (awe_calc_sustain(centibels) << 8) & 0xff00;
}

void awe_set_vol_release(awe_voice_info *vp, int msec)
{
	vp->parm.volrelease &= 0xff00;
	vp->parm.volrelease |= awe_calc_decay(msec) & 0xff;
}


void awe_set_lfo1_to_pitch(awe_voice_info *vp, int cents)
{
	vp->parm.fmmod &= 0x00ff;
	vp->parm.fmmod |= (awe_calc_pitch_height(cents) << 8) & 0xff00;
}

void awe_set_lfo1_to_cutoff(awe_voice_info *vp, int cents)
{
	vp->parm.fmmod &= 0xff00;
	vp->parm.fmmod |= awe_calc_env_height(cents, 3);
}

void awe_set_mod_to_pitch(awe_voice_info *vp, int cents)
{
	vp->parm.pefe &= 0x00ff;
	vp->parm.pefe |= (awe_calc_pitch_height(cents) << 8) & 0xff00;
}

void awe_set_mod_to_cutoff(awe_voice_info *vp, int cents)
{
	vp->parm.pefe &= 0xff00;
	vp->parm.pefe |= awe_calc_env_height(cents, 6);
}

void awe_set_cutoff(awe_voice_info *vp, int centibels)
{
	vp->parm.cutoff = awe_calc_cutoff(centibels);
}

void awe_set_filterQ(awe_voice_info *vp, int centibels)
{
	vp->parm.filterQ = awe_calc_filterQ(centibels);
}

void awe_set_chorus_effect(awe_voice_info *vp, int value)
{
	vp->parm.chorus = value;
}

void awe_set_reverb_effect(awe_voice_info *vp, int value)
{
	vp->parm.reverb = value;
}

