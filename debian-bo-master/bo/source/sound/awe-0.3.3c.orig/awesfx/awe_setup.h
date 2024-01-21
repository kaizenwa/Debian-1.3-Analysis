/*================================================================
 * awesetup.h
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

#ifndef AWESETUP_H_DEF
#define AWESETUP_H_DEF

#ifdef linux
#include <linux/awe_voice.h>
#else
#include <awe_voice.h>
#endif

/* modulation envelope parameters */
void awe_set_mod_delay(awe_voice_info *vp, int msec);
void awe_set_mod_attack(awe_voice_info *vp, int msec);
void awe_set_mod_hold(awe_voice_info *vp, int msec);
void awe_set_mod_decay(awe_voice_info *vp, int msec);
void awe_set_mod_sustain(awe_voice_info *vp, int centibels);
void awe_set_mod_release(awe_voice_info *vp, int msec);
void awe_set_mod_to_pitch(awe_voice_info *vp, int cents);
void awe_set_mod_to_cutoff(awe_voice_info *vp, int cents);
void awe_set_mod_keyhold(awe_voice_info *vp, int percent);
void awe_set_mod_keydecay(awe_voice_info *vp, int percent);

/* volume envelope parameters */
void awe_set_vol_delay(awe_voice_info *vp, int msec);
void awe_set_vol_attack(awe_voice_info *vp, int msec);
void awe_set_vol_hold(awe_voice_info *vp, int msec);
void awe_set_vol_decay(awe_voice_info *vp, int msec);
void awe_set_vol_sustain(awe_voice_info *vp, int centibels);
void awe_set_vol_release(awe_voice_info *vp, int msec);

/* LFO1 parameters */
void awe_set_lfo1_delay(awe_voice_info *vp, int msec);
void awe_set_lfo1_freq(awe_voice_info *vp, int abscents);
void awe_set_lfo1_to_pitch(awe_voice_info *vp, int cents);
void awe_set_lfo1_to_vol(awe_voice_info *vp, int centibels);
void awe_set_lfo1_to_cutoff(awe_voice_info *vp, int cents);

/* LFO2 parameters */
void awe_set_lfo2_delay(awe_voice_info *vp, int msec);
void awe_set_lfo2_freq(awe_voice_info *vp, int abscents);
void awe_set_lfo2_to_pitch(awe_voice_info *vp, int cents);

/* initial cutoff */
void awe_set_cutoff(awe_voice_info *vp, int centibels);
void awe_set_filterQ(awe_voice_info *vp, int decibels);

/* chorus and reverb effects */
void awe_set_chorus_effects(awe_voice_info *vp, int value);
void awe_set_reverb_effects(awe_voice_info *vp, int value);

#endif
