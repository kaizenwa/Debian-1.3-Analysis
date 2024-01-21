/*================================================================
 * awe_effect.h
 *	convert NRPM parameters to Emu8000 raw effects
 *================================================================*/

#ifndef AWE_EFFECT_H_DEF
#define AWE_EFFECT_H_DEF

unsigned short fx_delay(int val);
unsigned short fx_attack(int val);
unsigned short fx_hold(int val);
unsigned short fx_decay(int val);
unsigned short fx_the_value(int val);
unsigned short fx_twice_value(int val);
unsigned short fx_conv_pitch(int val);
unsigned short fx_conv_Q(int val);

/* function for each NRPN */		/* [range]  units */
#define fx_env1_delay	fx_delay	/* [0,5900] 4msec */
#define fx_env1_attack	fx_attack	/* [0,5940] 1msec */
#define fx_env1_hold	fx_hold		/* [0,8191] 1msec */
#define fx_env1_decay	fx_decay	/* [0,5940] 4msec */
#define fx_env1_release	fx_decay	/* [0,5940] 4msec */
#define fx_env1_sustain	fx_the_value	/* [0,127] 0.75dB */
#define fx_env1_pitch	fx_the_value	/* [-127,127] 9.375cents */
#define fx_env1_cutoff	fx_the_value	/* [-127,127] 56.25cents */

#define fx_env2_delay	fx_delay	/* [0,5900] 4msec */
#define fx_env2_attack	fx_attack	/* [0,5940] 1msec */
#define fx_env2_hold	fx_hold		/* [0,8191] 1msec */
#define fx_env2_decay	fx_decay	/* [0,5940] 4msec */
#define fx_env2_release	fx_decay	/* [0,5940] 4msec */
#define fx_env2_sustain	fx_the_value	/* [0,127] 0.75dB */

#define fx_lfo1_delay	fx_delay	/* [0,5900] 4msec */
#define fx_lfo1_freq	fx_twice_value	/* [0,127] 84mHz */
#define fx_lfo1_volume	fx_twice_value	/* [0,127] 0.1875dB */
#define fx_lfo1_pitch	fx_the_value	/* [-127,127] 9.375cents */
#define fx_lfo1_cutoff	fx_twice_value	/* [-64,63] 56.25cents */

#define fx_lfo2_delay	fx_delay	/* [0,5900] 4msec */
#define fx_lfo2_freq	fx_twice_value	/* [0,127] 84mHz */
#define fx_lfo2_pitch	fx_the_value	/* [-127,127] 9.375cents */

#define fx_init_pitch	fx_conv_pitch	/* [-8192,8192] cents */
#define fx_chorus	fx_the_value	/* [0,255] -- */
#define fx_reverb	fx_the_value	/* [0,255] -- */
#define fx_cutoff	fx_twice_value	/* [0,127] 62Hz */
#define fx_filterQ	fx_conv_Q	/* [0,127] -- */

#endif
