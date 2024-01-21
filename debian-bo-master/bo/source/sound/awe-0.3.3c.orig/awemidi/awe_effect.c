/*================================================================
 * awe_effect.h
 *	convert NRPM parameters to Emu8000 raw effects
 *================================================================*/

#include <stdio.h>
#include <math.h>
#include "awe_effect.h"


unsigned short fx_delay(int val)
{
	return (0x8000 - val * 4 * 1000 / 725);
}

unsigned short fx_attack(int val)
{
	unsigned short ret;
	if (val == 0)
		return 0x7f;
	else if (val >= 360)
		ret = (unsigned short)(11878 / val);
	else if (val < 360)
		ret = (unsigned short)(32 + 53.426 * log10(360.0/val));
	else
		return 0x7f;
	if (ret < 1) ret = 1;
	return ret;
}

unsigned short fx_hold(int val)
{
	return 0x7f - (unsigned char)(val / 92);
}

unsigned short fx_decay(int val)
{
	unsigned short ret;
	if (val == 0)
		return 0x7f;
	else
		ret = (unsigned short)(0x7f - 54.8 * log10(val * 4 / 23.04));
	if (ret < 1) ret = 1;
	if (ret > 0x7f) ret = 0x7f;
	return ret;
}

unsigned short fx_the_value(int val)
{
	return (unsigned short)(val & 0xff);
}

unsigned short fx_twice_value(int val)
{
	return (unsigned short)((val * 2) & 0xff);
}

unsigned short fx_conv_pitch(int val)
{
	return (short)(val * 4096 / 1200);
}

unsigned short fx_conv_Q(int val)
{
	return (unsigned short)((val / 8) & 0xff);
}

