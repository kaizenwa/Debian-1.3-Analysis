/*  SVGATextMode -- An SVGA textmode manipulation/enhancement tool
 *
 *  Copyright (C) 1995,1996  Koen Gadeyne
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/***
 *** SVGA clock programming functions for SVGATextMode
 ***/

#ifndef _STD_CLOCK_H
#define _STD_CLOCK_H

/*
 * Cirrus Logic specific clock stuff
 */


#define SET_CLOCKBITS_0_1(no)   ( outb(( inb(VGA_MISC_R) & 0xf3) | (((no) << 2) & 0x0C) , VGA_MISC_W) ) /* bits 0 and 1 of clock no */


void LegendClockSelect(int no);

void TVGAClockSelect(int chipset, int num_clocks, int no);

void s3ClockSelect(int no);

void ATIClockSelect(int chipset, int no);

void WDCClockSelect(int chipset, int num_clocks, int no);

void ET4000ClockSelect(int no);

void ET6000ClockSelect(int no);

void ET3000ClockSelect(int no);

void CirrusClockSelect(int freq);

void Video7ClockSelect(int no);

void ALIClockSelect(int chipset, int no);

void OAKClockSelect(int chipset, int no);

void SISClockSelect(int no);

void RealTekClockSelect(int no);

void ARKClockSelect(int no);

void NCRClockSelect(int chipset, int no);

void GVGAClockSelect(int no);

void MXClockSelect(int no);

#endif

