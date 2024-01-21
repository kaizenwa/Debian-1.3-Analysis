/*  SVGATextMode -- An SVGA textmode manipulation/enhancement tool
 *
 *  Copyright (C) 1995  Koen Gadeyne
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
 *** xfree_compat: misc. stuff to get the XFREE code to compile. This is junk :-(
 *** Written by Koen Gadeyne
 ***
 ***/
 


#include "common/compiler.h"   /* this will use the INVERTED outb() function! */
#include "xfree_compat.h"
#include "../misc.h"

/***************************************************************************/

/* next block is taken from xfree86/os-support/linux/lnx_video.c it was to
 * idiot to include the entire os-support/linux dir here, since these
 * functions were so simple...
 */

Bool xf86DisableInterrupts()
{
        return(TRUE);
}

void xf86EnableInterrupts()
{
        return;
}



/***************************************************************************/


/* next block is from xfree/accel/s3/s3TiCursor.c and Ti3026Curs.c. It was 
 * cut out and placed here because it looked like a big effort to get that
 * .c file to compile under SVGATextMode, and I needed only a few functions.
 */

#define S3_SERVER 1
#include "common_hw/Ti302X.h"

extern int vgaIOBase;
extern int vgaCRIndex;
extern int vgaCRReg;

/*
 * s3OutTiIndReg() and s3InTiIndReg() are used to access the indirect
 * 3020 registers only.
 */

#ifdef __STDC__
void s3OutTiIndReg(unsigned char reg, unsigned char mask, unsigned char data)
#else
void s3OutTiIndReg(reg, mask, data)
unsigned char reg;
unsigned char mask;
unsigned char data;
#endif
{
   unsigned char tmp, tmp1, tmp2 = 0x00;

   /* High 2 bits of reg in CR55 bits 0-1 (1 is cleared for the TI ramdac) */
   outb(vgaCRIndex, 0x55);
   tmp = inb(vgaCRReg) & 0xFC;
   outb(vgaCRReg, tmp | 0x01);  /* toggle to upper 4 direct registers */
   tmp1 = inb(TI_INDEX_REG);
   outb(TI_INDEX_REG, reg);

   /* Have to map the low two bits to the correct DAC register */
   if (mask != 0x00)
      tmp2 = inb(TI_DATA_REG) & mask;
   outb(TI_DATA_REG, tmp2 | data);

   /* Now clear 2 high-order bits so that other things work */
   outb(TI_INDEX_REG, tmp1);  /* just in case anyone relies on this */
   outb(vgaCRReg, tmp);
}

#ifdef __STDC__
unsigned char s3InTiIndReg(unsigned char reg)
#else
unsigned char s3InTiIndReg(reg)
unsigned char reg;
#endif
{
   unsigned char tmp, tmp1, ret;

   /* High 2 bits of reg in CR55 bits 0-1 (1 is cleared for the TI ramdac) */
   outb(vgaCRIndex, 0x55);
   tmp = inb(vgaCRReg) & 0xFC;
   outb(vgaCRReg, tmp | 0x01);  /* toggle to upper 4 direct registers */
   tmp1 = inb(TI_INDEX_REG);
   outb(TI_INDEX_REG, reg);

   /* Have to map the low two bits to the correct DAC register */
   ret = inb(TI_DATA_REG);

   /* Now clear 2 high-order bits so that other things work */
   outb(TI_INDEX_REG, tmp1);  /* just in case anyone relies on this */
   outb(vgaCRReg, tmp);

   return(ret);
}


/*
 * s3OutTi3026IndReg() and s3InTi3026IndReg() are used to access the indirect
 * 3026 registers only.
 */

#ifdef __STDC__
void s3OutTi3026IndReg(unsigned char reg, unsigned char mask, unsigned char data)
#else
void s3OutTi3026IndReg(reg, mask, data)
unsigned char reg;
unsigned char mask;
unsigned char data;
#endif
{
   unsigned char tmp, tmp1, tmp2 = 0x00;

   outb(vgaCRIndex, 0x55);
   tmp = inb(vgaCRReg) & 0xFC;
   outb(vgaCRReg, tmp | 0x00);
   tmp1 = inb(0x3c8);
   outb(0x3c8, reg);
   outb(vgaCRReg, tmp | 0x02);

   if (mask != 0x00)
      tmp2 = inb(0x3c6) & mask;
   outb(0x3c6, tmp2 | data);

   outb(vgaCRReg, tmp | 0x00);
   outb(0x3c8, tmp1);  /* just in case anyone relies on this */
   outb(vgaCRReg, tmp);
}

#ifdef __STDC__
unsigned char s3InTi3026IndReg(unsigned char reg)
#else
unsigned char s3InTi3026IndReg(reg)
unsigned char reg;
#endif
{
   unsigned char tmp, tmp1, ret;

   outb(vgaCRIndex, 0x55);
   tmp = inb(vgaCRReg) & 0xFC;
   outb(vgaCRReg, tmp | 0x00);
   tmp1 = inb(0x3c8);
   outb(0x3c8, reg);
   outb(vgaCRReg, tmp | 0x02);

   ret = inb(0x3c6);

   outb(vgaCRReg, tmp | 0x00);
   outb(0x3c8, tmp1);  /* just in case anyone relies on this */
   outb(vgaCRReg, tmp);

   return(ret);
}



/*
 * This piece is from s3.c from the XFree 3.1.2 code. 
 */

void
s3ProgramTi3025Clock(clk, n, m, p)
int clk;
unsigned char n;
unsigned char m;
unsigned char p;
{
   /*
    * Reset the clock data index
    */
   s3OutTiIndReg(TI_PLL_CONTROL, 0x00, 0x00);

   if (clk != TI_MCLK_PLL_DATA) {
      /*
       * Now output the clock frequency
       */
      s3OutTiIndReg(TI_PIXEL_CLOCK_PLL_DATA, 0x00, n);
      s3OutTiIndReg(TI_PIXEL_CLOCK_PLL_DATA, 0x00, m);
      s3OutTiIndReg(TI_PIXEL_CLOCK_PLL_DATA, 0x00, p | TI_PLL_ENABLE);

      /*
       * And now set up the loop clock for RCLK
       */
      s3OutTiIndReg(TI_LOOP_CLOCK_PLL_DATA, 0x00, 0x01);
      s3OutTiIndReg(TI_LOOP_CLOCK_PLL_DATA, 0x00, 0x01);
      s3OutTiIndReg(TI_LOOP_CLOCK_PLL_DATA, 0x00, p>0 ? p : 1);
      s3OutTiIndReg(TI_MISC_CONTROL, 0x00,
                    TI_MC_LOOP_PLL_RCLK | TI_MC_LCLK_LATCH | TI_MC_INT_6_8_CONTROL);

      /*
       * And finally enable the clock
       */
      s3OutTiIndReg(TI_INPUT_CLOCK_SELECT, 0x00, TI_ICLK_PLL);
   } else {
      /*
       * Set MCLK
       */
      s3OutTiIndReg(TI_MCLK_PLL_DATA, 0x00, n);
      s3OutTiIndReg(TI_MCLK_PLL_DATA, 0x00, m);
      s3OutTiIndReg(TI_MCLK_PLL_DATA, 0x00, p | 0x80);
   }
}

/* these ARK clockchip routines should be part of XFREE/common_hw/S3gendac.c */

#include "XFREE/common_hw/S3gendac.h"

static void setARKgendacpll(
#if NeedFunctionPrototypes
int reg, unsigned char data1, unsigned char data2
#endif
);

int
ARKgendacSetClock(freq, clk)
long freq;
int clk;
{
   unsigned char m, n;

   commonCalcClock(freq, 1, 100000, 250000, &m, &n);
 
   setARKgendacpll(clk, m, n);
   return 0;
}

static void
#if NeedFunctionPrototypes
setARKgendacpll(int reg, unsigned char data1, unsigned char data2)
#else
setARKgendacpll(reg, data1, data2)
int reg;
unsigned char data1;
unsigned char data2;
#endif
{
   unsigned char tmp, tmp1;
 
   /* set RS2 via SEQ 0x1C */
   outb(0x3C4, 0x1C);
   tmp = inb(0x3C5) & 0x7F;
   outb(0x3C4, tmp | 0x80);
   tmp1 = inb(GENDAC_INDEX);

   outb(GENDAC_INDEX, reg);
   outb(GENDAC_DATA, data1);
   outb(GENDAC_DATA, data2);

   /* Now clean up our mess */
   outb(GENDAC_INDEX, tmp1);
   outb(0x3C5, tmp);
}

/*
 * set the sync_on_green on TI 302X RAMDAC's
 * code borrowed from xc/programs/Xserver/hw/xfree86/accel/s3/s3init.c
 */
void set_ti_SOG(Bool SOG)
{
   unsigned char tmp, tmp1;

   /* change polarity on S3 to pass through control to the 3020 */
   tmp = inb(0x3CC);
   outb(0x3C2,tmp |= 0xC0);
   tmp1 = 0x00;
   if (!(tmp & 0x80)) tmp1 |= 0x02; /* invert bits for the 3020 */
   if (!(tmp & 0x40)) tmp1 |= 0x01;
   if (SOG) tmp1 |= 0x20;  /* add IOG sync */
   s3OutTiIndReg(TI_GENERAL_CONTROL, 0x00, tmp1);
}
