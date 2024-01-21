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
 *** SVGA clockchip programming functions for SVGATextMode
 ***/

#include <stdio.h>
#include <unistd.h>
#ifndef DOS
#  include <asm/io.h>
#endif
#include "misc.h"
#include "vga_prg.h"
#include "std_clock.h"
#include "chipset.h"
#include "clockchip.h"
#include "messages.h"  
#include "XFREE/common_hw/IBMRGB.h"

#define VGACLK0  25175
#define VGACLK1  28300
#define CKDEV       75

#define STDVGA25(f)  (((freq) > (VGACLK0-CKDEV)) && ((freq) < (VGACLK0+CKDEV)))
#define STDVGA28(f)  (((freq) > (VGACLK1-CKDEV)) && ((freq) < (VGACLK1+CKDEV)))

Bool prefer_vgaclocks(long freq, void clockselfunc())
{
  if STDVGA25(freq)
  { 
    clockselfunc(0);
    PDEBUG(("Clockcip: choosing standard VGA clock 25.175 instead of programmable clock.\n"));
    return(TRUE);
  }
  if ( (!OFLG_ISSET(OPT_CLKCHIP_X)) && (STDVGA28(freq)) )
  {
    clockselfunc(1);
    PDEBUG(("Clockcip: choosing standard VGA clock 28.3 instead of programmable clock.\n"));
    return(TRUE);
  }
  return(FALSE);
}

void set_clockchip_clock(int chipset, long freq)
{
  bool result=TRUE;
  int clk = 2;
  
  PDEBUG(("Setting clock for chipset #%d through clock chip #%d. Freq = %ld kHz.\n",\
           chipset, clock_data.clockchiptype, freq));
           
  switch (chipset)
  {
    case CS_S3:
     /* prefer standard VGA clocks over programmable clocks,
      * but don't do this when using the ICD clockchip and programming clock #1
      */
      if (prefer_vgaclocks(freq, s3ClockSelect)) break;
      switch(clock_data.clockchiptype)
      {
         case CLKCHIP_ICS9161A:
         case CLKCHIP_DCS2834:
         case CLKCHIP_ICD2061A:
                               /* setting exactly 120 MHz doesn't work all the time */
                               if (freq > 119900) freq = 119900;
                               freq *= 1000;
                               clk = (OFLG_ISSET(OPT_CLKCHIP_X)) ? 1 : 2; 
                               AltICD2061SetClock(freq, clk);
                               AltICD2061SetClock(freq, clk);
                               AltICD2061SetClock(freq, clk);
                               s3ClockSelect(OFLG_ISSET(OPT_SPEA_MERCURY) ? (4+clk) : clk); /* select the clock */
                               break;
        case CLKCHIP_SC11412:
                               result = SC11412SetClock(freq);
                               s3ClockSelect(OFLG_ISSET(OPT_SPEA_MERCURY) ? 6 : 2); /* select the clock */
                               break;
        case CLKCHIP_ICS2595:
                               result = ICS2595SetClock(freq);
                               result = ICS2595SetClock(freq);
                               break;
        case CLKCHIP_ICS5300:
        case CLKCHIP_S3_SDAC:
        case CLKCHIP_S3GENDAC:
                               (void) S3gendacSetClock(freq, 2); /* can't fail */
                               s3ClockSelect(2); /* select the clock */
                               break;
        case CLKCHIP_S3VIRGE:  
                               Outbit_SEQ(0x15, 1, 1);
                               /* fall through into S3Trio code */
        case CLKCHIP_S3TRIO:  
                               (void) S3TrioSetClock(freq, 2); /* can't fail */
                               break;
        case CLKCHIP_ICS5342:
                               (void) ICS5342SetClock(freq, 2); /* can't fail */
                               s3ClockSelect(2); /* select the clock */
                               break;
        case CLKCHIP_TI3025:
                               /* xfree_Ti3025_init(); PDEBUG(("TI3025 Init done\n")); */
                               Ti3025SetClock(freq, 2, s3ProgramTi3025Clock);
                               s3ClockSelect(2); /* select the clock */
                               break;
        case CLKCHIP_TI3026:
                               (void) Ti3026SetClock(freq, 2, 1); /* can't fail */
                               s3ClockSelect(2); /* select the clock */
                               break;
        case CLKCHIP_CH8391:
                               (void) Chrontel8391SetClock(freq, 2); /* can't fail */
                               s3ClockSelect(2); /* select the clock */
                               break;
        case CLKCHIP_STG1703:
                               (void) STG1703SetClock(freq, 2); /* can't fail */
                               s3ClockSelect(2); /* select the clock */
                               break;
        case CLKCHIP_IBMRGB5XX:
                               s3IBMRGB_Init();
                               /* IBM RGB ref clock MUST be set correctly, or all will go wrong... */
                               (void) IBMRGBSetClock(freq, 2, clock_data.maxclock, clock_data.refclk);
                               s3OutIBMRGBIndReg(IBMRGB_pll_ctrl1, 0xf8, 1);  /* Unlock clock select registers */
                               s3ClockSelect(2); /* select the clock */
                               break;
        default: PERROR(("Unknown clock chip #%d for S3 chipset.\n", clock_data.clockchiptype));
      }
      break;
    case CS_ET4000:
      if (prefer_vgaclocks(freq, ET4000ClockSelect)) break;
      /* change ET4000 option flags so everything works fine */
      OFLG_CLR(OPT_HIBIT_HIGH);
      OFLG_SET(OPT_HIBIT_LOW);
      OFLG_CLR(OPT_LEGEND);
      OFLG_CLR(OPT_ET4000_ALTCLK);
      PDEBUG(("Modified options mask (ET4000+ICS5341): 0x%x\n", STM_Options));
      switch(clock_data.clockchiptype)
      {
        case CLKCHIP_ICS5341:
                               (void) ET4000gendacSetClock(freq, 2); /* can't fail */
                               /* select clock #2 */
                               ET4000ClockSelect(2); /* make sure all Tseng-specific dividers are disabled */
                               break;
        case CLKCHIP_ICD2061A:
                               /* setting exactly 120 MHz doesn't work all the time */
                               if (freq > 119900) freq = 119900;
                               freq *= 1000;
                               clk = (OFLG_ISSET(OPT_CLKCHIP_X)) ? 1 : 2; 
                               Et4000AltICD2061SetClock(freq, clk); 
                               ET4000ClockSelect(clk); /* make sure all Tseng-specific dividers are disabled */
                               break;
        default: PERROR(("Unknown clock chip #%d for ET4000 chipset.\n", clock_data.clockchiptype));
      }
      break;
      
    case CS_CIRRUS:
      switch(clock_data.clockchiptype)
      {
        case CLKCHIP_CIRRUS:
                               CirrusClockSelect(freq);
                               break;
        default: PERROR(("Unknown clock chip #%d for Cirrus chipset.\n", clock_data.clockchiptype));
      }
      break;
    case CS_ARK:
      if (prefer_vgaclocks(freq, ARKClockSelect)) break;
      switch(clock_data.clockchiptype)
      {
        case CLKCHIP_ICS5342:
                               (void) ARKgendacSetClock(freq, 2); /* can't fail */
                               /* select clock #2 */
                               ARKClockSelect(2);
                               break;
        default: PERROR(("Unknown clock chip #%d for ARK chipset.\n", clock_data.clockchiptype));
      }
      break;
    case CS_ET6000:
      if (prefer_vgaclocks(freq, ET6000ClockSelect)) break;
      switch(clock_data.clockchiptype)
      {
        case CLKCHIP_ET6000:
                               (void) ET6000SetClock(freq, 2); /* can't fail */
                               ET6000ClockSelect(2); /* select the clock */
                               break;
        default: PERROR(("Unknown clock chip #%d for ET6000 chipset.\n", clock_data.clockchiptype));
      }
      break;
    default: PERROR(("Internal error in set_clockchip_clock: chipset #%d does not support any clockchip.\n", chipset));
  }
  if (result == FALSE) PWARNING(("ClockChip: error while programming clock chip\n"));
  usleep(50000);
}

/*****************************************************************************************************************************/

void set_clockchip_Mclock(int chipset, long freq)
{
  bool result=TRUE;
  
  PDEBUG(("Setting MClk for chipset #%d through clock chip #%d. Freq = %ld kHz.\n",\
           chipset, clock_data.clockchiptype, freq));
  SYNCRESET_SEQ;

  switch(chipset)
  {
    case CS_S3:
      switch(clock_data.clockchiptype)
      {
        case CLKCHIP_S3_SDAC:
        case CLKCHIP_S3GENDAC:
        case CLKCHIP_ICS5300:
                 (void) S3gendacSetClock(freq, 10);
                 break;
        case CLKCHIP_S3VIRGE:  
                 Outbit_SEQ(0x15, 0, 1);
                 (void) S3TrioSetClock(freq, 10);
                 Outbit_SEQ(0x15, 0, 0);
                 break;
        case CLKCHIP_S3TRIO:  
                 (void) S3TrioSetClock(freq, 10);
                 break;
        case CLKCHIP_ICS5342:
                 (void) ICS5342SetClock(freq, 10);
                 break;
        default:
                 ENDRESET_SEQ;
                 PERROR(("MClk programming not supported for this S3 clockchip.\n"));
      }
      break;
    case CS_ET4000:
      switch(clock_data.clockchiptype)
      {
        case CLKCHIP_ICS5341:
                 (void) ET4000gendacSetClock(freq, 10); /* can't fail */
                 break;
        default:
                 ENDRESET_SEQ;
                 PERROR(("MClk programming not supported for this ET4000 clockchip\n"));
      }
      break;
    case CS_ARK:
      switch(clock_data.clockchiptype)
      {
        case CLKCHIP_ICS5342:
                 (void) ARKgendacSetClock(freq, 10); /* can't fail */
                 break;
        default:
                 ENDRESET_SEQ;
                 PERROR(("MClk programming not supported for this ARK clockchip\n"));
      }
      break;
    case CS_ET6000:
      switch(clock_data.clockchiptype)
      {
        case CLKCHIP_ET6000:
                 (void) ET6000SetClock(freq, 10);
                 break;
        default:
                 ENDRESET_SEQ;
                 PERROR(("MClk programming not supported for this ET6000 clockchip\n"));
      }
      break;
    default:
             ENDRESET_SEQ;
             PERROR(("MClk programming not supported on this chipset.\n"));
  }
  ENDRESET_SEQ;
  if (result == FALSE) PWARNING(("ClockChip: error while programming clock chip\n"));
  usleep(50000);
}
