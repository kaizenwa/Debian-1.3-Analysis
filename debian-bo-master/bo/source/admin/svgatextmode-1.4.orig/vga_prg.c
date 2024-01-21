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
 *** VGA chip programming functions for SVGATextMode
 ***/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "misc.h"
#include "vga_prg.h"
#include "messages.h"  

/*
 * global vars
 */
 
 int vgaIOBase  = 0x3D0; /* default = color */
 unsigned long PCIIOBase  = 0xDEADBEEF;
 int vgaCRIndex = 0x3D4;
 int vgaCRReg   = 0x3D5;
 bool vga_open   = FALSE;

/*
 * Some functions to make life easier (?!)
 */

void Outb_ATR_CTL (int index, int data)
{
  inb(ATR_CTL_INDEX_DATA_SWITCH);
  outb (index & 0x1f, ATR_CTL_INDEX);
  outb ( data, ATR_CTL_DATA_W);
  inb(ATR_CTL_INDEX_DATA_SWITCH);
  outb ( (index & 0x1f) | 0x20, ATR_CTL_INDEX);
  outb ( data, ATR_CTL_DATA_W);
}

int inb_ATR_CTL (int index)
{
  int res;

  inb(ATR_CTL_INDEX_DATA_SWITCH);
  outb (index & 0x1f, ATR_CTL_INDEX);
  res=inb(ATR_CTL_DATA_R);
  inb(ATR_CTL_INDEX_DATA_SWITCH);
  outb ( (index & 0x1f) | 0x20, ATR_CTL_INDEX);
  inb(ATR_CTL_DATA_R);
  return res;
}


/*****************************************************************************************************************************/

void get_IO_range(long start, long len)
{
  PDEBUG(("Getting VGA IO permissions for addr. range 0x%x-0x%x\n", start, start+len-1));
  if (start>=0x400)
  {
    if (iopl(3) != 0)
    {
      perror("I/O Privilege permissions");
      PERROR(("Cannot set I/O privileges.\n\
               You must be superuser, or the program must be setuid root!\n"));
    }
  }
  else
  {
    if (ioperm(start, len, 1) != 0)
    {
      perror("VGA I/O Permissions");
      PERROR(("Cannot get I/O permissions for hardware address range 0x%x-0x%x.\n\
               You must be superuser, or the program must be setuid root!\n", start, start+len-1));
    }
  }
}

/*
 * Get IO permissions, and setup some global variables for VGA programming.
 */

void get_VGA_io_perm(int chipset)
{
  PDEBUG(("Getting VGA IO permissions for chipset #%d\n", chipset));
  vga_open=FALSE;
  get_IO_range(0x3b4, 0x3df - 0x3b4 + 1);
  switch(chipset)
  {
    case CS_ATI:
    case CS_ATIMACH32:
       get_IO_range(ATI_EXTREG, 2);
       break;
    case CS_S3:
       get_IO_range(0x200, 2);   /* used by ICD2061 clock code for IOdelay */
       break;
    case CS_ET6000:
       PCIIOBase = (Inb_CRTC(0x21)<<8);
       PCIIOBase += (Inb_CRTC(0x22)<<16);
       PCIIOBase += (Inb_CRTC(0x23)<<24); /* keep this split up */
       PDEBUG(("ET6000 PCI config space: 0x%X\n", PCIIOBase));
       get_IO_range(PCIIOBase, 256);
       PDEBUG(("PCI vendor ID = 0x%X ; device ID = 0x%X\n", inw(PCIIOBase), inw(PCIIOBase+2)));
       break;
  }
  
#ifdef RUN_SECURE
  Renounce_SUID; /* if we are Setuid Root: renounce to further superuser rights (safer) */
#endif

  vga_open=TRUE;  /* needed for PERROR, so it knows if it can restore the screen or not */

  /* this is probably not the best place to put this */
  vgaIOBase  = GET_VGA_BASE;
  vgaCRIndex = (vgaIOBase+4);
  vgaCRReg   = (vgaIOBase+5);
}

/*****************************************************************************************************************************/

void unlock(int chipset)
{
   /* unlock ALL locked registers for specified chipset. A bit rough, but simplest */
   PDEBUG(("Unlocking chipset %d\n",chipset));
   Outbit_CRTC (0x11, 7, 0); /* CRTC index 0x00..0x07 */
   switch(chipset)
   {
    case CS_CIRRUS :
       Outb_SEQ (0x6, 0x12);	/* unlock cirrus special */
       break;
    case CS_S3     : 
       Outb_CRTC(0x39, 0xa5); /* system extension regs (CRTC index 0x50..0x5E) */
       Outb_CRTC(0x38, 0x48); /* S3 register set (CRTC index 0x30..0x3C) */
       Outbit_CRTC(0x35, 4, 0); /* VERT timing regs (CRTC index 6,7(bit0,2,3,5,7),9,10,11(bits0..3),15,16 ) */
       Outbit_CRTC(0x35, 5, 0); /* HOR timing regs (CRTC index 0..5, 17(bit2) ) */
       Outbit_CRTC(0x34, 5, 0); /* bit 0 of Clocking mode reg unlocked (8/9 dot font selection) */
       Outbit_CRTC(0x34, 7, 0); /* Clock bits in MISC reg unlocked */
      /*  Outbit_CRTC(0x40, 0, 1); */ /* enhanced register access, only for access to accelerator commands. Does NOT seem to work on my 805 */
       break;
    case CS_ET4000 :
    case CS_ET3000 :
       outb(0x03, 0x3BF); outb(0xA0, 0x3D8); /* ET4000W32i key */
       break;
    case CS_ATI:
    case CS_ATIMACH32:
       ATI_PUTEXTREG(0xB4, ATI_GETEXTREG(0xB4) & 0x03);
       ATI_PUTEXTREG(0xB8, ATI_GETEXTREG(0xb8) & 0xC0);
       ATI_PUTEXTREG(0xB9, ATI_GETEXTREG(0xb9) & 0x7F);
       ATI_PUTEXTREG(0xBE, ATI_GETEXTREG(0xbe) | 0x01);
       break;
    case CS_PVGA1:
    case CS_WDC90C0X:
    case CS_WDC90C1X:
    case CS_WDC90C2X:
    case CS_WDC90C3X:
       Outb_GR_CTL(0x0F, (Inb_GR_CTL(0x0F) & 0x80) | 0x05);
       if (chipset != CS_PVGA1)    /* these might not be needed */
       {
         Outb_CRTC(0x29, (Inb_CRTC(0x29) & 0x70) | 0x85);
         Outb_CRTC(0x2a, Inb_CRTC(0x2a) & 0xF8);
         if (chipset != CS_WDC90C0X)
         {
           Outb_SEQ(0x06, 0x48);
           if (chipset != CS_WDC90C1X) Outb_CRTC(0x34, 0xA6);
         }
       }
       break;
    case CS_ALI:
    case CS_AL2101:
       Outbit_CRTC(0x1A, 4, 1);
       break; 
    case CS_OTI67:
    case CS_OTI77:  /* CS_OTI87 doesn't seem to need unlocking */
       Outb_OTI( OTI_CRT_CNTL, Inb_OTI(OTI_CRT_CNTL) & 0xF0 );
       break; 
    case CS_SIS:
       Outb_SEQ(0x05, 0x86);
       break;
    case CS_ARK:
       Outbit_SEQ(0x1D, 0, 1); 
       break;
    case CS_NCR22E:
    case CS_NCR32:
       Outbit_SEQ(0x05, 0, 1);
       break;
    case CS_MX:
       Outbit_SEQ(0x65, 6, 1); /* not in XFree86, but VGADOC4 says otherwise */
       Outb_SEQ(0xA7, 0x87);
       break;
    case CS_ET6000:
       outb(inb(vgaIOBase+0x08)|0xA0, vgaIOBase+0x08); /* ET6000 KEY register bits */
       break;
    default: PDEBUG(("UNLOCK VGA: No special register unlocking needed for chipset #%d\n",chipset));
   }
}
/*****************************************************************************************************************************/

void special(int chipset)
/* chipset specific settings, like memory speed and the likes */
{
   int tmp;  

   PDEBUG(("Setting chipset-specific special registers\n"));
   switch(chipset)
   {
     case CS_ATI:
        ATI_PUTEXTREG(0xB0, ATI_GETEXTREG(0xb0) & ~0x08);   /* (for 188xx chips) Enable 8 CRT accesses for each CPU access */
        break;
     case CS_S3:
        /* set `M-parameter' which controls the DRAM FIFO balancing */
        /* this was derived from the svgalib 1.2.8 code */
        tmp = Inb_CRTC(0x54) & 0x07;
        if (OFLG_ISSET(OPT_XFAST_DRAM)) Outb_CRTC(0x54,tmp | (0 << 3));
        if (OFLG_ISSET(OPT_FAST_DRAM))  Outb_CRTC(0x54,tmp | (2 << 3));
        if (OFLG_ISSET(OPT_MED_DRAM))   Outb_CRTC(0x54,tmp | (10 << 3));
        if (OFLG_ISSET(OPT_SLOW_DRAM))  Outb_CRTC(0x54,tmp | (20 << 3));
        break;
     case CS_ET6000:
        /* set System performance control register: FIFO underflow prevention */
        tmp = inb(PCIIOBase+0x41);
        outb(tmp | 0x10, PCIIOBase+0x41);
        break;
     default:
        PDEBUG(("SPECIAL VGA chip settings: no special settings for chipset #%d\n",chipset));
   }
}
/*****************************************************************************************************************************/

void interlace(int chipset, t_mode *m)
/*
 * chipset specific interlace settings (also used to un-set interlace mode)
 * some chipsets need to change some timings for interlacing (e.g. dividing V-timings by 2).
 */
{
   int il = MOFLG_ISSET(m, ATTR_INTERLACE);
   
   /* PDEBUG(("%sing interlacing mode\n", il ? "Sett" : "Disabl")); */
   switch(chipset)
   {
     case CS_S3    : Outbit_CRTC(0x42, 5, il ? 1 : 0);
                     if (il)
                     {
                       m->VDisplay /= 2;
                       m->VSyncStart /= 2;
                       m->VSyncEnd /= 2;
                       m->VTotal /= 2;
                       m->VBlankStart /= 2;
                       m->VBlankEnd /= 2;
                       Outb_CRTC(0x3C, m->HDisplay/2);
                     }
                     break;
     default: if (il) PDEBUG(("INTERLACING not supported yet on chipset #%d\n",chipset));
   }
}
/*****************************************************************************************************************************/

#define HSTEXT_MINCLOCK 36000

void S3_StartHSText_FontLoad(int pixclock, int do_it)
{
   bool hstext = OFLG_ISSET(OPT_S3_HS_TEXT) && (pixclock > HSTEXT_MINCLOCK);
   bool old_was_HS=(Inb_CRTC(0x31) & 0x40);

   PDEBUG(("Current S3 text mode: %s\n", old_was_HS ? "High Speed" : "Normal" ));

   if ( hstext && (!old_was_HS) && !OFLG_ISSET(OPT_LOADFONT) )
   {
      PWARNING(("\n\
       Switching from normal to High Speed text mode requires the Option `LoadFont'.\n\
       Normal text mode will be used until font loading is enabled.\n"));
      hstext=FALSE;
   }
   
   if ( (!hstext) && old_was_HS && !OFLG_ISSET(OPT_LOADFONT) )
   {
      PWARNING(("\n\
       Switching from High Speed to normal text mode requires the Option `LoadFont'.\n\
       High speed text mode will be used until font loading is enabled.\n"));
      hstext=TRUE;
   }
   
   if (do_it)
   {
     Outbit_CRTC(0x3A, 5, 0);   /* normal font store mode, just for sure... */
     if (hstext)
       {
         Outbit_CRTC(0x3A, 5, 1);   /* prepare S3 for high speed font store mode */
         Outbit_CRTC(0x31, 6, 1);   /* enable high speed font fetch mode */
       }
       else
       { 
         Outbit_CRTC(0x31, 6, 0);   /* disable high speed font fetch mode */
       }
   }
}

/*****************************************************************************************************************************/

void set_V_timings(int active, int start_sync, int stop_sync, int total)
{
  int vbs, vbe;
  if (total > 1023)
  {
    /* round UP around sync to avoid null sync width */
    active = active / 2;
    start_sync = start_sync / 2;
    stop_sync = (stop_sync + 1) / 2;
    total = (total + 1) / 2;
    Outbit_CRTC(0x17, 2, 1);  /* Vertical total double mode */
  }
  else Outbit_CRTC(0x17, 2, 0);
  Set_VERT_TOTAL(total);
  Set_VDISPL_END(active);
  Set_VRETRACE_START(start_sync); Set_VRETRACE_END(stop_sync);
  /* set 8 lines of overscan, the rest is blanking (if the sync doesn't come too close) */
  vbs = MIN(start_sync, active+8);
  vbe = MAX(stop_sync, total-8);
  if ((vbe-vbs)>=127)
   {
     PDEBUG(("V Blanking size >= 127 ; setting to 127\n"));
     vbe = vbs+127;
   }
  Set_VBLANK_START(vbs); Set_VBLANK_END(vbe);
}

void set_H_timings(int active, int start_sync, int stop_sync, int total)
{
  int hrs=start_sync/8;
  int hre=stop_sync/8;
  int hde=(active/8) & 0xFFFFFFFE;
  int htot=total/8;

  /* set 1 char of overscan, with the rest blanked (if sync positions don't overlap) */
  int hbs=MIN(hrs, hde+1);
  int hbe=MAX(hre, htot-1);
  if ((hbe-hbs)>=63)
   {
     PDEBUG(("H Blanking size >= 63 ; setting to 63\n"));
     hbe = hbs+63;
   }
  Set_HOR_TOTAL(htot);
  Set_HOR_DISPL_END(hde);
  Set_HSYNC_START(hrs); Set_HSYNC_END(hre);
  Set_LOG_SCREEN_WIDTH(hde);
  Set_HBLANK_START(hbs); Set_HBLANK_END(hbe);
}

int set_charwidth(int width)
{
   switch(width)
   {
      case 8: Outbit_SEQ(1, 0, 1);
              SET_PIXELPAN(0);
              break;
      case 9: Outbit_SEQ(1, 0, 0);
              SET_PIXELPAN(8);
              break;
      default: return(1);
   }
   return(0);
}

/*****************************************************************************************************************************/
/* "get" functions: used for getting certain parameters from the chips (for grabmode etc.) */

inline int get_charwidth()
{
  return((Inb_SEQ(1) & 0x01) ? 8 : 9);
}


inline int Get_VERT_TOTAL()
{
  return( (Inb_CRTC(0x6) + ((Inb_CRTC(0x7) & 0x01) ? 256 : 0) + ((Inb_CRTC(0x7) & 0x20) ? 512 : 0)) + 2);
}


inline int Get_HOR_TOTAL()
{
  return(Inb_CRTC(0)+5);
}


inline int Get_HOR_DISPL_END()
{
  return((int)Inb_CRTC(1)+1);
}

/* calculate end value from start value and mask. this is because "end" values in VGA regs define only the few
 * last bits of the entire value. See VGA data for more */
 
#define ENDCALC(start,end,mask) ( ((((start & mask) > end) ? (start + (mask+1)) : start) & ~mask) | end  )

inline int Get_HSYNC_START()
{
  return((int)Inb_CRTC(4));
}

inline int Get_HSYNC_END()
{
  int start, end;
  start = Get_HSYNC_START();
  end = Inb_CRTC(5) & 0x1f;
  return(ENDCALC(start, end, 0x1f));
}

inline int Get_HBLANK_START()
{
  return((int)Inb_CRTC(2) + 1);
}

inline int Get_HBLANK_END()
{
  /* this does not work correctly when Get_HOR_TOTAL() reads wrapped data (as in 2048+ pixel wide modes) */
  int start, end, htot, endblk;
  htot = Get_HOR_TOTAL();
  start = Get_HBLANK_START();
  end = ((int)Inb_CRTC(3) & 0x1F) + ((Inb_CRTC(5) & 0x80) ? 0x20 : 0);
  endblk = ENDCALC(start, end, 0x3f)+1;
  return((endblk < htot) ? endblk : (end+1));
}

inline int Get_VBLANK_START()
{
  return((int)Inb_CRTC(0x15) + ((Inb_CRTC(0x7) & 0x08) ? 256 : 0) + ((Inb_CRTC(0x9) & 0x20) ? 512 : 0));
}

inline int Get_VBLANK_END()
{
  int start = Get_VBLANK_START()-1;
  int end = Inb_CRTC(0x16);
  return(ENDCALC(start, end, 0xff));
}
 
 
inline int Get_VERT_DISPL_END()
{
  return( ( (int)Inb_CRTC(0x12) + ((Inb_CRTC(0x7) & 0x02) ? 256 : 0) + ((Inb_CRTC(0x7) & 0x40) ? 512 : 0) ) +1);
}

inline int Get_VRETRACE_START()
{
  int vrs = (int)Inb_CRTC(0x10) + ((Inb_CRTC(0x7) & 0x04) ? 256 : 0) + ((Inb_CRTC(0x7) & 0x80) ? 512 : 0);
  return( (vrs==0) ? 1024 : vrs );
}

inline int Get_VRETRACE_END()
{
  int start, end;
  start = Get_VRETRACE_START();
  end = Inb_CRTC(0x11) & 0x0f;
  return(ENDCALC(start, end, 0x0f));
}

inline int Get_MAX_SCANLINE()
{
  return((Inb_CRTC(0x9) & 0x1f) +1);
}

inline int Get_HSYNC_POLARITY()
{
  return( (inb(VGA_MISC_R) & 0x40) ? NEG : POS);
}

inline int Get_VSYNC_POLARITY()
{
  return( (inb(VGA_MISC_R) & 0x80) ? NEG : POS);
}

inline int Get_TX_GR_Mode()
/* returns 0 for text mode, 1 for graphics mode */
{
  return(Inb_GR_CTL(6) & 0x01);
}
 
