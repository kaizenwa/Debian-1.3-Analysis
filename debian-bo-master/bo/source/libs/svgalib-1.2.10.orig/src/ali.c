/* VGAlib version 1.2 - (c) 1993 Tommy Frandsen

   This library is free software; you can redistribute it and/or
   modify it without any restrictions. This library is distributed
   in the hope that it will be useful, but without any warranty.

   Multi-chipset support Copyright 1993 Harm Hanemaayer
   partially copyrighted (C) 1993 by Hartmut Schirmer

   ALI2301 support driver release 1.0
   converted from X by Ron Koerner (koerne00@marvin.informatik.uni-dortmund.de)
   took oak-driver as template


   Features in this release: 
   * 640x480x256 mode
   * 800x600x256 mode
   * 1024x768x256 mode

   Still left to do:
   * get this thing run ;)
   * 640x480x64K, 640x480x16M modes
   * 800x600x16, 800x600x64K modes
   * 1024x768x16 modes
   * 1280x1024x16 mode
 */

#include <stdio.h>
#include "vga.h"
#include "libvga.h"
#include "driver.h"
#include <sys/mman.h>
#include "ali.regs"

static int ali_memory;
static unsigned char last_page = 0;

static int ali_init(int, int, int);
static int ali_memorydetect(void);
static int GETB(int, int);

static void ali_unlock(void);
static void ali_setpage(int page);
static void SETB(int, int, int);

/* Mode table */

static ModeTable ali_modes[] =
{
/* *INDENT-OFF* */
   OneModeEntry(640x480x256),
/*   OneModeEntry(800x600x16), */
   OneModeEntry(800x600x256),
/*   OneModeEntry(1024x768x16),*/
   OneModeEntry(1024x768x256),
/*   OneModeEntry(1280x1024x16),*/
/*   OneModeEntry(640x480x64K),*/
/*   OneModeEntry(640x480x16M),*/
/*   OneModeEntry(800x600x64K),*/
   END_OF_MODE_TABLE  
/* *INDENT-ON* */
};

/* ali_getmodeinfo( int mode, vga_modeinfo *modeinfo)   Tell SVGALIB stuff

 * Return mode information (blityes/no, start address, interlace, linear
 */

static void ali_getmodeinfo(int mode, vga_modeinfo * modeinfo)
{
    if (modeinfo->bytesperpixel > 0) {
	modeinfo->maxpixels = ali_memory * 1024 / modeinfo->bytesperpixel;
    } else {
	modeinfo->maxpixels = ali_memory * 1024;	/* any val */
    }

    modeinfo->maxlogicalwidth = 2040;
    modeinfo->startaddressrange = 0xfffff;
    if (mode == G320x200x256) {
	modeinfo->maxpixels = 65535;
	modeinfo->maxlogicalwidth = 320;
	modeinfo->startaddressrange = 0x0;
    }
    modeinfo->haveblit = 0;
    modeinfo->flags |= HAVE_RWPAGE;
}

/* ali_saveregs( unsigned char regs[] )                 Save extended regs

 * Save extended registers into regs[].
 */

static int ali_saveregs(unsigned char regs[])
{
    int tmp;
    ali_unlock();
    tmp = GETB(0x3ce, 0x0c);
    regs[EXT + 8] = inb(0x3d6);
    regs[EXT + 9] = inb(0x3d7);
    outb(0x3d6, 0);
    regs[EXT + 1] = GETB(CRT_I, 0x19);
    regs[EXT + 2] = GETB(CRT_I, 0x1a);
    regs[EXT + 3] = GETB(CRT_I, 0x28);
    regs[EXT + 4] = GETB(CRT_I, 0x2a);
    regs[EXT + 5] = GETB(0x3ce, 0x0b);
    regs[EXT + 6] = tmp;
    inb(0x3c8);
    inb(0x3c6);
    inb(0x3c6);
    inb(0x3c6);
    inb(0x3c6);
    regs[EXT + 7] = inb(0x3c6);
    return 9;			/* 9 additional registers */
}

/* ali_setregs( unsigned char regs[], int mode )        

 * Set extended registers for specified graphics mode
 */

static void ali_setregs(const unsigned char regs[], int mode)
{
    int tmp;
    ali_unlock();
    tmp = GETB(0x3ce, 0x0f);
    outb(0x3cf, tmp | 4);
    outb(0x3d6, 0);
    outb(0x3d7, 0);
    SETB(CRT_I, 0x19, regs[EXT + 1]);
    SETB(CRT_I, 0x1a, regs[EXT + 2]);
    SETB(0x3ce, 0x0b, regs[EXT + 5]);
    SETB(0x3ce, 0x0c, regs[EXT + 6]);
    SETB(CRT_I, 0x28, regs[EXT + 3]);
    SETB(CRT_I, 0x2a, regs[EXT + 4]);
    inb(0x3c8);
    inb(0x3c6);
    inb(0x3c6);
    inb(0x3c6);
    inb(0x3c6);
    outb(0x3c6, regs[EXT + 7]);
    outb(0x3d6, regs[EXT + 8]);
    outb(0x3d7, regs[EXT + 9]);
}

/* ali_modeavailable( int mode )                        Check if mode supported

 * Verify that desired graphics mode can be displayed by chip/memory combo
 * Returns SVGADRV flag if SVGA, vga_chipsetfunctions if VGA, 0 otherwise
 */

static int ali_modeavailable(int mode)
{
    const unsigned char *regs;
    struct info *info;

    regs = LOOKUPMODE(ali_modes, mode);
    if (regs == NULL || mode == GPLANE16) {
	return vga_driverspecs.modeavailable(mode);
    }
    if (regs == DISABLE_MODE || mode <= TEXT || mode > GLASTMODE) {
	return 0;
    }
    info = &__svgalib_infotable[mode];
    if (ali_memory * 1024 < info->ydim * info->xbytes) {
	return 0;
    }
    return SVGADRV;
}



/* ali_setmode( int mode, int prv_mode )                Set graphics mode

 * Attempts to set a graphics mode.
 * Returns 0 if successful, 1 if unsuccessful
 * 
 * Calls vga_chipsetfunctions if VGA mode)
 */

static int ali_setmode(int mode, int prv_mode)
{
    const unsigned char *rp;
    unsigned char regs[sizeof(g640x480x256_regs)];

    rp = LOOKUPMODE(ali_modes, mode);
    if (rp == NULL || mode == GPLANE16)
	return (int) (vga_driverspecs.setmode(mode, prv_mode));
    if (!ali_modeavailable(mode))
	return 1;		/* mode not available */

    /* Update the register information */
    memcpy(regs, rp, sizeof(regs));

    /*  Number of memory chips */
    if (__svgalib_infotable[mode].colors == 16) {
	/* switch from 256 to 16 color mode (from XFree86) */
	regs[SEQ + 4] &= 0xf7;	/* Switch off chain 4 mode */
    }
    __vga_setregs(regs);
    ali_setregs(regs, mode);
    return 0;
}




/* ali_unlock()                                         Unlock ALI registers

 * Enable register changes
 * 
 */

static void ali_unlock(void)
{
    int temp;
    temp = GETB(CRT_I, 0x1a);
    outb(CRT_D, temp | 0x10);
    /* Unprotect CRTC[0-7] */
    temp = GETB(CRT_I, 0x11);
    outb(CRT_D, temp & 0x7f);

}

/* ali_lock()                                           Lock Ali registers

 * Prevents registers from accidental change
 * 
 */

static void ali_lock(void)
{
    int tmp;
    /* Protect CRTC[0-7] */
    tmp = GETB(CRT_I, 0x11);
    outb(CRT_D, tmp | 0x80);
    tmp = GETB(CRT_I, 0x1a);
    outb(CRT_D, tmp & 0xef);
}

static int cwg(void)
{
    int t, t1, t2, t3;

    t = inb(0x3ce);
    t1 = GETB(0x3ce, 0x0b);
    t2 = t1 & 0x30;
    outb(0x3cf, t2 ^ 0x30);
    t3 = ((inb(0x3cf) & 0x30) ^ 0x30);
    outb(0x3cf, t1);
    outb(0x3ce, t);
    return (t2 == t3);
}

/* ali_test()                                           Probe for Ali card

 * Checks for Ali segment register, then chip type and memory size.
 * 
 * Returns 1 for Ali, 0 otherwise.
 */

static int ali_test(void)
{
    int tmp, ov;

    ali_unlock();

    tmp = inb(CRT_I);
    ov = GETB(CRT_I, 0x1a);
    outb(CRT_D, ov & 0xef);
    if (cwg()) {
	ali_lock();
	return 0;
    }
    outb(CRT_D, ov | 0x10);
    if (!cwg()) {
	ali_lock();
	return 0;
    }
    return ali_init(0, 0, 0);
}

/* ali_setpage( int page )                              Set read/write pages

 * Sets both read and write extended segments (64k bank number)
 */

static void ali_setpage(int page)
{
    outw(0x3d6, (last_page = page) | (page << 8));
}

/* ali_setrdpage( int page )                            Set read page

 * Sets read extended segment (64k bank number)
 */

static void ali_setrdpage(int page)
{
    last_page = page;
    outb(0x3d6, page);
}

/* ali_setwrpage( int page )                            Set write page

 * Sets write extended segment (64k bank number)
 */

static void ali_setwrpage(int page)
{
    last_page = page;
    outb(0x3d7, page);
}

/* ali_setdisplaystart( int address )                   Set display address

 * Sets display start address.
 */

static void ali_setdisplaystart(int address)
{
    address >>= 3;
    outw(CRT_I, 0x0c | (address & 0x00ff00));
    outw(CRT_I, 0x0d | ((address & 0x00ff) << 8));
    outw(CRT_I, 0x20 | ((address & 0x070000) >> 8));
}

/* ali_setlogicalwidth( int width )                     Set scanline length

 * Set logical scanline length (usually multiple of 8)
 * Multiples of 8 to 2040
 */

static void ali_setlogicalwidth(int width)
{
    outw(0x3d4, 0x13 + (width >> 3) * 256);	/* lw3-lw11 */
}

/* ali_init ( int force, int par1, int par2)            Initialize chipset

 * Detects ALI chipset type and installed video memory.
 * Returns 1 if chipset is supported, 0 otherwise
 */

static int ali_init(int force, int par1, int par2)
{
    if (force) {
	ali_memory = par2;
    } else {
	ali_memory = ali_memorydetect();
    }
    if (__svgalib_driver_report) {
	printf("Using ALI driver (ALI2301, %dK).\n", ali_memory);
    }
    driverspecs = &ali_driverspecs;
    return 1;
}

/* ali_memorydetect()                           Report installed video RAM

 * Returns size (in Kb) of installed video memory
 * Defined values are 256, 512, 1024, and 2048
 */

static int ali_memorydetect(void)
{
    int tmp;

    tmp = GETB(CRT_I, 0x1e);

    switch (tmp & 3) {
    case 0:
	return 256;
    case 1:
	return 512;
    case 2:
	return 1024;
    case 3:
	return 2048;
    default:
	printf("ALI driver: More than 2MB installed. Using 2MB.\n");
	return 2048;
    }
}

/* SETB (port,index,value)                      Set extended register

 * Puts a specified value in a specified extended register.
 * Splitting this commonly used instruction sequence into its own subroutine
 * saves about 100 bytes of code overall ...
 */

static void SETB(int port, int idx, int value)
{
    if (idx != -1)
	outb(port, idx);
    outb(port + (idx != -1), value);
}

/* GETB (port,index)                            Returns ext. register

 * Returns value from specified extended register.
 * As with SETB, this tightens the code considerably.
 */

static int GETB(int port, int idx)
{
    if (idx != -1)
	outb(port, idx);
    return inb(port + (idx != -1));
}

/* Function table (exported) */

DriverSpecs ali_driverspecs =
{
    ali_saveregs,
    ali_setregs,
    ali_unlock,
    ali_lock,
    ali_test,
    ali_init,
    ali_setpage,
    ali_setrdpage,
    ali_setwrpage,
    ali_setmode,
    ali_modeavailable,
    ali_setdisplaystart,
    ali_setlogicalwidth,
    ali_getmodeinfo,
    0,				/* bitblt */
    0,				/* imageblt */
    0,				/* fillblt */
    0,				/* hlinelistblt */
    0,				/* bltwait */
    0,				/* extset */
    0,
    0,				/* linear */
    NULL,			/* accelspecs */
};
