/* VGAlib version 1.2 - (c) 1993 Tommy Frandsen                    */
/*                                                                 */
/* This library is free software; you can redistribute it and/or   */
/* modify it without any restrictions. This library is distributed */
/* in the hope that it will be useful, but without any warranty.   */

/* Multi-chipset support Copyright (C) 1993 Harm Hanemaayer */
/* Modified by Hartmut Schirmer */

/* TVGA 8900c code taken from tvgalib by Toomas Losin */


#include <stdio.h>
#include "vga.h"
#include "libvga.h"
#include "driver.h"

#include "tvga8900.regs"

/* static int tvga_chiptype; */
static int tvga8900_memory;	/* amount of video memory in K */
static int tvga8900_nonint;	/* nonzero if non-interlaced jumper set */

static int tvga8900_init(int, int, int);
static int tvga8900_interlaced(int mode);

static int reg_0c = 0xad;	/* value for 256k cards */


/* Mode table */
static ModeTable tvga_modes_1024[] =
{				/* 1Mb, non-interlace jumper set */
/* *INDENT-OFF* */
    OneModeEntry(640x480x256),
    OneModeEntry(800x600x256),
    OneModeEntry(1024x768x256),
    END_OF_MODE_TABLE
/* *INDENT-ON* */
};

#define INTERL(res,i) { G##res, g##res##i##_regs }

static ModeTable tvga_modes_1024i[] =
{				/* 1Mb, jumper set to interlaced */
/* *INDENT-OFF* */
    INTERL(640x480x256, i),
    INTERL(800x600x256, i),
    INTERL(1024x768x256, i),
    END_OF_MODE_TABLE
/* *INDENT-ON* */
};

static ModeTable tvga_modes_512[] =
{				/* 512K */
/* *INDENT-OFF* */
    INTERL(640x480x256, i),
    INTERL(800x600x256, i1),
    END_OF_MODE_TABLE
/* *INDENT-ON* */
};

static ModeTable *tvga_modes = NULL;

static void nothing(void)
{
}

/* Fill in chipset specific mode information */

static void tvga8900_getmodeinfo(int mode, vga_modeinfo * modeinfo)
{
    if (modeinfo->bytesperpixel > 0)
	modeinfo->maxpixels = tvga8900_memory * 1024 /
	    modeinfo->bytesperpixel;
    else
	modeinfo->maxpixels = tvga8900_memory * 1024;
    modeinfo->maxlogicalwidth = 2040;
    modeinfo->startaddressrange = 0xfffff;
    if (mode == G320x200x256) {
	/* Special case: bank boundary may not fall within display. */
	modeinfo->startaddressrange = 0xf0000;
	/* Hack: disable page flipping capability for the moment. */
	modeinfo->startaddressrange = 0xffff;
	modeinfo->maxpixels = 65536;
    }
    modeinfo->haveblit = 0;

    if (tvga8900_interlaced(mode))
	modeinfo->flags |= IS_INTERLACED;
    modeinfo->flags &= ~HAVE_RWPAGE;
}


/* select the correct register table */
static void setup_registers(void)
{
    if (tvga_modes == NULL) {
	if (tvga8900_memory < 1024)
	    tvga_modes = tvga_modes_512;
	else if (tvga8900_nonint)
	    tvga_modes = tvga_modes_1024;
	else
	    tvga_modes = tvga_modes_1024i;
    }
}


/* Read and store chipset-specific registers */

static int tvga8900_saveregs(unsigned char regs[])
{
    int i;

    /* save extended CRT registers */
    for (i = 0; i < 7; i++) {
	port_out(0x18 + i, CRT_I);
	regs[EXT + i] = port_in(CRT_D);
    }

    /* now do the sequencer mode regs */
    port_out(0x0b, SEQ_I);	/* force old mode regs */
    port_out(port_in(SEQ_D), SEQ_D);	/* by writing */

    /* outw(0x3C4, 0x820E); *//* unlock conf. reg */
    /* port_out(0x0c, SEQ_I); *//* save conf. reg */
    /* regs[EXT + 11] = port_in(SEQ_D); */

    port_out(0x0d, SEQ_I);	/* old reg 13 */
    regs[EXT + 7] = port_in(SEQ_D);
    port_out(0x0e, SEQ_I);	/* old reg 14 */
    regs[EXT + 8] = port_in(SEQ_D);

    port_out(0x0b, SEQ_I);	/* now use new regs */
    port_in(SEQ_D);
    port_out(0x0d, SEQ_I);	/* new reg 13 */
    regs[EXT + 9] = port_in(SEQ_D);
    port_out(0x0e, SEQ_I);	/* new reg 14 */
    regs[EXT + 10] = port_in(SEQ_D) ^ 0x02;

    /* we do the ^ 0x02 so that when the regs are restored */
    /* later we don't have a special case; see trident.doc */

    return 12;			/* tridents requires 12 additional registers */
}


/* Set chipset-specific registers */

static void tvga8900_setregs(const unsigned char regs[], int mode)
{
    int i;
    int crtc31 = 0;

    /* 7 extended CRT registers */
    /* 4 extended Sequencer registers (old and new) */
    /* CRTC reg 0x1f is apparently dependent */
    /* on the amount of memory installed. */

    switch (tvga8900_memory >> 8) {
    case 1:
	crtc31 = 0x94;
	reg_0c = 0xad;
	break;			/* 256K */
    case 2:
    case 3:
	crtc31 = 0x98;
	reg_0c = 0xcd;
	break;			/* 512/768K */
    case 4:			/* 1024K */
	crtc31 = 0x18;
	reg_0c = 0xcd;
	if (mode == G1024x768x256) {
	    reg_0c = 0xed;
	    crtc31 = 0x98;
	} else if (mode == G640x480x256 || mode == G800x600x256)
	    reg_0c = 0xed;
	break;
    }

    if (mode == TEXT) {
	reg_0c = regs[EXT + 11];
	crtc31 = regs[EXT + 12];
    }
#ifdef REG_DEBUG
    printf("Setting extended registers\n");
#endif

    /* write extended CRT registers */
    for (i = 0; i < 7; i++) {
	port_out(0x18 + i, CRT_I);
	port_out(regs[EXT + i], CRT_D);
    }

    /* update sequencer mode regs */
    port_out(0x0b, SEQ_I);	/* select old regs */
    port_out(port_in(SEQ_D), SEQ_D);
    port_out(0x0d, SEQ_I);	/* old reg 13 */
    port_out(regs[EXT + 7], SEQ_D);
    port_out(0x0e, SEQ_I);	/* old reg 14 */
#if 0
    port_out(regs[EXT + 8], SEQ_D);
#endif
    port_out(((port_in(SEQ_D) & 0x08) | (regs[EXT + 8] & 0xf7)), SEQ_D);


    port_out(0x0b, SEQ_I);
    port_in(SEQ_D);		/* select new regs */

    if (tvga8900_memory > 512) {
	port_out(0x0e, SEQ_I);	/* set bit 7 of reg 14  */
	port_out(0x80, SEQ_D);	/* to enable writing to */
	port_out(0x0c, SEQ_I);	/* reg 12               */
	port_out(reg_0c, SEQ_D);
    }
    /*      outw(0x3c4, 0x820e); *//* unlock conf. reg */
    /*      port_out(0x0c, SEQ_I); *//* reg 12 */

    port_out(0x0d, SEQ_I);	/* new reg 13 */
    port_out(regs[EXT + 9], SEQ_D);
    port_out(0x0e, SEQ_I);	/* new reg 14 */
    port_out(regs[EXT + 10], SEQ_D);

#ifdef REG_DEBUG
    printf("Now setting last two extended registers.\n");
#endif

    /* update CRTC reg 1f */
    port_out(0x1f, CRT_I);
    port_out((port_in(CRT_D) & 0x03) | crtc31, CRT_D);
}


/* Return nonzero if mode is available */

static int tvga8900_modeavailable(int mode)
{
    const unsigned char *regs;
    struct info *info;

    regs = LOOKUPMODE(tvga_modes, mode);
    if (regs == NULL || mode == GPLANE16)
	return vga_driverspecs.modeavailable(mode);
    if (regs == DISABLE_MODE || mode <= TEXT || mode > GLASTMODE)
	return 0;

    info = &__svgalib_infotable[mode];
    if (tvga8900_memory * 1024 < info->ydim * info->xbytes)
	return 0;

    return SVGADRV;
}


/* Check if mode is interlaced */

static int tvga8900_interlaced(int mode)
{
    const unsigned char *regs;

    setup_registers();
    regs = LOOKUPMODE(tvga_modes, mode);
    if (regs == NULL || regs == DISABLE_MODE)
	return 0;
    return tvga8900_nonint == 0;
}


/* Set a mode */

static int tvga8900_setmode(int mode, int prv_mode)
{
    const unsigned char *regs;

    regs = LOOKUPMODE(tvga_modes, mode);
    if (regs == NULL)
	return (int) (vga_driverspecs.setmode(mode, prv_mode));
    if (!tvga8900_modeavailable(mode))
	return 1;
    __vga_setregs(regs);
    tvga8900_setregs(regs, mode);
    return 0;
}


/* Indentify chipset; return non-zero if detected */

static int tvga8900_test(void)
{
    int origVal, newVal;
    int save0b;
    /* 
     * Check first that we have a Trident card.
     */
    outb(0x3c4, 0x0b);
    save0b = inb(0x3c5);
    outw(0x3C4, 0x000B);	/* Switch to Old Mode */
    inb(0x3C5);			/* Now to New Mode */
    outb(0x3C4, 0x0E);
    origVal = inb(0x3C5);
    outb(0x3C5, 0x00);
    newVal = inb(0x3C5) & 0x0F;
    outb(0x3C5, (origVal ^ 0x02));

    if (newVal != 2) {
	outb(0x3c5, origVal);
	outb(0x3c4, 0x0b);
	outb(0x3c5, save0b);
	return 0;
    }
    /* check version */
    outw(0x3c4, 0x000b);
    switch (inb(0x3c5)) {
    case 0x02:			/* 8800cs */
    case 0x03:			/* 8900b */
    case 0x04:			/* 8900c */
    case 0x13:
    case 0x33:			/* 8900cl */
    case 0x23:			/* 9000 */
	break;
    default:
	return 0;
    }

    tvga8900_init(0, 0, 0);
    return 1;
}


/* Bank switching function - set 64K bank number */

static void tvga8900_setpage(int page)
{
    port_out(0x0b, SEQ_I);
    port_out(port_in(SEQ_D), SEQ_D);
    port_in(SEQ_D);		/* select new mode regs */

    port_out(0x0e, SEQ_I);
    port_out(page ^ 0x02, SEQ_D);	/* select the page */
}


/* Set display start address (not for 16 color modes) */
/* Trident supports any address in video memory (up to 1Mb) */

static void tvga8900_setdisplaystart(int address)
{
    if (__svgalib_cur_mode == G320x200x256) {
	outw(0x3d4, 0x0d + (address & 0x00ff) * 256);
	outw(0x3d4, 0x0c + (address & 0xff00));
	address <<= 2;		/* Adjust address so that extended bits */
	/* are correctly set later (too allow for */
	/* multi-page flipping in 320x200). */
	goto setextendedbits;
    }
    if (tvga8900_memory == 1024) {
	outw(0x3d4, 0x0d + ((address >> 3) & 0x00ff) * 256);	/* sa2-sa9 */
	outw(0x3d4, 0x0c + ((address >> 3) & 0xff00));	/* sa10-sa17 */
    } else {
	outw(0x3d4, 0x0d + ((address >> 2) & 0x00ff) * 256);	/* sa2-sa9 */
	outw(0x3d4, 0x0c + ((address >> 2) & 0xff00));	/* sa10-sa17 */
    }
    if (__svgalib_cur_mode != G320x200x256) {
	inb(0x3da);		/* set ATC to addressing mode */
	outb(0x3c0, 0x13 + 0x20);	/* select ATC reg 0x13 */
	if (tvga8900_memory == 1024) {
	    outb(0x3c0, (inb(0x3c1) & 0xf0) | (address & 7));
	    /* write sa0-2 to bits 0-2 */
	    address >>= 1;
	} else
	    outb(0x3c0, (inb(0x3c1) & 0xf0) | ((address & 3) << 1));
	/* write sa0-1 to bits 1-2 */
    }
  setextendedbits:
    outb(0x3d4, 0x1e);
    outb(0x3d5, (inb(0x3d5) & 0x5f) | 0x80	/* set bit 7 */
	 | ((address & 0x40000) >> 13));	/* sa18: write to bit 5 */
    outb(0x3c4, 0x0b);
    outb(0x3c5, 0);		/* select 'old mode' */
    outb(0x3c4, 0x0e);
    outb(0x3c5, (inb(0x3c5) & 0xfe)
	 | ((address & 0x80000) >> 19));	/* sa19: write to bit 0 */
    outb(0x3c4, 0x0b);
    inb(0x3c5);			/* return to 'new mode' */
}


/* Set logical scanline length (usually multiple of 8) */
/* Trident supports multiples of 8 to 2040 */

static void tvga8900_setlogicalwidth(int width)
{
    outw(0x3d4, 0x13 + (width >> 3) * 256);	/* lw3-lw11 */
}


/* Function table */

DriverSpecs tvga8900_driverspecs =
{
    tvga8900_saveregs,
    tvga8900_setregs,
    nothing,			/* unlock */
    nothing,			/* lock */
    tvga8900_test,
    tvga8900_init,
    tvga8900_setpage,
    (void (*)(int)) nothing,	/* setrdpage */
    (void (*)(int)) nothing,	/* setwrpage */
    tvga8900_setmode,
    tvga8900_modeavailable,
    tvga8900_setdisplaystart,
    tvga8900_setlogicalwidth,
    tvga8900_getmodeinfo,
    0,				/* bitblt */
    0,				/* imageblt */
    0,				/* fillblt */
    0,				/* hlinelistblt */
    0,				/* bltwait */
    0,				/* extset */
    0,
    0,				/* linear */
    NULL			/* accelspecs */
};


/* Initialize chipset (called after detection) */

static int tvga8900_init(int force, int par1, int par2)
{
    if (force) {
#ifdef DEBUG
	printf("Forcing memory to %dK\n", par1);
#endif
	tvga8900_memory = par1;
	tvga8900_nonint = par2;
    } else {
	port_out(0x1f, CRT_I);
	tvga8900_memory = (port_in(CRT_D) & 0x03) * 256 + 256;

	/* Now is the card running in interlace mode? */
	port_out(0x0f, SEQ_I);
	tvga8900_nonint = port_in(SEQ_D) & 0x04;
    }

    if (__svgalib_driver_report) {
	printf("Using Trident 8900/9000 driver (%dK, %sinterlaced).\n",
	       tvga8900_memory, (tvga8900_nonint) ? "non-" : "");
    }
    driverspecs = &tvga8900_driverspecs;
    setup_registers();
    return 0;
}
