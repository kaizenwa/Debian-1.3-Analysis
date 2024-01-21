/*
 * s3dacs.c:
 * 
 * RAMDAC definitions for the S3-SDAC (86C716), S3-GENDAC, and Trio64.
 *
 * These contain S3-specific code.
 */

#include <stdio.h>
#include "libvga.h"

#include "timing.h"
#include "vgaregs.h"
#include "driver.h"		/* for __svgalib_driver_report */
#include "ramdac.h"

/* SDAC/GENDAC registers */
#if defined(INCLUDE_S3_SDAC_DAC) || defined(INCLUDE_S3_GENDAC_DAC)
#define SDAC_COMMAND		0	/* Register offsets into state. */
#define GENDAC_COMMAND		0
#define SDAC_PLL_WRITEINDEX	1
#define SDAC_PLL_READINDEX	2
#define SDAC_PLL_M		3	/* f2 programmed clock */
#define SDAC_PLL_N1_N2		4
#define SDAC_PLL_CONTROL	5

#define SDAC_STATESIZE 6	/* 6 registers. */
#define GENDAC_STATESIZE 6
#endif

#if defined(INCLUDE_S3_SDAC_DAC_TEST) || defined(INCLUDE_S3_GENDAC_DAC_TEST)
static int GENDAC_SDAC_probe(void)
{
/* Taken from XFree86, accel/s3.c. */
/* Return 1 if GENDAC found, 2 if SDAC, 0 otherwise. */
    /* probe for S3 GENDAC or SDAC */
    /*
     * S3 GENDAC and SDAC have two fixed read only PLL clocks
     *     CLK0 f0: 25.255MHz   M-byte 0x28  N-byte 0x61
     *     CLK0 f1: 28.311MHz   M-byte 0x3d  N-byte 0x62
     * which can be used to detect GENDAC and SDAC since there is no chip-id
     * for the GENDAC.
     *
     * NOTE: for the GENDAC on a MIRO 10SD (805+GENDAC) reading PLL values
     * for CLK0 f0 and f1 always returns 0x7f (but is documented "read only")
     */

    unsigned char saveCR55, savelut[6];
    int i;
    long clock01, clock23;

    saveCR55 = inCR(0x55);
    outbCR(0x55, saveCR55 & ~1);

    outb(0x3c7, 0);
    for (i = 0; i < 2 * 3; i++)	/* save first two LUT entries */
	savelut[i] = inb(0x3c9);
    outb(0x3c8, 0);
    for (i = 0; i < 2 * 3; i++)	/* set first two LUT entries to zero */
	outb(0x3c9, 0);

    outbCR(0x55, saveCR55 | 1);

    outb(0x3c7, 0);
    for (i = clock01 = 0; i < 4; i++)
	clock01 = (clock01 << 8) | (inb(0x3c9) & 0xff);
    for (i = clock23 = 0; i < 4; i++)
	clock23 = (clock23 << 8) | (inb(0x3c9) & 0xff);

    outbCR(0x55, saveCR55 & ~1);

    outb(0x3c8, 0);
    for (i = 0; i < 2 * 3; i++)	/* restore first two LUT entries */
	outb(0x3c9, savelut[i]);

    outbCR(0x55, saveCR55);

    if (clock01 == 0x28613d62 ||
	(clock01 == 0x7f7f7f7f && clock23 != 0x7f7f7f7f)) {

	inb(0x3c8);		/* dactopel */

	inb(0x3c6);
	inb(0x3c6);
	inb(0x3c6);

	/* the forth read will show the SDAC chip ID and revision */
	if (((i = inb(0x3c6)) & 0xf0) == 0x70) {
	    return 2;		/* SDAC found. */
	} else {
	    return 1;		/* GENDAC found. */
	}
	inb(0x3c8);		/* dactopel */
    }
    return 0;
}
#endif

#if defined(INCLUDE_S3_SDAC_DAC) || defined(INCLUDE_S3_GENDAC_DAC)
static void GENDAC_SDAC_init(void)
{
    unsigned char val;
    int m, n, n1, n2, MCLK;
    val = inCR(0x55);
    outbCR(0x55, val | 0x01);

    outb(0x3C7, 10);		/* Read MCLK. */
    m = inb(0x3C9);
    n = inb(0x3C9);

    outbCR(0x55, val);		/* Restore CR55. */

    m &= 0x7f;
    n1 = n & 0x1f;
    n2 = (n >> 5) & 0x03;
    /* Calculate MCLK in kHz. */
    MCLK = 14318 * (m + 2) / (n1 + 2) / (1 << n2);
    if (__svgalib_driver_report)
	printf("svgalib: S3-GENDAC/SDAC: MCLK = %d.%03d MHz\n",
	       MCLK / 1000, MCLK % 1000);
}
#endif


#if defined(INCLUDE_S3_SDAC_DAC) || defined(INCLUDE_S3_GENDAC_DAC) || defined(INCLUDE_S3_TRIO64_DAC)
/* From XFree86, common_hw/S3gendac.c and S3gendac.h */
/*
 * Progaming of the S3 gendac programable clocks, from the S3 Gendac
 * programing documentation by S3 Inc. 
 * Jon Tombs <jon@esix2.us.es>
 */

#include <math.h>

#define GENDAC_INDEX	     0x3C8
#define GENDAC_DATA	     0x3C9
#define BASE_FREQ	     14.31818	/* MHz */


static void S3gendacFindClock(int freq, int min_n2, int freq_min, int freq_max,
		     int *best_m_out, int *best_n1_out, int *best_n2_out)
{
    double ffreq, ffreq_min, ffreq_max;
    double div, diff, best_diff;
    unsigned int m;
    unsigned char n, n1, n2;
    unsigned char best_n1 = 16 + 2, best_n2 = 2, best_m = 125 + 2;

    ffreq = freq / 1000.0 / BASE_FREQ;
    ffreq_min = freq_min / 1000.0 / BASE_FREQ;
    ffreq_max = freq_max / 1000.0 / BASE_FREQ;

    if (ffreq < ffreq_min / 8) {
	printf("invalid frequency %1.3f MHz  [freq >= %1.3f MHz]\n",
	       ffreq * BASE_FREQ, ffreq_min * BASE_FREQ / 8);
	ffreq = ffreq_min / 8;
    }
    if (ffreq > ffreq_max / (1 << min_n2)) {
	printf("invalid frequency %1.3f MHz  [freq <= %1.3f MHz]\n",
	       ffreq * BASE_FREQ, ffreq_max * BASE_FREQ / (1 << min_n2));
	ffreq = ffreq_max / (1 << min_n2);
    }
    /* work out suitable timings */

    best_diff = ffreq;

    for (n2 = min_n2; n2 <= 3; n2++) {
	for (n1 = 1 + 2; n1 <= 31 + 2; n1++) {
	    m = (int) (ffreq * n1 * (1 << n2) + 0.5);
	    if (m < 1 + 2 || m > 127 + 2)
		continue;
	    div = (double) (m) / (double) (n1);
	    if ((div >= ffreq_min) &&
		(div <= ffreq_max)) {
		diff = ffreq - div / (1 << n2);
		if (diff < 0.0)
		    diff = -diff;
		if (diff < best_diff) {
		    best_diff = diff;
		    best_m = m;
		    best_n1 = n1;
		    best_n2 = n2;
		}
	    }
	}
    }

#if 0
    ErrorF("clk %d, setting to %1.6f MHz (m %d, n1 %d, n2 %d)\n", clk,
    ((double) (best_m) / (double) (best_n1) / (1 << best_n2)) * BASE_FREQ
	   ,best_m - 2, best_n1 - 2, best_n2
	);
#endif

    n = (best_n1 - 2) | (best_n2 << 5);
    m = best_m - 2;

    *best_m_out = best_m;
    *best_n1_out = best_n1;
    *best_n2_out = best_n2;
}

#endif

#if defined(INCLUDE_S3_SDAC_DAC) || defined(INCLUDE_S3_GENDAC_DAC)
static int SDAC_GENDAC_match_programmable_clock(int desiredclock)
{
    int min_m, min_n1, n2;
    S3gendacFindClock(desiredclock, 0, 100000, 250000,
		      &min_m, &min_n1, &n2);
/*
   For ICS5342, min_n2 parameter should be one.
   For Trio, min_n2 = 0, freq_min = 130000 and freq_max = 270000
 */

    return ((float) (min_m) / (float) (min_n1) / (1 << n2))
	* BASE_FREQ * 1000;
}

#endif

#if 0				/* Retain for reference. */
static void setdacpll(reg, data1, data2)
int reg;
unsigned char data1;
unsigned char data2;
{
    unsigned char tmp, tmp1;
    int vgaCRIndex = vgaIOBase + 4;
    int vgaCRReg = vgaIOBase + 5;

    /* set RS2 via CR55, yuck */
    tmp = inCR(0x55) & 0xFC;
    outCR(tmp | 0x01);
    tmp1 = inb(GENDAC_INDEX);

    outb(GENDAC_INDEX, reg);
    outb(GENDAC_DATA, data1);
    outb(GENDAC_DATA, data2);

    /* Now clean up our mess */
    outb(GENDAC_INDEX, tmp1);
    outbCR(0x55, tmp);
}

#endif

#if defined(INCLUDE_S3_SDAC_DAC) || defined(INCLUDE_S3_GENDAC_DAC)
static void SDAC_GENDAC_initialize_clock_state(unsigned char *regs, int freq)
{
    int min_m, min_n1, n2;
    int n, m;
    S3gendacFindClock(freq, 0, 100000, 250000, &min_m, &min_n1, &n2);
    n = (min_n1 - 2) | (n2 << 5);
    m = min_m - 2;
    regs[SDAC_PLL_M] = m;
    regs[SDAC_PLL_N1_N2] = n;
    if (__svgalib_driver_report)
	printf("Initializing DAC values; 0x%02X, 0x%02X.\n", m, n);
}

static void GENDAC_SDAC_savestate(unsigned char *regs)
{
    unsigned char tmp;
    tmp = inCR(0x55);
    outbCR(0x55, tmp | 1);

    regs[SDAC_COMMAND] = inb(0x3c6);
    regs[SDAC_PLL_WRITEINDEX] = inb(0x3c8);	/* PLL write index */
    regs[SDAC_PLL_READINDEX] = inb(0x3c7);	/* PLL read index */
    outb(0x3c7, 2);		/* index to f2 reg */
    regs[SDAC_PLL_M] = inb(0x3c9);	/* f2 PLL M divider */
    regs[SDAC_PLL_N1_N2] = inb(0x3c9);	/* f2 PLL N1/N2 divider */
    outb(0x3c7, 0x0e);		/* index to PLL control */
    regs[SDAC_PLL_CONTROL] = inb(0x3c9);	/* PLL control */

    outbCR(0x55, tmp & ~1);
}

static void GENDAC_SDAC_restorestate(const unsigned char *regs)
{
    unsigned char tmp;

    /* set RS2 via CR55, yuck */
    tmp = inCR(0x55) & 0xFC;
    outbCR(0x55, tmp | 0x01);

#ifdef DEBUG
    do {
	int m, n1, n2, clk;

	m = regs[SDAC_PLL_M] & 0x7f;
	n1 = regs[SDAC_PLL_N1_N2] & 0x1f;
	n2 = (regs[SDAC_PLL_N1_N2] & 0x60) >> 5;

	clk = 14318 * (m + 2) / (n1 + 2) / (1 << n2);
	printf("SDAC.restorestate, setting clock 0x%02X 0x%02X (%d.%3dMHz)\n",
	       regs[SDAC_PLL_M],
	       regs[SDAC_PLL_N1_N2], clk / 1000, clk % 1000);
    } while (0);
#endif

    outb(0x3c6, regs[SDAC_COMMAND]);
    outb(0x3c8, 2);		/* index to f2 reg */
    outb(0x3c9, regs[SDAC_PLL_M]);	/* f2 PLL M divider */
    outb(0x3c9, regs[SDAC_PLL_N1_N2]);	/* f2 PLL N1/N2 divider */
    outb(0x3c8, 0x0e);		/* index to PLL control */
    outb(0x3c9, regs[SDAC_PLL_CONTROL]);	/* PLL control */
    outb(0x3c8, regs[SDAC_PLL_WRITEINDEX]);	/* PLL write index */
    outb(0x3c7, regs[SDAC_PLL_READINDEX]);	/* PLL read index */

    outbCR(0x55, tmp);
}

#endif				/* defined(INCLUDE_S3_SDAC_DAC) || defined(INCLUDE_S3_GENDAC_DAC) */

/*
 * SDAC: 16-bit DAC, 110 MHz raw clock limit.
 *
 * The 135 MHz version supports pixel multiplexing in 8bpp modes with a
 * halved raw clock. (SL: at least mine doesn't.)
 */

#ifdef INCLUDE_S3_SDAC_DAC_TEST
static int SDAC_probe(void)
{
    return GENDAC_SDAC_probe() == 2;
}
#else
#define SDAC_probe 0
#endif

#ifdef INCLUDE_S3_SDAC_DAC
static int SDAC_map_clock(int bpp, int pixelclock)
{
    switch (bpp) {
    case 4:
    case 8:
#ifdef SDAC_8BPP_PIXMUX		/* SL: AFAIK it doesn't work */
	if (pixelclock >= 67500)
	    /* Use pixel multiplexing. */
	    return pixelclock / 2;
#endif
	break;
    case 24:
	return pixelclock * 3 / 2;
    case 32:
	return pixelclock * 2;
    }
    return pixelclock;
}

static int SDAC_map_horizontal_crtc(int bpp, int pixelclock, int htiming)
{
    switch (bpp) {
    case 16:
	return htiming * 2;
    case 24:
	return htiming * 3;
    case 32:
	return htiming * 4;
    }
    return htiming;
}

static void SDAC_initializestate(unsigned char *regs, int bpp, int colormode,
				 int pixelclock)
{
    int pixmux;			/* SDAC command register. */
    pixmux = 0;
    switch (colormode) {
    case CLUT8_6:
#ifdef SDAC_8BPP_PIXMUX
	if (pixelclock >= 67500)
	    pixmux = 0x10;
#endif
	break;
    case RGB16_555:
	pixmux = 0x30;
	break;
    case RGB16_565:
	pixmux = 0x50;
	break;
    case RGB24_888_B:
	/* Use 0x40 for 3 VCLK/pixel.  Change SDAC_map_clock and CR67 as well. */
	pixmux = 0x90;
	break;
    case RGB32_888_B:
	pixmux = 0x70;
	break;
    }
    regs[SDAC_COMMAND] = pixmux;
    SDAC_GENDAC_initialize_clock_state(regs,
				       SDAC_map_clock(bpp, pixelclock));
}

static void SDAC_qualify_cardspecs(CardSpecs * cardspecs, int dacspeed)
{
    dacspeed = _setDacSpeed(dacspeed, 110000);	/* most can do 135MHz. */
    cardspecs->maxPixelClock4bpp = dacspeed;
    cardspecs->maxPixelClock8bpp = dacspeed;
    cardspecs->maxPixelClock16bpp = dacspeed;
    cardspecs->maxPixelClock24bpp = dacspeed * 2 / 3;
    cardspecs->maxPixelClock32bpp = dacspeed / 2;
    cardspecs->mapClock = SDAC_map_clock;
    cardspecs->matchProgrammableClock = SDAC_GENDAC_match_programmable_clock;
    cardspecs->mapHorizontalCrtc = SDAC_map_horizontal_crtc;
    cardspecs->flags |= CLOCK_PROGRAMMABLE;
}

DacMethods S3_SDAC_methods =
{
    S3_SDAC,
    "S3-SDAC (86C716)",
    DAC_HAS_PROGRAMMABLE_CLOCKS,
    SDAC_probe,
    GENDAC_SDAC_init,
    SDAC_qualify_cardspecs,
    GENDAC_SDAC_savestate,
    GENDAC_SDAC_restorestate,
    SDAC_initializestate,
    SDAC_STATESIZE
};
#endif


/* S3-GENDAC, 8-bit DAC. */

#ifdef INCLUDE_S3_GENDAC_DAC_TEST
static int GENDAC_probe(void)
{
    return GENDAC_SDAC_probe() == 1;
}
#else
#define GENDAC_probe 0
#endif

#ifdef INCLUDE_S3_GENDAC_DAC
static int GENDAC_map_clock(int bpp, int pixelclock)
{
    if (bpp == 16)
	return pixelclock * 2;
    if (bpp == 24)
	return pixelclock * 3;
    if (bpp == 32)
	return pixelclock * 4;
    return pixelclock;
}

static int GENDAC_map_horizontal_crtc(int bpp, int pixelclock, int htiming)
{
    /* XXXX Not sure. */
    if (bpp == 24)
	return htiming * 3;
    if (bpp == 16)
	return htiming * 2;
    return htiming;
}

static void GENDAC_initializestate(unsigned char *regs, int bpp, int colormode,
				   int pixelclock)
{
    int daccomm;		/* DAC command register. */
    daccomm = 0;
    if (colormode == RGB16_555)
	daccomm = 0x20;
    else if (colormode == RGB16_565)
	daccomm = 0x60;
    else if (colormode == RGB24_888_B)
	daccomm = 0x40;
    regs[GENDAC_COMMAND] = daccomm;
    SDAC_GENDAC_initialize_clock_state(regs,
				       GENDAC_map_clock(bpp, pixelclock));
}

static void GENDAC_qualify_cardspecs(CardSpecs * cardspecs, int dacspeed)
{
    dacspeed = _setDacSpeed(dacspeed, 110000);
    cardspecs->maxPixelClock4bpp = dacspeed;
    cardspecs->maxPixelClock8bpp = dacspeed;
    cardspecs->maxPixelClock16bpp = dacspeed / 2;
    cardspecs->maxPixelClock24bpp = dacspeed / 3;
    cardspecs->maxPixelClock32bpp = 0;
    cardspecs->mapClock = GENDAC_map_clock;
    cardspecs->matchProgrammableClock = SDAC_GENDAC_match_programmable_clock;
    cardspecs->mapHorizontalCrtc = GENDAC_map_horizontal_crtc;
    cardspecs->flags |= CLOCK_PROGRAMMABLE;
}

DacMethods S3_GENDAC_methods =
{
    S3_GENDAC,
    "S3-GENDAC (86C708)",
    DAC_HAS_PROGRAMMABLE_CLOCKS,
    GENDAC_probe,
    GENDAC_SDAC_init,
    GENDAC_qualify_cardspecs,
    GENDAC_SDAC_savestate,
    GENDAC_SDAC_restorestate,
    GENDAC_initializestate,
    GENDAC_STATESIZE
};
#endif


#ifdef INCLUDE_S3_TRIO64_DAC
/* S3-Trio64, 16-bit integrated DAC. */

#define	TRIO64_SR15		0
#define TRIO64_SR18		1
#define TRIO64_PLL_N1_N2	2
#define TRIO64_PLL_M		3
#define TRIO64_CR67		4
#define TRIO64_STATESIZE	5

/* Note: s3.c also defines CR67, but doesn't use it for the Trio64. */

static void Trio64_init(void)
{
    unsigned char sr8;
    int m, n, n1, n2, mclk;

    outb(0x3c4, 0x08);
    sr8 = inb(0x3c5);
    outb(0x3c5, 0x06);

    outb(0x3c4, 0x11);
    m = inb(0x3c5);
    outb(0x3c4, 0x10);
    n = inb(0x3c5);

    outb(0x3c4, 0x08);
    outb(0x3c5, sr8);

    m &= 0x7f;
    n1 = n & 0x1f;
    n2 = (n >> 5) & 0x03;
    /* Calculate MCLK in kHz. */
    mclk = ((1431818 * (m + 2)) / (n1 + 2) / (1 << n2) + 50) / 100;
    if (__svgalib_driver_report)
	printf("svgalib: Trio64: MCLK = %d.%03d MHz\n",
	       mclk / 1000, mclk % 1000);
}


static int Trio64_map_clock(int bpp, int pixelclock)
{
    if (bpp == 8 && pixelclock >= 80000)
	return pixelclock / 2;
    if (bpp == 32)
	return pixelclock * 2;
    return pixelclock;
}

static int Trio64_map_horizontal_crtc(int bpp, int pixelclock, int htiming)
{
    if (bpp == 16)
	return htiming * 2;
    /* Normal mapping for 8bpp and 32bpp. */
    return htiming;
}

static void Trio64_initialize_clock_state(unsigned char *regs, int freq)
{
    int min_m, min_n1, n2;
    int n, m;
    S3gendacFindClock(freq, 0, 130000, 270000, &min_m, &min_n1, &n2);
    n = (min_n1 - 2) | (n2 << 5);
    m = min_m - 2;
    regs[TRIO64_PLL_M] = m;
    regs[TRIO64_PLL_N1_N2] = n;
}

static int Trio64_match_programmable_clock(int desiredclock)
{
    int min_m, min_n1, n2;
    S3gendacFindClock(desiredclock, 0, 130000, 270000,
		      &min_m, &min_n1, &n2);
    return ((float) (min_m) / (float) (min_n1) / (1 << n2))
	* BASE_FREQ * 1000;
}
static void Trio64_initializestate(unsigned char *regs, int bpp, int colormode,
				   int pixelclock)
{
    int pixmux, invert_vclk;
    regs[TRIO64_SR15] &= ~0x10;
    regs[TRIO64_SR18] &= ~0x80;
    pixmux = 0;
    invert_vclk = 0;
    if (colormode == CLUT8_6 && pixelclock >= 80000) {
	pixmux = 0x10;
	invert_vclk = 2;
	regs[TRIO64_SR15] |= 0x10;
	regs[TRIO64_SR18] |= 0x80;
    } else if (bpp == 16) {
	/* moderegs[S3_CR33] |= 0x08; *//* done in s3.c. */
	if (colormode == RGB16_555)
	    pixmux = 0x30;
	else
	    pixmux = 0x50;
    }
    if (colormode == RGB32_888_B) {
	pixmux = 0xD0;		/* 32-bit color, 2 VCLKs/pixel. */
    }
    regs[TRIO64_CR67] = pixmux | invert_vclk;

    Trio64_initialize_clock_state(regs, pixelclock);
}

static void Trio64_savestate(unsigned char *regs)
{
    unsigned char sr8;
    outb(0x3C4, 0x08);
    sr8 = inb(0x3C5);
    outb(0x3C5, 0x06);		/* Unlock. */

    regs[TRIO64_SR15] = inSR(0x15);
    regs[TRIO64_SR18] = inSR(0x18);
    regs[TRIO64_PLL_N1_N2] = inSR(0x12);
    regs[TRIO64_PLL_M] = inSR(0x13);
    regs[TRIO64_CR67] = inCR(0x67);

    outSR(0x08, sr8);
}

static void Trio64_restorestate(const unsigned char *regs)
{
    unsigned char sr8;

    outb(0x3C4, 0x08);
    sr8 = inb(0x3C5);
    outb(0x3C5, 0x06);		/* Unlock. */

    outCR(0x67, regs[TRIO64_CR67]);

    outSR(0x15, regs[TRIO64_SR15]);
    outSR(0x18, regs[TRIO64_SR18]);

    /* Clock. */
    outSR(0x12, regs[TRIO64_PLL_N1_N2]);
    outSR(0x13, regs[TRIO64_PLL_M]);

#if 0
    /*
     * XFree86 XF86_S3 (common_hw/gendac.c) has this, but it looks
     * incorrect, it should flip the bit by writing to 0x3c5, not
     * 0x3c4.
     */
    outb(0x3c4, 0x15);
    tmp = inb(0x3c5);
    outb(0x3c4, tmp & ~0x20);
    outb(0x3c4, tmp | 0x20);
    outb(0x3c4, tmp & ~0x20);
#endif

    outSR(0x08, sr8);
}


static void Trio64_qualify_cardspecs(CardSpecs * cardspecs, int dacspeed)
{
    if (dacspeed) {
	if (__svgalib_driver_report)
	    printf("svgalib: DAC speed setting ignored for Trio64.\n");
    }
    cardspecs->maxPixelClock4bpp = 80000;
    cardspecs->maxPixelClock8bpp = 135000;
    cardspecs->maxPixelClock16bpp = 80000;
    cardspecs->maxPixelClock24bpp = 0;	/* How to program? */
    cardspecs->maxPixelClock32bpp = 50000;
    cardspecs->mapClock = Trio64_map_clock;
    cardspecs->matchProgrammableClock = Trio64_match_programmable_clock;
    cardspecs->mapHorizontalCrtc = Trio64_map_horizontal_crtc;
    cardspecs->flags |= CLOCK_PROGRAMMABLE;
}

DacMethods Trio64_methods =
{
    TRIO64,
    "S3-Trio64 internal DAC",
    DAC_HAS_PROGRAMMABLE_CLOCKS,
    NULL,			/* probe */
    Trio64_init,
    Trio64_qualify_cardspecs,
    Trio64_savestate,
    Trio64_restorestate,
    Trio64_initializestate,
    TRIO64_STATESIZE
};
#endif
