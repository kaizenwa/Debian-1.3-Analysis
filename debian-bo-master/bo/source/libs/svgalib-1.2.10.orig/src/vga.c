/* VGAlib version 1.2 - (c) 1993 Tommy Frandsen                    */
/*                                                                 */
/* This library is free software; you can redistribute it and/or   */
/* modify it without any restrictions. This library is distributed */
/* in the hope that it will be useful, but without any warranty.   */

/* Multi-chipset support Copyright (C) 1993 Harm Hanemaayer */
/* partially copyrighted (C) 1993 by Hartmut Schirmer */
/* Changes by Michael Weller. */


/* The code is a bit of a mess; also note that the drawing functions */
/* are not speed optimized (the gl functions are much faster). */

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <termios.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/mman.h>
#include <sys/kd.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/vt.h>
#include <sys/wait.h>
#include <errno.h>
#include <ctype.h>
#include "vga.h"
#include "libvga.h"
#include "driver.h"
#include "mouse/vgamouse.h"
#include "keyboard/vgakeyboard.h"

/* Delay in microseconds after a mode is set (screen is blanked during this */
/* time), allows video signals to stabilize */
#define MODESWITCHDELAY 150000

/* Define this to disable video output during mode switches, in addition to */
/* 'turning off the screen', which is always done. */
/* Doesn't look very nice on my Cirrus. */
/* #define DISABLE_VIDEO_OUTPUT */

/* #define DONT_WAIT_VC_ACTIVE */

/* Use /dev/tty instead of /dev/tty0 (the previous behaviour may have been
 * silly). */
#define USE_DEVTTY


#define SETSIG(sa, sig, fun) {\
	sa.sa_handler = fun; \
	sa.sa_flags = 0; \
	sa.sa_mask = 0; \
	sigaction(sig, &sa, NULL); \
}

/* variables used to shift between monchrome and color emulation */
int CRT_I;			/* current CRT index register address */
int CRT_D;			/* current CRT data register address */
int IS1_R;			/* current input status register address */
static int color_text;		/* true if color text emulation */


struct info infotable[] =
{
    {80, 25, 16, 160, 0},	/* VGAlib VGA modes */
    {320, 200, 16, 40, 0},
    {640, 200, 16, 80, 0},
    {640, 350, 16, 80, 0},
    {640, 480, 16, 80, 0},
    {320, 200, 256, 320, 1},
    {320, 240, 256, 80, 0},
    {320, 400, 256, 80, 0},
    {360, 480, 256, 90, 0},
    {640, 480, 2, 80, 0},

    {640, 480, 256, 640, 1},	/* VGAlib SVGA modes */
    {800, 600, 256, 800, 1},
    {1024, 768, 256, 1024, 1},
    {1280, 1024, 256, 1280, 1},

    {320, 200, 1 << 15, 640, 2},	/* Hicolor/truecolor modes */
    {320, 200, 1 << 16, 640, 2},
    {320, 200, 1 << 24, 320 * 3, 3},
    {640, 480, 1 << 15, 640 * 2, 2},
    {640, 480, 1 << 16, 640 * 2, 2},
    {640, 480, 1 << 24, 640 * 3, 3},
    {800, 600, 1 << 15, 800 * 2, 2},
    {800, 600, 1 << 16, 800 * 2, 2},
    {800, 600, 1 << 24, 800 * 3, 3},
    {1024, 768, 1 << 15, 1024 * 2, 2},
    {1024, 768, 1 << 16, 1024 * 2, 2},
    {1024, 768, 1 << 24, 1024 * 3, 3},
    {1280, 1024, 1 << 15, 1280 * 2, 2},
    {1280, 1024, 1 << 16, 1280 * 2, 2},
    {1280, 1024, 1 << 24, 1280 * 3, 3},

    {800, 600, 16, 100, 0},	/* SVGA 16-color modes */
    {1024, 768, 16, 128, 0},
    {1280, 1024, 16, 160, 0},

    {720, 348, 2, 90, 0},	/* Hercules emulation mode */

    {320, 200, 1 << 24, 320 * 4, 4},
    {640, 480, 1 << 24, 640 * 4, 4},
    {800, 600, 1 << 24, 800 * 4, 4},
    {1024, 768, 1 << 24, 1024 * 4, 4},
    {1280, 1024, 1 << 24, 1280 * 4, 4},

    {1152, 864, 16, 144, 0},
    {1152, 864, 256, 1152, 1},
    {1152, 864, 1 << 15, 1152 * 2, 2},
    {1152, 864, 1 << 16, 1152 * 2, 2},
    {1152, 864, 1 << 24, 1152 * 3, 3},
    {1152, 864, 1 << 24, 1152 * 4, 4},

    {1600, 1200, 16, 200, 0},
    {1600, 1200, 256, 1600, 1},
    {1600, 1200, 1 << 15, 1600 * 2, 2},
    {1600, 1200, 1 << 16, 1600 * 2, 2},
    {1600, 1200, 1 << 24, 1600 * 3, 3},
    {1600, 1200, 1 << 24, 1600 * 4, 4},

    {0, 0, 0, 0, 0},		/* 16 user definable modes */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0}
};

#define MAX_MODES (sizeof(infotable) / sizeof(struct info))


/* default palette values */
static const unsigned char default_red[256]
=
{0, 0, 0, 0, 42, 42, 42, 42, 21, 21, 21, 21, 63, 63, 63, 63,
 0, 5, 8, 11, 14, 17, 20, 24, 28, 32, 36, 40, 45, 50, 56, 63,
 0, 16, 31, 47, 63, 63, 63, 63, 63, 63, 63, 63, 63, 47, 31, 16,
 0, 0, 0, 0, 0, 0, 0, 0, 31, 39, 47, 55, 63, 63, 63, 63,
 63, 63, 63, 63, 63, 55, 47, 39, 31, 31, 31, 31, 31, 31, 31, 31,
 45, 49, 54, 58, 63, 63, 63, 63, 63, 63, 63, 63, 63, 58, 54, 49,
 45, 45, 45, 45, 45, 45, 45, 45, 0, 7, 14, 21, 28, 28, 28, 28,
 28, 28, 28, 28, 28, 21, 14, 7, 0, 0, 0, 0, 0, 0, 0, 0,
 14, 17, 21, 24, 28, 28, 28, 28, 28, 28, 28, 28, 28, 24, 21, 17,
 14, 14, 14, 14, 14, 14, 14, 14, 20, 22, 24, 26, 28, 28, 28, 28,
 28, 28, 28, 28, 28, 26, 24, 22, 20, 20, 20, 20, 20, 20, 20, 20,
 0, 4, 8, 12, 16, 16, 16, 16, 16, 16, 16, 16, 16, 12, 8, 4,
 0, 0, 0, 0, 0, 0, 0, 0, 8, 10, 12, 14, 16, 16, 16, 16,
 16, 16, 16, 16, 16, 14, 12, 10, 8, 8, 8, 8, 8, 8, 8, 8,
 11, 12, 13, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 15, 13, 12,
 11, 11, 11, 11, 11, 11, 11, 11, 0, 0, 0, 0, 0, 0, 0, 0};
static const unsigned char default_green[256]
=
{0, 0, 42, 42, 0, 0, 21, 42, 21, 21, 63, 63, 21, 21, 63, 63,
 0, 5, 8, 11, 14, 17, 20, 24, 28, 32, 36, 40, 45, 50, 56, 63,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 31, 47, 63, 63, 63, 63,
 63, 63, 63, 63, 63, 47, 31, 16, 31, 31, 31, 31, 31, 31, 31, 31,
 31, 39, 47, 55, 63, 63, 63, 63, 63, 63, 63, 63, 63, 55, 47, 39,
 45, 45, 45, 45, 45, 45, 45, 45, 45, 49, 54, 58, 63, 63, 63, 63,
 63, 63, 63, 63, 63, 58, 54, 49, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 7, 14, 21, 29, 28, 28, 28, 28, 28, 28, 28, 28, 21, 14, 7,
 14, 14, 14, 14, 14, 14, 14, 14, 14, 17, 21, 24, 28, 28, 28, 28,
 28, 28, 28, 28, 28, 24, 21, 17, 20, 20, 20, 20, 20, 20, 20, 20,
 20, 22, 24, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 26, 24, 22,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 8, 12, 16, 16, 16, 16,
 16, 16, 16, 16, 16, 12, 8, 4, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 10, 12, 14, 16, 16, 16, 16, 16, 16, 16, 16, 16, 14, 12, 10,
 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 13, 15, 16, 16, 16, 16,
 16, 16, 16, 16, 16, 15, 13, 12, 0, 0, 0, 0, 0, 0, 0, 0};
static const unsigned char default_blue[256]
=
{0, 42, 0, 42, 0, 42, 0, 42, 21, 63, 21, 63, 21, 63, 21, 63,
 0, 5, 8, 11, 14, 17, 20, 24, 28, 32, 36, 40, 45, 50, 56, 63,
 63, 63, 63, 63, 63, 47, 31, 16, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 16, 31, 47, 63, 63, 63, 63, 63, 63, 63, 63, 63, 55, 47, 39,
 31, 31, 31, 31, 31, 31, 31, 31, 31, 39, 47, 55, 63, 63, 63, 63,
 63, 63, 63, 63, 63, 58, 54, 49, 45, 45, 45, 45, 45, 45, 45, 45,
 45, 49, 54, 58, 63, 63, 63, 63, 28, 28, 28, 28, 28, 21, 14, 7,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 14, 21, 28, 28, 28, 28,
 28, 28, 28, 28, 28, 24, 21, 17, 14, 14, 14, 14, 14, 14, 14, 14,
 14, 17, 21, 24, 28, 28, 28, 28, 28, 28, 28, 28, 28, 26, 24, 22,
 20, 20, 20, 20, 20, 20, 20, 20, 20, 22, 24, 26, 28, 28, 28, 28,
 16, 16, 16, 16, 16, 12, 8, 4, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 4, 8, 12, 16, 16, 16, 16, 16, 16, 16, 16, 16, 14, 12, 10,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 10, 12, 14, 16, 16, 16, 16,
 16, 16, 16, 16, 16, 15, 13, 12, 11, 11, 11, 11, 11, 11, 11, 11,
 11, 12, 13, 15, 16, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0};


static unsigned char text_regs[MAX_REGS];	/* VGA registers for saved text mode */


/* saved text mode palette values */
static unsigned char text_red[256];
static unsigned char text_green[256];
static unsigned char text_blue[256];

/* saved graphics mode palette values */
static unsigned char graph_red[256];
static unsigned char graph_green[256];
static unsigned char graph_blue[256];

static int prv_mode = TEXT;	/* previous video mode      */
static int flip_mode = TEXT;	/* flipped video mode       */

int CM = TEXT;			/* current video mode       */
struct info CI;			/* current video parameters */
int COL;			/* current color            */


static int initialized = 0;	/* flag: initialize() called ?  */
static int flip = 0;		/* flag: executing vga_flip() ? */

/* svgalib additions: */

int __svgalib_chipset = UNDEFINED;
int __svgalib_driver_report = 1;
	/* report driver used after chipset detection */
int __svgalib_videomemoryused = -1;
int __svgalib_modeX = 0;	/* true after vga_setmodeX() */
int __svgalib_modeflags = 0;	/* copy of flags for current mode */
int __svgalib_critical = 0;	/* indicates blitter is busy */
int __svgalib_screenon = 1;	/* screen visible if != 0 */
RefreshRange __svgalib_horizsync =
{31500U, 0U};			/* horz. refresh (Hz) min, max */
RefreshRange __svgalib_vertrefresh =
{50U, 70U};			/* vert. refresh (Hz) min, max */
int __svgalib_grayscale = 0;	/* grayscale vs. color mode */
int __svgalib_modeinfo_linearset = 0;	/* IS_LINEAR handled via extended vga_modeinfo */
const int __svgalib_max_modes = MAX_MODES;	/* Needed for dynamical allocated tables in mach32.c */

static unsigned __svgalib_maxhsync[] =
{
    31500, 35100, 35500, 37900, 48300, 56000, 60000
};

static int lastmodenumber = __GLASTMODE;	/* Last defined mode */
static int my_pid = 0;		/* process PID, used with atexit() */
static int currentpage;
static int vga_page_offset;	/* offset to add to all vga_set*page() calls */
static int currentlogicalwidth;
static int currentdisplaystart;
static int mouse_support = 0;
static int mouse_mode = 0;
static int mouse_type = -1;
static int mouse_modem_ctl = 0;
static int runinbackground = 0;
static int oktowrite = 1;
static int modeinfo_mask = ~0;

int __svgalib_mem_fd = -1;	/* /dev/mem file descriptor  */
int __svgalib_tty_fd = -1;	/* /dev/tty file descriptor */
int __svgalib_console_fd = -1;	/* /dev/console file descriptor */

/* Dummy buffer for mmapping grahics memory; points to 64K VGA framebuffer. */
unsigned char *__svgalib_graph_mem;
/* Exported variable (read-only) is shadowed from internal variable, for */
/* better shared library performance. */
unsigned char *graph_mem;

#ifdef __alpha__

/* same as graph mem, but mapped through sparse memory: */
unsigned char *__svgalib_sparse_mem;

#endif

static unsigned char *graph_buf = NULL;		/* saves graphics data during flip */

static unsigned char *font_buf1;	/* saved font data - plane 2 */
static unsigned char *font_buf2;	/* saved font data - plane 3 */

static struct termios text_termio;	/* text mode termio parameters     */
static struct termios graph_termio;	/* graphics mode termio parameters */

int flipchar = '\x1b';		/* flip character - initially  ESCAPE */


/* Chipset specific functions */

DriverSpecs *driverspecs = &vga_driverspecs;

static void (*setpage) (int);	/* gives little faster vga_setpage() */
static void (*setrdpage) (int);
static void (*setwrpage) (int);

static void readconfigfile(void);

DriverSpecs *driverspecslist[] =
{
    NULL,			/* chipset undefined */
    &vga_driverspecs,
#ifdef INCLUDE_ET4000_DRIVER
    &et4000_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_CIRRUS_DRIVER
    &cirrus_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_TVGA_DRIVER
    &tvga8900_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_OAK_DRIVER
    &oak_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_EGA_DRIVER
    &ega_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_S3_DRIVER
    &s3_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_ET3000_DRIVER
    &et3000_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_MACH32_DRIVER
    &mach32_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_GVGA6400_DRIVER
    &gvga6400_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_ARK_DRIVER
    &ark_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_ATI_DRIVER
    &ati_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_ALI_DRIVER
    &ali_driverspecs,
#else
    NULL,
#endif
#ifdef INCLUDE_MACH64_DRIVER
    &mach64_driverspecs,
#else
    NULL
#endif
};

static char *driver_names[] =
{"", "VGA", "ET4000", "Cirrus", "TVGA", "Oak", "EGA", "S3",
 "ET3000", "Mach32", "GVGA6400", "ARK", "ATI", "ALI", "Mach64", NULL};

/* Chipset drivers */

/* vgadrv       Standard VGA (also used by drivers below) */
/* et4000       Tseng ET4000 (from original vgalib) */
/* cirrus       Cirrus Logic GD542x */
/* tvga8900     Trident TVGA 8900/9000 (derived from tvgalib) */
/* oak          Oak Technologies 037/067/077 */
/* egadrv       IBM EGA (subset of VGA) */
/* s3           S3 911 */
/* mach32       ATI MACH32 */
/* ark          ARK Logic */
/* gvga6400     Genoa 6400 (old SVGA) */
/* ati          ATI */
/* ali          ALI2301 */
/* mach64	ATI MACH64 */

/*#define DEBUG */

/* Debug config file parsing.. */
/*#define DEBUG_CONF */

#ifdef DEBUG
static void _DEBUG(int dnr)
{
    static int first = 1;
    FILE *dfile;

    dfile = fopen("svgalib.debug", (first ? "w" : "a"));
    first = 0;
    if (dfile == NULL)
	exit(-1);
    fprintf(dfile, "debug #%d\n", dnr);
    fclose(dfile);
    sync();
}
#else
#define _DEBUG(d)
#endif

static void set_graphtermio(void)
{
    /* Leave keyboard alone when rawkeyboard is enabled! */
    if (kbd_fd < 0) {
	/* set graphics mode termio parameters */
	ioctl(0, TCSETSW, &graph_termio);
    }
}


static void set_texttermio(void)
{
    /* Leave keyboard alone when rawkeyboard is enabled! */
    if (kbd_fd < 0) {
	/* restore text mode termio parameters */
	ioctl(0, TCSETSW, &text_termio);
    }
}


static void disable_interrupt(void)
{
    struct termios cur_termio;

    ioctl(0, TCGETS, &cur_termio);
    cur_termio.c_lflag &= ~ISIG;
    ioctl(0, TCSETSW, &cur_termio);
}


static void enable_interrupt(void)
{
    struct termios cur_termio;

    ioctl(0, TCGETS, &cur_termio);
    cur_termio.c_lflag |= ISIG;
    ioctl(0, TCSETSW, &cur_termio);
}

/* The following is rather messy and inelegant. The only solution I can */
/* see is getting a extra free VT for graphics like XFree86 does. */

void __svgalib_waitvtactive(void)
{
    struct stat chkbuf;
/*      struct vt_stat vtstat; */
    int major, minor;
    int fd;

    /* get console number we are running in */
    fd = dup(2);		/* stderr */
    fstat(fd, &chkbuf);
    major = chkbuf.st_rdev >> 8;
    minor = chkbuf.st_rdev & 0xff;	/* console number */

/*      printf("major = %d, minor = %d\n", major, minor); */

    if (major != 4 || minor >= 64) {
	printf("Not running in graphics-capable virtual console.\n");
	exit(-1);
    }
    close(fd);

    fd = dup(2);
    while (ioctl(fd, VT_WAITACTIVE, minor) < 0)
	sleep(1);
    close(fd);
}


/* open /dev/mem */
static void open_mem(void)
{
    if (__svgalib_mem_fd < 0)
	if ((__svgalib_mem_fd = open("/dev/mem", O_RDWR)) < 0) {
	    printf("svgalib: Cannot open /dev/mem.\n");
	    exit(-1);
	}
}

static void open_devconsole(void)
{
    if (__svgalib_console_fd != -1)
	return;
    if ((__svgalib_console_fd = open("/dev/console", O_RDONLY)) < 0) {
	printf("svgalib: Cannot open /dev/console.\n");
	exit(-1);
    }
}

/* Check whether console is graphics-capable. If not, exit. */

#if 0

/* Check stderr (silly hack). This means stderr cannot be redirected when */
/* runnning a graphics program. */

static void checkconsole(void)
{
    struct stat chkbuf;
    int major, minor;
    int fd;

    /* get console number we are running in */
    fd = dup(2);		/* stderr */
    fstat(fd, &chkbuf);
    major = chkbuf.st_rdev >> 8;
    minor = chkbuf.st_rdev & 0xff;	/* console number */

    if (major != 4 || minor >= 64) {
	printf("Not running in graphics-capable virtual console.\n");
	exit(-1);
    }
    close(fd);
}

#else

/* Inspired by MKJ's article in Linux Journal #3 and #4. */

static int checkconsole(void)
{
    int e;
    struct vt_mode vtm;
    if (__svgalib_console_fd == -1)
	open_devconsole();
    e = ioctl(__svgalib_console_fd, VT_GETMODE, &vtm);
    if (e < 0)
	return 0;
    return 1;
}

static void doconsolecheck(void)
{
    if (!checkconsole()) {
	printf("Not running in graphics-capable virtual console.\n");
	exit(-1);
    }
}

#endif


void __vga_get_perm(void)
{
    static int done = 0;

    /* Only do this once. */
    if (done)
	return;
    done = 1;

    /* Get I/O permissions for VGA registers. */
    /* If IOPERM is set, assume permissions have already been obtained */
    /* by a calling (exec-ing) process, e.g. ioperm(1). */

    if (getenv("IOPERM") == NULL)
#ifdef __alpha__
	if (ioperm(0x0000, 0x10000, 1)) {
	    printf("svgalib: Cannot get I/O permissions.\n");
	    exit(-1);
	}
#else
	if (ioperm(0x3b4, 0x3df - 0x3b4 + 1, 1)) {
	    printf("svgalib: Cannot get I/O permissions.\n");
	    exit(-1);
	}
#endif

    /* Open /dev/mem (also needs supervisor rights; ioperm(1) can be */
    /* used together with a special group that has r/w access on */
    /* /dev/mem to facilitate this). */
    open_mem();

    open_devconsole();

    /* color or monochrome text emulation? */
    if (CHIPSET != EGA)
	color_text = port_in(MIS_R) & 0x01;
    else
	color_text = 1;		/* EGA is assumed color */

    /* chose registers for color/monochrome emulation */
    if (color_text) {
	CRT_I = CRT_IC;
	CRT_D = CRT_DC;
	IS1_R = IS1_RC;
    } else {
	CRT_I = CRT_IM;
	CRT_D = CRT_DM;
	IS1_R = IS1_RM;
    }
}


void __vga_delay(void)
{
    int i;
    for (i = 0; i < 10; i++);
}

/* The Xfree server uses a slow copy, may help us too ... */
#if defined(CONFIG_ALPHA_JENSEN)
extern unsigned long vga_readl(unsigned long base, unsigned long off);
extern void vga_writel(unsigned long b, unsigned long base, unsigned long off);
static void slowcpy_from_sm(unsigned char *dest, unsigned char *src, unsigned bytes)
{
    long i;
    if (((long) dest & 7) || ((long) src & 7) || bytes & 7) {
	printf("svgalib: unaligned slowcpy()!\n");
	exit(-1);
    }
    for (i = 0; i < bytes; i++) {
	*(dest + i) = (*(unsigned long *) (src + (i << 7)) >> ((i & 0x03) * 8))
	    & 0xffUL;
    }
}
static void slowcpy_to_sm(unsigned char *dest, unsigned char *src, unsigned bytes)
{
    long i;
    if (((long) dest & 7) || ((long) src & 7) || bytes & 7) {
	printf("svgalib: unaligned slowcpy()!\n");
	exit(-1);
    }
    for (i = 0; i < bytes; i++) {
	*(unsigned long *) (dest + (i << 7)) =
	    (*(unsigned char *) (src + i)) * 0x01010101UL;
    }
}

#else
static void slowcpy(unsigned char *dest, unsigned char *src, unsigned bytes)
{
#ifdef __alpha__
    if (((long) dest & 7) || ((long) src & 7) || bytes & 7) {
	printf("svgalib: unaligned slowcpy()!\n");
	exit(-1);
    }
    while (bytes > 0) {
	*(long *) dest = *(long *) src;
	dest += 8;
	src += 8;
	bytes -= 8;
    }
#else
    while (bytes-- > 0)
	*(dest++) = *(src++);
#endif
}
#endif

int __vga_saveregs(unsigned char *regs)
{
    int i;

    if (__svgalib_chipset == EGA) {
	/* Special case: Don't save standard VGA registers. */
	return chipset_saveregs(regs);
    }
    /* save VGA registers */
    for (i = 0; i < CRT_C; i++) {
	port_out(i, CRT_I);
	regs[CRT + i] = port_in(CRT_D);
    }
    for (i = 0; i < ATT_C; i++) {
	port_in(IS1_R);
	__vga_delay();
	port_out(i, ATT_IW);
	__vga_delay();
	regs[ATT + i] = port_in(ATT_R);
	__vga_delay();
    }
    for (i = 0; i < GRA_C; i++) {
	port_out(i, GRA_I);
	regs[GRA + i] = port_in(GRA_D);
    }
    for (i = 0; i < SEQ_C; i++) {
	port_out(i, SEQ_I);
	regs[SEQ + i] = port_in(SEQ_D);
    }
    regs[MIS] = port_in(MIS_R);

    i = chipset_saveregs(regs);	/* save chipset-specific registers */
    /* i : additional registers */
    if (!SCREENON) {		/* We turned off the screen */
	port_in(IS1_R);
	__vga_delay();
	port_out(0x20, ATT_IW);
    }
    return CRT_C + ATT_C + GRA_C + SEQ_C + 1 + i;
}


int __vga_setregs(const unsigned char *regs)
{
    int i;

    if (__svgalib_chipset == EGA) {
	/* Enable graphics register modification */
	port_out(0x00, GRA_E0);
	port_out(0x01, GRA_E1);
    }
    /* update misc output register */
    port_out(regs[MIS], MIS_W);

    /* synchronous reset on */
    port_out(0x00, SEQ_I);
    port_out(0x01, SEQ_D);

    /* write sequencer registers */
    port_out(1, SEQ_I);
    port_out(regs[SEQ + 1] | 0x20, SEQ_D);
    for (i = 2; i < SEQ_C; i++) {
	port_out(i, SEQ_I);
	port_out(regs[SEQ + i], SEQ_D);
    }

    /* synchronous reset off */
    port_out(0x00, SEQ_I);
    port_out(0x03, SEQ_D);

    if (__svgalib_chipset != EGA) {
	/* deprotect CRT registers 0-7 */
	port_out(0x11, CRT_I);
	port_out(port_in(CRT_D) & 0x7F, CRT_D);
    }
    /* write CRT registers */
    for (i = 0; i < CRT_C; i++) {
	port_out(i, CRT_I);
	port_out(regs[CRT + i], CRT_D);
    }

    /* write graphics controller registers */
    for (i = 0; i < GRA_C; i++) {
	port_out(i, GRA_I);
	port_out(regs[GRA + i], GRA_D);
    }

    /* write attribute controller registers */
    for (i = 0; i < ATT_C; i++) {
	port_in(IS1_R);		/* reset flip-flop */
	__vga_delay();
	port_out(i, ATT_IW);
	__vga_delay();
	port_out(regs[ATT + i], ATT_IW);
	__vga_delay();
    }

    return 0;
}

/* We invoke the old interrupt handler after setting text mode */
/* We catch all signals that cause an exit by default (aka almost all) */
static char sig2catch[] =
{SIGHUP, SIGINT, SIGQUIT, SIGILL,
 SIGTRAP, SIGIOT, SIGBUS, SIGFPE,
 SIGSEGV, SIGPIPE, SIGALRM, SIGTERM,
 SIGXCPU, SIGXFSZ, SIGVTALRM,
 SIGPROF, SIGPWR};
static struct sigaction old_signal_handler[sizeof(sig2catch)];

static void restoretextmode(void)
{
    /* handle unexpected interrupts - restore text mode and exit */
    keyboard_close();
    /* Restore a setting screwed by keyboard_close (if opened in graphicsmode) */
    set_texttermio();
    if (CM != TEXT)
	vga_setmode(TEXT);
    if (!__svgalib_screenon)
	vga_screenon();
    if (__svgalib_tty_fd >= 0) {
	ioctl(__svgalib_tty_fd, KDSETMODE, KD_TEXT);
    }
}


static void signal_handler(int v)
{
    int i;

    restoretextmode();
    printf("svgalib: Signal %d: %s received%s.\n", v, strsignal(v),
	   (v == SIGINT) ? " (ctrl-c pressed)" : "");

    for (i = 0; i < sizeof(sig2catch); i++)
	if (sig2catch[i] == v) {
	    sigaction(v, old_signal_handler + i, NULL);
	    raise(v);
	    break;
	}
    if (i >= sizeof(sig2catch)) {
	printf("svgalib: Aieeee! Illegal call to signal_handler, raising segfault.\n");
	raise(SIGSEGV);
    }
}

int __vga_getchipset(void)
{
    readconfigfile();		/* Make sure the config file is read. */

    __vga_get_perm();
    if (CHIPSET == UNDEFINED) {
	CHIPSET = VGA;		/* Protect against recursion */
#ifdef INCLUDE_MACH64_DRIVER_TEST
	if (mach64_driverspecs.test())
	    CHIPSET = MACH64;
	else
#endif
#ifdef INCLUDE_MACH32_DRIVER_TEST
	if (mach32_driverspecs.test())
	    CHIPSET = MACH32;
	else
#endif
#ifdef INCLUDE_EGA_DRIVER_TEST
	if (ega_driverspecs.test())
	    CHIPSET = EGA;
	else
#endif
#ifdef INCLUDE_ET4000_DRIVER_TEST
	if (et4000_driverspecs.test())
	    CHIPSET = ET4000;
	else
#endif
#ifdef INCLUDE_TVGA_DRIVER_TEST
	if (tvga8900_driverspecs.test())
	    CHIPSET = TVGA8900;
	else
#endif
#ifdef INCLUDE_CIRRUS_DRIVER_TEST
	    /* The Cirrus detection is not very clean. */
	if (cirrus_driverspecs.test())
	    CHIPSET = CIRRUS;
	else
#endif
#ifdef INCLUDE_OAK_DRIVER_TEST
	if (oak_driverspecs.test())
	    CHIPSET = OAK;
	else
#endif
#ifdef INCLUDE_S3_DRIVER_TEST
	if (s3_driverspecs.test())
	    CHIPSET = S3;
	else
#endif
#ifdef INCLUDE_ET3000_DRIVER_TEST
	if (et3000_driverspecs.test())
	    CHIPSET = ET3000;
	else
#endif
#ifdef INCLUDE_ARK_DRIVER_TEST
	if (ark_driverspecs.test())
	    CHIPSET = ARK;
	else
#endif
#ifdef INCLUDE_GVGA6400_DRIVER_TEST
	if (gvga6400_driverspecs.test())
	    CHIPSET = GVGA6400;
	else
#endif
#ifdef INCLUDE_ATI_DRIVER_TEST
	if (ati_driverspecs.test())
	    CHIPSET = ATI;
	else
#endif
#ifdef INCLUDE_ALI_DRIVER_TEST
	if (ali_driverspecs.test())
	    CHIPSET = ALI;
	else
#endif

	if (vga_driverspecs.test())
	    CHIPSET = VGA;
	else
	    /* else */
	{
	    fprintf(stderr, "svgalib: Cannot find EGA or VGA graphics device.\n");
	    exit(1);
	}
	setpage = driverspecs->setpage;
	setrdpage = driverspecs->setrdpage;
	setwrpage = driverspecs->setwrpage;
    }
    return CHIPSET;
}

void vga_setchipset(int c)
{
    CHIPSET = c;
#ifdef DEBUG
    printf("Setting chipset\n");
#endif
    if (c == UNDEFINED)
	return;
    if (driverspecslist[c] == NULL) {
	printf("svgalib: Invalid chipset. The driver may not be compiled in.\n");
	CHIPSET = UNDEFINED;
	return;
    }
    __vga_get_perm();
    driverspecslist[c]->init(0, 0, 0);
    setpage = driverspecs->setpage;
    setrdpage = driverspecs->setrdpage;
    setwrpage = driverspecs->setwrpage;
}

void vga_setchipsetandfeatures(int c, int par1, int par2)
{
    CHIPSET = c;
#ifdef DEBUG
    printf("Forcing chipset and features\n");
#endif
    __vga_get_perm();
    driverspecslist[c]->init(1, par1, par2);
#ifdef DEBUG
    printf("Finished forcing chipset and features\n");
#endif
    setpage = driverspecs->setpage;
    setrdpage = driverspecs->setrdpage;
    setwrpage = driverspecs->setwrpage;
}


static void savepalette(unsigned char *red, unsigned char *green,
			unsigned char *blue)
{
    int i;

    if (CHIPSET == EGA)
	return;

    if ((__svgalib_chipset == MACH32) && SVGAMODE(CM)) {
	/* Actually the same but we are in 8514 mode and the dac
	   does not respond to the VGA circuitry anymore... */

	port_out(0, PEL8514_IR);
	for (i = 0; i < 256; i++) {
	    __vga_delay();
	    *(red++) = port_in(PEL8514_D);
	    __vga_delay();
	    *(green++) = port_in(PEL8514_D);
	    __vga_delay();
	    *(blue++) = port_in(PEL8514_D);
	}
	return;
    }
    /* save graphics mode palette - first select palette index 0 */
    port_out(0, PEL_IR);

    /* read RGB components - index is autoincremented */
    for (i = 0; i < 256; i++) {
	__vga_delay();
	*(red++) = port_in(PEL_D);
	__vga_delay();
	*(green++) = port_in(PEL_D);
	__vga_delay();
	*(blue++) = port_in(PEL_D);
    }
}

static void restorepalette(const unsigned char *red,
		   const unsigned char *green, const unsigned char *blue)
{
    int i;

    if (CHIPSET == EGA)
	return;

    if ((__svgalib_chipset == MACH32) && SVGAMODE(CM)) {
	/* Actually the same but we are in 8514 mode and the dac
	   does not respond to the VGA circuitry anymore... */

	port_out(0, PEL8514_IW);
	for (i = 0; i < 256; i++) {
	    __vga_delay();
	    port_out(*(red++), PEL8514_D);
	    __vga_delay();
	    port_out(*(green++), PEL8514_D);
	    __vga_delay();
	    port_out(*(blue++), PEL8514_D);
	}
	return;
    }
    /* restore saved palette */
    port_out(0, PEL_IW);

    /* read RGB components - index is autoincremented */
    for (i = 0; i < 256; i++) {
	__vga_delay();
	port_out(*(red++), PEL_D);
	__vga_delay();
	port_out(*(green++), PEL_D);
	__vga_delay();
	port_out(*(blue++), PEL_D);
    }
}


/* Virtual console switching */

static int forbidvtrelease = 0;
static int forbidvtacquire = 0;
static int lock_count = 0;
static int release_flag = 0;

static void takevtcontrol(void);

void __vga_flipaway(void);
static void vga_flipback(void);

static void releasevt_signal(int n)
{
    struct sigaction siga;

    if (lock_count) {
	release_flag = 1;
	return;
    }
#ifdef DEBUG
    printf("Release request.\n");
#endif
    forbidvtacquire = 1;
    SETSIG(siga, SIGUSR1, releasevt_signal);
    if (forbidvtrelease) {
	forbidvtacquire = 0;
	ioctl(__svgalib_tty_fd, VT_RELDISP, 0);
	return;
    }
    __vga_flipaway();
    ioctl(__svgalib_tty_fd, VT_RELDISP, 1);
#ifdef DEBUG
    printf("Finished release.\n");
#endif
    forbidvtacquire = 0;

    /* Suspend program until switched to again. */
#ifdef DEBUG
    printf("Suspended.\n");
#endif
    oktowrite = 0;
    if (!runinbackground)
	__svgalib_waitvtactive();
#ifdef DEBUG
    printf("Waked.\n");
#endif
}

static void acquirevt_signal(int n)
{
    struct sigaction siga;
#ifdef DEBUG
    printf("Acquisition request.\n");
#endif
    forbidvtrelease = 1;
    SETSIG(siga, SIGUSR2, acquirevt_signal);
    if (forbidvtacquire) {
	forbidvtrelease = 0;
	return;
    }
    vga_flipback();
    ioctl(__svgalib_tty_fd, VT_RELDISP, VT_ACKACQ);
#ifdef DEBUG
    printf("Finished acquisition.\n");
#endif
    forbidvtrelease = 0;
    oktowrite = 1;
}

static struct vt_mode oldvtmode;
static struct vt_mode newvtmode;

void takevtcontrol(void)
{
    struct sigaction siga;

    ioctl(__svgalib_tty_fd, VT_GETMODE, &oldvtmode);
    newvtmode = oldvtmode;
    newvtmode.mode = VT_PROCESS;	/* handle VT changes */
    newvtmode.relsig = SIGUSR1;	/* I didn't find SIGUSR1/2 anywhere */
    newvtmode.acqsig = SIGUSR2;	/* in the kernel sources, so I guess */
    /* they are free */
    SETSIG(siga, SIGUSR1, releasevt_signal);
    SETSIG(siga, SIGUSR2, acquirevt_signal);
    ioctl(__svgalib_tty_fd, VT_SETMODE, &newvtmode);
}

static void __vga_mmap(void)
{
#if 0
    /* This may waste 64K; mmap can probably do better by now. */
    /* valloc allocates aligned on page boundary */
    if ((__svgalib_graph_mem = valloc(GRAPH_SIZE)) == NULL) {
	printf("svgalib: allocation error \n");
	exit(-1);
    }
    __svgalib_graph_mem = (unsigned char *) mmap(
						    (caddr_t) GM,
						    GRAPH_SIZE,
						  PROT_READ | PROT_WRITE,
						  MAP_SHARED | MAP_FIXED,
						    __svgalib_mem_fd,
						    GRAPH_BASE
	);
#endif
#if 1
    /* This assumes pl10+. */
    /* Still seems to waste 64K, so don't bother. */
    GM = (unsigned char *) mmap(
				   (caddr_t) 0,
				   GRAPH_SIZE,
				   PROT_READ | PROT_WRITE,
				   MAP_SHARED,
				   __svgalib_mem_fd,
				   GRAPH_BASE
	);
#ifdef __alpha__
    SM = (unsigned char *) mmap(
				   (caddr_t) 0,
				   GRAPH_SIZE << MEM_SHIFT,
				   PROT_READ | PROT_WRITE,
				   MAP_SHARED,
				   __svgalib_mem_fd,
				   SPARSE_GRAPH_BASE
	);
#endif
#endif
    graph_mem = __svgalib_graph_mem;	/* Exported variable. */
}

static void __vga_atexit(void)
{
    if (getpid() == my_pid)	/* protect against forked processes */
	restoretextmode();
}

static void setcoloremulation(void)
{
    /* shift to color emulation */
    CRT_I = CRT_IC;
    CRT_D = CRT_DC;
    IS1_R = IS1_RC;
    if (CHIPSET != EGA)
	port_out(port_in(MIS_R) | 0x01, MIS_W);
}

static void initialize(void)
{
    int i;
    struct sigaction siga;

    doconsolecheck();

    /* Make sure that textmode is restored at exit(). */
    if (my_pid == 0)
	my_pid = getpid();
    atexit(__vga_atexit);

#ifndef DONT_WAIT_VC_ACTIVE
    __svgalib_waitvtactive();
#endif

    /* save text mode termio parameters */
    ioctl(0, TCGETS, &text_termio);

    graph_termio = text_termio;

    /* change termio parameters to allow our own I/O processing */
    graph_termio.c_iflag &= ~(BRKINT | PARMRK | INPCK | IUCLC | IXON | IXOFF);
    graph_termio.c_iflag |= (IGNBRK | IGNPAR);

    graph_termio.c_oflag &= ~(ONOCR);

    graph_termio.c_lflag &= ~(ICANON | ECHO | ECHOE | ECHOK | ECHONL | NOFLSH);
    graph_termio.c_lflag |= (ISIG);	/* enable interrupt */

    graph_termio.c_cc[VMIN] = 1;
    graph_termio.c_cc[VTIME] = 0;
    graph_termio.c_cc[VSUSP] = 0;	/* disable suspend */

    disable_interrupt();	/* Is reenabled later by set_texttermio */

    __vga_getchipset();		/* make sure a chipset has been selected */
    chipset_unlock();

#ifdef USE_DEVTTY
    if ((__svgalib_tty_fd = open("/dev/tty", O_RDONLY)) < 0) {
	printf("svgalib: can't open /dev/tty \n");
	exit(-1);
    }
#else
    /* open /dev/tty0 - current virtual console */
    if ((__svgalib_tty_fd = open("/dev/tty0", O_RDONLY)) < 0) {
	printf("svgalib: can't open /dev/tty0 \n");
	exit(-1);
    }
#endif
/*    console = fdopen(devtty_fd, "r"); */

#if 0
    /* Check for EGA/VGA display */
/*   linux/kd.h defines this function but doesn't work (or am I wrong) ? */
    if (ioctl(__svgalib_tty_fd, KDDISPTYPE, &display_type)) {
	printf("VGAlib: can't get display type\n");
	exit(-1);
    }
    if (display_type != KD_EGA) {
	printf("VGAlib: EGA/VGA display required\n");
	exit(-1);
    }
#endif

    /* disable text output to console */
    ioctl(__svgalib_tty_fd, KDSETMODE, KD_GRAPHICS);

/* What the hell is this?!?? Why should using a mouse prevent  */
/* you from switching vt's ??                                  */
/*  if (!mouse_support)       */
    if (1)
	takevtcontrol();	/* HH: Take control over VT */

/*    vga_lockvc(); */

    /* open /dev/mem */
    open_mem();

    /* mmap graphics memory */

    __vga_mmap();

    if ((long) GM < 0) {
	printf("svgalib: mmap error \n");
	exit(-1);
    }
    /* disable video */
    vga_screenoff();

    /* Sanity check: (from painful experience) */

    i = __vga_saveregs(text_regs);
    if (i > MAX_REGS) {
	puts("svgalib: FATAL internal error:");
	printf("Set MAX_REGS at least to %d in src/driver.h and recompile everything.\n",
	       i);
	exit(1);
    }
    /* This appears to fix the Trident 8900 rebooting problem. */
    if (__svgalib_chipset == TVGA8900) {
	port_out(0x0c, SEQ_I);	/* reg 12 */
	text_regs[EXT + 11] = port_in(SEQ_D);
	port_out(0x1f, CRT_I);
	text_regs[EXT + 12] = port_in(CRT_D);
    }
    /* save text mode palette - first select palette index 0 */
    port_out(0, PEL_IR);

    /* read RGB components - index is autoincremented */
    savepalette(text_red, text_green, text_blue);

    /* shift to color emulation */
    setcoloremulation();

    /* save font data - first select a 16 color graphics mode */
    driverspecs->setmode(GPLANE16, prv_mode);

    /* Allocate space for textmode font. */
    font_buf1 = malloc(FONT_SIZE * 2);
    font_buf2 = font_buf1 + FONT_SIZE;

    /* save font data in plane 2 */
    port_out(0x04, GRA_I);
    port_out(0x02, GRA_D);
#ifdef __alpha__
    port_out(0x06, GRA_I);
    port_out(0x00, GRA_D);
#endif
#if defined(CONFIG_ALPHA_JENSEN)
    slowcpy_from_sm(font_buf1, SM, FONT_SIZE);
#else
    slowcpy(font_buf1, GM, FONT_SIZE);
#endif

    /* save font data in plane 3 */
    port_out(0x04, GRA_I);
    port_out(0x03, GRA_D);
#if defined(CONFIG_ALPHA_JENSEN)
    slowcpy_from_sm(font_buf2, SM, FONT_SIZE);
#else
    slowcpy(font_buf2, GM, FONT_SIZE);
#endif
    initialized = 1;

    /* do our own interrupt handling */
    for (i = 0; i < sizeof(sig2catch); i++) {
	siga.sa_handler = signal_handler;
	siga.sa_flags = 0;
	siga.sa_mask = 0;
	sigaction((int) sig2catch[i], &siga, old_signal_handler + i);
    }

    /* VMEM setting moved into setmode - Michael. */

    /* vga_unlockvc(); */
}


inline void vga_setpage(int p)
{
    p += vga_page_offset;
    if (p == currentpage)
	return;
    (*setpage) (p);
    currentpage = p;
}


void vga_setreadpage(int p)
{
    p += vga_page_offset;
    if (p == currentpage)
	return;
    (*setrdpage) (p);
    currentpage = -1;
}


void vga_setwritepage(int p)
{
    p += vga_page_offset;
    if (p == currentpage)
	return;
    (*setwrpage) (p);
    currentpage = -1;
}


void vga_safety_fork(void (*shutdown_routine) (void))
{
    pid_t childpid;
    int child_status, oldkbmode;

    if (initialized) {
	printf("svgalib: warning: vga_safety_fork() called when already initialized\n");
	goto no_fork;
    }
    initialize();

    /*
     * get current keyboard mode:
     *  If this didn't suffice we claim we are on an old system and just don't
     *  need to restore it.
     */
    ioctl(__svgalib_tty_fd, KDGKBMODE, &oldkbmode);

    childpid = fork();
    if (childpid < 0) {
      no_fork:
	printf("svgalib: warning: can't fork to enhance reliability; proceeding anyway\n");
	return;
    }
    if (childpid) {
	for (;;) {
	    while (waitpid(childpid, &child_status, WUNTRACED) != childpid);

	    if (shutdown_routine)
		shutdown_routine();

	    vga_setmode(TEXT);	/* resets termios as well */
	    ioctl(__svgalib_tty_fd, KDSKBMODE, oldkbmode);

	    if (WIFEXITED(child_status))
		exit(WEXITSTATUS(child_status));

	    if (WCOREDUMP(child_status))
		puts("svgalib:vga_safety_fork: Core dumped!");

	    if (WIFSIGNALED(child_status)) {
		printf("svgalib:vga_safety_fork: Killed by signal %d, %s.\n",
		       WTERMSIG(child_status),
		       strsignal(WTERMSIG(child_status)));
		exit(1);
	    }
	    if (WIFSTOPPED(child_status)) {
		printf("svgalib:vga_safety_fork: Stopped by signal %d, %s.\n",
		       WSTOPSIG(child_status),
		       strsignal(WSTOPSIG(child_status)));
		puts("\aWARNING! Continue stopped svgalib application at own risk. You are better\n"
		     "off killing it NOW!");
		continue;
	    }
	}
    }
    /* These need to be done again because the child doesn't inherit them.  */
    __vga_get_perm();

    /*
     * But alas. That doesn't suffice. We raise the iopl here what merely makes
     * the previous call pointless.
     *
     * If IOPERM is set, assume permissions have already been set by Olaf Titz'
     * ioperm(1).
     */

    if (getenv("IOPERM") == NULL) {
	if (iopl(3) < 0) {
	    printf("svgalib(vga_safety_fork): Cannot get I/O permissions.\n");
	    exit(1);
	}
    }
    /*
     * Actually the mmap's are inherited anyway (and not all are remade here),
     * but it does not really harm.
     */
    __vga_mmap();

    /*
     * We might still want to do vc switches.
     */

    takevtcontrol();
}

static void prepareforfontloading(void)
{
    if (__svgalib_chipset == CIRRUS) {
	outb(0x3c4, 0x0f);
	/* Disable CRT FIFO Fast-Page mode. */
	outb(0x3c5, inb(0x3c5) | 0x40);
    }
}

static void fontloadingcomplete(void)
{
    if (__svgalib_chipset == CIRRUS) {
	outb(0x3c4, 0x0f);
	/* Re-enable CRT FIFO Fast-Page mode. */
	outb(0x3c5, inb(0x3c5) & 0xbf);
    }
}


int vga_setmode(int mode)
{
    if (!initialized)
	initialize();

    if (mode != TEXT && !chipset_modeavailable(mode))
	return -1;

/*    if (!flip)
   vga_lockvc(); */
    disable_interrupt();

    prv_mode = CM;
    CM = mode;

    /* disable video */
    vga_screenoff();

    /* Should be more robust (eg. grabbed X modes) */
    if (__vga_getchipset() == ET4000
	&& prv_mode != G640x480x256
	&& SVGAMODE(prv_mode))
	chipset_setmode(G640x480x256, prv_mode);

    if (mode == TEXT) {
	/* Returning to textmode. */

	if (prv_mode != TEXT && mouse_mode == prv_mode) {
#ifdef DEBUG
	    printf("svgalib: Closing mouse.\n");
#endif
	    mouse_close();
	    mouse_mode = 0;
	    /* vga_unlockvc(); */
	}
	if (SVGAMODE(prv_mode))
	    vga_setpage(0);


	/* The extended registers are restored either by the */
	/* chipset setregs function, or the chipset setmode function. */

	/* restore font data - first select a 16 color graphics mode */
	/* Note: this should restore the old extended registers if */
	/* setregs is not defined for the chipset. */
	driverspecs->setmode(GPLANE16, prv_mode);

	if (CHIPSET != EGA)
	    /* restore old extended regs */
	    chipset_setregs(text_regs, mode);

	/* disable Set/Reset Register */
	port_out(0x01, GRA_I);
	port_out(0x00, GRA_D);

	prepareforfontloading();

	/* restore font data in plane 2 - necessary for all VGA's */
	port_out(0x02, SEQ_I);
	port_out(0x04, SEQ_D);
#ifdef __alpha__
	port_out(0x06, GRA_I);
	port_out(0x00, GRA_D);
#endif
#if defined(CONFIG_ALPHA_JENSEN)
	slowcpy_to_sm(SM, font_buf1, FONT_SIZE);
#else
	slowcpy(GM, font_buf1, FONT_SIZE);
#endif

	/* restore font data in plane 3 - necessary for Trident VGA's */
	port_out(0x02, SEQ_I);
	port_out(0x08, SEQ_D);
#if defined(CONFIG_ALPHA_JENSEN)
	slowcpy_to_sm(SM, font_buf2, FONT_SIZE);
#else
	slowcpy(GM, font_buf2, FONT_SIZE);
#endif

	fontloadingcomplete();

	/* change register adresses if monochrome text mode */
	/* EGA is assumed to use color emulation. */
	if (!color_text) {
	    CRT_I = CRT_IM;
	    CRT_D = CRT_DM;
	    IS1_R = IS1_RM;
	    port_out(port_in(MIS_R) & 0xFE, MIS_W);
	}
	/* restore saved palette */
	restorepalette(text_red, text_green, text_blue);

	/* restore text mode VGA registers */
	__vga_setregs(text_regs);

	/* Set VMEM to some minimum value .. probably pointless.. */
	{
	    vga_claimvideomemory(12);
	}

/*      if (!flip) */
	/* enable text output - restores the screen contents */
	ioctl(__svgalib_tty_fd, KDSETMODE, KD_TEXT);

	usleep(MODESWITCHDELAY);	/* wait for signal to stabilize */

	/* enable video */
	vga_screenon();

	if (!flip)
	    /* restore text mode termio */
	    set_texttermio();
    } else {
	/* Setting a graphics mode. */

	/* disable text output */
	ioctl(__svgalib_tty_fd, KDSETMODE, KD_GRAPHICS);

	if (SVGAMODE(prv_mode)) {
	    /* The current mode is an SVGA mode, and we now want to */
	    /* set a standard VGA mode. Make sure the extended regs */
	    /* are restored. */
	    /* Also used when setting another SVGA mode to hopefully */
	    /* eliminate lock-ups. */
	    vga_setpage(0);
	    chipset_setregs(text_regs, mode);
	    /* restore old extended regs */
	}
	/* shift to color emulation */
	setcoloremulation();

	CI.xdim = infotable[mode].xdim;
	CI.ydim = infotable[mode].ydim;
	CI.colors = infotable[mode].colors;
	CI.xbytes = infotable[mode].xbytes;
	CI.bytesperpixel = infotable[mode].bytesperpixel;

	chipset_setmode(mode, prv_mode);

	MODEX = 0;

	/* Set default claimed memory (moved here from initialize - Michael.) */
	if (mode == G320x200x256)
	    VMEM = 65536;
	else if (STDVGAMODE(mode))
	    VMEM = 256 * 1024;	/* Why always 256K ??? - Michael */
	else {
	    vga_modeinfo *modeinfo;

	    modeinfo = vga_getmodeinfo(mode);
	    VMEM = modeinfo->linewidth * modeinfo->height;
	}

	if (!flip) {
	    /* set default palette */
	    if (CI.colors <= 256)
		restorepalette(default_red, default_green, default_blue);

	    /* clear screen (sets current color to 15) */
	    currentpage = -1;
	    vga_clear();

	    if (SVGAMODE(__svgalib_cur_mode))
		vga_setpage(0);
	}
	currentpage = -1;
	currentlogicalwidth = CI.xbytes;
	currentdisplaystart = 0;

	usleep(MODESWITCHDELAY);	/* wait for signal to stabilize */

	/* enable video */
	if (!flip)
	    vga_screenon();

	if (mouse_support) {
#ifdef DEBUG
	    printf("svgalib: Opening mouse (type = %x).\n", mouse_type | mouse_modem_ctl);
#endif
	    if (mouse_init("/dev/mouse", mouse_type | mouse_modem_ctl, MOUSE_DEFAULTSAMPLERATE))
		printf("svgalib: Failed to initialize mouse.\n");
	    else {
		/* vga_lockvc(); */
		mouse_setxrange(0, CI.xdim - 1);
		mouse_setyrange(0, CI.ydim - 1);
		mouse_setwrap(MOUSE_NOWRAP);
		mouse_mode = mode;
	    }
	} {
	    vga_modeinfo *modeinfo;
	    modeinfo = vga_getmodeinfo(mode);
	    MODEX = ((MODEFLAGS = modeinfo->flags) & IS_MODEX);
	}

	if (!flip)
	    /* set graphics mode termio */
	    set_graphtermio();
	else if (kbd_fd < 0)
	    enable_interrupt();
    }

/*    if (!flip)
   vga_unlockvc(); */

    return 0;
}

void vga_gettextfont(void *font)
{
    memcpy(font, font_buf1, FONT_SIZE);
}

void vga_puttextfont(void *font)
{
    memcpy(font_buf1, font, FONT_SIZE);
    memcpy(font_buf2, font, FONT_SIZE);
}

void vga_gettextmoderegs(void *regs)
{
    memcpy(regs, text_regs, MAX_REGS);
}

void vga_settextmoderegs(void *regs)
{
    memcpy(text_regs, regs, MAX_REGS);
}

int vga_getcurrentmode(void)
{
    return CM;
}

int vga_getcurrentchipset(void)
{
    return __vga_getchipset();
}

void vga_disabledriverreport(void)
{
    DREP = 0;
}

vga_modeinfo *vga_getmodeinfo(int mode)
{
    static vga_modeinfo modeinfo;
    int is_modeX = (CM == mode) && MODEX;

    if (mode > vga_lastmodenumber())
	return NULL;
    __vga_getchipset();
    modeinfo.width = infotable[mode].xdim;
    modeinfo.height = infotable[mode].ydim;
    modeinfo.bytesperpixel = infotable[mode].bytesperpixel;
    modeinfo.colors = infotable[mode].colors;
    if (is_modeX) {
	modeinfo.linewidth = modeinfo.width / 4;
	modeinfo.bytesperpixel = 0;
    } else
	modeinfo.linewidth = infotable[mode].xbytes;
    modeinfo.flags = 0;
    if (mode == TEXT)
	return &modeinfo;
    if ((STDVGAMODE(mode) && mode != G320x200x256) || is_modeX)
	vga_driverspecs.getmodeinfo(mode, &modeinfo);
    else
	/* Get chipset specific info for SVGA modes and */
	/* 320x200x256 (chipsets may support more pages) */
	chipset_getmodeinfo(mode, &modeinfo);

    if (modeinfo.colors == 256 && modeinfo.bytesperpixel == 0)
	modeinfo.flags |= IS_MODEX;
    if (mode > __GLASTMODE)
	modeinfo.flags |= IS_DYNAMICMODE;

    /* Maskout CAPABLE_LINEAR if requested by config file */
    modeinfo.flags &= modeinfo_mask;

    /* If all needed info is here, signal if linear support has been enabled */
    if ((modeinfo.flags & (CAPABLE_LINEAR | EXT_INFO_AVAILABLE)) ==
	(CAPABLE_LINEAR | EXT_INFO_AVAILABLE)) {
	modeinfo.flags |= __svgalib_modeinfo_linearset;
    }
    return &modeinfo;
}

int vga_hasmode(int mode)
{
    readconfigfile();		/* Make sure the config file is read. */
    __vga_getchipset();		/* Make sure the chipset is known. */
    if (mode == TEXT)
	return 1;
    if (mode < 0 || mode > lastmodenumber)
	return 0;
    return (chipset_modeavailable(mode) != 0);
}


int vga_lastmodenumber(void)
{
    __vga_getchipset();
    return lastmodenumber;
}


int __vga_addmode(int xdim, int ydim, int cols, int xbytes, int bytespp)
{
    int i;

    for (i = 0; i <= lastmodenumber; ++i)
	if (infotable[i].xdim == xdim &&
	    infotable[i].ydim == ydim &&
	    infotable[i].colors == cols &&
	    infotable[i].bytesperpixel == bytespp &&
	    infotable[i].xbytes == xbytes)
	    return i;
    if (lastmodenumber >= MAX_MODES - 1)
	return -1;		/* no more space available */
    ++lastmodenumber;
    infotable[lastmodenumber].xdim = xdim;
    infotable[lastmodenumber].ydim = ydim;
    infotable[lastmodenumber].colors = cols;
    infotable[lastmodenumber].xbytes = xbytes;
    infotable[lastmodenumber].bytesperpixel = bytespp;
    return lastmodenumber;
}


int vga_setcolor(int color)
{
    switch (CI.colors) {
    case 2:
	if (color != 0)
	    color = 15;
    case 16:			/* update set/reset register */
	port_out(0x00, GRA_I);
	port_out((color & 15), GRA_D);
	break;
    default:
	COL = color;
	break;
    }

    return 0;
}


int vga_screenoff(void)
{
    /* Dunno how to support that in MACH32 modes... */
    /* But it is used to switch the VGA off for better
       mem access performance... */
    /* turn off screen for faster VGA memory acces */
    if (CHIPSET != EGA) {
	port_out(0x01, SEQ_I);
	port_out(port_in(SEQ_D) | 0x20, SEQ_D);
    }
    /* Disable video output */
#ifdef DISABLE_VIDEO_OUTPUT
    port_in(IS1_R);
    __vga_delay();
    port_out(0x00, ATT_IW);
#endif

    SCREENON = 0;
    return 0;
}


int vga_screenon(void)
{
    /* Dunno how to support that in MACH32 modes... */
    if ((__svgalib_chipset == MACH32) && SVGAMODE(CM)) {
	/* Warning! This is also used for faking of vgapal.c! */
	/* Not needed anymore... had to hack vgapal.c anyway.. */
	/* Let it in anyway to show that std-vga is disabled */

	/* Anyway force Mach32 to ATI mode (used by setmode) */
	outw(0x4AEE, inw(0x4AEE) | 1);
	SCREENON = 0;
	return 0;
    }
    /* turn screen back on */
    if (CHIPSET != EGA) {
	port_out(0x01, SEQ_I);
	port_out(port_in(SEQ_D) & 0xDF, SEQ_D);
    }
/* #ifdef DISABLE_VIDEO_OUTPUT */
    /* enable video output */
    port_in(IS1_R);
    __vga_delay();
    port_out(0x20, ATT_IW);
/* #endif */

    SCREENON = 1;
    return 0;
}


int vga_getxdim(void)
{
    return CI.xdim;
}


int vga_getydim(void)
{
    return CI.ydim;
}


int vga_getcolors(void)
{
    return CI.colors;
}

int vga_white(void)
{
    switch (CI.colors) {
    case 2:
    case 16:
    case 256:
	return 15;
    case 1 << 15:
	return 32767;
    case 1 << 16:
	return 65535;
    case 1 << 24:
	return (1 << 24) - 1;
    }
    return CI.colors - 1;
}

int vga_claimvideomemory(int m)
{
    vga_modeinfo *modeinfo;
    int cardmemory;

    modeinfo = vga_getmodeinfo(CM);
    if (m < VMEM)
	return 0;
    if (modeinfo->colors == 16)
	cardmemory = modeinfo->maxpixels / 2;
    else
	cardmemory = (modeinfo->maxpixels * modeinfo->bytesperpixel
		      + 2) & 0xffff0000;
    /* maxpixels * bytesperpixel can be 2 less than video memory in */
    /* 3 byte-per-pixel modes; assume memory is multiple of 64K */
    if (m > cardmemory)
	return -1;
    VMEM = m;
    return 0;
}

int vga_setmodeX(void)
{
    switch (CM) {
    case TEXT:
/*    case G320x200x256: */
    case G320x240x256:
    case G320x400x256:
    case G360x480x256:
	return 0;
    }
    if (CI.colors == 256 && VMEM < 256 * 1024) {
	port_out(4, SEQ_I);	/* switch from linear to plane memory */
	port_out((port_in(SEQ_D) & 0xF7) | 0x04, SEQ_D);
	port_out(0x14, CRT_I);	/* switch double word mode off */
	port_out((port_in(CRT_D) & 0xBF), CRT_D);
	port_out(0x17, CRT_I);
	port_out((port_in(CRT_D) | 0x40), CRT_D);
	CI.xbytes = CI.xdim / 4;
	vga_setpage(0);
	MODEX = 1;
	return 1;
    }
    return 0;
}


static int saved_page;
static int saved_logicalwidth;
static int saved_displaystart;
static int saved_modeX;

static void savestate(void)
{
    int i;

    vga_screenoff();

    savepalette(graph_red, graph_green, graph_blue);

    saved_page = currentpage;
    saved_logicalwidth = currentlogicalwidth;
    saved_displaystart = currentdisplaystart;
    saved_modeX = MODEX;

    if (CM == G320x200x256 && VMEM <= 65536) {
	/* 320x200x256 is a special case; only 64K is addressable */
	/* (unless more has been claimed, in which case we assume */
	/* SVGA bank-switching) */
	if ((graph_buf = malloc(GRAPH_SIZE)) == NULL) {
	    printf("Cannot allocate memory for VGA state\n");
	    vga_setmode(TEXT);
	    exit(-1);
	}
	memcpy(graph_buf, GM, GRAPH_SIZE);
    } else if (MODEX || CM == G800x600x16 || (STDVGAMODE(CM) && CM != G320x200x256)) {
	/* for planar VGA modes, save the full 256K */
	vga_driverspecs.setmode(GPLANE16, prv_mode);
	if ((graph_buf = malloc(4 * GRAPH_SIZE)) == NULL) {
	    printf("Cannot allocate memory for VGA state\n");
	    vga_setmode(TEXT);
	    exit(-1);
	}
	for (i = 0; i < 4; i++) {
	    /* save plane i */
	    port_out(0x04, GRA_I);
	    port_out(i, GRA_D);
	    memcpy(graph_buf + i * GRAPH_SIZE, GM, GRAPH_SIZE);
	}
    } else if (CI.colors == 16) {
	int page, size, sbytes;
	unsigned char *sp;

	size = VMEM;
	if ((graph_buf = malloc(4 * size)) == NULL) {
	    printf("Cannot allocate memory for VGA state\n");
	    vga_setmode(TEXT);
	    exit(-1);
	}
	sp = graph_buf;
	for (page = 0; size > 0; ++page) {
	    vga_setpage(page);
	    sbytes = (size > GRAPH_SIZE) ? GRAPH_SIZE : size;
	    for (i = 0; i < 4; i++) {
		/* save plane i */
		port_out(0x04, GRA_I);
		port_out(i, GRA_D);
		memcpy(sp, GM, sbytes);
		sp += sbytes;
	    }
	    size -= sbytes;
	}
    } else {			/* SVGA, and SVGA 320x200x256 if videomemoryused > 65536 */
	int size;
	int page;

	size = VMEM;

#ifdef DEBUG
	printf("Saving %dK of video memory.\n", (size + 2) / 1024);
#endif
	if ((graph_buf = malloc(size)) == NULL) {
	    printf("Cannot allocate memory for SVGA state.\n");
	    vga_setmode(TEXT);
	    exit(-1);
	}
	page = 0;
	while (size >= 65536) {
	    vga_setpage(page);
	    memcpy(graph_buf + page * 65536, GM, 65536);
	    page++;
	    size -= 65536;
	}
	if (size > 0) {
	    vga_setpage(page);
	    memcpy(graph_buf + page * 65536, GM, size);
	}
    }
}

static void restorestate(void)
{
    int i;

    vga_screenoff();

    if (saved_modeX)
	vga_setmodeX();

    restorepalette(graph_red, graph_green, graph_blue);

    if (CM == G320x200x256 && VMEM <= 65536) {
	memcpy(GM, graph_buf, 65536);
    } else if (MODEX || CM == G800x600x16 || (STDVGAMODE(CM) && CM != G320x200x256)) {
	int setresetreg, planereg;
	/* disable Set/Reset Register */
	port_out(0x01, GRA_I);
	setresetreg = inb(GRA_D);
	port_out(0x00, GRA_D);
	outb(SEQ_I, 0x02);
	planereg = inb(SEQ_D);

	for (i = 0; i < 4; i++) {
	    /* restore plane i */
	    port_out(0x02, SEQ_I);
	    port_out(1 << i, SEQ_D);
	    memcpy(GM, graph_buf + i * GRAPH_SIZE, GRAPH_SIZE);
	}
	outb(GRA_I, 0x01);
	outb(GRA_D, setresetreg);
	outb(SEQ_I, 0x02);
	outb(SEQ_D, planereg);
    } else if (CI.colors == 16) {
	int page, size, rbytes;
	unsigned char *rp;
	int setresetreg, planereg;

	/* disable Set/Reset Register */
	port_out(0x01, GRA_I);
	if (CHIPSET == EGA)
	    setresetreg = 0;
	else
	    setresetreg = inb(GRA_D);
	port_out(0x00, GRA_D);
	port_out(0x02, SEQ_I);
	if (CHIPSET == EGA)
	    planereg = 0;
	else
	    planereg = inb(SEQ_D);

	size = VMEM;
	rp = graph_buf;
	for (page = 0; size > 0; ++page) {
	    vga_setpage(page);
	    rbytes = (size > GRAPH_SIZE) ? GRAPH_SIZE : size;
	    for (i = 0; i < 4; i++) {
		/* save plane i */
		port_out(0x02, SEQ_I);
		port_out(1 << i, SEQ_D);
		memcpy(GM, rp, rbytes);
		rp += rbytes;
	    }
	    size -= rbytes;
	}

	outb(GRA_I, 0x01);
	outb(GRA_D, setresetreg);
	outb(SEQ_I, 0x02);
	outb(SEQ_D, planereg);
    } else {
/*              vga_modeinfo *modeinfo; */
	int size;
	int page;
	size = VMEM;

#ifdef DEBUG
	printf("Restoring %dK of video memory.\n", (size + 2) / 1024);
#endif
	page = 0;
	while (size >= 65536) {
	    vga_setpage(page);
	    memcpy(GM, graph_buf + page * 65536, 65536);
	    size -= 65536;
	    page++;
	}
	if (size > 0) {
	    vga_setpage(page);
	    memcpy(GM, graph_buf + page * 65536, size);
	}
    }

    if (saved_logicalwidth != CI.xbytes)
	vga_setlogicalwidth(saved_logicalwidth);
    if (saved_page != 0)
	vga_setpage(saved_page);
    if (saved_displaystart != 0)
	vga_setdisplaystart(saved_displaystart);

    vga_screenon();

    free(graph_buf);
}


int vga_getch(void)
{
    char c;

    if (CM == TEXT)
	return -1;

    while ((read(__svgalib_tty_fd, &c, 1) < 0) && (errno == EINTR));

    return c;
}

/* I have kept the slightly funny 'flip' terminology. */

void __vga_flipaway(void)
{
    /* Leaving console. */
    flip_mode = CM;
    if (CM != TEXT) {
	/* wait for any blitter operation to finish */
	if (vga_getmodeinfo(CM)->haveblit & HAVE_BLITWAIT)
	    vga_blitwait();
	/* Save state and go to textmode. */
	savestate();
	flip = 1;
	vga_setmode(TEXT);
	flip = 0;
    }
}

static void vga_flipback(void)
{
    /* Entering console. */
    /* Hmmm... and how about unlocking anything someone else locked? */
    chipset_unlock();
    if (flip_mode != TEXT) {
	/* Restore graphics mode and state. */
	flip = 1;
	vga_setmode(flip_mode);
	flip = 0;
	restorestate();
    }
}

int vga_flip(void)
{
    if (CM != TEXT) {		/* save state and go to textmode */
	savestate();
	flip_mode = CM;
	flip = 1;
	vga_setmode(TEXT);
	flip = 0;
    } else {			/* restore graphics mode and state */
	flip = 1;
	vga_setmode(flip_mode);
	flip = 0;
	restorestate();
    }
    return 0;
}


int vga_setflipchar(int c)
/* This function is obsolete. Retained for VGAlib compatibility. */
{
    flipchar = c;

    return 0;
}

void vga_setlogicalwidth(int w)
{
    driverspecs->setlogicalwidth(w);
    currentlogicalwidth = w;
}

void vga_setdisplaystart(int a)
{
    currentdisplaystart = a;
    if (CHIPSET != VGA && CHIPSET != EGA)
	if (MODEX || CI.colors == 16) {
	    /* We are currently using a Mode X-like mode on a */
	    /* SVGA card, use the standard VGA function */
	    /* that works properly for Mode X. */
	    /* Same goes for 16 color modes. */
	    vga_driverspecs.setdisplaystart(a);
	    return;
	}
    /* Call the regular display start function for the chipset */
    driverspecs->setdisplaystart(a);
}

void vga_bitblt(int srcaddr, int destaddr, int w, int h, int pitch)
{
    driverspecs->bitblt(srcaddr, destaddr, w, h, pitch);
}

void vga_imageblt(void *srcaddr, int destaddr, int w, int h, int pitch)
{
    driverspecs->imageblt(srcaddr, destaddr, w, h, pitch);
}

void vga_fillblt(int destaddr, int w, int h, int pitch, int c)
{
    driverspecs->fillblt(destaddr, w, h, pitch, c);
}

void vga_hlinelistblt(int ymin, int n, int *xmin, int *xmax, int pitch,
		      int c)
{
    driverspecs->hlinelistblt(ymin, n, xmin, xmax, pitch, c);
}

void vga_blitwait(void)
{
    driverspecs->bltwait();
}

int vga_ext_set(unsigned what,...)
{
    va_list params;
    register int retval = 0;

    if (what == VGA_EXT_AVAILABLE) {
	/* Does this use of the arglist corrupt non-AVAIL_ACCEL ext_set? */
	va_start(params, what);
	switch (va_arg(params, int)) {
	case VGA_AVAIL_ACCEL:
	    if (driverspecs->accelspecs == NULL)
		return 0;
	    else
		return driverspecs->accelspecs->operations;
	case VGA_AVAIL_SET:
	    retval = 1 << VGA_EXT_PAGE_OFFSET;	/* This is handled by us */
	}
	va_end(params);
    } else if (what == VGA_EXT_PAGE_OFFSET) {
	/* Does this use of the arglist corrupt it? */
	va_start(params, what);
	retval = vga_page_offset;
	vga_page_offset = va_arg(params, int);
	va_end(params);
	return retval;
    }
    if (MODEFLAGS & HAVE_EXT_SET) {
	va_start(params, what);
	retval |= driverspecs->ext_set(what, params);
	va_end(params);
    }
    return retval;
}

/* Parse a string for options.. str is \0-terminated source,
   commands is an array of char ptrs (last one is NULL) containing commands
   to parse for. (if first char is ! case sensitive),
   func is called with ind the index of the detected command.
   func has to return the ptr to the next unhandled token returned by strtok(NULL," ").
   Use strtok(NULL," ") to get the next token from the file..
   mode is 1 when reading from conffile and 0 when parsing the env-vars. This is to
   allow disabling of dangerous (hardware damaging) options when reading the ENV-Vars
   of Joe user.
   Note: We use strtok, that is str is destroyed! */
static void parse_string(char *str, char **commands, char *(*func) (int ind, int mode), int mode)
{
    int index;
    register char *ptr, **curr;

    /*Pass one, delete comments,ensure only whitespace is ' ' */
    for (ptr = str; *ptr; ptr++) {
	if (*ptr == '#') {
	    while (*ptr && (*ptr != '\n')) {
		*ptr++ = ' ';
	    }
	    if (*ptr)
		*ptr = ' ';
	} else if (isspace(*ptr)) {
	    *ptr = ' ';
	}
    }
    /*Pass two, parse commands */
    ptr = strtok(str, " ");
    while (ptr) {
#ifdef DEBUG_CONF
	printf("Parsing: %s\n", ptr);
#endif
	for (curr = commands, index = 0; *curr; curr++, index++) {
#ifdef DEBUG_CONF
	    printf("Checking: %s\n", *curr);
#endif
	    if (**curr == '!') {
		if (!strcmp(*curr + 1, ptr)) {
		    ptr = (*func) (index, mode);
		    break;
		}
	    } else {
		if (!strcasecmp(*curr, ptr)) {
		    ptr = (*func) (index, mode);
		    break;
		}
	    }
	}
	if (!*curr)		/*unknow command */
	    ptr = strtok(NULL, " ");	/* skip silently til' next command */
    }
}

static int allowoverride = 0;	/* Allow dangerous options in ENV-Var */

/* This is a service function for drivers. commands and func are as above,
   the config file and (afterwards) the contents of the environment variable
   SVGALIB_CONFIG are parsed.. */
void __svgalib_read_options(char **commands, char *(*func) (int ind, int mode))
{
/* Parse configuration file. */
    FILE *f;
    char *buf = NULL, *ptr;
    struct stat st;
    int i;

    st.st_size = 0;		/*safety for readenv */

    f = fopen(SVGALIB_CONFIG_FILE, "r");
    if (f == NULL) {
	printf("svgalib: Configuration file %s not found.\n",
	       SVGALIB_CONFIG_FILE);
	goto readenv;
    }
    fstat(fileno(f), &st);	/* Some error analysis may be fine here.. */
    buf = alloca(st.st_size + 1);	/* + a final \0 */
    fread(buf, 1, st.st_size, f);
    fclose(f);
    /*Erase any maybe embedded \0 */
    for (i = 0, ptr = buf; i < st.st_size; i++, ptr++) {
	if (!*ptr)
	    *ptr = ' ';
    }
    /*Trailing \0 */
    *ptr = 0;
    /*parse config file */
    parse_string(buf, commands, func, 1);
  readenv:
    if (!(ptr = getenv("SVGALIB_CONFIG")))
	return;
    if (!(i = strlen(ptr)))
	return;
    if (i > st.st_size)		/* Note that we allocated one more char... */
	buf = alloca(i + 1);
    strcpy(buf, ptr);		/* Copy for safety and strtok!! */
    /*parse environment variable */
    parse_string(buf, commands, func, allowoverride);
}

/* Configuration file, mouse interface, initialization. */

static int configfileread = 0;	/* Boolean. */

static char *vga_conf_commands[] =
{
    "mouse", "monitor", "!m", "!M", "chipset", "overrideenable", "!m0", "!m1", "!m2", "!m3",
"!m4", "!m9", "!M0", "!M1", "!M2", "!M3", "!M4", "!M5", "!M6", "nolinear",
    "linear", "!C0", "!C1", "!C2", "!C3", "!C4", "!C5", "!C6", "!C7", "!C8", "!C9",
    "!c0", "!c1", "monotext", "colortext", "!m5",
    "leavedtr", "cleardtr", "setdtr", "leaverts", "clearrts",
    "setrts", "grayscale", "horizsync", "vertrefresh", "modeline",
    NULL};

static char *conf_mousenames[] =
{
  "Microsoft", "MouseSystems", "MMSeries", "Logitech", "Busmouse", "PS2",
    "MouseMan", NULL};

static int check_digit(char *ptr, char *digits)
{
    if (ptr == NULL)
	return 0;
    return strlen(ptr) == strspn(ptr, digits);
}

static char *process_option(int command, int mode)
{
    static char digits[] = ".0123456789";
    char *ptr, **tabptr, *ptb;
    int i, j;
    float f;

#ifdef DEBUG_CONF
    printf("command %d detected.\n", command);
#endif
    switch (command) {
    case 5:
#ifdef DEBUG_CONF
	puts("Allow override");
#endif
	if (mode)
	    allowoverride = 1;
	else
	    puts("Overrideenable denied. (Gee.. Do you think I'm that silly?)");
	break;
    case 0:			/* mouse */
    case 2:			/* m */
	ptr = strtok(NULL, " ");
	if (ptr == NULL)
	    goto inv_mouse;
	if (check_digit(ptr, digits + 1)) {	/* It is a number.. */
	    i = atoi(ptr);
	    if ((i != 9) && (i > MOUSE_LOGIMAN))
		goto inv_mouse;
	    mouse_type = i;
	} else {		/* parse for symbolic name.. */
	    if (!strcasecmp(ptr, "none")) {
		mouse_type = 9;
		break;
	    }
	    for (i = 0, tabptr = conf_mousenames; *tabptr; tabptr++, i++) {
		if (!strcasecmp(ptr, *tabptr)) {
		    mouse_type = i;
		    goto leave;
		}
	    }
	  inv_mouse:
	    printf("svgalib: Illegal mouse setting: {mouse|m} %s\n"
		   "Correct usage: {mouse|m} mousetype\n"
		   "where mousetype is one of 0, 1, 2, 3, 4, 5, 6, 9,\n",
		   (ptr != NULL) ? ptr : "");
	    for (tabptr = conf_mousenames; *tabptr; tabptr++) {
		printf("%s, ", *tabptr);
	    }
	    puts("or none.");
	    return ptr;		/* Allow a second parse of str */
	}
	break;
    case 1:			/* monitor */
    case 3:			/* M */
	ptr = strtok(NULL, " ");
	if (check_digit(ptr, digits + 1)) {	/* It is an int.. */
	    i = atoi(ptr);
	    if (i < 7) {
		command = i + 12;
		goto monnum;
	    } else {
		f = i;
		goto monkhz;
	    }
	} else if (check_digit(ptr, digits)) {	/* It is a float.. */
	    f = atof(ptr);
	  monkhz:
	    if (!mode)
		goto mon_deny;
	    __svgalib_horizsync.max = f * 1000.0f;
	} else {
	    printf("svgalib: Illegal monitor setting: {monitor|M} %s\n"
		   "Correct usage: {monitor|M} monitortype\n"
		   "where monitortype is one of 0, 1, 2, 3, 4, 5, 6, or\n"
		   "maximal horz. scan frequency in khz.\n"
		   "Example: monitor 36.5\n",
		   (ptr != NULL) ? ptr : "");
	    return ptr;		/* Allow a second parse of str */
	}
	break;
    case 4:			/* chipset */
	ptr = strtok(NULL, " ");
	if (ptr == NULL) {
	    puts("svgalib: Illegal chipset setting: no chipset given");
	    goto chip_us;
	}
	/*First param is chipset */
	for (i = 0, tabptr = driver_names; *tabptr; tabptr++, i++) {
	    if ((!strcasecmp(ptr, *tabptr)) && (driverspecslist[i] != NULL)) {
		ptr = strtok(NULL, " ");
		if (check_digit(ptr, digits + 1)) {
		    j = atoi(ptr);
		    ptr = strtok(NULL, " ");
		    if (check_digit(ptr, digits + 1)) {
			if (mode)
			    vga_setchipsetandfeatures(i, j, atoi(ptr));
			else {
			  chipdeny:
			    puts("chipset override from environment denied.");
			}
			return strtok(NULL, " ");
		    } else {
			puts("svgalib: Illegal chipset setting: memory is not a number");
			goto chip_us;
		    }
		}
		if (mode)
		    vga_setchipset(i);
		else
		    puts("chipset override from environment denied.");
		return ptr;
	    }
	}
	printf("svgalib: Illegal chipset setting: chipset %s\n", ptr);
      chip_us:
	puts("Correct usage: chipset driver [par1 par2]\n"
	     "where driver is one of:");
	ptb = "%s";
	for (i = 0, tabptr = driver_names; *tabptr; tabptr++, i++) {
	    if (driverspecslist[i] != NULL) {
		printf(ptb, *tabptr);
		ptb = ", %s";
	    }
	}
	puts("\npar1 and par2 are river dependant integers.\n"
	     "Example: Chipset VGA    or\n"
	     "Chipset VGA 0 512");
	return ptr;
    case 6:			/* oldstyle config: m0-m4 */
    case 7:
    case 8:
    case 9:
    case 10:
	mouse_type = command - 6;
	break;
    case 11:			/* m9 */
	mouse_type = 9;
	break;
    case 12:			/* oldstyle config: M0-M6 */
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 18:
      monnum:
	if (!mode) {
	  mon_deny:
	    puts("Monitor setting from environment denied.");
	    break;
	} else {
	    __svgalib_horizsync.max = __svgalib_maxhsync[command - 12];
	}
	break;
    case 19:			/*nolinear */
	modeinfo_mask &= ~CAPABLE_LINEAR;
	break;
    case 20:			/*linear */
	modeinfo_mask |= CAPABLE_LINEAR;
	break;
    case 21:			/* oldstyle chipset C0 - C9 */
    case 22:
    case 23:
    case 24:
    case 25:
    case 26:
    case 27:
    case 28:
    case 29:
    case 30:
	if (!mode)
	    goto chipdeny;
	vga_setchipset(command - 21);
	break;
    case 31:			/* c0-c1 color-text selection */
	if (!mode) {
	  coltexdeny:
	    puts("Color/mono text selection from environment denied.");
	    break;
	}
	color_text = 0;
	break;
    case 32:
	if (!mode) {
	    puts("Color/mono text selection from environment denied.");
	    break;
	}
	color_text = 1;
	break;
    case 33:
    case 34:
	if (!mode)
	    goto coltexdeny;
	color_text = command - 32;
	break;
    case 35:			/* Mouse type 5 - "PS2". */
	mouse_type = 5;
	break;
    case 36:
	mouse_modem_ctl &= ~(MOUSE_CHG_DTR | MOUSE_DTR_HIGH);
	break;
    case 37:
	mouse_modem_ctl &= ~MOUSE_DTR_HIGH;
	mouse_modem_ctl |= MOUSE_CHG_DTR;
	break;
    case 38:
	mouse_modem_ctl |= (MOUSE_CHG_RTS | MOUSE_RTS_HIGH);
	break;
    case 39:
	mouse_modem_ctl &= ~(MOUSE_CHG_RTS | MOUSE_RTS_HIGH);
	break;
    case 40:
	mouse_modem_ctl &= ~MOUSE_RTS_HIGH;
	mouse_modem_ctl |= MOUSE_CHG_RTS;
	break;
    case 41:
	mouse_modem_ctl |= (MOUSE_CHG_RTS | MOUSE_RTS_HIGH);
	break;
    case 42:			/* grayscale */
	__svgalib_grayscale = 1;
	break;
    case 43:			/* horizsync */
	ptr = strtok(NULL, " ");
	if (check_digit(ptr, digits)) {		/* It is a float.. */
	    f = atof(ptr);
	    if (!mode)
		goto mon_deny;
	    __svgalib_horizsync.min = f * 1000;
	} else
	    goto hs_bad;

	ptr = strtok(NULL, " ");
	if (check_digit(ptr, digits)) {		/* It is a float.. */
	    f = atof(ptr);
	    if (!mode)
		goto mon_deny;
	    __svgalib_horizsync.max = f * 1000;
	} else {
	  hs_bad:
	    printf("svgalib: Illegal HorizSync setting.\n"
		   "Correct usage: HorizSync min_kHz max_kHz\n"
		   "Example: HorizSync 31.5 36.5\n");
	}
	break;
    case 44:			/* vertrefresh */
	ptr = strtok(NULL, " ");
	if (check_digit(ptr, digits)) {		/* It is a float.. */
	    f = atof(ptr);
	    if (!mode)
		goto mon_deny;
	    __svgalib_vertrefresh.min = f;
	} else
	    goto vr_bad;

	ptr = strtok(NULL, " ");
	if (check_digit(ptr, digits)) {		/* It is a float.. */
	    f = atof(ptr);
	    if (!mode)
		goto mon_deny;
	    __svgalib_vertrefresh.max = f;
	} else {
	  vr_bad:
	    printf("svgalib: Illegal VertRefresh setting.\n"
		   "Correct usage: VertRefresh min_Hz max_Hz\n"
		   "Example: VertRefresh 50 70\n");
	}
	break;
    case 45:{			/* modeline */
	    MonitorModeTiming mmt;
	    const struct {
		char *name;
		int val;
	    } options[] = {
		{
		    "-hsync", NHSYNC
		},
		{
		    "+hsync", PHSYNC
		},
		{
		    "-vsync", NVSYNC
		},
		{
		    "+vsync", PVSYNC
		},
		{
		    "interlace", INTERLACED
		},
		{
		    "interlaced", INTERLACED
		}
	    };
#define ML_NR_OPTS (sizeof(options)/sizeof(*options))

	    /* Skip the name of the mode */
	    ptr = strtok(NULL, " ");
	    if (!ptr)
		break;

	    ptr = strtok(NULL, " ");
	    if (!ptr)
		break;
	    mmt.pixelClock = atof(ptr) * 1000;

#define ML_GETINT(x) \
	ptr = strtok(NULL, " "); if(!ptr) break; \
	mmt.##x = atoi(ptr);

	    ML_GETINT(HDisplay);
	    ML_GETINT(HSyncStart);
	    ML_GETINT(HSyncEnd);
	    ML_GETINT(HTotal);
	    ML_GETINT(VDisplay);
	    ML_GETINT(VSyncStart);
	    ML_GETINT(VSyncEnd);
	    ML_GETINT(VTotal);
	    mmt.flags = 0;
	    while ((ptr = strtok(NULL, " "))) {
		for (i = 0; i < ML_NR_OPTS; i++)
		    if (!strcasecmp(ptr, options[i].name))
			mmt.flags |= options[i].val;
		if (i == ML_NR_OPTS)
		    break;
	    }
#undef ML_GETINT
#undef ML_NR_OPTS

	    addusertiming(&mmt);
	    return ptr;
	}
    }
  leave:
    return strtok(NULL, " ");
}

static void readconfigfile(void)
{
    if (configfileread)
	return;
    configfileread = 1;
    mouse_type = -1;
    __svgalib_read_options(vga_conf_commands, process_option);
    if (mouse_type == -1) {
	mouse_type = MOUSE_MICROSOFT;	/* Default. */
	puts("svgalib: Assuming Microsoft mouse.");
    }
    if (__svgalib_horizsync.max == 0U) {
	/* Default monitor is low end SVGA/8514. */
	__svgalib_horizsync.min = 31500U;
	__svgalib_horizsync.max = 35500U;
	puts("svgalib: Assuming low end SVGA/8514 monitor (35.5 KHz).");
    }
#ifdef DEBUG_CONF
    printf("Mouse is: %d Monitor is: H(%5.1f, %5.1f) V(%u,%u)\n", mouse_type,
      __svgalib_horizsync.min / 1000.0, __svgalib_horizsync.max / 1000.0,
	   __svgalib_vertrefresh.min, __svgalib_vertrefresh.max);
#endif
}

int vga_getmousetype(void)
{
    readconfigfile();
    return mouse_type | mouse_modem_ctl;
}

int vga_getmonitortype(void)
{				/* obsolete */
    int i;
    readconfigfile();
    for (i = 1; i <= MON1024_72; i++)
	if (__svgalib_horizsync.max < __svgalib_maxhsync[i])
	    return i - 1;

    return MON1024_72;
}

void vga_setmousesupport(int s)
{
    mouse_support = s;
}

void vga_lockvc(void)
{
    lock_count++;
    if (flip)
	__svgalib_waitvtactive();
}

void vga_unlockvc(void)
{
    if (--lock_count <= 0) {
	lock_count = 0;
	if (release_flag) {
	    release_flag = 0;
	    releasevt_signal(SIGUSR1);
	}
    }
}

void vga_runinbackground(int stat)
{
    runinbackground = stat;
}

int vga_oktowrite(void)
{
    return oktowrite;
}

int vga_init(void)
{
    open_devconsole();
    if (!checkconsole()) {
	/* Return with error code. */
	seteuid(getuid());
	setegid(getgid());
	return -1;
    }
    readconfigfile();
    vga_hasmode(TEXT);		/* Force driver message and initialization. */
    seteuid(getuid());		/* Don't need supervisor rights anymore. */
    setegid(getgid());
    return 0;
}

#ifdef __alpha__

#define vuip	volatile unsigned int *

extern void sethae(unsigned long hae);

static struct hae hae;


int iopl(int level)
{
    return 0;
}

unsigned long vga_readb(unsigned long base, unsigned long off)
{
    unsigned long result, shift;
#if !defined(CONFIG_ALPHA_JENSEN)
    unsigned long msb;
#endif

    shift = (off & 0x3) * 8;
#if !defined(CONFIG_ALPHA_JENSEN)
    if (off >= (1UL << 24)) {
	msb = off & 0xf8000000;
	off -= msb;
	if (msb && msb != hae.cache) {
	    sethae(msb);
	}
    }
#endif
    result = *(vuip) ((off << MEM_SHIFT) + base + MEM_TYPE_BYTE);
    result >>= shift;
    return 0xffUL & result;
}

unsigned long vga_readw(unsigned long base, unsigned long off)
{
    unsigned long result, shift;
#if !defined(CONFIG_ALPHA_JENSEN)
    unsigned long msb;
#endif

    shift = (off & 0x3) * 8;
#if !defined(CONFIG_ALPHA_JENSEN)
    if (off >= (1UL << 24)) {
	msb = off & 0xf8000000;
	off -= msb;
	if (msb && msb != hae.cache) {
	    sethae(msb);
	}
    }
#endif
    result = *(vuip) ((off << MEM_SHIFT) + base + MEM_TYPE_WORD);
    result >>= shift;
    return 0xffffUL & result;
}

#if defined(CONFIG_ALPHA_JENSEN)
unsigned long vga_readl(unsigned long base, unsigned long off)
{
    unsigned long result;
    result = *(vuip) ((off << MEM_SHIFT) + base + MEM_TYPE_LONG);
    return 0xffffffffUL & result;
}
#endif

void vga_writeb(unsigned char b, unsigned long base, unsigned long off)
{
#if !defined(CONFIG_ALPHA_JENSEN)
    unsigned long msb;
    unsigned int w;

    if (off >= (1UL << 24)) {
	msb = off & 0xf8000000;
	off -= msb;
	if (msb && msb != hae.cache) {
	    sethae(msb);
	}
    }
  asm("insbl %2,%1,%0": "r="(w):"ri"(off & 0x3), "r"(b));
    *(vuip) ((off << MEM_SHIFT) + base + MEM_TYPE_BYTE) = w;
#else
    *(vuip) ((off << MEM_SHIFT) + base + MEM_TYPE_BYTE) = b * 0x01010101;
#endif
}

void vga_writew(unsigned short b, unsigned long base, unsigned long off)
{
#if !defined(CONFIG_ALPHA_JENSEN)
    unsigned long msb;
    unsigned int w;

    if (off >= (1UL << 24)) {
	msb = off & 0xf8000000;
	off -= msb;
	if (msb && msb != hae.cache) {
	    sethae(msb);
	}
    }
  asm("inswl %2,%1,%0": "r="(w):"ri"(off & 0x3), "r"(b));
    *(vuip) ((off << MEM_SHIFT) + base + MEM_TYPE_WORD) = w;
#else
    *(vuip) ((off << MEM_SHIFT) + base + MEM_TYPE_WORD) = b * 0x00010001;
#endif
}

#if defined(CONFIG_ALPHA_JENSEN)
void vga_writel(unsigned long b, unsigned long base, unsigned long off)
{
    *(vuip) ((off << MEM_SHIFT) + base + MEM_TYPE_LONG) = b;
}

#endif

#endif
