
/*
 * This is only a tool to test the acceleration primitives.
 * Do not use the primitives as library graphics functions;
 * have higher level functions make use of acceleration
 * primitives if available, otherwise using normal framebuffer code.
 * For example, the vgagl should use acceleration when it can.
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include "vga.h"
#include "vgagl.h"


int vgamode;
int background_blits;
unsigned char *bitmap;


void Configure(void);
void DrawDots(void);
void DoAccelTest(void (*accelfunc) (void), char *name, int exp_pixelrate,
		 int pixels_per_call);
void FillBoxTest(void);
void ScreenCopyTest(void);
void ScrollTest(void);
void FillBoxXORTest(void);
void PutBitmapTest(void);


void main(void)
{
    int accelfuncs;
    vga_init();

    Configure();

    /* Getting accel info only works if the mode it set. */
    vga_setmode(vgamode);

    accelfuncs = vga_ext_set(VGA_EXT_AVAILABLE, VGA_AVAIL_ACCEL);

    usleep(100000);
    vga_setmode(TEXT);

    if (accelfuncs == 0) {
	printf("No acceleration supported.\n");
	exit(0);
    }
    printf("Accelflags: 0x%08X\n", accelfuncs);

    if (accelfuncs & ACCELFLAG_FILLBOX)
	printf("FillBox supported.\n");
    if (accelfuncs & ACCELFLAG_SCREENCOPY)
	printf("ScreenCopy supported.\n");
    if (accelfuncs & ACCELFLAG_PUTIMAGE)
	printf("PutImage supported.\n");
    if (accelfuncs & ACCELFLAG_DRAWLINE)
	printf("DrawLine.\n");
    if (accelfuncs & ACCELFLAG_SETFGCOLOR)
	printf("SetFGColor supported.\n");
    if (accelfuncs & ACCELFLAG_SETBGCOLOR)
	printf("SetBGColor supported.\n");
    if (accelfuncs & ACCELFLAG_SETTRANSPARENCY)
	printf("SetTransparency supported.\n");
    if (accelfuncs & ACCELFLAG_SETRASTEROP)
	printf("SetRasterOp supported.\n");
    if (accelfuncs & ACCELFLAG_PUTBITMAP)
	printf("PutBitmap supported.\n");
    if (accelfuncs & ACCELFLAG_SCREENCOPYBITMAP)
	printf("ScreenCopyBitmap supported.\n");
    if (accelfuncs & ACCELFLAG_DRAWHLINELIST)
	printf("DrawHLineList supported.n");
    if (accelfuncs & ACCELFLAG_SETMODE)
	printf("SetMode supported.\n");
    if (accelfuncs & ACCELFLAG_SYNC)
	printf("Sync supported.\n");

    printf("\nPress enter to start test.\n");
    getchar();

    vga_setmode(vgamode);
    gl_setcontextvga(vgamode);

    background_blits = 0;
    if (accelfuncs & ACCELFLAG_SYNC)
	background_blits = 1;

    if (accelfuncs & ACCELFLAG_FILLBOX)
	DoAccelTest(
		       FillBoxTest,
		       "FillBox",
		       50000000 / BYTESPERPIXEL,
		       WIDTH * HEIGHT * 256
	    );

    if (accelfuncs & ACCELFLAG_SCREENCOPY) {
	DrawDots();
	DoAccelTest(
		       ScreenCopyTest,
		       "ScreenCopy",
		       20000000 / BYTESPERPIXEL,
		       WIDTH * (HEIGHT / 3) * 3
	    );
    }
    if ((accelfuncs & ACCELFLAG_SCREENCOPY)
	&& (accelfuncs & ACCELFLAG_FILLBOX)) {
	DrawDots();
	DoAccelTest(
		       ScrollTest,
		       "Scroll Demo",
		       20000000 / BYTESPERPIXEL,
		       WIDTH * HEIGHT
	    );
    }
    if ((accelfuncs & ACCELFLAG_FILLBOX)
	&& (accelfuncs & ACCELFLAG_SETRASTEROP)) {
	DrawDots();
	DoAccelTest(
		       FillBoxXORTest,
		       "FillBox XOR",
		       30000000 / BYTESPERPIXEL,
		       WIDTH * HEIGHT * 256
	    );
    }
    if (accelfuncs & ACCELFLAG_PUTBITMAP) {
	bitmap = malloc(WIDTH * HEIGHT / 8);
	memset(bitmap, 0x55, WIDTH * HEIGHT / 8);
	DrawDots();
	DoAccelTest(
		       PutBitmapTest,
		       "PutBitmap",
		       30000000 / BYTESPERPIXEL,
		       WIDTH * HEIGHT * 2
	    );
    }
    vga_setmode(TEXT);
    exit(-1);
}


void Configure(void)
{
    int allowed[GLASTMODE + 1];

    for (;;) {
	int i;
	int m;
	for (i = G320x200x16; i <= GLASTMODE; i++) {
	    allowed[i] = 0;
	    if (vga_hasmode(i)
		&& vga_getmodeinfo(i)->bytesperpixel >= 1) {
		printf("%2d  %s\n", i, vga_getmodename(i));
		allowed[i] = 1;
	    }
	}

	printf("\nWhich mode? ");
	scanf("%d", &m);
	getchar();
	printf("\n");
	if (m >= G320x200x16 && m <= GLASTMODE) {
	    vgamode = m;
	    break;
	}
    }
}

void DrawDots(void)
{
    int i, n;
    /* Fill the screen with random dots. */
    gl_clearscreen(0);
    n = WIDTH * HEIGHT / 100;	/* 1% filled. */
    for (i = 0; i < n; i++)
	gl_setpixel(rand() % WIDTH, rand() % HEIGHT, rand() % COLORS);
}

void DoAccelTest(void (*accelfunc) (void), char *name, int exp_pixelrate,
		 int pixels_per_call)
{
    int i, n, startclock, diffclock;
    int rate, byterate;
    if (exp_pixelrate < 0)
	/* Special case, #iterations indicated. */
	n = -exp_pixelrate;
    else
	/* Aim for about 5 seconds. */
	n = exp_pixelrate * 5 / pixels_per_call + 1;
    if (background_blits)
	vga_accel(ACCEL_SETMODE, BLITS_IN_BACKGROUND);
    startclock = clock();
    for (i = 0; i < n; i++)
	(*accelfunc) ();
    if (background_blits)
	vga_accel(ACCEL_SYNC);
    diffclock = clock() - startclock;
    rate = (long long) n *pixels_per_call * 100 / diffclock;
    byterate = rate * BYTESPERPIXEL;
    printf("%s: %ld.%ld Mpixels/s (%ld.%ld Mbytes/s)\n", name,
	   rate / 1000000L, (rate % 1000000L) / 100000L,
	   byterate / 1000000L, (byterate % 1000000L) / 100000L);
}

void FillBoxTest(void)
{
    int i;
    for (i = 0; i < 256; i++) {
	vga_accel(ACCEL_SETFGCOLOR, i);
	vga_accel(ACCEL_FILLBOX, 0, 0, WIDTH, HEIGHT);
    }
}

void ScreenCopyTest(void)
{
    /* Copy first 1/3 to second 1/3. */
    vga_accel(ACCEL_SCREENCOPY, 0, 0, 0, HEIGHT / 3, WIDTH, HEIGHT / 3);
    /* Copy third 1/3 to first 1/3. */
    vga_accel(ACCEL_SCREENCOPY, 0, (HEIGHT / 3) * 2, 0, 0,
	      WIDTH, HEIGHT / 3);
    /* Copy second 1/3 to third 1/3. */
    vga_accel(ACCEL_SCREENCOPY, 0, HEIGHT / 3, 0, (HEIGHT / 3) * 2,
	      WIDTH, HEIGHT / 3);
}

void ScrollTest(void)
{
    int i;
    /*
     * First write the line that will be scrolled into the screen,
     * located after the visible screen.
     */
    /* Clear the line. */
    vga_accel(ACCEL_SETFGCOLOR, 0);
    vga_accel(ACCEL_FILLBOX, 0, HEIGHT, WIDTH, 1);
    if (background_blits)
	vga_accel(ACCEL_SYNC);
    /* Write some new dots. */
    for (i = 0; i < WIDTH / 100; i++)
	gl_setpixel(rand() % WIDTH, HEIGHT, rand() % COLORS);
    /* Scroll. */
    vga_accel(ACCEL_SCREENCOPY, 0, 1, 0, 0,
	      WIDTH, HEIGHT);
}

void FillBoxXORTest(void)
{
    vga_accel(ACCEL_SETRASTEROP, ROP_XOR);
    FillBoxTest();
    vga_accel(ACCEL_SETRASTEROP, ROP_COPY);
}

void PutBitmapTest(void)
{
    vga_accel(ACCEL_SETBGCOLOR, 0);
    vga_accel(ACCEL_SETFGCOLOR, 99);
    vga_accel(ACCEL_PUTBITMAP, 0, 0, WIDTH, HEIGHT, bitmap);
    vga_accel(ACCEL_SETFGCOLOR, 111);
    vga_accel(ACCEL_PUTBITMAP, 0, 0, WIDTH, HEIGHT, bitmap);
}
