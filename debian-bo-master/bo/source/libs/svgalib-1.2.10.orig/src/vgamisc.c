/* VGAlib version 1.2 - (c) 1993 Tommy Frandsen                    */
/*                                                                 */
/* This library is free software; you can redistribute it and/or   */
/* modify it without any restrictions. This library is distributed */
/* in the hope that it will be useful, but without any warranty.   */

/* Multi-chipset support Copyright 1993 Harm Hanemaayer */
/* partially copyrighted (C) 1993 by Hartmut Schirmer */

#include <stdlib.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/time.h>

#include "vga.h"
#include "libvga.h"
#include "driver.h"
#include "mouse/vgamouse.h"
#include "keyboard/vgakeyboard.h"

/* This routine is a pain in my ass and definitely needs to be virtualized..
   M. Weller. (mach32 support) */
void vga_waitretrace(void)
{
    if ((__svgalib_chipset == MACH32) && SVGAMODE(CM)) {
	unsigned char csync;

	csync = inb(0xd2ee) >> 3;	/* Current sync polarity in bit 2,
					   0-pos, 1-neg  */
	/*Wait until in displayed area.. */
	while ((inb(0x2e8) ^ csync) & 2);
	/*Wait until V_SYNC */
	csync = ~csync;
	while ((inb(0x2e8) ^ csync) & 2);
	/* Hmm, is a return missing here? -- HH */
	/* Umm.. *scratch head* Certainly.. -- MW (mach32) */
	return;
    }
    while (!(inb(0x3da) & 8));
    while (inb(0x3da) & 8);
}

static void *linearframebuffer;

/*
 * The way IS_LINEAR gets indicated is rather convoluted; if the driver
 * has EXT_INFO_AVAILABLE, setlinearaddressing will enable
 * the flag in __svgalib_linearset which gets set in the modeinfo by
 * vga_getmodeinfo(). The driver must turn off the flag in
 * __svgalib_linearset if linear addressing gets disabled (e.g. when
 * setting another mode).
 * 
 * For any driver, the chipset getmodeinfo flag can examine a hardware
 * register and set the IS_LINEAR flag if linear addressing is enabled.
 */

unsigned char *
 vga_getgraphmem(void)
{
    if (vga_getmodeinfo(__svgalib_cur_mode)->flags & IS_LINEAR)
	return linearframebuffer;
    return __svgalib_graph_mem;
}

#include <syscall.h>
#include <linux/kernel.h>

int __svgalib_physmem(void)
{
#ifdef __alpha__
    printf("__svgalib_physmem: are you sure you wanna do this??\n");
    return -1;
#else
    struct sysinfo si;
    si.totalram = 0;
    syscall(SYS_sysinfo, &si);
    return si.totalram;
#endif
}

static unsigned char *
 map_framebuffer(unsigned address, int size)
{
    int mem_fd;
    unsigned char *fb;
    mem_fd = open("/dev/mem", O_RDWR);
    fb = valloc(size);
    return mmap(
		   (caddr_t) fb,
		   size,
		   PROT_READ | PROT_WRITE,
		   MAP_FIXED | MAP_SHARED,
		   mem_fd,
		   (off_t) address
	);
    close(mem_fd);
    /* Using addr 0 (kernel comes up with 0x40000000), 
       and only MAP_SHARED works, but wastes 2Mb 
       of memory. */
}

/*
 * This function is to be called after a SVGA graphics mode set
 * in banked mode. Probing in VGA-compatible textmode is not a good
 * idea.
 */

static int verify_one_linear_mapping(int base, int size)
{
    unsigned char *fb;
    int i, result;
    int (*lfn) (int op, int param) = driverspecs->linear;
    /* Write signatures. */
    for (i = 0; i < size / 65536; i++) {
	vga_setpage(i);
	gr_writel(i, 0);
    }
    vga_setpage(0);
    /* Enable the linear mapping on the card. */
    (*lfn) (LINEAR_ENABLE, base);
    /* Let the OS map it. */
    fb = map_framebuffer(base, size);
    if ((long) fb == -1 || fb == NULL) {
	(*lfn) (LINEAR_DISABLE, base);
	result = -1;
	goto cleanupsignatures;
    }
    /* Verify framebuffer signatures. */
    for (i = 0; i < size / 65536; i++) {
	unsigned long data;
	data = *(unsigned long *) ((unsigned char *) fb + i * 65536);
	if (data != i) {
	    munmap((caddr_t) fb, size);
	    (*lfn) (LINEAR_DISABLE, base);
	    result = -1;
#if 0
	    printf("svgalib: Signature mismatch at 0x%08X (0x%08X), 0x%08X != 0x%08X.\n",
		   fb + i * 65536, base + i * 65536, data, i);
#endif
	    goto cleanupsignatures;
	}
    }
    /* Linear framebuffer is OK. */
    printf("svgalib: Found linear framebuffer at 0x%08X.\n", base);
    result = 0;
    munmap((caddr_t) fb, size);
    (*lfn) (LINEAR_DISABLE, base);
  cleanupsignatures:
    /* Clean up the signatures (write zeroes). */
    for (i = 0; i < size / 65536; i++) {
	vga_setpage(i);
	gr_writel(0, 0);
    }
    vga_setpage(0);
    return result;
}


static int verify_linear_mapping(int base, int size)
{
    while (size > 256 * 256) {
	if (verify_one_linear_mapping(base, size) == 0)
	    return size;
	size >>= 1;
    }
    return -1;
}


/* cf. vga_waitretrace, M.Weller */
int vga_setlinearaddressing(void)
{
    int (*lfn) (int op, int param) = driverspecs->linear;
    int memory, mappedMemory;
    vga_modeinfo *modeinfo;
    modeinfo = vga_getmodeinfo(__svgalib_cur_mode);
    if (modeinfo->flags & EXT_INFO_AVAILABLE)
	memory = modeinfo->memory;
    else
	memory = (modeinfo->bytesperpixel * modeinfo->maxpixels
		  + 0xFFF) & 0xFFFFF000;	/* Round up 4K. */
    mappedMemory = memory;

    if (lfn) {
	/* 'Linear' driver function available. */
	unsigned long mapaddr;
	int i;
#ifndef __alpha__
	unsigned long gran, maxmap;
	int range;
#endif
	/* First try the fixed bases that the driver reports. */
	i = 0;
	for (;; i++) {
	    mapaddr = (*lfn) (LINEAR_QUERY_BASE, i);
	    if ((int) mapaddr == -1)
		break;
#ifndef __alpha__
	    if (mapaddr <= __svgalib_physmem())
		continue;
#endif
	    /* Check that the linear framebuffer is writable. */
	    if ((mappedMemory = verify_linear_mapping(
						 mapaddr, memory)) != -1)
		goto foundlinearframebuffer;
	}
#ifndef __alpha__
	/* Attempt mapping for device that maps in low memory range. */
	gran = (*lfn) (LINEAR_QUERY_GRANULARITY, 0);
	range = (*lfn) (LINEAR_QUERY_RANGE, 0);
	if (range == 0)
	    return -1;
	/* Place it immediately after the physical memory. */
	mapaddr = (__svgalib_physmem() + (gran + gran - 1)) & ~(gran - 1);
	maxmap = (range - 1) * gran;
	if (mapaddr > maxmap) {
	    puts("svgalib: Too much physical memory, cannot map aperture\n");
	    return -1;
	}
	if ((mappedMemory = verify_linear_mapping(mapaddr, memory)) == -1)
	    return -1;
#endif

      foundlinearframebuffer:
	(*lfn) (LINEAR_ENABLE, mapaddr);
	linearframebuffer = map_framebuffer(mapaddr, mappedMemory);
	if ((long) linearframebuffer == -1) {
	    /* Shouldn't happen. */
	    (*lfn) (LINEAR_DISABLE, mapaddr);
	    return -1;
	}
	__svgalib_modeinfo_linearset |= IS_LINEAR;

	if (memory != mappedMemory)
	    printf("svgalib: Warning, card has %dK, only %dK available in linear mode.\n",
		   memory >> 10, mappedMemory >> 10);
	return mappedMemory;
    }
    if ((modeinfo->flags & (CAPABLE_LINEAR | EXT_INFO_AVAILABLE)) ==
	(CAPABLE_LINEAR | EXT_INFO_AVAILABLE)) {
	if (modeinfo->aperture_size >= modeinfo->memory) {
	    __svgalib_modeinfo_linearset |= IS_LINEAR;
	    linearframebuffer = modeinfo->linear_aperture;
	    return modeinfo->memory;
	}
    }
    return -1;
}

#if 1

/*
 * The other code doesn't work under Linux/Alpha (I think
 * it should).  For now, this is a quick work-around).
 */

int vga_getkey(void)
{
    struct timeval tv;
    fd_set fds;
    int fd = fileno(stdin);
    char c;

    tv.tv_sec = tv.tv_usec = 0;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    if (select(fd + 1, &fds, 0, 0, &tv) > 0) {
	if (read(fileno(stdin), &c, 1) != 1) {
	    return 0;
	}
	return c;
    }
    return 0;
}

#else

int vga_getkey(void)
{
    struct termio zap, original;
    int e;
    char c;

    ioctl(fileno(stdin), TCGETA, &original);	/* Get termio */
    zap = original;
    zap.c_cc[VMIN] = 0;		/* Modify termio  */
    zap.c_cc[VTIME] = 0;
    zap.c_lflag = 0;
    ioctl(fileno(stdin), TCSETA, &zap);		/* Set new termio */
    e = read(fileno(stdin), &c, 1);	/* Read one char */
    ioctl(fileno(stdin), TCSETA, &original);	/* Restore termio */
    if (e != 1)
	return 0;		/* No key pressed. */
    return c;			/* Return key. */
}

#endif

int vga_waitevent(int which, fd_set * in, fd_set * out, fd_set * except,
		  struct timeval *timeout)
{
    fd_set infdset;
    int fd, retval;

    if (!in) {
	FD_ZERO(in);
	in = &infdset;
    }
    fd = __svgalib_mouse_fd;	/* __svgalib_mouse_fd might change on
				   vc switch!! */
    if ((which & VGA_MOUSEEVENT) && (fd >= 0))
	FD_SET(fd, in);
    if (which & VGA_KEYEVENT) {
	fd = kbd_fd;
	if (fd >= 0) {		/* we are in raw mode */
	    FD_SET(fd, in);
	} else {
	    FD_SET(__svgalib_tty_fd, in);
	}
    }
    if (select(FD_SETSIZE, in, out, except, timeout) < 0)
	return -1;
    retval = 0;
    fd = __svgalib_mouse_fd;
    if ((which & VGA_MOUSEEVENT) && (fd >= 0)) {
	if (FD_ISSET(fd, in)) {
	    retval |= VGA_MOUSEEVENT;
	    FD_CLR(fd, in);
	    mouse_update();
	}
    }
    if (which & VGA_KEYEVENT) {
	fd = kbd_fd;
	if (fd >= 0) {		/* we are in raw mode */
	    if (FD_ISSET(fd, in)) {
		FD_CLR(fd, in);
		retval |= VGA_KEYEVENT;
		keyboard_update();
	    }
	} else if (FD_ISSET(__svgalib_tty_fd, in)) {
	    FD_CLR(__svgalib_tty_fd, in);
	    retval |= VGA_KEYEVENT;
	}
    }
    return retval;
}

static void reserved_message(void)
{
    printf("svgalib: Function not present in shared library image.\n");
    printf("         You need a newer version of svgalib.\n");
}

void vga_reserved19(void)
{
    reserved_message();
}
void vga_reserved20(void)
{
    reserved_message();
}
void vga_reserved21(void)
{
    reserved_message();
}
void vga_reserved22(void)
{
    reserved_message();
}
