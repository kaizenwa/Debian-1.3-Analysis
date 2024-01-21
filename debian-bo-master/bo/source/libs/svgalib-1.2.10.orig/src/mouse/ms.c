/* Based on:
 * simple driver for serial mouse
 * Andrew Haylett, 14th December 1992
 * and on the driver in XFree86.
 * Edited for svgalib (hhanemaa@cs.ruu.nl).
 * This probably doesn't work with all types of bus mouse.
 * HH: Added PS/2 mouse support.
 * Fixed Logitech support thanks to Daniel Jackson.
 * MouseSystems movement overflow fixed by Steve VanDevender.
 * Logitech fixed again.
 * Michael: Added support for controlling DTR and RTS.
 */

/* This file is included by mouse.c. */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <time.h>
#include <vga.h>


/* #define DEBUG */

static int mtype;
static int mbaud = 1200;	/* Should be 1200. */
static int msample;
static char *mdev;
int __svgalib_mouse_fd = -1;
static int mfdmode = 0;		/* 0 means don't wait (NDELAY) */
static int m_modem_ctl = 0;

/* Settings found on mouse open.. Probably std termios should be restored as well */
static unsigned long mold_modem_info;	/*  original state of DTR/RTS */
static char mmodem_info_valid = 0;	/*  ==0 means: couldn't get it: old kernel? */

static const unsigned short cflag[7] =
{
    (CS7 | CREAD | CLOCAL | HUPCL),	/* MicroSoft */
    (CS8 | CSTOPB | CREAD | CLOCAL | HUPCL),	/* MouseSystems */
    (CS8 | PARENB | PARODD | CREAD | CLOCAL | HUPCL),	/* MMSeries */
    (CS8 | CSTOPB | CREAD | CLOCAL | HUPCL),	/* Logitech */
    0,				/* BusMouse */
    0,				/* PS/2 */
    (CS7 | CREAD | CLOCAL | HUPCL),	/* MouseMan */
};

static const unsigned char proto[7][5] =
{
    /*  hd_mask hd_id   dp_mask dp_id   nobytes */
    {0x40, 0x40, 0x40, 0x00, 3},	/* MicroSoft */
    {0xf8, 0x80, 0x00, 0x00, 5},	/* MouseSystems */
    {0xe0, 0x80, 0x80, 0x00, 3},	/* MMSeries */
    {0xe0, 0x80, 0x00, 0x00, 3},	/* Logitech */
    {0xf8, 0x80, 0x00, 0x00, 5},	/* BusMouse */
    {0xc0, 0x00, 0x00, 0x00, 3},	/* PS/2 mouse */
    {0x40, 0x40, 0x40, 0x00, 3},	/* Mouseman */
};

static void ms_setspeed(const int old, const int new,
			const unsigned short c_cflag)
{
    struct termios tty;
    char *c;

    tcgetattr(__svgalib_mouse_fd, &tty);

    tty.c_iflag = IGNBRK | IGNPAR;
    tty.c_oflag = 0;
    tty.c_lflag = 0;
    tty.c_line = 0;
    tty.c_cc[VTIME] = 0;
    tty.c_cc[VMIN] = 1;

    switch (old) {
    case 9600:
	tty.c_cflag = c_cflag | B9600;
	break;
    case 4800:
	tty.c_cflag = c_cflag | B4800;
	break;
    case 2400:
	tty.c_cflag = c_cflag | B2400;
	break;
    case 1200:
    default:
	tty.c_cflag = c_cflag | B1200;
	break;
    }

    tcsetattr(__svgalib_mouse_fd, TCSAFLUSH, &tty);

    switch (new) {
    case 9600:
	c = "*q";
	tty.c_cflag = c_cflag | B9600;
	break;
    case 4800:
	c = "*p";
	tty.c_cflag = c_cflag | B4800;
	break;
    case 2400:
	c = "*o";
	tty.c_cflag = c_cflag | B2400;
	break;
    case 1200:
    default:
	c = "*n";
	tty.c_cflag = c_cflag | B1200;
	break;
    }

    write(__svgalib_mouse_fd, c, 2);
    usleep(10000);
    tcsetattr(__svgalib_mouse_fd, TCSAFLUSH, &tty);
}

static int ms_init(void)
{
#ifdef ALLOW_MOUSE_OVERRIDE
/*------------------------------------------------------------------*/
/* Define ALLOW_MOUSE_OVERRIDE to recognize the SVGA_MOUSE_OVERRIDE */
/* environment variable.  If this environment variable is set       */
/* then ignore the program's specified mouse type and use           */
/* the configuration file's type.                                   */
/* In particular, DOOM does not understand "MouseMan" as a valid    */
/* mouse type and so defaults the mouse type to "MouseSystems".     */
/*------------------------------------------------------------------*/
    int newmtype;

    if (getenv("SVGA_MOUSE_OVERRIDE") != (char *) NULL) {
	newmtype = vga_getmousetype();
	if (mtype != newmtype) {
	    printf("ms_init: mouse type override %d to %d\n",
		   mtype, newmtype);
	    mtype = newmtype;
	}
    }
#endif				/* ALLOW_MOUSE_OVERRIDE */

    /* Added O_NDELAY here. */
    if ((__svgalib_mouse_fd = open(mdev, O_RDWR | O_NDELAY)) < 0)
	return -1;

    if (mtype == MOUSE_BUSMOUSE || mtype == MOUSE_PS2)
	m_modem_ctl = 0;

    /* If no signal will change there is no need to restore
       or safe original settings. */
    if (!m_modem_ctl)
	mmodem_info_valid = 0;
    else {
	/* Get current modem signals; keep silent on errors.. */
	mmodem_info_valid = !ioctl(__svgalib_mouse_fd,
				   TIOCMGET, &mold_modem_info);

	if (mmodem_info_valid) {
	    unsigned long param = mold_modem_info;

	    /* Prepare new stat: */

	    /*set DTR as ordered.. */
	    if (m_modem_ctl & MOUSE_CHG_DTR) {
		param &= ~TIOCM_DTR;
		if (m_modem_ctl & MOUSE_DTR_HIGH)
		    param |= TIOCM_DTR;
	    }
	    /*set RTS as ordered.. */
	    if (m_modem_ctl & MOUSE_CHG_RTS) {
		param &= ~TIOCM_RTS;
		if (m_modem_ctl & MOUSE_RTS_HIGH)
		    param |= TIOCM_RTS;
	    }
	    if (ioctl(__svgalib_mouse_fd, TIOCMSET, &param))
		mmodem_info_valid = 0;	/* No try to restore if this failed */
	}
    }

    if (mtype == MOUSE_LOGIMAN) {
	ms_setspeed(1200, 1200, cflag[mtype]);
	write(__svgalib_mouse_fd, "*X", 2);
	ms_setspeed(1200, mbaud, cflag[mtype]);
    } else if (mtype != MOUSE_BUSMOUSE && mtype != MOUSE_PS2) {
	ms_setspeed(9600, mbaud, cflag[mtype]);
	ms_setspeed(4800, mbaud, cflag[mtype]);
	ms_setspeed(2400, mbaud, cflag[mtype]);
	ms_setspeed(1200, mbaud, cflag[mtype]);

	if (mtype == MOUSE_LOGITECH) {
	    write(__svgalib_mouse_fd, "S", 1);
	    ms_setspeed(mbaud, mbaud, cflag[MOUSE_MMSERIES]);
	}
	if (msample <= 0)
	    write(__svgalib_mouse_fd, "O", 1);
	else if (msample <= 15)
	    write(__svgalib_mouse_fd, "J", 1);
	else if (msample <= 27)
	    write(__svgalib_mouse_fd, "K", 1);
	else if (msample <= 42)
	    write(__svgalib_mouse_fd, "L", 1);
	else if (msample <= 60)
	    write(__svgalib_mouse_fd, "R", 1);
	else if (msample <= 85)
	    write(__svgalib_mouse_fd, "M", 1);
	else if (msample <= 125)
	    write(__svgalib_mouse_fd, "Q", 1);
	else
	    write(__svgalib_mouse_fd, "N", 1);
    }
    return 0;
}

/* Scooped from X driver. */
static inline void ms_close(void)
{
    if (__svgalib_mouse_fd == -1)
	return;
    if (mtype == MOUSE_LOGITECH) {
	write(__svgalib_mouse_fd, "U", 1);
	ms_setspeed(mbaud, 1200, cflag[MOUSE_LOGITECH]);
    }
    /* Try to restore modem signals if we could get them. */
    if (mmodem_info_valid)
	ioctl(__svgalib_mouse_fd, TIOCMSET, &mold_modem_info);

    close(__svgalib_mouse_fd);
    __svgalib_mouse_fd = -1;
}

#define MOUSEBUFFERSIZE 256

static int get_ms_event(int wait)
/*
   Changed to process multiple packets.
   wait value:
   0    Process any mouse events, and return status.
   1    Wait for mouse event, then return.

   Status indicates whether an event was processed.
 */
{
    static unsigned char buf[MOUSEBUFFERSIZE];
    static int nu_bytes = 0;
    char event_handled = 0;
    int bytesread;
    int i;
/*  int but; */
    static int but = 0;		/* static is hack for MouseMan */
    int dx, dy;
    int j;

    if (__svgalib_mouse_fd == -1)
	return -1;

  again:

    if (mfdmode == 1) {
	/* We don't want to wait, set NDELAY mode. */
	fcntl(__svgalib_mouse_fd, F_SETFL, O_RDONLY | O_NDELAY);
	mfdmode = 0;
    }
    bytesread = read(__svgalib_mouse_fd,
		     &buf[nu_bytes], MOUSEBUFFERSIZE - nu_bytes);

    i = 0;

    if (bytesread >= 1)
	nu_bytes += bytesread;

#ifdef DEBUG
    printf("#bytes in buffer: %d\n", nu_bytes);
#endif

  handle_packets:

    /* Handle packets in buffer. */

#ifdef DEBUG
    printf("Bytes left in buffer: %d, packet is %d bytes\n",
	   nu_bytes - i, proto[mtype][4]);
#endif

    if ((mtype == MOUSE_LOGIMAN) &&
	((nu_bytes - i) >= 1) &&
	((buf[i] & proto[mtype][0]) != proto[mtype][1]) &&
	((char) (buf[i] & ~0x23) == 0)) {
	/* Hack-o-matic, stolen from xf86_Mouse.c */
	but = ((buf[i] & 0x20) >> 4) | (but & 0x05);
	__mouse_eventhandler(but, 0, 0);
	event_handled++;
	i++;
    }
    if (nu_bytes - i < proto[mtype][4])
	/* No full packet available. */
	if (wait == 0 || (wait == 1 && event_handled)) {
	    if (i >= nu_bytes) {
		nu_bytes = 0;
		i = 0;
	    } else {
		/* Move partial packet to front of buffer. */
		for (j = i; j < nu_bytes; j++)
		    buf[j - i] = buf[j];
		nu_bytes -= i;
	    }
	    return event_handled;
	} else {		/* (wait == 1 && !event_handled) */
	    if (i >= nu_bytes) {
		nu_bytes = 0;
		i = 0;
	    }
	    /* Wait mode, we'll sleep on reads. */
	    fcntl(__svgalib_mouse_fd, F_SETFL, O_RDONLY);
	    mfdmode = 1;
	    read(__svgalib_mouse_fd, &buf[nu_bytes], 1);
	    nu_bytes++;
	    if (nu_bytes - i < proto[mtype][4])
		/* Not a complete packet. */
		goto again;
	}
    /* Check header byte. */
    if ((buf[i] & proto[mtype][0]) != proto[mtype][1]) {
	/* Not a header byte. */
	i++;
	goto handle_packets;
    }
    /* Check whether it's a valid data packet. */
    if (mtype != MOUSE_PS2)
	for (j = 1; j < proto[mtype][4]; j++)
	    if ((buf[i + j] & proto[mtype][2]) != proto[mtype][3]
		|| buf[i + j] == 0x80) {
		i = i + j + 1;
		goto handle_packets;
	    }
    /* Construct the event. */
    switch (mtype) {
    case MOUSE_MICROSOFT:	/* Microsoft */
    case MOUSE_LOGIMAN:	/* MouseMan / TrackMan */
    default:
	but = (but & 2) | ((buf[i] & 0x20) >> 3) | ((buf[i] & 0x10) >> 4);
	dx = (char) (((buf[i] & 0x03) << 6) | (buf[i + 1] & 0x3F));
	dy = (char) (((buf[i] & 0x0C) << 4) | (buf[i + 2] & 0x3F));
	break;
    case MOUSE_MOUSESYSTEMS:	/* Mouse Systems Corp */
	but = (~buf[i]) & 0x07;
	dx = (char) (buf[i + 1]);
	dx += (char) (buf[i + 3]);
	dy = -((char) (buf[i + 2]));
	dy -= (char) (buf[i + 4]);
	break;
    case MOUSE_MMSERIES:	/* MM Series */
    case MOUSE_LOGITECH:	/* Logitech */
	but = buf[i] & 0x07;
	dx = (buf[i] & 0x10) ? buf[i + 1] : -buf[i + 1];
	dy = (buf[i] & 0x08) ? -buf[i + 2] : buf[i + 2];
	break;
    case MOUSE_BUSMOUSE:	/* BusMouse */
	but = (~buf[i]) & 0x07;
	dx = (char) buf[i + 1];
	dy = -(char) buf[i + 2];
	break;
    case MOUSE_PS2:		/* PS/2 mouse */
	but = (buf[i] & 0x04) >> 1 |	/* Middle */
	    (buf[i] & 0x02) >> 1 |	/* Right */
	    (buf[i] & 0x01) << 2;	/* Left */
	dx = (buf[i] & 0x10) ? buf[i + 1] - 256 : buf[i + 1];
	dy = (buf[i] & 0x20) ? -(buf[i + 2] - 256) : -buf[i + 2];
	break;
    }

    i += proto[mtype][4];

    /* Try to snag that optional mouseman fourth byte, if present */
    if ((mtype == MOUSE_LOGIMAN) &&
	((nu_bytes - i) >= 1) &&
	((buf[i] & proto[mtype][0]) != proto[mtype][1]) &&
	((char) (buf[i] & ~0x23) == 0)) {
	/* Hack-o-matic, stolen from xf86_Mouse.c */
	but = ((buf[i] & 0x20) >> 4) | (but & 0x05);
	i++;
    }
    __mouse_eventhandler(but, dx, dy);

    event_handled = 1;

    goto handle_packets;
}
