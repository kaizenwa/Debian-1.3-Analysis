/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * mouse.c - mouse driver code.
 *
 *
 * mouse.[ch] are taken from 'selection', so this copyright applies:
 *
 *    Copyright (c) 1994, 1995 Andrew J. Haylett.  All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * licence or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the above
 * copyright notice and the following two paragraphs appear (1) in all 
 * source copies of this software and (2) in accompanying documentation
 * wherever the programatic interface of this software, or any derivative
 * of it, is described.
 *
 * IN NO EVENT SHALL ANDREW J. HAYLETT BE LIABLE TO ANY PARTY FOR DIRECT,
 * INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF
 * THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF HE HAS BEEN 
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ANDREW J. HAYLETT SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" 
 * BASIS, AND ANDREW J. HAYLETT HAS NO OBLIGATION TO PROVIDE MAINTENANCE,
 * SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/* Simple driver for serial mouse */
/* Andrew Haylett */
/* [# Edit 8, Date 30-Jul-94, Module mouse.c #] */
/* Modified by Edwin Fong 31-Dec-94 */

#include <unistd.h>
#include <stdlib.h>
#include <termios.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>

#include "mouse.h"

int mx=32768, my=32768;
int x, y;
int mfd = -1;
static mouse_type mtype;
static int maccel;
static int mdelta;
static unsigned char prev_butstate;

static const unsigned short cflag[NR_TYPES] =
{
      (CS7                   | CREAD | CLOCAL | HUPCL ),   /* MicroSoft */
      (CS8 | CSTOPB          | CREAD | CLOCAL | HUPCL ),   /* MouseSystems 3 */
      (CS8 | CSTOPB          | CREAD | CLOCAL | HUPCL ),   /* MouseSystems 5 */
      (CS8 | PARENB | PARODD | CREAD | CLOCAL | HUPCL ),   /* MMSeries */
      (CS8 | CSTOPB          | CREAD | CLOCAL | HUPCL ),   /* Logitech */
      0,                                                   /* BusMouse */
      0                                                    /* PS/2 */
};

static const unsigned char proto[NR_TYPES][5] =
{
    /*  hd_mask hd_id   dp_mask dp_id   nobytes */
    { 	0x40,	0x40,	0x40,	0x00,	3 	},  /* MicroSoft */
    {	0xf8,	0x80,	0x00,	0x00,	3	},  /* MouseSystems 3 (Sun) */
    {	0xf8,	0x80,	0x00,	0x00,	5	},  /* MouseSystems 5 */
    {	0xe0,	0x80,	0x80,	0x00,	3	},  /* MMSeries */
    {	0xe0,	0x80,	0x80,	0x00,	3	},  /* Logitech */
    {	0xf8,	0x80,	0x00,	0x00,	5	},  /* BusMouse */
    {   0xc0,	0x00,	0x00,	0x00,	3	}   /* PS/2 */
};

static void
ms_setspeed(const int old, const int new,
            const unsigned short c_cflag)
{
    struct termios tty;
    char *c;

    tcgetattr(mfd, &tty);
    
    tty.c_iflag = IGNBRK | IGNPAR;
    tty.c_oflag = 0;
    tty.c_lflag = 0;
    tty.c_line = 0;
    tty.c_cc[VTIME] = 0;
    tty.c_cc[VMIN] = 1;

    switch (old)
    {
    	case 9600:	tty.c_cflag = c_cflag | B9600; break;
    	case 4800:	tty.c_cflag = c_cflag | B4800; break;
    	case 2400:	tty.c_cflag = c_cflag | B2400; break;
    	case 1200:
	default:	tty.c_cflag = c_cflag | B1200; break;
    }

    tcsetattr(mfd, TCSAFLUSH, &tty);

    switch (new)
    {
    	case 9600:	c = "*q";  tty.c_cflag = c_cflag | B9600; break;
    	case 4800:	c = "*p";  tty.c_cflag = c_cflag | B4800; break;
    	case 2400:	c = "*o";  tty.c_cflag = c_cflag | B2400; break;
    	case 1200:
	default:	c = "*n";  tty.c_cflag = c_cflag | B1200; break;
    }

    write(mfd, c, 2);
    usleep(10000);
    tcsetattr(mfd, TCSAFLUSH, &tty);
}

int
ms_init(const int accel, const int baud, const int delta,
	const char *dev, const unsigned int toggle, const int sample,
	const mouse_type type)
{
    unsigned int modem_lines;

    if (mfd != -1)
	close(mfd);

    if ((mfd = open(dev, O_RDWR)) < 0)
	return -1;

fcntl(mfd,F_SETFL,O_NONBLOCK);


    switch (type)
    {
	case P_BM:
	    break;
 
	case P_PS2:
	    write (mfd, "\364", 1);
	    break;
 
	default:
	    ms_setspeed(9600, baud, cflag[type]);
	    ms_setspeed(4800, baud, cflag[type]);
	    ms_setspeed(2400, baud, cflag[type]);
	    ms_setspeed(1200, baud, cflag[type]);

	    if (type == P_LOGI)
	    {
		write(mfd, "S", 1);
		ms_setspeed(baud, baud, cflag[P_MM]);
	    }

	    if		(sample <= 0)	write(mfd, "O", 1);
	    else if	(sample <= 15)	write(mfd, "J", 1);
	    else if	(sample <= 27)	write(mfd, "K", 1);
	    else if	(sample <= 42)	write(mfd, "L", 1);
	    else if	(sample <= 60)	write(mfd, "R", 1);
	    else if	(sample <= 85)	write(mfd, "M", 1);
	    else if	(sample <= 125)	write(mfd, "Q", 1);
	    else			write(mfd, "N", 1);
    }

    if (toggle != 0)
    {
	ioctl(mfd, TIOCMGET, &modem_lines);
	modem_lines &= ~toggle;
	ioctl(mfd, TIOCMSET, &modem_lines);
    }

    mtype = type;
    maccel = accel;
    mdelta = delta;
    x = 0;
    y = 0;
    prev_butstate = '\0';
    return 1;
}

int
get_ms_event(struct ms_event *ev)
{
    unsigned char buf[6];
    static unsigned char leftover = '\0';
    char dx, dy;
    int i, acc;
    struct timeval timeout;
    int fdmask;

    if (mfd == -1)
	return -1;
    if (mtype != P_BM)
    {
	if (leftover)
	    buf[0] = leftover, leftover = '\0';
	else {             
	    fdmask = 1 << mfd;
            timeout.tv_sec = 0;
            timeout.tv_usec = 1;
            if (select(mfd + 1, (fd_set *)&fdmask, NULL, NULL, &timeout)) {
	        if (read(mfd, &buf[0], 1) != 1)
    	    	    return -1;
            } else return 0;
       }

restart:
	/* find a header packet */
	while ((buf[0] & proto[mtype][0]) != proto[mtype][1])
	{
            fdmask = 1 << mfd;
            timeout.tv_sec = 0;
            timeout.tv_usec = 1;
            if (select(mfd + 1, (fd_set *)&fdmask, NULL, NULL, &timeout)) {
		    if (read(mfd, &buf[0], 1) != 1)
			return -1;
            } else return 0;
	}

	/* read in the rest of the packet */
	for (i = 1; i < proto[mtype][4]; ++i)
	{   
            fdmask = 1 << mfd;
            timeout.tv_sec = 0;
            timeout.tv_usec = 1;
            if (select(mfd + 1, (fd_set *)&fdmask, NULL, NULL, &timeout)) {
		    if (read(mfd, &buf[i], 1) != 1)
			return -1;
            } else return 0;
	/* check whether it's a data packet */
	    if (mtype != P_PS2 && ((buf[i] & proto[mtype][2]) != proto[mtype][3]
		    || buf[i] == 0x80))
		goto restart;
	}

	leftover = '\0';
	/* Needed for Logitech mice in Microsoft mode;
	   we will not catch the middle button otherwise. */
	if (mtype == P_MS)
	{
	    fdmask = 1 << mfd;
	    timeout.tv_sec = 0;
	    timeout.tv_usec = 1;
	    if (select(mfd + 1, (fd_set *)&fdmask, NULL, NULL, &timeout))
	    {
		if (read(mfd, &buf[i], 1) != 1)
		    return -1;
		if ((buf[i] & proto[mtype][0]) == proto[mtype][1])
		    leftover = buf[i], buf[i] = '\0';
	    }
	    else
		buf[i] = '\0';
	}
    }
    else	/* bus mouse */
    {
	fdmask = 1 << mfd;
	select(mfd + 1, (fd_set *)&fdmask, NULL, NULL, NULL);
	if (read(mfd, buf, 3) != 3)
	    return -1;
    }

/* construct the event */
    switch (mtype)
    {
	case P_MS:		/* Microsoft, or other mice in Microsoft mode */
	default:
	    ev->ev_butstate = ((buf[0] & 0x20) >> 3) | ((buf[0] & 0x10) >> 4)
			    | ((buf[3] & 0x20) >> 4);
	    dx = (char)(((buf[0] & 0x03) << 6) | (buf[1] & 0x3F));
	    dy = (char)(((buf[0] & 0x0C) << 4) | (buf[2] & 0x3F));
	    break;
        case P_SUN:		/* Mouse Systems 3-byte as used in Suns */
	case P_BM:              /* BusMouse */
	    ev->ev_butstate = (~buf[0]) & 0x07;
	    dx =   (char)buf[1];
	    dy = - (char)buf[2];
	    break;
	case P_MSC:             /* Mouse Systems Corp (5 bytes, PC) */
	    ev->ev_butstate = (~buf[0]) & 0x07;
	    dx =    (char)(buf[1]) + (char)(buf[3]);
	    dy = - ((char)(buf[2]) + (char)(buf[4]));
	    break;
	case P_MM:              /* MM Series */
	case P_LOGI:            /* Logitech */
	    ev->ev_butstate = buf[0] & 0x07;
	    dx = (buf[0] & 0x10) ?   buf[1] : - buf[1];
	    dy = (buf[0] & 0x08) ? - buf[2] :   buf[2];
	    break;
	case P_PS2:            /* PS/2 Mouse */
	    ev->ev_butstate = 0;
	    if (buf[0] & 0x01)
		ev->ev_butstate |= MS_BUTLEFT;
	    if (buf[0] & 0x02)
		ev->ev_butstate |= MS_BUTRIGHT;
	    if (buf[0] & 0x04)
		ev->ev_butstate |= MS_BUTMIDDLE;
	    dx =    (buf[0] & 0x10) ? buf[1]-256 : buf[1];
	    dy = - ((buf[0] & 0x20) ? buf[2]-256 : buf[2]);
	    break;
    }

    acc = (abs(ev->ev_dx) + abs(ev->ev_dy) > mdelta) ? maccel : 1;
    ev->ev_dx = dx * acc;
    ev->ev_dy = dy * acc;
    x += ev->ev_dx;
    y += ev->ev_dy;
    if (x < 0) x = 0;
    if (y < 0) y = 0;
    if (x > mx ) x = mx;
    if (y > my) y = my;
    ev->ev_x = x;
    ev->ev_y = y;
    if (ev->ev_butstate > prev_butstate)
	ev->ev_code = MS_BUTDOWN;
    else if (ev->ev_butstate < prev_butstate)
	ev->ev_code = MS_BUTUP;
    else if (dx || dy)
    {
	if (ev->ev_butstate)
	    ev->ev_code = MS_DRAG;
	else
	    ev->ev_code = MS_MOVE;
    }
    else
	ev->ev_code = MS_NONE;
    prev_butstate = ev->ev_butstate;
    return 1;
}
