/* $Source: /usr/local/src/pax/RCS/ms_dio.c,v $
 *
 * $Revision: 2.1 $
 *
 * ms_dio.c - functions to make RAW devices things works under MS-DOS
 *
 * DESCRIPTION
 *
 *	These routines provide the necessary functions to make pax work
 *	under MS-DOS.
 *
 *	NOTE: Before these routines can be used to read/write directly to
 *	the disk, bypassing the logical file structure, MSDOS MUST know
 *	what kind of disk is in the drive you intend to write to.  This can
 *	be accomplished by putting a formatted disk in the drive of
 *	interest and doing a DIR on it.  MSDOS then remembers the disk type
 *	for a while.
 *
 *	WARNING: DISABLING THE BUILT IN CHECK AND CALLING THESE ROUTINES
 *	WITH THE DRIVE SET TO CORRESPOND TO YOUR HARD DISK WILL PROBABLY
 *	TRASH IT COMPLETELY!
 *
 * AUTHOR
 *
 *	Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
 *	Harold Walters, Oklahoma State University (walters@1.ce.okstate.edu)
 *
 * COPYRIGHT
 *
 *	Copyright (c) 1989 Mark H. Colburn.  All rights reserved.
 *
 *	Redistribution and use in source and binary forms are permitted
 *	provided that the above copyright notice and this paragraph are
 *	duplicated in all such forms and that any documentation,
 *	advertising materials, and other materials related to such
 *	distribution and use acknowledge that the software was developed
 *	by Mark H. Colburn.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * $Log: ms_dio.c,v $
 * Revision 2.1  1996/10/18  21:36:18  istewart
 * Initial 2.1 port
 *
 *
 */

#ifndef lint
static char *ident = "$Id: ms_dio.c,v 2.1 1996/10/18 21:36:18 istewart Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.";
#endif /* not lint */

/* Headers */

#include "pax.h"

#ifdef MSDOS
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <dos.h>


/*
 * MSDOS Direct I/O functions.  Alternative approach to DISKACC.
 */

#ifdef DIO

/* Function Prototypes */

static int	dio_adw (unsigned int, char *, unsigned int, unsigned int,
			    unsigned int *);
static int	dio_adr (unsigned int, char *, unsigned int, unsigned int,
			    unsigned int *);
static int	dio_drive_check (int);
static int	dio_err_hand (unsigned int, int, unsigned int);
static int	dio_read1 (int, char *, unsigned int);
static int	dio_write1 (int, char *, unsigned int);

/* Defines */

#define SECSIZ 512

/* Local Variables */

static unsigned long	fptr = 0L;
static char		secbuf[SECSIZ];
static int		rwsec = 0;

static union REGS	reg;
static union REGS	rreg;

#if defined(M_I86LM) || defined(M_I86CM)
static struct SREGS	sreg;
#endif /* !M_I86LM */


void dio_str (char *s)
{
    for ( ; *s; s++) {
	if (*s == '\\') {
	    *s = '/';
	} else if (isupper(*s)) {
	    *s = tolower(*s);
	}
    }
}


static int dio_adw (unsigned int drive, char *buf, unsigned int secnum,
		    unsigned int secknt, unsigned int *err)
{
    rwsec = secnum;
    reg.x.ax = drive;
    reg.x.dx = secnum;
    reg.x.cx = secknt;
#if defined(M_I86LM) || defined(M_I86CM)
    reg.x.bx = FP_OFF(buf);
    sreg.ds = FP_SEG(buf);
    int86x(0x26, &reg, &rreg, &sreg);
#else /* !M_I86LM */
    reg.x.dx = (int) buf;
    int86(0x26, &reg, &rreg);
#endif /* !M_I86LM */
    *err = rreg.x.ax;
    if (rreg.x.cflag) {
	return (-1);
    } else {
	return (0);
    }
}


static int dio_adr (unsigned int drive, char *buf, unsigned int secnum,
		    unsigned int secknt, unsigned int *err)
{
    rwsec = secnum;
    reg.x.ax = drive;
    reg.x.dx = secnum;
    reg.x.cx = secknt;
#if defined(M_I86LM) || defined(M_I86CM)
    reg.x.bx = FP_OFF(buf);
    sreg.ds = FP_SEG(buf);
    int86x(0x25, &reg, &rreg, &sreg);
#else /* !M_I86LM */
    reg.x.dx = (int) buf;
    int86(0x25, &reg, &rreg);
#endif /* !M_I86LM */
    *err = rreg.x.ax;
    if (rreg.x.cflag) {
	return (-1);
    } else {
	return (0);
    }

}


static char        *doserr[] = {
    "write-protect error",
    "unknown unit",
    "drive not ready",
    "unknown command",
    "data error (bad CRC)",
    "bad request structure length",
    "seek error",
    "unknown media type",
    "sector not found",
    "printer out of paper",
    "write fault",
    "read fault",
    "general failure",
    " ",
    " ",
    "invalid disk change (DOS 3.x)",
    ""
};

static char        *bioserr[] = {
    "general error",
    "",
    "bad address mark",
    "write-protect error",
    "sector not found",
    "", "", "",
    "DMA failure",
    "", "", "", "", "",
    "", "",
    "data error (bad CRC)",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "",
    "controller failed",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "",
    "",
    "seek error",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "",
    "", "", "",
    "disk time out",
    ""
};


static int dio_drive_check (int d)
{
    d = -(d + 1);
    switch (d) {

    case 0:			/* a */
    case 1:			/* b */
	break;

    default:
	fprintf(stderr, "dio: dio to drive %1c: not supported\n",
		'a' + d);
	exit(1);
    }
    return (d);
}


static int dio_err_hand (unsigned int drive, int rw, unsigned int err)
{
    unsigned int        high,
                        low;

    low = err & 0x000f;
    high = (err >> 8) & 0x00ff;
    if (!(high == 0x04 && low == 0x08)) {
	fprintf(stderr,
		"dio: sector %d: %s error 0x%x\ndio: dos: %s: bios: %s\n",
		(rw == (int) 'r') ? rwsec : rwsec,
		(rw == (int) 'r') ? "read" : "write", err,
		doserr[low], bioserr[high]);
    }
    if (high == 0x04 && low == 0x08) {	/* sector not found */
	if (rw == (int) 'r') {
	    rwsec = 0;
	    return (0);
	} else {
	    errno = ENOSPC;
	    rwsec = 0;
	    return (-1);
	}
    }
    exit(1);
}


static int dio_read1 (int drive, char *buf, unsigned int sec)
{
    unsigned int        err;

    if (dio_adr(drive, buf, sec, 1, &err) == -1) {
	if (dio_adr(drive, buf, sec, 1, &err) == -1) {
	    if (dio_adr(drive, buf, sec, 1, &err) == -1) {
		if (dio_err_hand(drive, (int) 'r', err) == 0) {
		    return (0);
		}
	    }
	}
    }
    return (SECSIZ);
}

static int dio_write1 (int drive, char *buf, unsigned int sec)
{
    unsigned int        err;

    if (dio_adw(drive, buf, sec, 1, &err) == -1) {
	if (dio_adw(drive, buf, sec, 1, &err) == -1) {
	    if (dio_adw(drive, buf, sec, 1, &err) == -1) {
		if (dio_err_hand(drive, (int) 'w', err) == -1) {
		    return (-1);
		}
	    }
	}
    }
    return (SECSIZ);
}


int dio_write (int drive, /* a -> -1, b -> -2, etc */
	       char *from_buf, unsigned int from_cnt)
{
    unsigned int        amt;
    unsigned int        err;
    unsigned int        nn;
    unsigned int        fquo;
    unsigned int        frem;
    unsigned int        cquo;
    unsigned int        crem;

    drive = dio_drive_check(drive);
    amt = 0;
    err = 0;
    cquo = 0;
    crem = 0;

    fquo = (unsigned int) (fptr / SECSIZ);
    frem = (unsigned int) (fptr % SECSIZ);
    if (frem > 0) {
	if (dio_read1(drive, secbuf, fquo) == 0) {
	    return (-1);
	}
	if ((nn = SECSIZ - frem) > from_cnt) {
	    nn = from_cnt;
	}
	memcpy(&secbuf[frem], from_buf, nn);
	if (dio_write1(drive, secbuf, fquo) == -1) {
	    return (-1);
	}
	amt += nn;
	fptr += nn;
	if (SECSIZ - frem <= from_cnt) {
	    fquo++;
	}
	from_buf += nn;
	from_cnt -= nn;
    }
    cquo = from_cnt / SECSIZ;
    crem = from_cnt % SECSIZ;
    if (cquo > 0) {
	if (dio_adw(drive, from_buf, fquo, cquo, &err) == -1) {
	    if (dio_adw(drive, from_buf, fquo, cquo, &err) == -1) {
		if (dio_adw(drive, from_buf, fquo, cquo, &err) == -1) {
		    if (dio_err_hand(drive, (int) 'w', err) == -1) {
			return (-1);
		    }
		}
	    }
	}
	nn = cquo * SECSIZ;
	amt += nn;
	fptr += nn;
	fquo += cquo;
	from_buf += nn;
	from_cnt -= nn;
    }
    if (crem > 0) {
	if (dio_read1(drive, secbuf, fquo) == 0) {
	    return (-1);
	}
	nn = crem;
	memcpy(&secbuf[0], from_buf, nn);
	if (dio_write1(drive, secbuf, fquo) == -1) {
	    return (-1);
	}
	amt += nn;
	fptr += nn;
	from_buf += nn;
	from_cnt -= nn;
    }
    return (amt);
}


/* read data directly from the disk using INT 25 */

int dio_read (int drive, /* a -> -1, b -> -2, etc */
	      char *to_buf, unsigned int to_cnt)
{
    unsigned int        amt;
    unsigned int        err;
    unsigned int        nn;
    unsigned int        fquo;
    unsigned int        frem;
    unsigned int        cquo;
    unsigned int        crem;

    drive = dio_drive_check(drive);
    amt = 0;
    err = 0;
    cquo = 0;
    crem = 0;

    fquo = (unsigned int) (fptr / SECSIZ);
    frem = (unsigned int) (fptr % SECSIZ);
    if (frem > 0) {
	if (dio_read1(drive, secbuf, fquo) == 0) {
	    return (0);
	}
	if ((nn = SECSIZ - frem) > to_cnt) {
	    nn = to_cnt;
	}
	memcpy(to_buf, &secbuf[frem], nn);
	amt += nn;
	fptr += nn;
	if (SECSIZ - frem <= to_cnt) {
	    fquo++;
	}
	to_buf += nn;

	to_cnt -= nn;
    }
    cquo = to_cnt / SECSIZ;
    crem = to_cnt % SECSIZ;
    if (cquo > 0) {
	if (dio_adr(drive, to_buf, fquo, cquo, &err) == -1) {
	    if (dio_adr(drive, to_buf, fquo, cquo, &err) == -1) {
		if (dio_adr(drive, to_buf, fquo, cquo, &err) == -1) {
		    if (dio_err_hand(drive, (int) 'r', err) == 0) {
			return (0);
		    }
		}
	    }
	}
	nn = cquo * SECSIZ;
	amt += nn;
	fptr += nn;
	fquo += cquo;
	to_buf += nn;
	to_cnt -= nn;
    }
    if (crem > 0) {
	if (dio_read1(drive, secbuf, fquo) == 0) {
	    return (0);
	}
	nn = crem;
	memcpy(to_buf, &secbuf[0], nn);
	amt += nn;
	fptr += nn;
	to_buf += nn;
	to_cnt -= nn;
    }
    return (amt);
}

/* changes true character devices to binary mode
 * ignores dio and dos files but will change all of
 * stdin, stdout, stderr to binary mode if called with
 * h = to 0, 1, or 2
 * inspired by John B. Thiel (jbthiel@ogc.cse.edu)
 */

int dio_to_binary (int h)
{
   union REGS           regs;

   if (h < 0) {
	   return(0);
   }
   regs.h.ah = 0x44;
   regs.h.al = 0x00;
   regs.x.bx = h;
   intdos(&regs, &regs);
   if (regs.x.cflag || regs.x.ax == 0xff) {
	   return(-1);
   }
   if (regs.h.dl & 0x80) {
       regs.h.ah = 0x44;
       regs.h.al = 0x01;
       regs.x.bx = h;
       regs.h.dh = 0;
       regs.h.dl |= 0x20;
       intdos(&regs, &regs);
   }
   if (regs.x.cflag || regs.x.ax == 0xff) {
   	   return(-1);
   } else {
   	   return(0);
   }
}

int dio_open_check (char *s)
{
    if (!stricmp(s, "a:dio") || !stricmp(s, "b:dio")) {
	return (-(*s - 'a' + 1));
    } else {
	return (0);
    }
}


int dio_open2 (char *p, int f)
{
    int                 h;

    h = dio_open_check(p);
    if (h < 0) {
	fptr = 0L;
    }
    return (h);
}


int dio_open3 (char *p, int f, int m)
{
    int                 h;

    h = dio_open_check(p);
    if (h < 0) {
	fptr = 0L;
    }
    return (h);
}


int dio_close (int h)
{
    return(0);
}


long dio_lseek (int h, long o, int r)
{
    long                check;

    if (h >= 0) {
	errno = EBADF;
	return (-1L);
    }
    check = fptr;
    switch (r) {

    case 0:
	check = o;
	break;

    case 1:
	check += o;
	break;

    case 2:
    default:
	errno = EINVAL;
	fprintf(stderr, "dio: origin %d not supported\n", r);
	return (-1L);
    }

    if (check < 0L) {
	errno = EINVAL;
	return (-1L);
    }
    fptr = check;
    return (fptr);
}

#endif
#endif /* MSDOS */
