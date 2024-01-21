/*
 *  linux/ibcs/bsdioctl.c
 *
 *  Copyright (C) 1994  Mike Jagdis
 *
 * $Id: bsdioctl.c,v 1.14 1995/11/30 16:24:25 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/bsdioctl.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/errno.h>
#include <linux/stat.h>
#include <linux/fs.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/sys.h>
#include <linux/termios.h>
#include <linux/time.h>
#include <linux/mm.h>

#include <ibcs/ibcs.h>
#include <ibcs/bsd.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


#ifdef EMU_BSD
#define BSD_NCCS 20
struct bsd_termios {
	unsigned long	c_iflag;
	unsigned long	c_oflag;
	unsigned long	c_cflag;
	unsigned long	c_lflag;
	unsigned char	c_cc[BSD_NCCS];
	long		c_ispeed;
	long		c_ospeed;
};

static unsigned long speed_map[] = {
	0, 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 2400,
	4800, 9600, 19200, 38400
};

static unsigned long
bsd_to_linux_speed(unsigned long s)
{
	unsigned int i;

#ifdef B57600
	if (s == 57600)
		return B57600;
#endif
#ifdef B115200
	if (s == 115200)
		return B115200;
#endif
	
	for (i=0; i<sizeof(speed_map)/sizeof(speed_map[0]); i++)
		if (s <= speed_map[i])
			return i;
	return B38400;
}

static unsigned long
linux_to_bsd_speed(unsigned long s)
{
#ifdef B57600
	if (s == B57600)
		return 57600;
#endif
#ifdef B115200
	if (s == B115200)
		return 115200;
#endif
	return speed_map[s];
}


static int
bsd_to_linux_termios(int fd, int op, struct bsd_termios *it)
{
	struct termios t;
	int old_fs;
	unsigned long temp;
	char bsd_cc[BSD_NCCS];
	int error;

	error = verify_area(VERIFY_READ, it, sizeof(struct bsd_termios));
	if (error)
		return error;

	old_fs = get_fs();
	set_fs(get_ds());
	error = SYS(ioctl)(fd, TCGETS, &t);
	set_fs(old_fs);
	if (error)
		return error;

	t.c_iflag = get_fs_long(&it->c_iflag);
	t.c_iflag = (t.c_iflag & ~0xc00)
			| ((t.c_iflag & 0x400) << 1)
			| ((t.c_iflag & 0x800) >> 1);

	temp = get_fs_long(&it->c_oflag);
	t.c_oflag = (t.c_oflag & ~0x1805)
			| (temp & 9)
			| ((temp & 2) << 1)
			| ((temp & 4) << 10)
			| ((temp & 4) << 9);

	temp = get_fs_long(&it->c_cflag);
	t.c_cflag = (t.c_cflag & ~0xfff)
			| ((temp & 0xff00) >> 4);
	if (t.c_cflag & 0x30000)
		t.c_cflag |= 020000000000;
	t.c_cflag |= bsd_to_linux_speed(get_fs_long(&it->c_ospeed))
		| (bsd_to_linux_speed(get_fs_long(&it->c_ispeed)) << 16);

	temp = get_fs_long(&it->c_lflag);
	t.c_lflag = (t.c_lflag & ~0157663)
			| ((temp & 1) << 12)
			| ((temp & 0x46) << 3)
			| ((temp & 0x420) << 5)
			| ((temp & 0x180) >> 7)
			| ((temp & 0x400000) >> 14)
			| ((temp & 0x2800000) >> 11)
			| ((temp & 0x80000000) >> 24);

	memcpy_fromfs(bsd_cc, &it->c_cc, BSD_NCCS);
	t.c_cc[VEOF] = bsd_cc[0];
	t.c_cc[VEOL] = bsd_cc[1];
	t.c_cc[VEOL2] = bsd_cc[2];
	t.c_cc[VERASE] = bsd_cc[3];
	t.c_cc[VWERASE] = bsd_cc[4];
	t.c_cc[VKILL] = bsd_cc[5];
	t.c_cc[VREPRINT] = bsd_cc[6];
	t.c_cc[VSWTC] = bsd_cc[7];
	t.c_cc[VINTR] = bsd_cc[8];
	t.c_cc[VQUIT] = bsd_cc[9];
	t.c_cc[VSUSP] = bsd_cc[10];
/*	t.c_cc[VDSUSP] = bsd_cc[11];*/
	t.c_cc[VSTART] = bsd_cc[12];
	t.c_cc[VSTOP] = bsd_cc[13];
	t.c_cc[VLNEXT] = bsd_cc[14];
	t.c_cc[VDISCARD] = bsd_cc[15];
	t.c_cc[VMIN] = bsd_cc[16];
	t.c_cc[VTIME] = bsd_cc[17];
/*	t.c_cc[VSTATUS] = bsd_cc[18];*/

	set_fs(get_ds());
	error = SYS(ioctl)(fd, op, &t);
	set_fs(old_fs);

	return error;
}


static int
linux_to_bsd_termios(int fd, int op, struct bsd_termios *it)
{
	struct termios t;
	char bsd_cc[BSD_NCCS];
	int old_fs;
	int error;

	error = verify_area(VERIFY_WRITE, it, sizeof(struct bsd_termios));
	if (error)
		return error;

	old_fs = get_fs();
	set_fs(get_ds());
	error = SYS(ioctl)(fd, op, &t);
	set_fs(old_fs);
	if (error)
		return error;

	put_fs_long((t.c_iflag & 0777)
			| ((t.c_iflag & 02000) >> 1)
			| ((t.c_iflag & 010000) >> 2)
			| ((t.c_iflag & 020000) >> 4),
		&it->c_iflag);

	put_fs_long((t.c_oflag & 1)
			| ((t.c_oflag & 04) >> 1)
			| ((t.c_oflag & 014000) == 014000 ? 4 : 0),
		&it->c_oflag);

	put_fs_long((t.c_cflag & ~020000007777)
			| ((t.c_cflag & 0xff0) << 4)
			| ((t.c_cflag & 020000000000) ? 0x30000 : 0),
		&it->c_cflag);

	put_fs_long(linux_to_bsd_speed(t.c_cflag & CBAUD), &it->c_ospeed);
	if ((t.c_cflag & CIBAUD) != 0)
		put_fs_long(linux_to_bsd_speed((t.c_cflag & CIBAUD) >> 16),
			&it->c_ispeed);
	else
		put_fs_long(linux_to_bsd_speed(t.c_cflag & CBAUD),
			&it->c_ispeed);

	put_fs_long((t.c_lflag & 07777626010)
			| ((t.c_lflag & 03) << 7)
			| ((t.c_lflag & 01160) >> 3)
			| ((t.c_lflag & 0400) << 14)
			| ((t.c_lflag & 02000) >> 4)
			| ((t.c_lflag & 04000) >> 11)
			| ((t.c_lflag & 010000) << 11)
			| ((t.c_lflag & 040000) << 15)
			| ((t.c_lflag & 0100000) >> 5),
		&it->c_lflag);

	bsd_cc[0] = t.c_cc[VEOF];
	bsd_cc[1] = t.c_cc[VEOL];
	bsd_cc[2] = t.c_cc[VEOL2];
	bsd_cc[3] = t.c_cc[VERASE];
	bsd_cc[4] = t.c_cc[VWERASE];
	bsd_cc[5] = t.c_cc[VKILL];
	bsd_cc[6] = t.c_cc[VREPRINT];
	bsd_cc[7] = t.c_cc[VSWTC];
	bsd_cc[8] = t.c_cc[VINTR];
	bsd_cc[9] = t.c_cc[VQUIT];
	bsd_cc[10] = t.c_cc[VSUSP];
	bsd_cc[11] = t.c_cc[VSUSP];
	bsd_cc[12] = t.c_cc[VSTART];
	bsd_cc[13] = t.c_cc[VSTOP];
	bsd_cc[14] = t.c_cc[VLNEXT];
	bsd_cc[15] = t.c_cc[VDISCARD];
	bsd_cc[16] = t.c_cc[VMIN];
	bsd_cc[17] = t.c_cc[VTIME];
	bsd_cc[18] = 0; /* t.c_cc[VSTATUS]; */
	bsd_cc[19] = 0;

	memcpy_tofs(&it->c_cc, bsd_cc, BSD_NCCS);

	return error;
}
#endif /* EMU_BSD */


struct v7_sgttyb {
	unsigned char	sg_ispeed;
	unsigned char	sg_ospeed;
	unsigned char	sg_erase;
	unsigned char	sg_kill;
	int	sg_flags;
};

struct v7_tchars {
	char	t_intrc;
	char	t_quitc;
	char	t_startc;
	char	t_stopc;
	char	t_eofc;
	char	t_brkc;
};

struct v7_ltchars {
	char	t_suspc;
	char	t_dsuspc;
	char	t_rprntc;
	char	t_flushc;
	char	t_werasc;
	char	t_lnextc;
};


int
bsd_ioctl_termios(int fd, unsigned int func, void *arg)
{
	switch (func & 0xff) {
		case 0:	 {				/* TIOCGETD */
			unsigned long ldisc;
			int old_fs, error;

			error = verify_area(VERIFY_WRITE, arg,
					sizeof(unsigned short));
			if (error)
				return error;

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TIOCGETD, &ldisc);
			set_fs(old_fs);
			if (!error)
				put_fs_word(ldisc, (unsigned short *)arg);
			return error;
		}
		case 1: {				/* TIOCSETD */
			unsigned long ldisc;
			int old_fs, error;

			error = verify_area(VERIFY_READ, arg,
					sizeof(unsigned short));
			if (error)
				return error;

			ldisc = get_fs_word((unsigned short *)arg);
			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TIOCSETD, &ldisc);
			set_fs(old_fs);
			return error;
		}

		case 2: {				/* TIOCHPCL */
			int error, old_fs;
			struct termios t;

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCGETS, &t);
			set_fs(old_fs);
			if (error)
				return error;

			if (arg)
				t.c_cflag |= HUPCL;
			else
				t.c_cflag &= ~HUPCL;

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCSETS, &t);
			set_fs(old_fs);
			return error;
		}

		case 8: {				/* TIOCGETP */
			int error, old_fs;
			struct termios t;
			struct v7_sgttyb sg;

			error = verify_area(VERIFY_WRITE, arg, sizeof(sg));
			if (error)
				return error;

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCGETS, &t);
			set_fs(old_fs);
			if (error)
				return error;

			sg.sg_ispeed = sg.sg_ospeed = 0;
			sg.sg_erase = t.c_cc[VERASE];
			sg.sg_kill = t.c_cc[VKILL];
			sg.sg_flags =
				/* Old - became TANDEM instead.
				 * ((t.c_cflag & HUPCL) >> 10)
				 * |
				 */
/* O_ODDP */			((t.c_cflag & PARODD) >> 3)
/* O_EVENP */			| ((t.c_cflag & PARENB) >> 1)
/* LITOUT */			| ((t.c_cflag & OPOST) ? 0 : 0x200000)
/* O_CRMOD */			| ((t.c_oflag & ONLCR) << 2)
/* O_NL1|O_VTDELAY */		| (t.c_oflag & (NL1|VTDLY))
/* O_TBDELAY */			| ((t.c_oflag & TABDLY) ? 02000 : 0)
/* O_CRDELAY */			| ((t.c_oflag & CRDLY) << 3)
/* O_BSDELAY */			| ((t.c_oflag & BSDLY) << 2)
/* O_ECHO|O_LCASE */		| (t.c_lflag & (XCASE|ECHO))
				| ((t.c_lflag & ICANON)
/* O_CBREAK or O_RAW */		? 0 : ((t.c_lflag & ISIG) ? 0x02 : 0x20))
				/* Incomplete... */
				;

			memcpy_tofs(arg, &sg, sizeof(sg));
			return 0;
		}

		case 9:					/* TIOCSETP */
		case 10: {				/* TIOCSETN */
			int error, old_fs;
			struct termios t;
			struct v7_sgttyb sg;

			error = verify_area(VERIFY_READ, arg, sizeof(sg));
			if (error)
				return error;
			memcpy_fromfs(&sg, arg, sizeof(sg));

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCGETS, &t);
			set_fs(old_fs);
			if (error)
				return error;

			t.c_cc[VERASE] = sg.sg_erase;
			t.c_cc[VKILL] = sg.sg_kill;
			t.c_iflag = ICRNL | IXON;
			t.c_oflag = 0;
			t.c_lflag = ISIG | ICANON;
			if (sg.sg_flags & 0x02)		/* O_CBREAK */
				t.c_lflag &= (~ICANON);
			if (sg.sg_flags & 0x08)		/* O_ECHO */
				t.c_lflag |= ECHO|ECHOE|ECHOK|ECHOCTL|ECHOKE|IEXTEN;
			if (sg.sg_flags & 0x10)	/* O_CRMOD */
				t.c_oflag |= OPOST|ONLCR;
			if (sg.sg_flags & 0x20) {	/* O_RAW */
				t.c_iflag = 0;
				t.c_lflag &= ~(ISIG|ICANON);
			}
			if (sg.sg_flags & 0x200000)	/* LITOUT */
				t.c_oflag &= (~OPOST);
			if (!(t.c_lflag & ICANON)) {
				t.c_cc[VMIN] = 1;
				t.c_cc[VTIME] = 0;
			}

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCSETS, &t);
			set_fs(old_fs);
			return error;
		}

		case 17: {				/* TIOCSETC */
			int error, old_fs;
			struct termios t;
			struct v7_tchars tc;

			error = verify_area(VERIFY_READ, arg, sizeof(tc));
			if (error)
				return error;
			memcpy_fromfs(&tc, arg, sizeof(tc));

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCGETS, &t);
			set_fs(old_fs);
			if (error)
				return error;

			t.c_cc[VINTR] = tc.t_intrc;
			t.c_cc[VQUIT] = tc.t_quitc;
			t.c_cc[VSTART] = tc.t_startc;
			t.c_cc[VSTOP] = tc.t_stopc;
			t.c_cc[VEOF] = tc.t_eofc;
			t.c_cc[VEOL2] = tc.t_brkc;

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCSETS, &t);
			set_fs(old_fs);
			return error;
		}

		case 18: {				/* TIOCGETC */
			int error, old_fs;
			struct termios t;
			struct v7_tchars tc;

			error = verify_area(VERIFY_WRITE, arg, sizeof(tc));
			if (error)
				return error;

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCGETS, &t);
			set_fs(old_fs);
			if (error)
				return error;

			tc.t_intrc = t.c_cc[VINTR];
			tc.t_quitc = t.c_cc[VQUIT];
			tc.t_startc = t.c_cc[VSTART];
			tc.t_stopc = t.c_cc[VSTOP];
			tc.t_eofc = t.c_cc[VEOF];
			tc.t_brkc = t.c_cc[VEOL2];

			memcpy_tofs(arg, &tc, sizeof(tc));
			return 0;
		}

		case 116: {				/* TIOCGLTC */
			int error, old_fs;
			struct termios t;
			struct v7_ltchars tc;

			error = verify_area(VERIFY_WRITE, arg, sizeof(tc));
			if (error)
				return error;

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCGETS, &t);
			set_fs(old_fs);
			if (error)
				return error;

			tc.t_suspc = t.c_cc[VSUSP];
			tc.t_dsuspc = t.c_cc[VSUSP];
			tc.t_rprntc = t.c_cc[VREPRINT];
			tc.t_flushc = t.c_cc[VEOL2];
			tc.t_werasc = t.c_cc[VWERASE];
			tc.t_lnextc = t.c_cc[VLNEXT];

			memcpy_tofs(arg, &tc, sizeof(tc));
			return 0;
		}

		case 117: {				/* TIOCSLTC */
			int error, old_fs;
			struct termios t;
			struct v7_ltchars tc;

			error = verify_area(VERIFY_READ, arg, sizeof(tc));
			if (error)
				return error;
			memcpy_fromfs(&tc, arg, sizeof(tc));

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCGETS, &t);
			set_fs(old_fs);
			if (error)
				return error;

			t.c_cc[VSUSP] = tc.t_suspc;
			t.c_cc[VEOL2] = tc.t_dsuspc;
			t.c_cc[VREPRINT] = tc.t_rprntc;
			t.c_cc[VEOL2] = tc.t_flushc;
			t.c_cc[VWERASE] = tc.t_werasc;
			t.c_cc[VLNEXT] = tc.t_lnextc;

			old_fs = get_fs();
			set_fs(get_ds());
			error = SYS(ioctl)(fd, TCSETS, &t);
			set_fs(old_fs);
			return error;
		}

		case 13:				/* TIOEXCL */
			return SYS(ioctl)(fd, TIOCEXCL, arg);

		case 14:				/* TIOCNXCL */
			return SYS(ioctl)(fd, TIOCNXCL, arg);

		case 16:				/* TIOCFLUSH */
			return SYS(ioctl)(fd, TCFLSH, arg);

		/* ISC (maybe SVR4 in general?) has some extensions over
		 * the sgtty stuff. So do later BSDs. Needless to say they
		 * both have different extensions.
		 */
		case 20: /* TCSETPGRP  (TIOC|20) set pgrp of tty */
#ifdef EMU_SVR4
			if (current->personality == PER_SVR4)
				return SYS(ioctl)(fd, TIOCSPGRP, arg);
#else
			if (0) ;
#endif
#ifdef EMU_BSD
			else /* BSD TIOCSETA */
				return bsd_to_linux_termios(fd, TCSETS, arg);
#else
			break;
#endif
		case 21: /* TCGETPGRP  (TIOC|21) get pgrp of tty */
#ifdef EMU_SVR4
			if (current->personality == PER_SVR4)
				return SYS(ioctl)(fd, TIOCGPGRP, arg);
#else
			if (0) ;
#endif
#ifdef EMU_BSD
			else /* BSD TIOCSETAW */
				return bsd_to_linux_termios(fd, TCSETSW, arg);
#else
			break;
#endif

#ifdef EMU_BSD
		case 19:				/* TIOCGETA */
			return linux_to_bsd_termios(fd, TCGETS, arg);

		case 22:				/* TIOCSETAF */
 			return bsd_to_linux_termios(fd, TCSETSF, arg);

		case 26:				/* TIOCGETD */
			return SYS(ioctl)(fd, TIOCGETD, arg);

		case 27:				/* TIOCSETD */
			return SYS(ioctl)(fd, TIOCSETD, arg);

		case 97:				/* TIOCSCTTY */
			return SYS(ioctl)(fd, TIOCSCTTY, arg);

		case 103:				/* TIOCSWINSZ */
			return SYS(ioctl)(fd, TIOCSWINSZ, arg);

		case 104:				/* TIOCGWINSZ */
			return SYS(ioctl)(fd, TIOCGWINSZ, arg);

		case 113:				/* TIOCNOTTY */
			return SYS(ioctl)(fd, TIOCNOTTY, arg);

		case 118:	 			/* TIOCSPGRP */
			return SYS(ioctl)(fd, TIOCSPGRP, arg);

		case 119:				/* TIOCGPGRP */
			return SYS(ioctl)(fd, TIOCGPGRP, arg);

		case 123:				/* TIOCSBRK */
			return SYS(ioctl)(fd, TCSBRK, arg);

		case 124:				/* TIOCLGET */
		case 125:				/* TIOCLSET */
			return 0;


		case 3:					/* TIOCMODG */
		case 4:					/* TIOCMODS */
		case 94:				/* TIOCDRAIN */
		case 95:				/* TIOCSIG */
		case 96:				/* TIOCEXT */
		case 98:				/* TIOCCONS */
		case 102:				/* TIOCUCNTL */
		case 105:				/* TIOCREMOTE */
		case 106:				/* TIOCMGET */
		case 107:				/* TIOCMBIC */
		case 108:				/* TIOCMBIS */
		case 109:				/* TIOCMSET */
		case 110:				/* TIOCSTART */
		case 111:				/* TIOCSTOP */
		case 112:				/* TIOCPKT */
		case 114:				/* TIOCSTI */
		case 115:				/* TIOCOUTQ */
		case 120:				/* TIOCCDTR */
		case 121:				/* TIOCSDTR */
		case 122:				/* TIOCCBRK */
#endif /* EMU_BSD */
	}

	printk(KERN_ERR "BSD/V7: terminal ioctl 0x%08lx unsupported\n",
		(unsigned long)func);
	return -EINVAL;
}
