#define I_MEMORY
#define I_TYPES
#define I_IOCTL
#define	I_TTY
#include "includes.h"

#ifndef CBAUD
#define CBAUD (B0|B50|B75|B110|B150|B200|B300|B600|B1200|B1800|B2400|B4800|B9600|B19200|B38400);
#endif

#define ctrl(c) ((c)-'@')

void lose_ctty(void) {
#ifdef USE_TIOCNOTTY
  {
    int slavefd;
    if ((slavefd = open ("/dev/tty", O_RDWR, 0)) >= 0) {
      ioctl (slavefd, TIOCNOTTY, (char *) 0);
      x__close (slavefd);
    }
  }
#endif
#ifdef USE_SETSID
  setsid();
#endif
#ifdef USE_SETPGRP
#ifdef SYSV
  setpgrp();
#else
  { int mypid;
  mypid = getpid();
  setpgrp(mypid, 0); }
#endif
#endif
}

#ifdef USE_TERMIOS

#ifdef ultrix
#define TCSETS TCSANOW
#define TCGETS TCGETP
#endif

static struct termios oldterm;

void terminal_save(int fd) {
#ifdef USE_TCATTR
  tcgetattr(fd, &oldterm);
#else
  ioctl( fd, TCGETS, &oldterm);
#endif
}

void terminal_raw(int fd) {
  struct termios tempio;
#ifdef USE_TCATTR
  tcgetattr(fd, &tempio);
#else
  ioctl( fd, TCGETS, &tempio);
#endif
  tempio.c_iflag = 0;
  tempio.c_oflag = 0;
  tempio.c_lflag = 0;
  tempio.c_cc[VMIN] = 1;
  tempio.c_cc[VTIME] = 0;
#ifdef USE_TCATTR
  tcsetattr(fd, TCSANOW, &tempio);
#else
  ioctl( fd, TCSETS, &tempio);
#endif
}

void terminal_restore(int fd,int hangup) {
  if (hangup) {
    struct termios disconnect = oldterm;
    disconnect.c_cflag &= ~CBAUD;
    disconnect.c_cflag |= B0;
#ifdef USE_TCATTR
    tcsetattr(fd, TCSANOW, &disconnect);
#else
    ioctl( fd, TCSETS, &disconnect);
#endif
    sleep(1);		/* make sure the hangup takes effect */
  }
#ifdef USE_TCATTR
  tcsetattr(fd, TCSANOW, &oldterm);
#else
  ioctl( fd, TCSETS, &oldterm);
#endif
}

#ifndef ECHOCTL
#define ECHOCTL 0 /* not POSIX */
#endif
#ifndef ONCLR
#define ONCLR 0
#endif


void terminal_new(int fd) {
  struct termios newterm;
  newterm = oldterm;   /* hopefully sensible defaults */
  newterm.c_iflag = BRKINT|IGNPAR|ICRNL|IXON|IXANY;
  newterm.c_oflag = OPOST|ONLCR;
#ifndef convex
  newterm.c_cflag = B38400|CS8|CREAD|HUPCL;
#else
  newterm.c_cflag = CS8|CREAD|HUPCL;
  newterm.c_tflag = _SETTFLAG|_SETWINSIZE;
  newterm.c_ispeed = B38400;
  newterm.c_ospeed = B38400;
  newterm.c_winsize.ws_row = 24;
  newterm.c_winsize.ws_col = 80;
  newterm.c_winsize.ws_xpixel = 480;
  newterm.c_winsize.ws_ypixel = 312;
#endif
  newterm.c_lflag = ISIG|ICANON|ECHO|ECHOE|ECHOCTL|IEXTEN;
  newterm.c_cc[VINTR] = ctrl('C');
  newterm.c_cc[VQUIT] = ctrl('\\');
  newterm.c_cc[VERASE] = 127; /* ^? */
  newterm.c_cc[VKILL] = ctrl('U');
  newterm.c_cc[VEOF] = ctrl('D');
#ifdef VSUSP
  newterm.c_cc[VSUSP] = ctrl('Z');
#endif
#ifdef VREPRINT
  newterm.c_cc[VREPRINT] = ctrl('R');
#endif
#ifdef VDISCARD
  newterm.c_cc[VDISCARD] = ctrl('O');
#endif
#ifdef VSTOP
  newterm.c_cc[VSTOP] = ctrl('S');
#endif
#ifdef VSTART
  newterm.c_cc[VSTART] = ctrl('Q');
#endif
#ifdef VWERASE
  newterm.c_cc[VWERASE] = ctrl('W');
#endif
#ifdef VLNEXT
  newterm.c_cc[VLNEXT] = ctrl('V');
#endif
#ifdef USE_TCATTR
  tcsetattr(fd, TCSANOW, &newterm);
#else
  ioctl( fd, TCSETS, &newterm);
#endif
}

#else /* bsd */

static int o_ldisc;
static struct sgttyb o_ttyb;
static struct tchars o_tchars;
static int o_lmode;
static struct ltchars o_ltchars;

void terminal_save(int fd)
{
  ioctl(fd, TIOCGETD, &o_ldisc);
  ioctl(fd, TIOCGETP, &o_ttyb);
  ioctl(fd, TIOCGETC, &o_tchars);
  ioctl(fd, TIOCLGET, &o_lmode);
  ioctl(fd, TIOCGLTC, &o_ltchars);

}

void terminal_restore(int fd,int hangup)
{
  ioctl(fd, TIOCSETD, &o_ldisc);
  ioctl(fd, TIOCSETC, &o_tchars);
  ioctl(fd, TIOCLSET, &o_lmode);
  ioctl(fd, TIOCSLTC, &o_ltchars);
  if (hangup) {
    struct sgttyb disconnect;
    disconnect = o_ttyb;
    disconnect.sg_ispeed = B0;
    disconnect.sg_ospeed = B0;
    ioctl(fd, TIOCSETP, &disconnect);
    sleep(1);		/* make sure the hangup takes effect */
  }
  ioctl(fd, TIOCSETP, &o_ttyb);
}

void terminal_raw(int fd)
{
  struct sgttyb m_ttyb;
  struct tchars m_tchars;
  struct ltchars m_ltchars;
  int m_ldisc;
  int m_lmode;

  /* initialize structures */
  ioctl(fd, TIOCGETP, &m_ttyb);
  ioctl(fd, TIOCGETC, &m_tchars);
  ioctl(fd, TIOCGLTC, &m_ltchars);

  m_ldisc = NTTYDISC;
  m_lmode = LLITOUT;

  /* modify structures */

/*  HSW 93/02/03, these shouldn't be set!
  m_ttyb.sg_ispeed = B9600;
  m_ttyb.sg_ospeed = B9600;
*/

  m_ttyb.sg_erase = -1;
  m_ttyb.sg_kill = -1;
  m_ttyb.sg_flags = RAW;

  m_tchars.t_quitc = -1;

  m_ltchars.t_suspc = -1;
  m_ltchars.t_dsuspc = -1;
  m_ltchars.t_flushc = -1;
  m_ltchars.t_lnextc = -1;
  
  /* update terminal */
  ioctl(fd, TIOCSETD, &m_ldisc);
  ioctl(fd, TIOCSETP, &m_ttyb);
  ioctl(fd, TIOCSETC, &m_tchars);
  ioctl(fd, TIOCLSET, &m_lmode);
  ioctl(fd, TIOCSLTC, &m_ltchars);

}

void terminal_new(int fd)
{
  struct sgttyb m_ttyb;
  struct tchars m_tchars;
  struct ltchars m_ltchars;
  int m_ldisc;
  int m_lmode;

  m_ldisc = NTTYDISC;
  m_lmode = LCRTBS|LCRTERA|LCRTKIL|LCTLECH|LTOSTOP;

  /* update sgttyb */
  ioctl(fd, TIOCGETP, &m_ttyb);
  m_ttyb.sg_flags = ECHO|CRMOD;
  m_ttyb.sg_erase = 127; /* ^? */
  m_ttyb.sg_kill = ctrl('U');

  /* initialize tchars and ltchars */
  m_tchars.t_intrc = ctrl('C');
  m_tchars.t_quitc = ctrl('\\');
  m_tchars.t_eofc = ctrl('D');
  m_tchars.t_startc = ctrl('Q');
  m_tchars.t_stopc = ctrl('S');
  m_tchars.t_brkc = 255;       /* disable */
  m_ltchars.t_werasc = ctrl('W');
  m_ltchars.t_suspc = ctrl('Z');
  m_ltchars.t_dsuspc = ctrl('Y');
  m_ltchars.t_rprntc = ctrl('R');
  m_ltchars.t_flushc = ctrl('O');
  m_ltchars.t_lnextc = ctrl('V');
  
  /* update terminal */
  ioctl(fd, TIOCSETD, &m_ldisc);
  ioctl(fd, TIOCSETP, &m_ttyb);
  ioctl(fd, TIOCSETC, &m_tchars);
  ioctl(fd, TIOCSLTC, &m_ltchars);
  ioctl(fd, TIOCLSET, &m_lmode);
}
#endif

#ifndef B19200
#define B19200 EXTA
#endif
#ifndef B38400
#define B38400 EXTB
#endif

unsigned long terminal_baud(int fd) {
  int baud;
#ifdef USE_TERMIOS
  struct termios terminfo;
#ifdef USE_TCATTR
  tcgetattr(fd, &terminfo);
#else
  ioctl( fd, TCGETS, &terminfo);
#endif
  baud = terminfo.c_cflag & CBAUD;
#else
  ioctl(fd, TIOCGETP, &o_ttyb);
  baud = o_ttyb.sg_ispeed < o_ttyb.sg_ospeed ? 
      o_ttyb.sg_ispeed : o_ttyb.sg_ospeed;
#endif

  switch ( baud ) {
  case B0:
	return  0;
	break;
  case B50:
	return  50;
	break;
  case B75:
	return  75;
	break;
  case B110:
	return  110;
	break;
  case B134:
	return  134;
	break;
  case B150:
	return  150;
	break;
  case B200:
	return  200;
	break;
  case B300:
	return  300;
	break;
  case B600:
	return  600;
	break;
  case B1200:
	return  1200;
	break;
  case B2400:
	return  2400;
	break;
  case B4800:
	return  4800;
	break;
  case B9600:
	return  9600;
	break;
  case B19200:
	return  19200;
	break;
  case B38400:
	return  38400;
	break;
  default:
	return  0;
	break;
  } 
  return 2400;
}

