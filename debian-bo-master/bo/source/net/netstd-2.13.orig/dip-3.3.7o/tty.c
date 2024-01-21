/*
 * dip		A program for handling dialup IP connecions.
 *		UNIX terminal I/O support functions.  This file takes
 *		care of opening, setting up and maintaining the line,
 *		and it takes care of allocating the buffers and init-
 *		ializing their control structures.
 *
 * Version:	@(#)tty.c	3.3.6	12/13/93
 * Modified:	@(#)tty.c	3.3.7	05/22/94
 *              @(#)tty.c	3.3.7f	08/18/94
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *		Lock file stuff stolen from Taylor UUCP
 *		Copyright (C) 1991, 1992 Ian Lance Taylor
 *
 *              Uri Blumenthal <uri@watson.ibm.com>
 *              (C) 1994
 *
 *		Paul Cadach, <paul@paul.east.alma-ata.su>
 *		(C) 1994
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#include "dip.h"
#include <sys/stat.h>
#ifdef LINUX
#   include <linux/fs.h>
#   include <linux/tty.h>
#ifndef NO_SERIAL
#   include <linux/serial.h> /* for Linux-1.1.13 */
#endif /* ~NO_SERIAL */
#endif


#define TTY_BUFSIZE	1024		/* size of one serial buffer	*/


static struct {
  char	*speed;
  int	code;
} tty_speeds[] = {			/* table of usable baud rates	*/
  { "0",        B0      },
  { "50",	B50	}, { "75",	B75  	},	
  { "110",	B110	}, { "300",	B300	},
  { "600",	B600	}, { "1200",	B1200	},
  { "2400",	B2400	}, { "4800",	B4800	},
  { "9600",	B9600	},
#ifdef B14400
  { "14400",	B14400	},
#endif
#ifdef B19200
  { "19200",	B19200	},
#endif
#ifdef B38400
  { "38400",	B38400	},
#endif
#ifdef B57600
  { "57600",	B57600	},
#endif
#ifdef B115200
  { "115200",	B115200	},
#endif
  { NULL,	0	}
};


static struct termios	tty_saved;	/* saved TTY device state	*/

static struct termios   tty_current;	/* current TTY device state	*/

static char		*in_buff,	/* line input/output buffers	*/
			*out_buff,
			*in_ptr,
			*out_ptr;

static int		in_size,	/* buffer sizes and counters	*/
			out_size,
			in_cnt,
			out_cnt,
			tty_sdisc,	/* saved TTY line discipline	*/
			tty_ldisc,	/* current TTY line discipline	*/
			tty_fd = -1,	/* TTY file descriptor		*/
			tty_mode;   	/* TTY mode (login or not)      */


/* Disable any messages to the input channel of this process. */
int
tty_nomesg(void)
{
  return(fchmod(0, 0600));
}


static int
tty_already_locked(char *nam) /* nam - complete path to lock file */
{
  int  i = 0, pid = 0;
  FILE *fd = (FILE *)0;

  /* Does the lock file on our device exist? */
  if ((fd = fopen(nam, "r")) == (FILE *)0)
    return 0; /* No, return perm to continue */

  /* Yes, the lock is there.  Now let's make sure */
  /* at least there's no active process that owns */
  /* that lock.                                   */
  i = fscanf(fd, "%d", &pid);
  (void) fclose(fd);
  
  if (i != 1) /* Lock file format's wrong! Kill't */
    return 0;

  /* We got the pid, check if the process's alive */
  if (kill(pid, 0) == 0)      /* it found process */
      return 1;           /* Yup, it's running... */

  /* Dead, we can proceed locking this device...  */
  return 0;
}


/* Lock or unlock a terminal line. */
int
tty_lock(char *path, int mode)
{
  static char saved_path[PATH_MAX];
  static char nam_tmp[PATH_MAX];
  static char dev_nam[20];
  static int saved_lock = 0;
  struct passwd *pw;
  pid_t ime;
#if HAVE_V2_LOCKFILES
  int cwrote;
#endif /* HAVE_V2_LOCKFILES */

  bzero(nam_tmp, sizeof(nam_tmp));
  if (mode == 1) {	/* lock */
    if (path == NULL) return(0);	/* standard input */
    bzero(dev_nam, sizeof(dev_nam));
    sprintf(saved_path, "%s/LCK..%s", _PATH_LOCKD, path);
    strcpy(dev_nam, path);
    {
      FILE *fd;
      if (tty_already_locked(saved_path) == 1) {
	syslog(LOG_ERR, "DIP: attempt to use already locked tty %s\n",
	       saved_path);
	fprintf(stderr, "DIP: attempt to use already locked tty %s\n",
		saved_path);
	return (-1);
      }
      if ((fd = fopen(saved_path, "w")) == (FILE *)0) {
	syslog(LOG_ERR, "DIP: tty: lock: (%s): %s\n",
	       saved_path, strerror(errno));
	fprintf(stderr, "DIP: tty: lock: (%s): %s\n",
		saved_path, strerror(errno));
	return(-1);
      }
      ime = getpid();
#ifdef HAVE_V2_LOCKFILES
      cwrote = write (fileno(fd), &ime, sizeof(ime));
#else
      fprintf(fd, "%10d\n", (int)ime);
#endif
      (void)fclose(fd);
    }
    
    /* Make sure UUCP owns the lockfile.  Required by some packages. */
    if ((pw = getpwnam(_UID_UUCP)) == NULL) {
      fprintf(stderr, "DIP: tty: lock: UUCP user %s unknown!\n",
	      _UID_UUCP);
      return(0);	/* keep the lock anyway */
    }
    (void) chown(saved_path, pw->pw_uid, pw->pw_gid);
    saved_lock = 1;
  } else {
    if (mode == 2) { /* re-acquire a lock after a fork() */
      FILE *fd;
      
      if (saved_lock != 1) {
	syslog(LOG_ERR, 
	       "DIP: tty_lock reaquire: lock was not saved!\n");
	return (-1);
      }
      if ((fd = fopen(saved_path, "w")) == (FILE *)0) {
	syslog(LOG_ERR, "DIP:tty_lock(%s) reaquire: %s\n",
	       saved_path, strerror(errno));
	fprintf(stderr, "DIP:tty_lock: reacquire (%s): %s\n",
		saved_path, strerror(errno));
	return(-1);
      }
      ime = getpid();
#ifdef HAVE_V2_LOCKFILES
      cwrote = write (fileno(fd), &ime, sizeof(ime));
#else
      fprintf(fd, "%10d\n", (int)ime);
#endif
      (void)fclose(fd);
      (void) chmod(saved_path, 0444);
      (void) chown(saved_path, getuid(), getgid());
      return(0);
    } else {	/* unlock */
      FILE *fd;
      
      if (saved_lock != 1) {
	syslog(LOG_ERR, "DIP:tty_lock: lock was not saved?!\n");
	return(0);
      }
      if ((fd = fopen(saved_path, "w")) == (FILE *)0) {
	syslog(LOG_ERR, "DIP:tty_lock: can't reopen to delete: %s\n",
	       strerror(errno));
	return (-1);
      }
      if (unlink(saved_path) < 0) {
	syslog(LOG_ERR, "DIP: tty: unlock: (%s): %s\n", saved_path,
	       strerror(errno));
	fprintf(stderr, "DIP: tty: unlock: (%s): %s\n", saved_path,
		strerror(errno));
	saved_lock = 0;
	return(-1);
      }
      saved_lock = 0;
    }
  }
  
  return(0);
}


/* Find a serial speed code in the table. */
static int
tty_find_speed(char *speed)
{
  int i;

  i = 0;
  while (tty_speeds[i].speed != NULL) {
	if (! strcmp(tty_speeds[i].speed, speed)) return(tty_speeds[i].code);
	i++;
  }
  return(-EINVAL);
}


/* Set the number of stop bits. */
static int
tty_set_stopbits(struct termios *tty, char *stopbits)
{
  if (opt_v) printf("DIP: tty: set_stopbits: %c\n", *stopbits);
  switch(*stopbits) {
	case '1':
		tty->c_cflag &= ~CSTOPB;
		break;

	case '2':
		tty->c_cflag |= CSTOPB;
		break;

	default:
		return(-EINVAL);
  }
  return(0);
}


/* Set the number of data bits. */
static int
tty_set_databits(struct termios *tty, char *databits)
{
  if (opt_v) printf("DIP: tty: set_databits: %c\n", *databits);
  tty->c_cflag &= ~CSIZE;
  switch(*databits) {
	case '5':
		tty->c_cflag |= CS5;
		break;

	case '6':
		tty->c_cflag |= CS6;
		break;

	case '7':
		tty->c_cflag |= CS7;
		break;

	case '8':
		tty->c_cflag |= CS8;
		break;

	default:
		return(-EINVAL);
  }
  return(0);
}


/* Set the type of parity encoding. */
static int
tty_set_parity(struct termios *tty, char *parity)
{
  if (opt_v) printf("DIP: tty: set_parity: %c\n", *parity);
  switch(toupper(*parity)) {
	case 'N':
		tty->c_cflag &= ~(PARENB | PARODD);
		break;  

	case 'O':
		tty->c_cflag &= ~(PARENB | PARODD);
		tty->c_cflag |= (PARENB | PARODD);
		break;

	case 'E':
		tty->c_cflag &= ~(PARENB | PARODD);
		tty->c_cflag |= (PARENB);
		break;

	default:
		return(-EINVAL);
  }
  return(0);
}


/* Set the line speed of a terminal line. */
static int
tty_set_speed(struct termios *tty, char *speed)
{
  int code;

  if (opt_v) printf("DIP: tty: set_speed: %s\n", speed);
  code = tty_find_speed(speed);
  if (code < 0) return(-1);
  tty->c_cflag &= ~CBAUD;
  tty->c_cflag |= code;

  return(0);
}


/* Put a terminal line in a transparent state. */
static int
tty_set_raw(struct termios *tty)
{
  int i;
  int speed;

  for(i = 0; i < NCCS; i++)
		tty->c_cc[i] = '\0';		/* no spec chr		*/
  tty->c_cc[VMIN] = 1;
  tty->c_cc[VTIME] = 0;
  tty->c_iflag = (IGNBRK | IGNPAR);		/* input flags		*/
  tty->c_oflag = (0);				/* output flags		*/
  tty->c_lflag = (0);				/* local flags		*/
  speed = (tty->c_cflag & CBAUD);		/* save current speed	*/
  tty->c_cflag = (CRTSCTS|HUPCL|CREAD|CLOCAL);	/* UART flags		*/
  tty->c_cflag |= speed;			/* restore speed	*/
  return(0);
}


/* Fetch the state of a terminal. */
static int
tty_get_state(struct termios *tty)
{
  if (tty_fd >= 0) {
  	if (ioctl(tty_fd, TCGETS, tty) < 0) {
  	  syslog(LOG_ERR, "DIP: tty: get_state: %s\n", strerror(errno));
	  if (opt_v == 1)
  	     fprintf(stderr, "DIP: tty: get_state: %s\n", strerror(errno));
  	  return(-errno);
  	}
  	return(0);
  }
  return (-1);
}


/* Set the state of a terminal. */
static int
tty_set_state(struct termios *tty)
{
  if (tty_fd >= 0) {
  	if (ioctl(tty_fd, TCSETS, tty) < 0) {
 	    syslog(LOG_ERR, "DIP: tty: set_state: %s\n", strerror(errno));
	    if (opt_v == 1)
	       fprintf(stderr, "DIP: tty: set_state: %s\n", strerror(errno));
	    return(-errno);
	  }
	  return(0);
  }
  return (-1);
}


/* Get the line discipline of a terminal line. */
int
tty_get_disc(int *disc)
{
  if (tty_fd >= 0) {
  	if (ioctl(tty_fd, TIOCGETD, disc) < 0) {
	  if (opt_v == 1)
   	    syslog(LOG_ERR, "DIP: tty: get_disc: %s\n", strerror(errno));
  	  return(-errno);
  	}
  	return(0);
  }
  return (-1);
}


/* Set the line discipline of a terminal line. */
int
tty_set_disc(int disc)
{
  if (tty_fd >= 0) {
 	 if (disc == -1) disc = tty_sdisc;

 	 if (ioctl(tty_fd, TIOCSETD, &disc) < 0) {
	   if (opt_v == 1) {
	     fprintf(stderr, 
		    "Probably you don't have SLIP/CSLIP/PPP in your kernel?\n");
 	     syslog(LOG_ERR, "DIP: tty: set_disc(%d): %s\n", disc, strerror(errno));
	   }
 	   return(-errno);
 	 }
 	 return(0);
  }
  return (-1);
}


/* Get the encapsulation type of a terminal line. */
int
tty_get_encap(int *encap)
{
  if (tty_fd >= 0) {
	  if (ioctl(tty_fd, SIOCGIFENCAP, encap) < 0) {
	    syslog(LOG_ERR, "DIP: tty: get_encap: %s\n", strerror(errno));
	    fprintf(stderr, "DIP: tty: get_encap: %s\n", strerror(errno));
	    return(-errno);
	  }
	  return(0);
  }
  return (-1);
}


/* Set the encapsulation type of a terminal line. */
int
tty_set_encap(int encap)
{
  if (tty_fd >= 0) {
  	if (ioctl(tty_fd, SIOCSIFENCAP, &encap) < 0) {
		fprintf(stderr, "DIP: tty: set_encap(%d): %s\n",
			encap, strerror(errno));
		return(-errno);
  	}
  	return(0);
  }
  return (-1);
}


/* Fetch the name of the network interface attached to this terminal. */
int
tty_get_name(char *name)
{
  if (tty_fd >= 0) {
  	if (ioctl(tty_fd, SIOCGIFNAME, name) < 0) {
 	   syslog(LOG_ERR, "DIP: tty_get_name: %s", strerror(errno));
    	   fprintf(stderr, "DIP: tty_get_name: %s\n", strerror(errno));
	   return(-errno);
	  }
	  return(0);
  }
  return (-1);
}


/* Read one character (byte) from the TTY link. */
int
tty_getc(void)
{
  int s;

  if (tty_fd < 0)
	return (-1);

  if (in_cnt <= 0) {
	s = read(tty_fd, in_buff, in_size);
	in_cnt = s;
	in_ptr = in_buff;
  }

  if (in_cnt < 0) {
    if (opt_v == 1) {
	syslog(LOG_ERR, "DIP: tty_getc: I/O error. (%s)",
	       strerror(errno));
	syslog(LOG_ERR, "DIP: Probably line disconnected!");
    }
    return(-1);
  }

  s = (int) *in_ptr;
  s &= 0xFF;
  in_ptr++;
  in_cnt--;
  return(s);
}


/* Write one character (byte) to the TTY link. */
int
tty_putc(int c)
{
  int s;

  if ((out_cnt == out_size) || (c == -1)) {
    s = write(tty_fd, out_buff, out_cnt);
    out_cnt = 0;
    out_ptr = out_buff;
    if (s < 0) {
      if (opt_v == 1) {
	syslog(LOG_ERR, "DIP: Probably line disconnected!");
	syslog(LOG_ERR, "DIP: tty_puts: failed to write to tty (%s)...",
	       strerror(errno));
      }
      return(-1);
    }
  }
  
  if (c != -1) {
    *out_ptr = (char) c;
    out_ptr++;
    out_cnt++;
  }
  
  return(0);
}


/* Output a string of characters to the TTY link. */
int
tty_puts(char *s)
{
  while(*s != '\0') 
    if (tty_putc((int) *s++) < 0)
	return (-1);
  if (tty_putc(-1) < 0)		/* flush */
	return (-1);
  return (0);
}


/* Return the TTY link's file descriptor. */
int
tty_askfd(void)
{
  return(tty_fd);
}


/* Set the number of databits a terminal line. */
int
tty_databits(char *bits)
{
  if (tty_set_databits(&tty_current, bits) < 0) return(-1);
  return(tty_set_state(&tty_current));
}


/* Set the number of stopbits of a terminal line. */
int
tty_stopbits(char *bits)
{
  if (tty_set_stopbits(&tty_current, bits) < 0) return(-1);
  return(tty_set_state(&tty_current));
}


/* Set the type of parity of a terminal line. */
int
tty_parity(char *type)
{
  if (tty_set_parity(&tty_current, type) < 0) return(-1);
  return(tty_set_state(&tty_current));
}


/* Set the line speed of a terminal line. */
int
tty_speed(char *speed)
{
#if defined(LINUX) || defined(linux)
  struct serial_struct info;
  int spd;

  spd = atoi(speed);
  if (ioctl(tty_fd, TIOCGSERIAL, &info)) return(-1);
  info.flags = info.flags & ~ASYNC_SPD_MASK;

  if (spd == 115200) {
          if (tty_set_speed(&tty_current, speed) < 0) return(-1);
          info.flags |= ASYNC_SPD_VHI;
  } else if (spd == 57600) {
          if (tty_set_speed(&tty_current, speed) < 0) return(-1);
          info.flags |= ASYNC_SPD_HI;
  } else {
	  if (tty_set_speed(&tty_current, speed) < 0) return(-1);
  }
  if (ioctl(tty_fd, TIOCSSERIAL, &info)) return(-1);
#else
  if (tty_set_speed(&tty_current, speed) < 0) return(-1);
#endif

  return(tty_set_state(&tty_current));
}


/* Hangup the line. */
int
hanguptty(void)
{
  struct termios tty;

  tty = tty_current;
  (void) tty_set_speed(&tty, "0");
  if (tty_set_state(&tty) < 0) {
    syslog(LOG_ERR, "DIP: tty: hangup(DROP): %s\n", strerror(errno));
    fprintf(stderr, "DIP: tty: hangup(DROP): %s\n", strerror(errno));
    return(-errno);
  }
  
  (void) sleep(3);
  
  if (tty_set_state(&tty_current) < 0) {
    syslog(LOG_ERR, "DIP: tty: hangup(RAISE): %s\n", strerror(errno));
    fprintf(stderr, "DIP: tty: hangup(RAISE): %s\n", strerror(errno));
    return(-errno);
  }
  return(0);
}

/* Clear the CLOCAL bit. (detect carrier loss) */
int
tty_notlocal(void)
{
  if (tty_fd < 0)
	return (-1);

  /* Release any control terminal we might have. */
  if (setsid() < 0) {
    syslog(LOG_ERR, 
	   "DIP: tty_notlocal cannot setsid: %s\n", strerror(errno));
    fprintf(stderr, 
	    "DIP: tty_notlocal cannot setsid: %s\n", strerror(errno));
  }

  if (opt_v) {
    char ch0[20];
    char ch1[20];

    if (isatty(0)) /* "isatty()" is suggested by Mike Castle */
       strcpy(ch0, ttyname(0));
    else
       strcpy(ch0, "not defined");

    if (isatty(tty_fd))
       strcpy(ch1, ttyname(tty_fd));
    else
       strcpy(ch0, "not defined");
    syslog(LOG_ERR, "tty_notlocal: file0: %s  flle%1d %s\n", ch0, tty_fd, ch1);
  }

  if (ioctl(tty_fd, TIOCSCTTY, 1) < 0) {
    syslog(LOG_ERR, 
	   "DIP: tty_notlocal cannot TIOCSCTTY: %s\n", strerror(errno));
    fprintf(stderr, 
	    "DIP: tty_notlocal cannot TIOCSCTTY: %s\n", strerror(errno));
  }

  tty_current.c_cflag &= ~CLOCAL;
  if (tty_set_state(&tty_current) < 0) {
    syslog(LOG_ERR, 
	   "DIP: tty_notlocal cannot clr CLOCAL: %s\n", strerror(errno));
    fprintf(stderr, 
	    "DIP: tty_notlocal cannot clr CLOCAL: %s\n", strerror(errno));
    return(-errno);
  }
  return(0);
}

int tty_login(void)
{
  if (ioctl(tty_fd, TIOCSCTTY, 1) < 0)
    syslog(LOG_ERR, 
	   "DIP: tty_login cannot TIOCSCTTY: %s\n", strerror(errno));  
  tty_current.c_cflag &= ~CLOCAL;
  if (tty_set_state(&tty_current) < 0)
    syslog(LOG_ERR, 
	   "DIP: tty_notlocal cannot clr CLOCAL: %s\n", strerror(errno));
  return 0;
} /* tty_login() */


/* Flush input on the terminal. */
int
tty_flush(void)
{
  int blkmode;
  char buf;

  if (tty_fd < 0)
	return (-1);

  blkmode = fcntl(tty_fd, F_GETFL, NULL);
  fcntl(tty_fd, F_SETFL, (O_NDELAY | blkmode));
  while (read(tty_fd, &buf, 1) > 0)
		;
  fcntl(tty_fd, F_SETFL, blkmode);
  return(0);
}


/* Close down a terminal line. */
int
tty_close(void)
{
  if (tty_fd < 0)
     return (-1);

  (void) tty_set_disc(tty_sdisc);
  
  if (tty_set_state(&tty_saved) < 0) {
	syslog(LOG_ERR, "DIP: tty_close:tcsetattr(tty_saved): %s\n", 
	       strerror(errno));
	fprintf(stderr, "DIP: tty_close: restore: %s\n", strerror(errno));
	(void) close(tty_fd);
	tty_fd = -1;
        sleep (1);
	(void) tty_lock("no_diff", 0);
	return(-errno);
  }

  (void) hanguptty();
  (void) mdm_hangup();
  (void) close(tty_fd);
  tty_fd = -1;
  (void) tty_lock("dont_care", 0);

  return(0);
}


int tty_login_close(void)
{
#ifdef NE_PAUL
  if (tty_fd >= 0) {
    (void) tty_get_name(mydip.ifname);
    (void) detach(&mydip);
    (void) mdm_hangup();
  }
#else
  if (tty_fd >= 0)
      (void) mdm_hangup();
#endif
  return 0;
}


/* Open and initialize a terminal line. */
int
tty_open(char *name)
{
  char path[PATH_MAX];
  register char *sp;
  int fd=-1, flags=0;
  uid_t euid = geteuid();

  tty_mode = 0;


  /* Try opening the TTY device. */
  if (name != NULL) {
	if ((sp = strrchr(name, '/')) != (char *)NULL) *sp++ = '\0';
	  else sp = name;
	sprintf(path, "/dev/%s", sp);
  	/* Now - can we lock it? */
  	if(tty_lock(sp, 1)) {
		fprintf(stderr, "DIP: can't open - problems with locking %s\n",
			sp);
		return (-1);
  	}
	/* drop privileges here for security reasons */
	seteuid(getuid()); /* now can attempt to open tty */
	if ((fd = open(path, O_RDWR|O_NDELAY)) < 0) {
		fprintf(stderr, "DIP: tty: open(%s, RW): %s\n",
						path, strerror(errno));
		seteuid(euid); /* return it back, just in case */
		tty_lock("drop_it", 0);
		return(-errno);
	}
	seteuid(euid); /* get the privileges back */
	tty_fd = fd;
	if (opt_v) {
	  printf("DIP: tty_open: %s (%d) ", path, fd);
	  syslog(LOG_INFO, "DIP: tty_open: %s (%d) ", path, fd);
	}
  } else {
	tty_fd = 0;
	sp = (char *)NULL;
  }

  /* Size and allocate the I/O buffers. */
  in_size = TTY_BUFSIZE;
  out_size = in_size;
  in_buff = (char *) malloc(in_size);
  out_buff = (char *) malloc(out_size);
  if (in_buff == (char *)NULL || out_buff == (char *)NULL) {
	fprintf(stderr, "DIP: tty_open: cannot alloc(%d, %d) buffers (%d)\n",
						in_size, out_size, errno);
	return(-ENOMEM);
  }
  in_cnt = 0;
  out_cnt = 0;
  in_ptr = in_buff;
  out_ptr = out_buff;
  out_size -= 4; /* safety */
  if (opt_v) printf("DIP: tty_open: IBUF=%d OBUF=%d\n", in_size, out_size);

  /* Fetch the current state of the terminal. */
  if (tty_get_state(&tty_saved) < 0) {
	fprintf(stderr, "DIP: tty_open: cannot get current state!\n");
	return(-errno);
  }
  
  (void) memcpy ((char *)&tty_current,
	  (char *)&tty_saved, sizeof(struct termios));

  /* Fetch the current line discipline of this terminal. */
  if (tty_get_disc(&tty_sdisc) < 0) {
	fprintf(stderr, "DIP: tty_open: cannot get current line disc!\n");
	return(-errno);
  } 
  tty_ldisc = tty_sdisc;

  /* Put this terminal line in a 8-bit transparent mode. */
  if (tty_set_raw(&tty_current) < 0) {
	fprintf(stderr, "DIP: tty_open: cannot set RAW mode!\n");
	return(-errno);
  }

  /* If we are running in MASTER mode, set the default speed. */
  if ((name != NULL) && (tty_set_speed(&tty_current, "38400") != 0)) {
	fprintf(stderr, "DIP: tty_open: cannot set 38400 bps!\n");
	return(-errno);
  }

  /* Set up a completely 8-bit clean line. */
  if (tty_set_databits(&tty_current, "8") ||
      tty_set_stopbits(&tty_current, "1") ||
      tty_set_parity(&tty_current, "N")) {
	fprintf(stderr, "DIP: tty_open: cannot set 8N1 mode!\n");
	return(-errno);
  }

  /* Set the new line mode. */
  if ((fd = tty_set_state(&tty_current)) < 0) return(fd);
  /* Clear the NDELAY flag now (line is in CLOCAL) */
  flags = fcntl(tty_fd, F_GETFL, 0);
  fcntl(tty_fd, F_SETFL, flags & ~O_NDELAY);

  /* OK, all done.  Lock this terminal line. */
  return(0);
}

void
tty_setmode(int mode)
{
  tty_mode = mode;
}

int tty_getmode(void)
{
#if 0
  return(tty_mode);
#else
  return 1;
#endif
}


void 
tty_sendbreak(void)
{
    if (tty_fd >= 0)
        tcsendbreak(tty_fd, 0);
#if 0
        ioctl(tty_fd, TCSBRK, 0);
#endif
}

