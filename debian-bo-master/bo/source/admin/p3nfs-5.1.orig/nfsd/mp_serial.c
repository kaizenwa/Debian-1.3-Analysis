#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>	/* for usleep() */
#include <string.h>	/* for bzero() */
#include <termios.h>
#if defined(linux) || defined(_IBMR2)
# include <sys/ioctl.h>	/* for ioctl() */
#endif
#include <sys/errno.h>
#ifdef sun
# include <sys/ttold.h>	/* sun has TIOCEXCL there */
#endif
#if defined (__SVR4)
#include <stdlib.h>
#endif

#include "nfs_prot.h"
#include "mp.h"
#include "config.h"

extern int debug, dowakeup, errno;
static struct termios sti;

#ifndef hpux
# define mflag int
# define getline(fd, z) if(ioctl(fd,TIOCMGET,(caddr_t)&z)<0){perror("TCGET");return 0;}
# define PSION_ALIVE(z) (z & (TIOCM_DSR | TIOCM_CD | TIOCM_CTS))
# define dtr(fd, set) { int d = TIOCM_DTR;\
		if(ioctl(fd, set ? TIOCMBIS : TIOCMBIC, (caddr_t)&d) < 0) {\
		perror("TIOCMBIC/S"); return 0; } }
#else /* hpux */
# include <sys/termiox.h>
# include <sys/modem.h>

# define getline(fd, z) if(ioctl(fd,MCGETA,&z)<0) {perror("MCGETA");return 0;}
# define PSION_ALIVE(z) (z & (MDSR | MDCD | MCTS))
# define dtr(fd, set) { if (set) z |= (mflag)MDTR; else z &= (mflag)~MDTR;\
      			if(ioctl(fd, MCSETA, &z) < 0) {\
			perror("MCSETA"); return 0; } }
#endif


/*
#if !defined(CRTSCTS) && defined(_IBMR2)
#define CRTSCTS 0x80000000
#endif
*/

#ifdef __sgi
#define CRTSCTS CNEW_RTSCTS
#endif

#ifndef O_NOCTTY
# define O_NOCTTY 0
#endif


/* returns 1 if OK. */
int 
fd_is_still_alive(fd, wake)
int fd, wake;
{
  int w;
  mflag z;

  if (fd < 0) {
    if (debug > 2) printf("fd_is_alive: Checking on closed fd\n") ;
    return 0 ;
  }
  if(debug > 2) { printf("fd_is_alive: check lines "); fflush(stdout);}
  getline(fd, z)

  if(dowakeup)
    {
      if(debug > 2) printf("fd_is_alive: %s dtr\n", wake ? "set" : "clear");
      dtr(fd, wake);

      if(! PSION_ALIVE(z) && wake)
	{
	  if(debug > 1) printf("Trying to wake psion\n");
	  /* wake up psion by raising DTR */
	  dtr(fd, 1);

	  w = 10;
	  do
	    {
	      if(w < 10)
		usleep(100000);
	      getline(fd, z)
	    }
	  while(! PSION_ALIVE(z) && --w);
	  
	  if(debug > 1)
	    printf("Is %sawake (%d tries left)\n",PSION_ALIVE(z)?"":"not ",w);
	}
    }

  if(debug > 2) printf("fd_is_alive: %d\n", PSION_ALIVE(z));
  return PSION_ALIVE(z);
}

int
init_serial(dev, speed)
  char *dev;
  int speed;
{
  int fd, baud, clocal;
  struct termios ti;
#ifdef hpux
  struct termiox tx;
#endif
  static struct baud { int speed, baud; } btable[] =
    {
      { 9600,	B9600},

#ifdef B19200
      { 19200,	B19200},
#else
# ifdef EXTA
      { 19200,	EXTA},
# endif
#endif

#ifdef B38400
      { 38400,	B38400},
#else
# ifdef EXTB
      { 38400,	EXTB},
# endif
#endif

# ifdef B57600
      { 57600,	B57600},
# endif
      { 4800,	B4800},
      { 2400,	B2400},
      { 1200,	B1200},
      { 300, 	B300},
      { 75,	B75},
      { 50,	B50},
      {0,0}
    }, *bptr;
  
  if(speed)
    {
      for (bptr = btable; bptr->speed; bptr++)
	if (bptr->speed == speed)
	  break;
      if (!bptr->baud)
	{
	  fprintf(stderr, "Cannot match selected speed %d\n", speed);
	  exit(1);
	}
      baud = bptr->baud;
    }
  else
    {
      baud = 0;
    }

  if(background)
    {
      int uid, euid;

      if(debug) printf("using %s...\n", dev);
      euid = geteuid();
      uid = getuid();

#ifdef hpux
#define seteuid(a) setresuid(-1, a, -1)
#endif

      clocal = CLOCAL;
      if (seteuid(uid))
	{
	  perror("seteuid"); exit(1);
	}
      if((fd = open(dev, O_RDWR | O_NDELAY | O_NOCTTY , 0)) < 0)
	{
	  perror(dev); exit(1);
	}
      if (seteuid(euid))
	{
	  perror("seteuid back"); exit(1);
	}

      if(debug) printf("open done\n");
#ifdef TIOCEXCL
      ioctl(fd, TIOCEXCL, (char *) 0);	/* additional open() calls shall fail */
#else
      fprintf(stderr, "WARNING: opened %s non-exclusive!\n", dev);
#endif
    }
  else
    {
      char *f;

      fd = dup(0);

      if(tcgetattr(fd, &ti) < 0)
        {
	  perror("tcgetattr serial");
	  exit(1);
	}
      sti = ti ;      /* Save the state for shutdown time */
      clocal = ti.c_cflag & CLOCAL ;  /* and for setup */

      close(0);				/* Let's close 0,1,2 */
      f = "/dev/null";
      if(open(f, O_RDONLY, 0) != 0)
        {
	  perror(f); exit(0);
	}
      close(1);
      f = "/tmp/p3nfsd.out";
      if(open(f, O_WRONLY|O_CREAT|O_APPEND, 0666) != 1)
        {
	  perror(f); exit(0);
	}
      fprintf(stderr, "p3nfsd: output written to %s\n", f);
      close(2);
      dup(1);
    }

  bzero((char *)&ti, sizeof(struct termios));
#if defined(hpux) || defined(_IBMR2)
  ti.c_cflag = CS8 | HUPCL | clocal | CREAD;
#endif
#if defined(sun) || defined(linux) || defined(__sgi) || defined(__NetBSD__)
  ti.c_cflag = CS8 | HUPCL | clocal | CRTSCTS | CREAD;
  ti.c_iflag = IGNBRK | IGNPAR;
  ti.c_cc[VMIN] = 1;
  ti.c_cc[VTIME] = 0;
#endif
  cfsetispeed(&ti, baud);
  cfsetospeed(&ti, baud);

  if(tcsetattr(fd, TCSADRAIN, &ti) < 0)
    perror("tcsetattr TCSADRAIN");

#ifdef hpux
  bzero(&tx, sizeof(struct termiox));
  tx.x_hflag = RTSXOFF | CTSXON;
  if (ioctl(fd, TCSETXW, &tx) < 0)
    perror("TCSETXW");
#endif

#if defined(_IBMR2)
  ioctl(fd, TXDELCD, "dtr");
  ioctl(fd, TXDELCD, "xon");
  ioctl(fd, TXADDCD, "rts");  /* That's how AIX does CRTSCTS */
#endif

  return fd;
}

/*
 * We want to put the tty line back like we found it. It may be that
 * NetBSD is the only supported OS that needs this done to it.
 */
void
reset_serial(fd)
int fd;
{
  if(!background && fd >= 0)
    if(tcsetattr(fd, TCSANOW, &sti) < 0)
      perror("tcsetattr TCSANOW");
}

#if 0
#ifdef sun
/*
 * Before exiting we should close the serial line file descriptor
 * as we are probably on a "wicked sun", we have to reset the CRTSCTS
 * flag first, otherwise we will hang in close(2).
 * (suns that are not "wicked" are "broken" , i.e.: they do not have
 * patch 100513-04. There are also suns that are "wicked" and "broken".
 * These run Solaris >= 2.0 !! * YESSSSSS!!! gec
 */

void
ser_exit(status,fd)
int status,fd;
{
  struct termios ti;
  if(ioctl(fd, TCGETS, (caddr_t)&ti) < 0) {
    perror("TCGETSW");
  }
  ti.c_cflag &= ~CRTSCTS;
  if(ioctl(fd, TCSETS, (caddr_t)&ti) < 0) {
    perror("TCSETSW");
  }
  (void) close(fd);
}
#endif /* sun */
#endif
