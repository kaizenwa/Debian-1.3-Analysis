#ifdef __NetBSD__
#define EDEADLOCK EDEADLK
#endif


#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#ifndef EDEADLOCK
  #define EDEADLOCK EDEADLK
#endif
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <limits.h>
#include <assert.h>

#ifdef __NetBSD__
#include <signal.h>
#include <machine/pcvt_ioctl.h>
#include "netbsd_vm86.h"
#endif
#ifdef __linux__
#include <linux/vt.h>
#include <linux/fd.h>
#include <linux/hdreg.h>
#include <sys/vm86.h>
#include <syscall.h>
#endif

#include <sys/socket.h>
#include <sys/ioctl.h>
#ifndef __NetBSD__
#include <linux/if_ether.h>
#endif
#ifdef __NetBSD__
#include <sys/mman.h>
#else
extern caddr_t mmap __P ((caddr_t __addr, size_t __len,
                          int __prot, int __flags, int __fd, off_t __off));
                          extern int munmap __P ((caddr_t __addr, size_t
__len));
#include <linux/mman.h>
#endif



#include "config.h"
#include "memory.h"

#ifdef USE_MHPDBG
#include "mhpdbg.h"
#endif

#include "emu.h"

#include "bios.h"
#include "termio.h"
#include "video.h"
#include "timers.h"
#include "cmos.h"
#include "mouse.h"
#include "dosio.h"
#include "disks.h"
#include "xms.h"
#include "hgc.h"
#include "ipx.h"		/* TRB - add support for ipx */
#include "serial.h"
#include "int.h"
#include "bitops.h"
#include "pic.h"
#include "dpmi.h"
#ifdef __NetBSD__
#include <setjmp.h>
#endif

#ifdef NEW_KBD_CODE
#include "keyb_clients.h"
#endif

#ifdef USE_MHPDBG
  #include "mhpdbg.h"
#endif
  
#ifdef SIG
extern SillyG_t *SillyG;
#endif

#ifndef PAGE_SIZE
#define PAGE_SIZE       4096
#endif

inline int process_interrupt(SillyG_t *sg);


void io_select_init(void);
#if defined(SIG) && defined(REQUIRES_VM86PLUS)
int SillyG_pendind_irq_bits=0;

int SillyG_do_irq(void)
{
  int irq=pic_level_list[pic_ilevel], ret;
  ret = do_irq();
  SillyG_pendind_irq_bits &= ~(1 << irq);
  return ret;
}
#endif

inline int process_interrupt(SillyG_t *sg)
{
  int irq, ret=0;

  if ((irq = sg->irq) != 0) {
    h_printf("INTERRUPT: 0x%02x\n", irq);
    ret=pic_request(pic_irq_list[irq]);
  }
  return ret;
}


#if defined(SIG) && defined(REQUIRES_VM86PLUS)
inline void irq_select()
{
  if (SillyG) {
    int irq_bits =vm86_plus(VM86_GET_IRQ_BITS,0) & ~SillyG_pendind_irq_bits;
    if (irq_bits) {
      SillyG_t *sg=SillyG;
      while (sg->fd) {
        if (irq_bits & (1 << sg->irq)) {
          if (process_interrupt(sg)) {
            vm86_plus(VM86_GET_AND_RESET_IRQ,sg->irq);
            SillyG_pendind_irq_bits |= 1 << sg->irq;
            h_printf("SIG: We have an interrupt\n");
          }
        }
        sg++;
      }
    }
  }
}
#endif

/*  */
/* io_select @@@  24576 MOVED_CODE_BEGIN @@@ 01/23/96, ./src/base/misc/dosio.c --> src/base/misc/ioctl.c  */

void
io_select(fd_set fds)
{
  static int selrtn;
  static struct timeval tvptr;

  tvptr.tv_sec=0L;
  tvptr.tv_usec=0L;

#if defined(SIG) && defined(REQUIRES_VM86PLUS)
  irq_select();
#endif

  while ( ((selrtn = select(25, &fds, NULL, NULL, &tvptr)) == -1)
        && (errno == EINTR)) {
    tvptr.tv_sec=0L;
    tvptr.tv_usec=0L;
    g_printf("WARNING: interrupted io_select: %s\n", strerror(errno));
  }

  switch (selrtn) {
    case 0:			/* none ready, nothing to do :-) */
      return;
      break;

    case -1:			/* error (not EINTR) */
      error("ERROR: bad io_select: %s\n", strerror(errno));
      break;

    default:			/* has at least 1 descriptor ready */

      if ((mice->intdrv || mice->type == MOUSE_PS2) && mice->fd >= 0)
	if (FD_ISSET(mice->fd, &fds)) {
		m_printf("MOUSE: We have data\n");
	  pic_request(PIC_IMOUSE);
	}
      if (FD_ISSET(kbd_fd, &fds)) {
#ifdef NEW_KBD_CODE
	 keyb_client_run();
#else
	 getKeys();
#endif
      }
#ifdef USE_MHPDBG
      if (mhpdbg.fdin != -1) if (FD_ISSET(mhpdbg.fdin, &fds)) mhp_input();
#endif
      /* XXX */
#if 0
      fflush(stdout);
#endif
      break;
    }
#if 0
#ifdef USING_NET
    pic_request(16);
#endif
#endif

}

/* @@@ MOVE_END @@@ 24576 */



/*  */
/* io_select_init,add_to_io_select,remove_from_io_select,do_ioctl,queue_ioctl,do_queued_ioctl @@@  32768 MOVED_CODE_BEGIN @@@ 01/23/96, ./src/emu.c --> src/base/misc/ioctl.c  */
/*
 * DANG_BEGIN_FUNCTION io_select_init
 * 
 * description: 
 * Initialize fd_sets to NULL for both SIGIO and NON-SIGIO.
 * 
 * DANG_END_FUNCTION
 */
void 
io_select_init(void) {
    FD_ZERO(&fds_sigio);	/* initialize both fd_sets to 0 */
    FD_ZERO(&fds_no_sigio);
}

/*
 * DANG_BEGIN_FUNCTION add_to_io_select
 * 
 * arguments: 
 * fd - File handle to add to select statment
 * want_sigio - want SIGIO (1) if it's available, or not (0).
 * 
 * description: 
 * Add file handle to one of 2 select FDS_SET's depending on
 * whether the kernel can handle SIGIO.
 * 
 * DANG_END_FUNCTION
 */
void 
add_to_io_select(int new_fd, u_char want_sigio)
{
    if (use_sigio && want_sigio) {
	int             flags;
	flags = fcntl(new_fd, F_GETFL);
	fcntl(new_fd, F_SETOWN, getpid());
	fcntl(new_fd, F_SETFL, flags | use_sigio);
	FD_SET(new_fd, &fds_sigio);
	g_printf("GEN: fd=%d gets SIGIO, use_sigio=%d\n", new_fd, use_sigio);
    } else {
	FD_SET(new_fd, &fds_no_sigio);
	g_printf("GEN: fd=%d does not get SIGIO, use_sigio=%d\n", new_fd, use_sigio);
	not_use_sigio++;
    }
}

/*
 * DANG_BEGIN_FUNCTION remove_from_io_select
 * 
 * arguments: 
 * fd - File handle to remove from select statment. 
 * used_sigio - used SIGIO (1) if it's available, or not (0).
 * 
 * description: 
 * Remove a file handle from one of 2 select FDS_SET's depending
 * on whether the kernel can handle SIGIO.
 * 
 * DANG_END_FUNCTION
 */
void 
remove_from_io_select(int new_fd, u_char used_sigio)
{
    if (new_fd < 0) {
	g_printf("GEN: removing bogus fd %d (ignoring)\n", new_fd);
	return;
    }
    if (use_sigio && used_sigio) {
	int             flags;
	flags = fcntl(new_fd, F_GETFL);
	fcntl(new_fd, F_SETOWN, NULL);
	fcntl(new_fd, F_SETFL, flags & ~(use_sigio));
	FD_CLR(new_fd, &fds_sigio);
	g_printf("GEN: fd=%d removed from select SIGIO\n", new_fd);
    } else {
	FD_CLR(new_fd, &fds_no_sigio);
	g_printf("GEN: fd=%d removed from select\n", new_fd);
	not_use_sigio++;
    }
}

int
do_ioctl(int fd, int req, int param3)
{
    int             tmp;

    if (in_sighandler && in_ioctl) {
	k_printf("KBD: do_ioctl(): in ioctl %d 0x%04x 0x%04x.\nqueuing: %d 0x%04x 0x%04x\n",
		 curi.fd, curi.req, curi.param3, fd, req, param3);
	queue_ioctl(fd, req, param3);
	errno = EDEADLOCK;
#ifdef SYNC_ALOT
	fflush(stdout);
	sync();			/* for safety */
#endif
	return -1;
    } else {
	in_ioctl = 1;
	curi.fd = fd;
	curi.req = req;
	curi.param3 = param3;
	if (iq.queued) {
	    k_printf("KBD: detected queued ioctl in do_ioctl(): %d 0x%04x 0x%04x\n",
		     iq.fd, iq.req, iq.param3);
	}
	k_printf("KBD: IOCTL fd=0x%x, req=0x%x, param3=0x%x\n", fd, req, param3);
	tmp = ioctl(fd, req, param3);
	in_ioctl = 0;
	return tmp;
    }
}

int
queue_ioctl(int fd, int req, int param3)
{
    if (iq.queued) {
	error("ioctl already queued: %d 0x%04x 0x%04x\n", iq.fd, iq.req,
	      iq.param3);
	return 1;
    }
    iq.fd = fd;
    iq.req = req;
    iq.param3 = param3;
    iq.queued = 1;

    return 0;			/* success */
}

void
do_queued_ioctl(void)
{
    if (iq.queued) {
	iq.queued = 0;
	do_ioctl(iq.fd, iq.req, iq.param3);
    }
}
/* @@@ MOVE_END @@@ 32768 */



