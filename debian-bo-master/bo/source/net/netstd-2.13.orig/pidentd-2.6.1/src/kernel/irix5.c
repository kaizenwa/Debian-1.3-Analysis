/*
** kernel/irix5.c             Kernel access functions to retrieve user number
**
** This program is in the public domain and may be used freely by anyone
** who wants to. 
**
** Last update: 26 Jan 1995
**
** Please send bug fixes/bug reports to: Peter Eriksson <pen@lysator.liu.se>
**
**
** Hacked to work with irix5, 27 May 1994 by
** Robert Banz (banz@umbc.edu) Univ. of Maryland, Baltimore County
**
** does some things the irix4 way, some the svr4 way, and some just the 
** silly irix5 way.
**
** Hacked to work with irix5.3, 26 Jan 1995 by
** Frank Maas (maas@wi.leidenuniv.nl) Leiden University, The Netherlands
** but all the credits go to Robert Banz (again), who found out about the
** hacks and included them in sources for pidentd-2.3.
**
*/
#define _KMEMUSER

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <nlist.h>
#include <pwd.h>
#include <signal.h>
#include <syslog.h>
#include "kvm.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/cred.h>
#include <sys/socketvar.h>
#ifdef IRIX53
/** hack 1: IRIX 5.3 uses 64bit int's for file offsets in the kernel **/
/**         but a 32bit int is used in user programs. sadly though,  **/
/**         there is no way of using <sys/file.h> properly, so we    **/
/**         create our own struct.                                   **/
typedef struct file {
   struct file 	  *f_next;
   struct file 	  *f_prev;
   int 		  f_flag;	/* ushort in <sys/file.h> */
   cnt_t 	  f_count;
   unsigned short f_lock;	/* lock_t (uint) in <sys/file.h> */
   struct vnode   *f_vnode;
   __uint64_t 	  f_offset;	/* off_t (long) in <sys/file.h> */
   struct cred 	  *f_cred;
   cnt_t 	  f_msgcount;
} file_t;
#else
#include <sys/file.h>
#endif	/* IRIX53 */

#ifdef IRIX6
#define nlist nlist64
#endif

/** Well... here some problems begin: when upgrading IRIX to 5.3 the **/
/** `inst' program shows one of its peculiar bugs: the file vnode.h  **/
/** has changed location in between versions and now the file is up- **/
/** grade first (new package) and then deleted (old package). So if  **/
/** you have problems finding this file: reinstall eoe1.sw.unix.     **/
#include <sys/vnode.h>

#include <fcntl.h>

#include <net/if.h>
#include <net/route.h>
#include <netinet/in.h>

#include <netinet/in_pcb.h>

#include <netinet/tcp.h>
#include <netinet/ip_var.h>
#include <netinet/tcp_timer.h>
#include <netinet/tcp_var.h>

#include <arpa/inet.h>

#include "identd.h"
#include "error.h"


extern void *calloc();
extern void *malloc();


struct nlist nl[] =
{
#define N_FILE 0 
#define N_TCB  1
 
  { "file" },
  { "tcb" },
  { "" }
};

static kvm_t *kd;

static struct file *xfile;
static int nfile;

static struct inpcb tcb;

int k_open()
{
  /*
  ** Open the kernel memory device
  */
  if (!(kd = kvm_open(path_unix, path_kmem, NULL, O_RDONLY, NULL)))
    ERROR("main: kvm_open");
  
  /*
  ** Extract offsets to the needed variables in the kernel
  */
  if (kvm_nlist(kd, nl) != 0)
    ERROR("main: kvm_nlist");

  return 0;
}


/*
** Get a piece of kernel memory with error handling.
** Returns 1 if call succeeded, else 0 (zero).
*/
static int getbuf(addr, buf, len, what)
  long addr;
  char *buf;
  int len;
  char *what;
{

  if (kvm_read(kd, addr, buf, len) < 0)
  {
    if (syslog_flag)
      syslog(LOG_ERR, "getbuf: kvm_read(%08x, %d) - %s : %m",
	     addr, len, what);

    return 0;
  }
  
  return 1;
}


/*
** Traverse the inpcb list until a match is found.
** Returns NULL if no match.
** (this is basically the same as the irix4 code)
*/
static struct socket *
    getlist(pcbp, faddr, fport, laddr, lport)
  struct inpcb *pcbp;
  struct in_addr *faddr;
  int fport;
  struct in_addr *laddr;
  int lport;
{
  struct inpcb *head;
/** hack 2: there is a slight problem when scanning through lists that **/
/**         change while identd scans; to prevent infinite loops a     **/
/**         counter is introduced                                      **/
  int count = 0xffff;

  if (!pcbp)
    return NULL;

  head = pcbp->inp_prev;
  do 
  {
    if ( pcbp->inp_faddr.s_addr == faddr->s_addr &&
	 pcbp->inp_laddr.s_addr == laddr->s_addr &&
	 pcbp->inp_fport        == fport &&
	 pcbp->inp_lport        == lport ) {
      return pcbp->inp_socket;
    }
  } while ((count--) && pcbp->inp_next != head &&
	   getbuf((long) pcbp->inp_next,
		  pcbp,
		  sizeof(struct inpcb),
		  "tcblist"));
  return NULL;
}



/*
** Return the user number for the connection owner
*/
int k_getuid(faddr, fport, laddr, lport, uid)
  struct in_addr *faddr;
  int fport;
  struct in_addr *laddr;
  int lport;
  int *uid;
{
  struct socket *sockp;
  struct file *fp;
  struct file file;
  int count = 0xffff;	/** Yep, it's hack 2 again **/
  
  /* -------------------- TCP PCB LIST -------------------- */
  if (!getbuf(nl[N_TCB].n_value, &tcb, sizeof(tcb), "tcb")) {
    return -1;
  }
  
  tcb.inp_prev = (struct inpcb *) nl[N_TCB].n_value;
  sockp = getlist(&tcb, faddr, fport, laddr, lport);
  
  if (!sockp)
    return -1;

  /* -------------------- OPEN FILE TABLE ----------------- */

  fp = (struct file *) nl[N_FILE].n_value;

  if (!getbuf(fp,&file,sizeof(struct file), "file"))
    return -1;

  do {
    struct vnode tvnode;
    struct cred creds;

    if (getbuf(file.f_vnode,&tvnode,sizeof(struct vnode),"vnode")){
      if ((void *) sockp == (void *) tvnode.v_data) {
	/* have a match!  return the user information! */
	if (getbuf(file.f_cred,&creds,sizeof(struct cred),"cred")) {
	  *uid = creds.cr_ruid;
	  return 0;
	}
	break;
      } 
    }
    /* if it's the end of the list _or_ we can't get the next
       entry, then get out of here...*/
    if ((!file.f_next) ||
	(!getbuf(file.f_next,&file,sizeof(struct file), "file")))
      break;
    
  } while (file.f_next != fp && (count--)); 
	/* heck, if we ever get here, something is really messed up.*/
  
  syslog(LOG_ERR,"ident: k_getuid: lookup failure.");
  return -1;
}
