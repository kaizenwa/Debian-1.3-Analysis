/*
** kernel/other.c	Low level kernel access functions for FreeBSD 2.x
**
** This program is in the public domain and may be used freely by anyone
** who wants to. 
**
** Last update: 11 April 1995 - torstenb@FreeBSD.ORG
*/

#include <stdio.h>
#include <nlist.h>
#include <syslog.h>

#include <kvm.h>

#include <sys/param.h>
#include <sys/socket.h>

#include <sys/socketvar.h>

#define KERNEL

#include <sys/file.h>

#include <fcntl.h>

#undef KERNEL

#include <sys/resource.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/wait.h>

#include <net/if.h>
#include <net/route.h>
#include <netinet/in.h>

#include <netinet/in_systm.h>
#include <netinet/ip.h>
 
#include <netinet/in_pcb.h>

#include <netinet/tcp.h>
#include <netinet/ip_var.h>
 
#include <netinet/tcpip.h>
 
#include <netinet/tcp_timer.h>
#include <netinet/tcp_var.h>

#include <arpa/inet.h>

#include "identd.h"
#include "error.h"

#ifdef INPLOOKUP_SETLOCAL
#define	_HAVE_OLD_INPCB
#endif

extern void *calloc();
extern void *malloc();


struct nlist nl[] =
{
#define N_FILE 0  
#define N_NFILE 1
#define N_TCB 2

  { "_filehead" },
  { "_nfiles" },
  { "_tcb" },
  { "" }
};

static kvm_t *kd;

static struct file *xfile;

static int nfile;

#ifdef _HAVE_OLD_INPCB
static struct inpcb tcb;
#else
static struct inpcbhead tcb;
#endif

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
      syslog(LOG_DEBUG,"getbuf: kvm_read(%08x, %d) - %s : %m",
	     addr, len, what);

    return 0;
  }
  
  return 1;
}



/*
** Traverse the inpcb list until a match is found.
** Returns NULL if no match.
*/
static struct socket *
#ifdef _HAVE_OLD_INPCB
    getlist(pcbp, faddr, fport, laddr, lport)
  struct inpcb *pcbp;
#else
    getlist(pcbhead, faddr, fport, laddr, lport)
  struct inpcbhead *pcbhead;
#endif
  struct in_addr *faddr;
  int fport;
  struct in_addr *laddr;
  int lport;
{
#ifdef _HAVE_OLD_INPCB
  struct inpcb *head;
#else
  struct inpcb *head, pcbp;
#endif

#ifdef _HAVE_OLD_INPCB
  if (!pcbp)
    return NULL;
#else
  head = pcbhead->lh_first;
  if (!head)
    return NULL;
#endif
 

#ifdef _HAVE_OLD_INPCB
  head = pcbp->inp_prev;
#endif
  do 
  {
#ifdef _HAVE_OLD_INPCB
    if ( pcbp->inp_faddr.s_addr == faddr->s_addr &&
	 pcbp->inp_laddr.s_addr == laddr->s_addr &&
	 pcbp->inp_fport        == fport &&
	 pcbp->inp_lport        == lport )
       return pcbp->inp_socket;
  } while (pcbp->inp_next != head &&
	   getbuf((long) pcbp->inp_next,
		  pcbp,
		  sizeof(struct inpcb),
		  "tcblist"));
#else
    if (!getbuf((long) head, &pcbp, sizeof(struct inpcb), "tcblist"))
       break;
    if (pcbp.inp_faddr.s_addr == faddr->s_addr &&
	pcbp.inp_fport        == fport &&
	pcbp.inp_lport        == lport )
	return(pcbp.inp_socket);
    head = pcbp.inp_list.le_next;
  } while (head != NULL);
#endif

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
  long addr;
  struct socket *sockp;
  int i;
  struct ucred ucb;
 
  /* -------------------- FILE DESCRIPTOR TABLE -------------------- */

  char	*filebuf;
  filebuf=kvm_getfiles(kd,KERN_FILE,0,&nfile);
  if(filebuf==0)
     ERROR("k_getuid: out of memory (file table)");
  xfile = (struct file *)(filebuf + sizeof (struct file *));

  /* -------------------- TCP PCB LIST -------------------- */
  if (!getbuf(nl[N_TCB].n_value, &tcb, sizeof(tcb), "tcb"))
    return -1;
  
#ifdef _HAVE_OLD_INPCB
  tcb.inp_prev = (struct inpcb *) nl[N_TCB].n_value;
#endif
  sockp = getlist(&tcb, faddr, fport, laddr, lport);
 
  if (!sockp)
    return -1;

  /*
  ** Locate the file descriptor that has the socket in question
  ** open so that we can get the 'ucred' information
  */
  for (i = 0; i < nfile; i++)
  {
    if (xfile[i].f_count == 0)
      continue;

    if (xfile[i].f_type == DTYPE_SOCKET &&
	(struct socket *) xfile[i].f_data == sockp)
    {
      if (!getbuf(xfile[i].f_cred, &ucb, sizeof(ucb), "ucb"))
	return -1;

      *uid = ucb.cr_uid;

      return 0;
    }
  }
  
  return -1;
}

