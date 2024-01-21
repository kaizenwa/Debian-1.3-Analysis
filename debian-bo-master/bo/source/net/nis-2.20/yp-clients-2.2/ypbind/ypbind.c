/* 
 * $Log: ypbind.c,v $
 * Revision 1.1.1.1  1997/01/23 20:48:30  hjl
 * Import ypbind 3.0.
 *
 * Revision 1.5  1996/10/08 09:55:11  swen
 * Made ipc and mmap runtime options. Updated for release 3.0
 *
 * Revision 1.4  1995/11/15 10:06:25  swen
 * Added SYSV-IPC (from Michael Rausch <mrausch@ernie.mi.uni-koeln.de>)
 * Make mmap (not MAP_ANON) work again with latest kernels.
 *
 * Revision 1.3  1995/10/13 11:51:58  swen
 * Added -v flag for version info.
 *
 * Revision 1.2  1995/10/13 11:24:14  swen
 * Added code to restart slave in case of failure. This should make ypbind
 * more robust.
 *
 * Revision 1.1  1995/07/25  14:27:59  swen
 * ypbind version 2.0.
 *
 * Revision 2.9  1995/01/24  12:05:18  swen
 * Added RCS keywords.
 *
 */

static char rcsid[] = "$Id: ypbind.c,v 1.1.1.1 1997/01/23 20:48:30 hjl Exp $" ;

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <sys/uio.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h> /* getenv, exit */
#include <stdarg.h>
#include <string.h> /* strcmp */
#include <ctype.h>
#include <memory.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h> /* setsid */
#include <fcntl.h>
#include <syslog.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
#ifdef __sun__
#include <rpc/svc_soc.h>
#endif
#include <rpc/pmap_clnt.h> /* for pmap_unset */
#include "ourhdr.h"
#include "ypbind.h"
#include "patchlevel.h"

extern SVCXPRT * svcudp_create(int);
extern SVCXPRT * svctcp_create(int, u_int, u_int);
#if 0
extern svc_register(SVCXPRT *, u_long, u_long,
                    void (*)(struct svc_req *, SVCXPRT *), u_long);
#endif

char **Argv = NULL;
int Argc = 0;

extern int use_broadcast;
SVCXPRT *udptransp, *tcptransp;
domainname mydomain = NULL;
int ypsetmode = YPSET_NO;
pid_t childpid;
int broken_server, use_ipc, use_mmap;
volatile int debug;

void *
ypbindproc_null_2_svc(void *argp, struct svc_req *rqstp)
{
  static char * result;
  memset((char *)&result, 0, sizeof(result));
  return((void *) &result);
}

struct ypbind_resp *
ypbindproc_domain_2_svc(domainname *arg1, struct svc_req *rqstp)
{
  static struct ypbind_resp  result;
  struct binding *ypdb;
  
  memset((char *) &result, 0, sizeof(result));
  result.ypbind_status = YPBIND_FAIL_VAL;
  ypdb = find_entry(*arg1);
  
  read_lock_binding();

  if (NULL == ypdb || 0 == ypdb->is_alive)
    {
      result.ypbind_status = YPBIND_FAIL_VAL;
      result.ypbind_respbody.ypbind_error = YPBIND_ERR_NOSERV;
    }
  else
    {
      result.ypbind_status = YPBIND_SUCC_VAL;
      memcpy(&result.ypbind_respbody.ypbind_bindinfo.ypbind_binding_addr.s_addr,
             &ypdb->server[ypdb->active].server_addr,
             sizeof ypdb->server[ypdb->active].server_addr);
      result.ypbind_respbody.ypbind_bindinfo.ypbind_binding_port =
        ypdb->server[ypdb->active].server_port;
      if (debug)
        log_msg("YPBINDPROC_DOMAIN_2: server %s, port %d",
                inet_ntoa(ypdb->server[ypdb->active].server_addr),
                ntohs(ypdb->server[ypdb->active].server_port));
    }
  
  un_lock_read_binding();
  return &result;
}

void *
ypbindproc_setdom_2_svc(struct ypbind_setdom *arg1, struct svc_req *rqstp)
{
  static char *result;
  struct sockaddr_in *fromsin, bindsin;
  
#ifdef SOLARIS
  fromsin = (rqstp->rq_xprt)->xp_raddr;
#else
  fromsin = svc_getcaller(rqstp->rq_xprt);
#endif
  
  memset((char *)&result, 0, sizeof(result));
  switch (ypsetmode)
    {
    case YPSET_LOCAL:
      if (fromsin->sin_addr.s_addr != htonl(INADDR_LOOPBACK))
        return (void *) &result;
      break;
    case YPSET_ALL:
      break;
    case YPSET_NO:
    default:
      return (void *) &result;
    }
  
  if (ntohs(fromsin->sin_port) >= IPPORT_RESERVED)
    return (void *) &result;
  
  if (YPVERS != arg1->ypsetdom_vers)
    return (void *) &result;
  memset((char *) &bindsin, 0, sizeof bindsin);
  bindsin.sin_family = AF_INET;
  bindsin.sin_addr.s_addr = arg1->ypsetdom_addr.s_addr;
  bindsin.sin_port = arg1->ypsetdom_port;
  add_server(arg1->ypsetdom_domain, &bindsin, NULL, NULL, 1);
  return((void *) &result);
}

void *
_ypbindproc_null_2(void  *argp, struct svc_req *rqstp)
{
  return (ypbindproc_null_2_svc(argp, rqstp));
}

struct ypbind_resp *
_ypbindproc_domain_2(domainname *argp, struct svc_req *rqstp)
{
  return (ypbindproc_domain_2_svc(argp, rqstp));
}

void *
_ypbindproc_setdom_2(struct ypbind_setdom  *argp, struct svc_req *rqstp)
{
  return (ypbindproc_setdom_2_svc(argp, rqstp));
}

void
ypbindprog_2(struct svc_req *rqstp, register SVCXPRT *transp)
{
  union {
    char ypbindproc_domain_2_arg[YPMAXDOMAIN];
    struct ypbind_setdom ypbindproc_setdom_2_arg;
  } argument;
  
  struct authunix_parms *creds;
  char *result;
  xdrproc_t xdr_argument, xdr_result;
  char *(*local)(char *, struct svc_req *);
  
  switch (rqstp->rq_proc)
    {
    case YPBINDPROC_NULL:
      xdr_argument = (xdrproc_t) xdr_void;
      xdr_result = (xdrproc_t) xdr_void;
      local = (char *(*)(char *, struct svc_req *)) _ypbindproc_null_2;
      break;
      
    case YPBINDPROC_DOMAIN:
      xdr_argument = (xdrproc_t) xdr_domainname;
      xdr_result = (xdrproc_t) xdr_ypbind_resp;
      local = (char *(*)(char *, struct svc_req *)) _ypbindproc_domain_2;
      break;
      
    case YPBINDPROC_SETDOM:
      switch (rqstp->rq_cred.oa_flavor)
        {
        case AUTH_UNIX:
          creds = (struct authunix_parms *) rqstp->rq_clntcred;
          if (0 != creds->aup_uid)
            {
              svcerr_auth(transp, AUTH_BADCRED);
              return;
            }
          break;
        default:
          svcerr_weakauth(transp);
          return;
        }
      xdr_argument = (xdrproc_t) xdr_ypbind_setdom;
      xdr_result = (xdrproc_t) xdr_void;
      local = (char *(*)(char *, struct svc_req *)) _ypbindproc_setdom_2;
      break;
      
    default:
      svcerr_noproc(transp);
      return;
    }
  memset((char *)&argument, 0, sizeof (argument));
  if (!svc_getargs(transp, xdr_argument, (caddr_t) &argument))
    {
      svcerr_decode(transp);
      return;
    }
  result = (*local)((char *)&argument, rqstp);
  if (NULL != result && !svc_sendreply(transp, xdr_result, result))
    {
      svcerr_systemerr(transp);
    }
  return;
}

int
main(int argc, char *argv[])
{
  int i;
  struct sigaction sact;
#if USE_BINDINGDIR
  char path[MAXPATHLEN];
#endif

  Argv = argv;
  Argc = argc;

  debug = broken_server = use_ipc = use_mmap = 0;

  for (i = 1; i < argc; i++)
    {
      if (0 == strcmp("-v", argv[i]))
	{
          fprintf(stderr, "ypbind version %s\n", YPBIND_VERSION);
	  exit (1);
	}
      else if (0 == strcmp("-ypset", argv[i]))
        ypsetmode = YPSET_ALL;
      else if (0 == strcmp("-ypsetme", argv[i]))
        ypsetmode = YPSET_LOCAL;
      else if (0 == strcmp("-debug", argv[i]))
        debug = 1;
      else if (0 == strcmp("-ipc", argv[i]))
        use_ipc = 1;
      else if (0 == strcmp("-mmap", argv[i]))
        use_mmap = 1;
      else if (0 == strcmp("-broken_server", argv[i]))
        broken_server = 1;
    }

  log_open("ypbind", (LOG_PID | LOG_CONS), LOG_DAEMON);
  yp_get_default_domain(&mydomain);
  if (NULL == mydomain || '\0' == mydomain[0])
    log_quit("domainname not set. Aborting.\n");
  
  if (0 != getuid())
    log_quit("ypbind must be run as root");
  
  if (!debug)
    daemon_start();

  create_pidfile();
  
  if(0 != atexit(terminate))
    log_quit("Could not install exit handler");

  init_master_slave_communication();
  
      /* no check for return code here */
  pmap_unset(YPBINDPROG, YPBINDVERS);
  udptransp = svcudp_create(RPC_ANYSOCK);
  if (NULL == udptransp)
    log_sys("cannot create udp service.\n");
  if (!svc_register(udptransp, YPBINDPROG, YPBINDVERS, ypbindprog_2, IPPROTO_UDP))
    log_sys("unable to register (YPBINDPROG, YPBINDVERS, udp).\n");
  
  tcptransp = svctcp_create(RPC_ANYSOCK, 0, 0);
  if (NULL == tcptransp)
    log_sys("cannot create tcp service.\n");
  if (!svc_register(tcptransp, YPBINDPROG, YPBINDVERS, ypbindprog_2, IPPROTO_TCP))
    log_sys("unable to register (YPBINDPROG, YPBINDVERS, tcp).\n");
  
#if USE_BINDINGDIR
  sprintf(path, "%s/%s.%ld", BINDINGDIR, mydomain, YPVERS);
  unlink(path);
#endif

  childpid = start_slave();
  
  inststr(Argv, Argc, "ypbind (master)");

  sigemptyset(&sact.sa_mask);
  sigaddset(&sact.sa_mask, SIGCHLD);
  sact.sa_handler = sighandler;
  sact.sa_flags = 0;
  if ( 0 != sigaction(SIGTERM, &sact, NULL))
    log_ret("Could not install signal handler for SIGTERM.");
  if ( 0 != sigaction(SIGINT, &sact, NULL))
    log_ret("Could not install signal handler for SIGINT.");
  if ( 0 != sigaction(SIGQUIT, &sact, NULL))
    log_ret("Could not install signal handler for SIGQUIT.");
  if ( 0 != sigaction(SIGSEGV, &sact, NULL))
    log_ret("Could not install signal handler for SIGSEGV.");
  if ( 0 != sigaction(SIGCHLD, &sact, NULL))
    log_ret("Could not install signal handler for SIGCHLD.");
  sact.sa_flags |= SA_RESTART;
  if ( 0 != sigaction(SIGHUP, &sact, NULL))
    log_ret("Could not install signal handler for SIGHUP.");
  sact.sa_handler = toggle_debug;
  if (0 != sigaction(SIGUSR1, &sact, NULL))
    log_ret("Could not install signal handler for SIGUSR1");

  svc_run();
  log_quit("svc_run returned");
      /* NOTREACHED */
  return 1;
}


void
toggle_debug(int sig)
{
  debug = 1 - debug;
  return;
}

void
sighandler(int sig)
{
  sigset_t set;
  struct sigaction sact;

  if (debug)
    log_msg("received signal %d", sig);
  
  if (SIGHUP == sig)
    {
      kill(childpid, sig);
      return;
    }

  if (SIGCHLD == sig)
    {
      waitpid(childpid, NULL, WNOHANG);
      /* colas@sohia.inria.fr: re-spawn a child if died... */
      childpid = start_slave();
    }
  else
    {
      pmap_unset(YPBINDPROG, YPBINDVERS);
      /* reset signal handler, make sure, signals are not blocked for raise */
      sact.sa_handler = SIG_DFL;
      sigemptyset(&sact.sa_mask);
      sact.sa_flags = 0;
      sigemptyset(&set);
      sigaddset(&set, sig);
      sigprocmask(SIG_UNBLOCK, &set, NULL);
      if ( 0 != sigaction(sig, &sact, NULL))
        log_quit("Could not uninstall signal handler for signal %d.", sig);
      kill(childpid, sig);
      if (use_ipc)
        terminate_master_slave_communication();
      unlink(_PATH_YPPIDFILE);
      raise(sig);
      exit(0); /* just in case */
    }
}

void
terminate(void)
{
  unlink(_PATH_YPPIDFILE);
  if (0 != childpid)
    kill(childpid, SIGTERM);
  if (use_ipc)
    terminate_master_slave_communication();
  pmap_unset(YPBINDPROG, YPBINDVERS);
}


/* stolen from bdflush */
void
inststr(char *dst[], int argc, const char *src)
{
#ifdef __linux__
  if (strlen(src) <= strlen(dst[0]))
    {
      char *ptr;
      
      for (ptr = dst[0]; *ptr; *(ptr++) = '\0');
      
      strcpy(dst[0], src);
    }
  else
    {
          /* stolen from the source to perl 4.036 (assigning to $0) */
      char *ptr, *ptr2;
      int count;
      ptr = dst[0] + strlen(dst[0]);
      for (count = 1; count < argc; count++)
        {
          if (dst[count] == ptr + 1)
            ptr += strlen(++ptr);
        }
      if (environ[0] == ptr + 1)
        {
          for (count = 0; environ[count]; count++)
            if (environ[count] == ptr + 1)
              ptr += strlen(++ptr);
        }
      count = 0;
      for (ptr2 = dst[0]; ptr2 <= ptr; ptr2++)
        {
          *ptr2 = '\0';
          count++;
        }
      strncpy(dst[0], src, count);
    }
#endif /* __linux__ */
}

void
create_pidfile(void)
{
  int fd;
  pid_t pid;
  char pbuf[10];
  
  fd = open(_PATH_YPPIDFILE, O_CREAT | O_RDWR, FILE_MODE);
  if (fd < 0)
    log_sys("cannot create pidfile %s", _PATH_YPPIDFILE);
  pid = is_writelock(fd, 0, SEEK_SET, 0);
  if (0 != pid)
    log_quit("ypbind already running (pid %d) - exiting", pid);
  if (0 != write_lock(fd, 0, SEEK_SET, 0))
    log_sys("cannot lock pidfile");
  sprintf(pbuf, "%6d\n", getpid());
  if (writen(fd, pbuf, strlen(pbuf)) <= 0)
    log_sys("cannot write pidfile");
  return;
}
