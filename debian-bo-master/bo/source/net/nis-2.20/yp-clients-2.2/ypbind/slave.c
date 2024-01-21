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
#include <rpc/pmap_clnt.h> /* for pmap_unset */
#include "ourhdr.h"
#include "ypbind.h"
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>

#if 0
extern int putenv(const char *);
#endif

extern SVCXPRT *udptransp;
extern domainname mydomain;
extern int broken_server;
extern int debug, use_ipc, use_mmap;
extern char **Argv;
extern int Argc;

extern CLIENT * clntudp_create(struct sockaddr_in *, u_long, u_long,
                               struct timeval, int *);

struct binding *ypbindlist;
int semid = -1;
int lockfd;
domainname askdomain;
volatile int hangup;

pid_t
start_slave(void)
{
  struct sigaction sact;
  pid_t pid;
  
  pid = fork();
  if (pid < 0)
    log_sys("fork failed");
  if (0 != pid) /* parent */
    return pid;
  
  inststr(Argv, Argc, "ypbind (slave)");

  sigemptyset(&sact.sa_mask);
  sact.sa_handler = toggle_debug;
  sact.sa_flags = SA_RESTART;
  if (0 != sigaction(SIGUSR1, &sact, NULL))
    log_ret("Could not install signal handler for SIGUSR1");
  sact.sa_handler = handle_hangup;
  if (0 != sigaction(SIGHUP, &sact, NULL))
    log_ret("Could not install signal handler for SIGHUP");
  
  for (;;)
    {
      hangup = 0;
      init_binding();
      
      while (!hangup)
        {
          check_binding();
          sleep(PING_INTERVAL);
        }
    }
}

void
init_binding(void)
{
  struct binding ypdb;

  write_lock_binding();
  memset(ypbindlist, 0, sizeof(struct binding) * _MAXDOMAIN);
  if (!use_mmap)
    {
      lseek(lockfd, 0, SEEK_SET);
      if (writen(lockfd, ypbindlist, sizeof(struct binding) * _MAXDOMAIN) <= 0)
        log_sys("cannot write ypbindlist");
    }
  un_lock_write_binding();

  memset((void *)&ypdb, 0, sizeof(struct binding));
  strncpy(ypdb.domain, mydomain, YPMAXDOMAIN);
  ypdb.is_bound = TRUE; /* always bind to our NIS domain */
  ypdb.active = 0;
  ypdb.lockfd = -1;
  ypdb.server[0].client_handle = NULL;
  ypdb.is_alive = FALSE;
  ypdb.server[0].use_broadcast = TRUE; /* default is using broadcast */
  update_entry(&ypdb);
  parse_config_file(_PATH_YPCONF);
  return;
}

void
check_binding(void)
{
  struct binding ypdb;
  time_t t;
  int i, j;
  
  time(&t);
  for (i = 0; i < _MAXDOMAIN; i++)
    {
      read_lock_binding();
      memcpy(&ypdb, &ypbindlist[i], sizeof ypdb);
      un_lock_read_binding();
      if (ypdb.is_bound)
        {
          if (ypdb.is_alive)
            {
              int active;
              for (j = 0; j < _MAXSERVER; j++)
                {
                  active = (ypdb.active + j) % _MAXSERVER;
                  if (!ypdb.server[active].filled) continue;
                  if (debug)
                    log_msg("pinging server %s, port %d",
                            inet_ntoa(ypdb.server[active].server_addr),
                            ntohs(ypdb.server[active].server_port));
                  ypdb.is_alive = FALSE;
                  ping_server(&ypdb, active);
                  if (ypdb.is_alive) break;
                }
              if (active != ypdb.active && ypdb.server[active].filled)
                {
                  ypdb.active=active;
                  update_entry(&ypdb); 
                }
            }
          if (!ypdb.is_alive)
            {
              if (ypdb.server[ypdb.active].use_broadcast)
                {
                  if (debug)
                    log_msg("broadcasting for domain %s", ypdb.domain);
                  broadcast(&ypdb);
                }
              else
                {
                  if (debug)
                    log_msg("binding to server %s", ypdb.server[ypdb.active].host);
                  bindto_server(ypdb.domain, ypdb.server[ypdb.active].host);
                }
              continue;
            }
        }
    }
}

void
add_server(char *dom, struct sockaddr_in *raddrp, CLIENT *clnt_handlep,
           const char *host, bool_t use_broadcast)
{
  struct binding entry;
#if USE_BINDINGDIR
  struct iovec iov[2];
  struct ypbind_resp ybr;
  pid_t lockpid;
  char path[MAXPATHLEN];
  int fd, len, status, active;
#endif
  
  if (NULL == dom || (0 != get_entry(dom, &entry)))
    return;

  if (!entry.is_bound)
    {
      entry.active = 0;
      entry.lockfd = -1;
      memset((void *)&entry.server, 0, sizeof(struct bound_server));
      entry.is_alive = FALSE;
    }

  /* find empty slot */
  for (active = 0; active < _MAXSERVER; active++)

    if (!entry.server[active].filled) break;
  
  active = active % _MAXSERVER;
  if (use_broadcast) active =  _MAXSERVER - 1;

  if (debug)
    log_msg("add_server() domain: %s, host: %s, %sbroadcast, slot: %d",
            dom, host ? host : "unknown", use_broadcast ? "" : "no",
            active);
  
  if (NULL != raddrp)
    {
      if (!broken_server &&
	  (ntohs(raddrp->sin_port) >= IPPORT_RESERVED ||
	   ntohs(raddrp->sin_port) <  IPPORT_RESERVED/2))
	{
	  log_msg("Answer from %s on illegal port", inet_ntoa(raddrp->sin_addr));
	  return;
	}
      memcpy(&entry.server[active].server_addr, &raddrp->sin_addr,
             sizeof entry.server[active].server_addr);
      entry.server[active].server_port = raddrp->sin_port;
      entry.is_alive = TRUE;
    }
  if (NULL != entry.server[active].client_handle)
    clnt_destroy(entry.server[active].client_handle);
  entry.server[active].client_handle = clnt_handlep;
  if (NULL != host)
    {
      if (entry.server[active].host) free(entry.server[active].host);
      entry.server[active].host = strdup(host);
    }
  entry.server[active].version = YPVERS;
  entry.server[active].use_broadcast = use_broadcast;
  entry.server[active].filled = TRUE;
  entry.active = active;
  
#if USE_BINDINGDIR
  if (NULL != raddrp)
    {
      if (-1 != entry.lockfd)
        close(entry.lockfd);
      sprintf(path, "%s/%s.%ld", BINDINGDIR,
              entry.domain, entry.server[active].version);
      if ((fd = open(path, O_CREAT | O_RDWR | O_TRUNC, FILE_MODE )) == -1)
        {
          if (-1 == mkdir(BINDINGDIR, DIR_MODE))
            log_ret("mkdir");
          if ((fd = open(path, O_CREAT | O_RDWR | O_TRUNC, FILE_MODE)) == -1)
            return;
        }
      
      lockpid = lock_test(fd, F_RDLCK, 0, SEEK_SET, 0);
      if (0 != lockpid) 
        log_quit("%s already locked by pid %d", path, lockpid);
      
      status = read_lock(fd, 0, SEEK_SET, 0);
      if (0 != status) 
        log_sys("set lock");
      
      
      /*
       * ok, if BINDINGDIR exists, and we can create the binding file, then
       * write to it..
       */
      entry.lockfd = fd;
      
      iov[0].iov_base = (caddr_t) &(udptransp->xp_port);
      iov[0].iov_len = sizeof udptransp->xp_port;
      iov[1].iov_base = (caddr_t) &ybr;
      iov[1].iov_len = sizeof ybr;
      
      memset(&ybr, 0, sizeof ybr);
      ybr.ypbind_status = YPBIND_SUCC_VAL;
      ybr.ypbind_respbody.ypbind_bindinfo.ypbind_binding_addr = raddrp->sin_addr;
      ybr.ypbind_respbody.ypbind_bindinfo.ypbind_binding_port = raddrp->sin_port;
      
      len = iov[0].iov_len + iov[1].iov_len ;
      if (writev(entry.lockfd, iov, 2) != len )
        {
          log_ret("writev");
          close(entry.lockfd);
          (void) unlink(path);
          entry.lockfd = -1;
        }
    }
#endif
  update_entry(&entry);
  return;
}

void
bindto_server(char *ypdomain, char *server)
{
  struct sockaddr_in server_addr;
  int sock;
  bool_t out;
  enum clnt_stat status;
  struct timeval timeout;
  CLIENT *clnt_handlep = NULL;
  int i = 0;
  
  struct hostent *host;
  static char *order = NULL;
  int result;

  if (debug)
    log_msg("bindto_server: domain %s, host %s", ypdomain, server);
  
  /*
   * FIXME: gethostbyname may in turn ask ypbind (entry in 
   * /etc/host.conf)!!! 
   * so much for shooting yourself in the foot.
   * Using RESOLV_SERV_ORDER is a kludge, should use gethostent()
   */
  
  if (!order)
    order = strdup("RESOLV_SERV_ORDER=hosts");
  result = putenv(order);
  if (0 != result)
    log_ret("putenv failed");
  host = gethostbyname(server);
  if (NULL == host)
    {
      switch (h_errno)
        {
        case HOST_NOT_FOUND:
          log_msg("Unknown host: %s", server);
          break;
        case TRY_AGAIN:
          log_msg("Host name lookup failure");
          break;
        case NO_DATA:
          log_msg("No address associated with name: %s", server);
          break;
        case NO_RECOVERY:
          log_msg("Unknown server error");
          break;
        default:
          log_ret("gethostbyname: Unknown error");
          break;
        }
      return;
    }
  
  memset((char *)&server_addr, 0, sizeof server_addr);
  server_addr.sin_family = host->h_addrtype;
  server_addr.sin_port = htons(0);
  sock = RPC_ANYSOCK;
  while(NULL != host->h_addr_list[i])
    {
      memcpy(&server_addr.sin_addr, host->h_addr_list[i],
             host->h_length);
      
      timeout.tv_sec = 1;
      timeout.tv_usec = 0;
      clnt_handlep = clntudp_create(&server_addr, YPPROG, YPVERS,
                                    timeout, &sock);
      if (NULL != clnt_handlep)
        break;
      i++;
    }
  
  if (NULL == clnt_handlep)
    {
      log_msg("clnt_create for server %s failed", host->h_name);
      return; 
    }
  
  timeout.tv_sec = 5;
  timeout.tv_usec = 0;
  status = clnt_call(clnt_handlep, YPPROC_DOMAIN,
                     (xdrproc_t) xdr_domainname, &ypdomain,
                     (xdrproc_t) xdr_bool, &out,
                     timeout);
  if (RPC_SUCCESS != status)
    {
      log_msg(clnt_sperror(clnt_handlep, host->h_name));
      clnt_destroy(clnt_handlep);
    }
  else if (TRUE != out)
    {
      log_msg("domain %s not served by %s", ypdomain, host->h_name);
      clnt_destroy(clnt_handlep);
    }
  else
    add_server(ypdomain, &server_addr, clnt_handlep, host->h_name, FALSE);
  
  return;
}

void
ping_server(struct binding *ypdb, int active)
{
  int status;
  bool_t out;
  struct timeval timeout;
  
  if (NULL != ypdb->server[active].client_handle)
    {
      char *domainname = ypdb->domain;
      timeout.tv_sec = 2;
      timeout.tv_usec = 0;
      status = clnt_call(ypdb->server[active].client_handle, YPPROC_DOMAIN,
                         (xdrproc_t) xdr_domainname, &domainname,
                         (xdrproc_t) xdr_bool, &out,
                         timeout);
      
      if ((RPC_SUCCESS != status) || (TRUE != out))
        {
          RPC_SUCCESS != status
	    ? log_msg(clnt_sperror(ypdb->server[active].client_handle,
	                           ypdb->server[active].host ? ypdb->server[active].host : "unknown"))
            : log_msg("domain %s not served by %s",
                      ypdb->domain, ypdb->server[active].host ? ypdb->server[active].host : "unknown") ;
          clnt_destroy(ypdb->server[active].client_handle);
          ypdb->server[active].client_handle = NULL;
          ypdb->is_alive = FALSE;
        }
      else
        ypdb->is_alive=TRUE;
    }
  return;
}

bool_t
eachresult(bool_t *out, struct sockaddr_in *addr)
{
  CLIENT *clnt_handlep;
  struct timeval timeout;
  int sock;
  
  if (*out)
    {
      if(debug)
        {
          struct hostent *hostentp;
          hostentp = gethostbyaddr((char *) &addr->sin_addr.s_addr,
                                   sizeof(addr->sin_addr.s_addr), AF_INET);
          log_msg("Answer from server %s .", hostentp->h_name);
        }
      
      sock = RPC_ANYSOCK;
      timeout.tv_sec = 1;
      timeout.tv_usec = 0;
      clnt_handlep = clntudp_create(addr, YPPROG, YPVERS, timeout, &sock);
      add_server(askdomain, addr, clnt_handlep, NULL, TRUE);
      return 1;
    }
  else
    {
      return 0;
    }
}

void
broadcast(struct binding *ypdb)
{
  bool_t out;
  enum clnt_stat  status;
#if USE_BINDINGDIR
  char path[MAXPATHLEN];
#endif
  
      /* update global variable for eachresult */
  askdomain = ypdb->domain;
  status = clnt_broadcast(YPPROG, YPVERS, YPPROC_DOMAIN_NONACK,
                          (xdrproc_t) xdr_domainname, (void *) &askdomain,
                          (xdrproc_t) xdr_bool, (void *)&out,
                          (resultproc_t) eachresult);
  if (RPC_SUCCESS != status)
    {
#if USE_BINDINGDIR
      if ( -1 != ypdb->lockfd)
        {
          close(ypdb->lockfd);
          ypdb->lockfd = -1;
          sprintf(path, "%s/%s.%ld", BINDINGDIR,
                  ypdb->domain, ypdb->server[ypdb->active].version);
          unlink(path);
        }
#endif
      log_msg("broadcast: %s.", clnt_sperrno(status));
    }
}


/*
 * Routines for parsing the config file ( /etc/yp.conf )
 *
 */

void
parse_config_file(const char *path)
      /* parse the config file, check bindings */
{
  FILE *fp;
  char buf[1024];
  char *cp, *tmp;
  char tmpserver[81], tmpdomain[YPMAXDOMAIN + 1];
  int count;
  
  fp = fopen(path, "r");
  if (NULL == fp)
    return;

  if (debug)
    log_msg("parsing config file");

  while ((cp = fgets(buf, sizeof(buf), fp)) != NULL)
    {
      tmp = strchr(cp, '#');  /* Kommentare ausblenden */
      if (tmp)
        *tmp = '\0';
      while (isspace(*cp))    /* Leerraum ueberlesen */
        cp++;
      if (*cp == '\0')        /* Leerzeile ignorieren */
        continue;
      
      if (debug)
        log_msg("Trying entry: %s", cp);
      count = sscanf(cp, "domain %64s server %80s", tmpdomain, tmpserver);
      if (2 == count)
        {
          if (debug)
            log_msg("parsed domain %s server %s", tmpdomain, tmpserver);
          bindto_server(tmpdomain, tmpserver);
          continue;
        }
      count = sscanf(cp, "domain %s broadcast", tmpdomain);
      if (1 == count)
        {
          if (debug)
            log_msg("parsed domain %s broadcast", tmpdomain);
          add_server(mydomain, NULL, NULL, NULL, TRUE);
          continue;
        }
      count = sscanf(cp, "ypserver %80s", tmpserver);
      if (1 == count)
        {
          if (debug)
            log_msg("parsed ypserver %s", tmpserver);
          bindto_server(mydomain, tmpserver);
          continue;
        }
    }
  fclose(fp);
  return;
}

void
init_master_slave_communication(void)
{
  int shmid;
  struct shmid_ds dummy;
  ushort empty [2] = {0,0};
  union semun semarg;

  if (use_ipc)
    {
      if ((semid = semget(IPC_PRIVATE, 2, IPC_CREAT|SHM_R|SHM_W)) < 0)
        log_sys("cannot create semaphore");
      
      
      if ((shmid = shmget(IPC_PRIVATE, 
                          sizeof(struct binding) * _MAXDOMAIN,
                          IPC_CREAT|SHM_R|SHM_W)) < 0)
        log_sys("cannot create shared memory segment");
      
      ypbindlist = (struct binding *)shmat(shmid, NULL, 0);
      
      /* mark it for deletion just in case we are not able to
         handle it on our own. handles case of failed attachment
         as well */
      (void)shmctl(shmid, IPC_RMID, &dummy);  
      
      if (!ypbindlist)
        log_sys("cannot attach to shared memory segment");
      
      memset(ypbindlist, 0, sizeof(struct binding) * _MAXDOMAIN);
      
      semarg.array = empty;
      semctl(semid, 0, SETALL, semarg);
    }
  else
    {
      lockfd = open_lockfile();
      if (use_mmap)
        {
          ypbindlist = (struct binding *) mmap(0,
                                               sizeof(struct binding) * _MAXDOMAIN,
                                               (PROT_READ | PROT_WRITE), 
#if MMAP_SHARED_ANON_OK
                                               (MAP_ANON | MAP_SHARED), -1,
#else  
                                               MAP_SHARED, lockfd,
#endif /* MMAP_SHARED_ANON_OK */
                                               0);
          if ((caddr_t) -1 == (caddr_t) ypbindlist)
            log_sys("cannot create shared region");
        }
      else
        {
          ypbindlist = (struct binding *) calloc(_MAXDOMAIN, sizeof(struct binding));
          if (NULL == ypbindlist)
            log_sys("cannot create shared region");
          if (writen(lockfd, ypbindlist, sizeof(struct binding) * _MAXDOMAIN) <= 0)
            log_sys("cannot write shared region");
        }
    }
}

void
terminate_master_slave_communication(void)
{
  if(semid != -1)
  {
    union semun dummy;

    (void)semctl(semid, 0, IPC_RMID, dummy);
    semid = -1;
  }
}

#define IPC_LOCK_RETRY 5

void 
read_lock_binding(void)
{
  struct sembuf sops[2];
  int i, status;
  
  if (use_ipc)
    {
      sops[0].sem_num = 1; /* check if write semaphore is clear */
      sops[0].sem_op = 0;
      sops[0].sem_flg = IPC_NOWAIT;
      sops[1].sem_num = 0; /* then signal reading */
      sops[1].sem_op = 1;
      sops[1].sem_flg = SEM_UNDO|IPC_NOWAIT;
      
      for(i=0; i<IPC_LOCK_RETRY; i++)
        {
          if (!semop(semid, sops, sizeof(sops)/sizeof(struct sembuf)))
            break;
          sleep(1);
        }
      if(i==IPC_LOCK_RETRY)
        log_sys("cannot create read lock");
    }
  else
    {
      status = lock_reg(lockfd, F_SETLKW, F_RDLCK, 0, SEEK_SET, 0);
      if (0 != status) 
        log_sys("set read lock");
      if (!use_mmap)
        {
          lseek(lockfd, 0, SEEK_SET);
          if (readn(lockfd, ypbindlist, sizeof(struct binding) * _MAXDOMAIN) < 0)
            log_sys("cannot read ypbindlist");
        }
    }
  return;
}

void 
write_lock_binding(void)
{
  struct sembuf sops[3];
  int i, status;
      
  if (use_ipc)
    {
      sops[0].sem_num = 0; /* check if read semaphore is clear */
      sops[0].sem_op = 0;
      sops[0].sem_flg = IPC_NOWAIT;
      sops[1].sem_num = 1; /* check if write semaphore is clear */
      sops[1].sem_op = 0;
      sops[1].sem_flg = IPC_NOWAIT;
      sops[2].sem_num = 1; /* then signal writing */
      sops[2].sem_op = 1;
      sops[2].sem_flg = SEM_UNDO|IPC_NOWAIT;
      
      for(i=0; i<IPC_LOCK_RETRY; i++)
        {
          if (!semop(semid, sops, sizeof(sops)/sizeof(struct sembuf)))
            break;
          sleep(1);
        }
      if(i==IPC_LOCK_RETRY)
        log_sys("cannot create write lock");
    }
  else
    {
      status = lock_reg(lockfd, F_SETLKW, F_WRLCK, 0, SEEK_SET, 0); 
      if (0 != status) 
        log_sys("set write lock");
    }
  return;
}


void
un_lock_read_binding(void)
{
  struct sembuf sops[1];
  int i;
  
  if (use_ipc)
    {
      sops[0].sem_num = 0; /* clear read semaphore */
      sops[0].sem_op = -1;
      sops[0].sem_flg = SEM_UNDO|IPC_NOWAIT;
      
      for(i=0; i<IPC_LOCK_RETRY; i++)
        {
          if (!semop(semid, sops, sizeof(sops)/sizeof(struct sembuf)))
            break;
          sleep(1);
        }
      if(i==IPC_LOCK_RETRY)
        log_sys("error unlocking");
    }
  else
    un_lock_write_binding();
  return;
}

void 
un_lock_write_binding(void)
{
  struct sembuf sops[1];
  int i, status;
  
  if (use_ipc)
    {
      sops[0].sem_num = 1; /* clear write semaphore */
      sops[0].sem_op = -1;
      sops[0].sem_flg = SEM_UNDO|IPC_NOWAIT;
      
      for(i=0; i<IPC_LOCK_RETRY; i++)
        {
          if (!semop(semid, sops, sizeof(sops)/sizeof(struct sembuf)))
            break;
          sleep(1);
        }
      if(i==IPC_LOCK_RETRY)
        log_sys("error unlocking");
    }
  else
    {
      status = lock_reg(lockfd, F_SETLKW, F_UNLCK, 0, SEEK_SET, 0);
      if (0 != status)
        log_sys("unlock");
    }
  return;
}


int
open_lockfile(void)
{
  char name[L_tmpnam];
  int fd;
  
  if (NULL == tmpnam(name))
    log_sys("tmpnam failed");
  
  fd = open(name, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  if (fd < 0)
    log_sys("cannot open lockfile");
  /* Need to extend the file for mmap + fcntl */
  if (0 != ftruncate(fd, sizeof(struct binding) * _MAXDOMAIN))
    log_sys("ftruncate");
  /*
   * remove the file, but keep it open, so we have an invisible
   * lockfile. Note: This does not work with NFS-mounted directories
   */
  unlink(name);
  return fd;
}

int
get_entry(char *dom, struct binding *entry)
{
  struct binding *ypdb = NULL;
  
  ypdb = find_entry(dom);
  if (NULL == ypdb)
    return -1;
  read_lock_binding();
  memcpy(entry, ypdb, sizeof (struct binding));
  un_lock_read_binding();
  return 0;
}

void
update_entry(struct binding *entry)
{
  /* update entry in ypbindlist. Lock while updating */
  struct binding *ypdb = NULL;

  ypdb = find_entry(entry->domain);
  if (NULL == ypdb)
    return;
  write_lock_binding();
  memcpy(ypdb, entry, sizeof (struct binding));
  if (!use_mmap)
    {
      lseek(lockfd, 0, SEEK_SET);
      if (writen(lockfd, ypbindlist, sizeof(struct binding) * _MAXDOMAIN) <= 0)
        log_sys("cannot write ypbindlist");
    }
  un_lock_write_binding();
  if (debug)
    log_msg("%s entry for domain %s: server %s, port %d",
            entry->is_alive ? "updated" : "cleared", entry->domain,
            inet_ntoa(entry->server[entry->active].server_addr),
            ntohs(entry->server[entry->active].server_port));
  return;
}

struct binding *
find_entry(char *dom)
{
  int i;
  struct binding *ypdb = NULL;
  
  read_lock_binding();
      /* Find entry for domain dom */
  for (i = 0; i < _MAXDOMAIN; i++)
    {
      if (ypbindlist[i].is_bound && 0 == strcmp(ypbindlist[i].domain, dom))
        {
          ypdb = &ypbindlist[i];
          break;
        }
    }
  
  if (NULL == ypdb) /* no entry for domain dom */
    {
          /* find empty slot */
      for (i = 0; i < _MAXDOMAIN; i++)
        {
          if (FALSE == ypbindlist[i].is_bound)
            {
              ypdb = &ypbindlist[i];
              break;
            }
        }
    }
  un_lock_read_binding();
  return ypdb;
}

void
handle_hangup(int sig)
{
  log_msg("rereading config file");

  hangup = 1;

  return;
}
