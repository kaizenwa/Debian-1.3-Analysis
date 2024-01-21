/* 
 * Network accounting
 * process.c - process packets
 * (C) 1994, 1995 Ulrich Callmeier
 */

#include <sys/time.h>
#include <sys/wait.h>
#include "netacct.h"

#include <stdlib.h>
#include <unistd.h>
#include <malloc.h>
#include <strings.h>
#include <signal.h>
#include <fcntl.h>
#include <utmp.h>
#include <sys/ioctl.h>
#include <sys/stat.h>

char *rcs_revision_process_c = "$Revision: 1.2be $";

volatile int running;

struct statistics *packets;

static struct ipdata *plist; /* data being collected */
static struct ipdata *olist; /* data being written */
unsigned long int plistsize;
unsigned long int olistsize;
static volatile sig_atomic_t lck;
static volatile sig_atomic_t writing;
static volatile sig_atomic_t dumping;
volatile pid_t writepid;
volatile pid_t dumppid;

volatile int may_write;

int err_delay, max_err_delay;

volatile time_t now;

/* statistics */
unsigned int list_compares, list_lookups;

void reopen_socket(void)
{
    int save1, save2;
    
    /* critical section */
    save1 = may_write;
    may_write = 0;
    save2 = lck;
    lck = 1;
    /* end */
    
    exit_capture();
    init_capture();

    lck = save2;
    may_write = save1;
}


void do_acct()
{
  packets = malloc(sizeof(struct statistics));

  if(!packets)
    {
      syslog(LOG_ERR,"out of memory");
      daemon_stop(0);
    }
  
  packets->ignored = packets->netignored = packets->ip = packets->local = 0;
  packets->ip_icmp = packets->ip_tcp = packets->ip_udp = packets->ip_other = 0;
  packets->notdev = packets->unenc = 0;

  olist = plist = NULL;
  olistsize = plistsize = 0;
  lck = writing = dumping = 0;

  max_err_delay = cfg -> err_delay;
  err_delay = 0;

  list_lookups = list_compares = 0;

  may_write = 1;

  now = time(NULL);

  alarm(1);
  running=1;

  packet_loop();

}

#define DISABLED(n) (cfg->disabled & BITMASK(n))

/* simplified version, performance ??? */
void register_packet(unsigned long int src,unsigned long int dst, unsigned char proto, unsigned short srcport, unsigned short dstport, int size, char *devname, char *user)
{
    if(lck==0)
	{
	    struct ipdata *p;
	    lck = 1;
	    p = plist;
	    list_lookups++;
	    while(p)
		{
		    list_compares++;
		    if( (DISABLED(DIS_PROTO) || (p->proto == proto)) && ( DISABLED(DIS_SRC) || (p->src == src)) && (DISABLED(DIS_DST) || (p->dst == dst)) &&  (DISABLED(DIS_SRCPORT) || (p->srcport == srcport)) && (DISABLED(DIS_DSTPORT) || (p->dstport == dstport)) && (strcmp(p->devname, devname)==0))
			{
			    p->bytes +=size;
			    p->when = now;
                            p->count++;
			    lck = 0;
			    return;
			}
		    p = p->next;
		}
	    p = malloc(sizeof(struct ipdata));
	    if(p == NULL)
		{
		    packets -> dropped++;
		    return;
		}
	    plistsize++;
	    p -> src = src;
	    p -> dst = dst;
	    p -> proto = proto;
	    p -> srcport = srcport;
	    p -> dstport = dstport;
	    p -> bytes = size;
	    p -> count = 1;
	    p -> devname = strdup(devname);
            if (user)
	        p -> user = strdup(user);
	    else
	        p -> user = NULL;
	    p -> next = plist;
	    p -> when = now;
	    
	    plist = p;
	    lck = 0;
	}
    else
	{
	    packets->dropped++;
	}
}

int do_write_list(FILE *f, struct ipdata *list)
{
    struct ipdata *p;

    p = list;

    while(p)
	{
	    if(fprintf(f, "%lu",time(0))<0) return 1;
	    
	    if(!DISABLED(DIS_PROTO)) if(fprintf(f, "\t%d",p->proto)<0) return 1;

	    if(!DISABLED(DIS_SRC)) if(fprintf(f, "\t%s",intoa(p->src))<0) return 1;

	    if(!DISABLED(DIS_SRCPORT)) if(fprintf(f, "\t%d",p->srcport)<0) return 1;

	    if(!DISABLED(DIS_DST)) if(fprintf(f, "\t%s",intoa(p->dst))<0) return 1;

	    if(!DISABLED(DIS_DSTPORT)) if(fprintf(f, "\t%d",p->dstport)<0) return 1;

	    if(!DISABLED(DIS_COUNT)) if(fprintf(f, "\t%u",p->count)<0) return 1;

	    if(!DISABLED(DIS_BYTES)) if(fprintf(f, "\t%lu",p->bytes)<0) return 1;

	    if(!DISABLED(DIS_DEV)) if(fprintf(f, "\t%s",p->devname)<0) return 1;

	    if(!DISABLED(DIS_USER)) if(fprintf(f, "\t%s\n",p->user?p->user:"unknown")<0) return 1;

	    if(fprintf(f, "\n")<0) return 1;
   
	    p = p->next;
	}

    if(fclose(f)<0)
	{
	    return 1;
	}

    return 0;
}

static int pfd1[2], pfd2[2];

void TELL_WAIT_INIT(void)
{
  if(pipe(pfd1) < 0 || pipe(pfd2) < 0)
    {
      syslog(LOG_ERR, "pipe error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"pipe error: %s\n", strerror(errno)));
    }
}

void TELL_WAIT_EXIT(void)
{
  if(close(pfd1[0])!=0)
    {
      syslog(LOG_ERR, "pipe close error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"pipe close error: %s\n", strerror(errno)));
    }

  if(close(pfd1[1])!=0)
    {
      syslog(LOG_ERR, "pipe close error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"pipe close error: %s\n", strerror(errno)));
    }

  if(close(pfd2[0])!=0)
    {
      syslog(LOG_ERR, "pipe close error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"pipe close error: %s\n", strerror(errno)));
    }

  if(close(pfd2[1])!=0)
    {
      syslog(LOG_ERR, "pipe close error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"pipe close error: %s\n", strerror(errno)));
    }
}

void TELL_PARENT(void)
{
  if(write(pfd2[1], "c", 1) != 1)
    {
      syslog(LOG_ERR, "write error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"write error: %s\n", strerror(errno)));
    }
}

void WAIT_PARENT(void)
{
  char c; int n;

 again:

  if((n = read(pfd1[0], &c, 1)) != 1)
    {
      if((n == -1) && (errno == EINTR)) goto again;
      syslog(LOG_ERR, "read error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"read error: %s\n", strerror(errno)));
    }
  if(c!='p')
    {
      syslog(LOG_ERR, "WAIT_PARENT: incorrect data");
      DEBUG(DBG_ERR, sprintf(dbg,"WAIT_PARENT: incorrect data\n"));
    }
}  

void TELL_CHILD(void)
{
  if(write(pfd1[1], "p", 1) != 1)
    {
      syslog(LOG_ERR, "write error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"write error: %s\n", strerror(errno)));
    }
}

void WAIT_CHILD(void)
{
  char c; int n;

 again:

  if((n = read(pfd2[0], &c, 1)) != 1)
    {
      if((n == -1) && (errno == EINTR)) goto again;
      syslog(LOG_ERR, "read error: %s", strerror(errno));
      DEBUG(DBG_ERR, sprintf(dbg,"read error: %s\n", strerror(errno)));
    }
  if(c!='c')
    {
      syslog(LOG_ERR, "WAIT_CHILD: incorrect data");
      DEBUG(DBG_ERR, sprintf(dbg,"WAIT_CHILD: incorrect data\n"));
    }
}  

/* write and clear olist */
void write_list(void)
{
    FILE *f;
    char tmpn[255];

    while( (writepid = fork()) < 0) sleep(1);
    if (writepid!=0) return;

    /* Here goes the child */

    TELL_PARENT();
    WAIT_PARENT();

    DEBUG(DBG_STATE, sprintf(dbg, "write child: synchronized with parent\n"));

    sprintf(tmpn, "/tmp/nacctd.write.%d", (int) getpid());
    creat(tmpn, S_IRUSR);

    openlog("nacctd (write)", 0, LOG_DAEMON);

    DEBUG(DBG_STATE, sprintf(dbg, "* write process %d forked\n", (int) getpid()));

    f = fopen(cfg->filename, "a");
    if(f==NULL)
	{
	    unlink(tmpn);
	    syslog(LOG_ERR, "error opening file %s: %m\n",cfg->filename);
	    exit(1);
	}

    if(do_write_list(f, olist) != 0)
	{
	    unlink(tmpn);
	    syslog(LOG_ERR, "error writing to file %s: %m\n", cfg->filename);
	    exit(1);
	}

    olist = NULL;

    unlink(tmpn);

    DEBUG(DBG_STATE, sprintf(dbg, "* write finished, count = %ld\n", olistsize));

    exit(0);
}

void dump_curr_list(void)
{
    FILE *f;
    char tmpn[255];

    while( (dumppid = fork()) < 0) sleep(1);
    if (dumppid!=0) return;

    TELL_PARENT();
    WAIT_PARENT();

    DEBUG(DBG_STATE, sprintf(dbg, "dump child: synchronized with parent\n"));

    /* Here goes the child */

    sprintf(tmpn, "/tmp/nacctd.dump.%d", (int) getpid());
    creat(tmpn, S_IRUSR);

    openlog("nacctd (dump)", 0, LOG_DAEMON);

    DEBUG(DBG_STATE, sprintf(dbg, "* dump process %d forked\n", (int) getpid()));

    if(plistsize == 0)
	{
	    unlink(tmpn);
	    unlink(cfg->dumpname);
	    DEBUG(DBG_STATE, sprintf(dbg, "* dump finished, dump empty\n"));
	    exit(0);
	}

    f = fopen(cfg->dumpname, "w");
    if(f==NULL)
	{
	    unlink(tmpn);
	    syslog(LOG_ERR, "error opening file %s: %m\n",cfg->dumpname);
	    exit(1);
	}

    if(do_write_list(f, plist) != 0)
	{
	    unlink(tmpn);
	    syslog(LOG_ERR, "error writing to file %s: %m\n", cfg->dumpname);
	    exit(1);
	}

    plist = NULL;

    unlink(tmpn);

    DEBUG(DBG_STATE, sprintf(dbg, "* dump finished, count = %ld\n", plistsize));

    exit(0);
}


void child_finished(int sig)
{
    int status;
    pid_t pid;

    DEBUG((DBG_SIGNAL | DBG_STATE), sprintf(dbg, "-> got signal %d, handling\n", sig));

    while((pid = waitpid((pid_t) -1, &status, WNOHANG)) != 0)
	{
	  DEBUG(DBG_SIGNAL, sprintf(dbg, "  waitpid returned %d, status = %d, errno = %d\n", pid, status, errno));
	     
	  if(pid == -1)
	    {
	      if(errno == ECHILD)
		break; /* no child processes */
	      DEBUG(DBG_SIGNAL, sprintf(dbg, "waitpid: signaled error: %s\n", strerror(errno)));
	    }
	      
	  if((pid == writepid) || (pid == dumppid))
	    {
	      if(WIFEXITED(status))
		{
		  if(WEXITSTATUS(status)==0)
		    {
		      if(pid == writepid)
			{
			  writing = 0;
			  DEBUG((DBG_SIGNAL | DBG_STATE), sprintf(dbg, "  set writing to 0\n"));
			}
		      else
			{
			  dumping = 0;
			  DEBUG((DBG_SIGNAL | DBG_STATE), sprintf(dbg, "  set dumping to 0\n"));
			}
		    }
		  else
		    {
		      syslog(LOG_ERR, 
			     "child %d exited with error status %d.\n",
			     pid, WEXITSTATUS(status));
		      if(pid == writepid)
			{
			  err_delay = max_err_delay;
			  writing = 0;
			  DEBUG((DBG_SIGNAL | DBG_STATE), sprintf(dbg, "  set writing to 0, setting err_delay\n"));
			}
		      else
			{
			  dumping = 0;
			  DEBUG((DBG_SIGNAL | DBG_STATE), sprintf(dbg, "  set dumping to 0, ignored error condition\n"));
			}
		      
		    }
		}
	      else
		{
		  syslog(LOG_ERR,
			 "Huh? Child %d terminated or stopped by signal (%m)\n",
			 pid);
		  if(pid == writepid)
		    {
		      writing = 0;
		      DEBUG((DBG_SIGNAL | DBG_STATE), sprintf(dbg, "  set writing to 0, ignored return code\n"));
		    }
		  else
		    {
		      dumping = 0;
		      DEBUG((DBG_SIGNAL | DBG_STATE), sprintf(dbg, "  set dumping to 0, ignored return code\n"));
		    }
		}
	    }
	  else
	    {
	      syslog(LOG_ERR, "Huh? Child (%d) returned, but not the one we expected (%d, %d)!\n", (int) pid, writepid, dumppid);
	      DEBUG(DBG_STATE, sprintf(dbg, "  unexpected child %d signaled return (writepid = %d, dumppid = %d\n",(int) pid, writepid, dumppid));

	    }
	  DEBUG(DBG_STATE, sprintf(dbg, "  child %d signaled return\n",(int) pid));
	}
    DEBUG((DBG_SIGNAL | DBG_STATE), sprintf(dbg, "<- got signal %d, done handling\n", sig));
}

void alarm_handler(int sig)
{
    static time_t last_check = 0;
    static time_t next_write_log = 0;

    DEBUG( ((sig == SIGALRM) ? DBG_ANNOYING : (DBG_SIGNAL | DBG_STATE)), sprintf(dbg, "got signal %d, handling\n", sig));

    now++;

    if((now - last_check) > 60)
	{
	    time_t nnow;
	    
	    nnow = time(NULL);
	    if(nnow!=now)
		{
		    if((abs(nnow - now) > 2))
			{
			    DEBUG(DBG_MISC, sprintf(dbg, "internal clock corrected (off by %d seconds)\n",(int) (nnow-now)));
			}
		    now = nnow;
		}
	    last_check = now;
	}

    if(now >= next_write_log)
	{
	    write_log(0);
	    next_write_log = now + cfg -> flush;
	}

    alarm(1);
}

void write_log(int force)
{
  struct ipdata *p, *q;
  static struct ipdata *tlist; /* temp */

  DEBUG(DBG_STATE, sprintf(dbg, "write_log called\n"));

  if(err_delay!=0)
    {
      err_delay--;
      syslog(LOG_INFO,"flushing delayed due to error\n");
      DEBUG(DBG_STATE, sprintf(dbg, "flushing delayed due to error\n"));
    }
  else if((writing == 0) && (lck == 0) && (may_write == 1)) /* delay if another write cycle is still in progress */
    {
      DEBUG(DBG_STATISTICS, sprintf(dbg, "ignored: %ld netignored: %ld local:%ld ip:%ld unenc:%ld notdev:%ld dropped:%ld\n", 
				    packets->ignored, packets->netignored, packets->local, packets->ip, 
				    packets->unenc, packets->notdev, packets->dropped));
      DEBUG(DBG_STATISTICS, sprintf(dbg, "udp: %ld tcp:%ld icmp:%ld other:%ld\n", 
				    packets->ip_udp, packets->ip_tcp, packets->ip_icmp, 
				    packets->ip_other));
      if(list_lookups != 0)
	{
	  DEBUG(DBG_STATISTICS, sprintf(dbg, "lookups:%d compares:%d compares/lookup:%f\n",
					list_lookups, list_compares, 
					((float) list_compares / (float) list_lookups)));
	}
      lck = 1; /* can't update the list now */
      
      DEBUG(DBG_MISC, sprintf(dbg, "Total of %ld entries\n", plistsize));
      
      /* We build two lists:
	 1) olist, which will be written out
	 2) tlist, which will be the new plist
	 */
      
      p = plist;
      tlist = NULL;
      olist = NULL;
      olistsize = 0;
      plistsize = 0;
      
      while(p)
	{
	  q = p->next;
	  if(((now - p->when) > cfg->fdelay) || force)
	    {
	      p->next = olist;
	      olist = p;
	      olistsize++;
	    }
	  else
	    {
	      p->next = tlist;
	      tlist = p;
	      plistsize++;
	    }
	  p = q;
	}
      
      plist = tlist;
      
      if(dumping == 0)
	{
	  dumping = 1;

	  TELL_WAIT_INIT();

	  dump_curr_list();

	  TELL_CHILD();
	  WAIT_CHILD();
	  
	  DEBUG(DBG_STATE, sprintf(dbg, "parent: synchronized with dump child\n"));
	  
	  TELL_WAIT_EXIT();

	  DEBUG(DBG_STATE, sprintf(dbg, "dumppid is %d\n", (int) dumppid));
	}
      
      writing = 1; /* no further writing 'til this is finished */
      lck = 0;
      
      DEBUG(DBG_MISC, sprintf(dbg, "Split into %ld [hold] and %ld [write] = %ld [total] entries\n", plistsize, olistsize, plistsize + olistsize));
      
      TELL_WAIT_INIT();

      write_list(); /* this forks off a child to do the actual writing */
      
      TELL_CHILD();
      WAIT_CHILD();

      DEBUG(DBG_STATE, sprintf(dbg, "parent: synchronized with write child\n"));
      
      TELL_WAIT_EXIT();

      DEBUG(DBG_STATE, sprintf(dbg, "writepid is %d\n", (int) writepid));
      
      p=olist;
      while(p)
	{
	  olist = p->next;
	  free(p->devname);
	  if (p->user)
	      free(p->user);
	  free(p);
	  p=olist;
	}
      
      DEBUG(DBG_STATE, sprintf(dbg, "done freeing\n"));
    }
  else
    {
      DEBUG(DBG_STATE, sprintf(dbg, "flushing delayed (writing == %d, lck == %d, may_write == %d)\n",writing,lck,may_write));
    }
}

void signal_debug(int sig)
{
  DEBUG(DBG_SIGNAL, sprintf(dbg, "got signal %d, handling\n", sig));
    
  if(sig==SIGUSR1)
    {
      debug_level++;
    }
  else if(sig==SIGUSR2)
    {
      syslog(LOG_DEBUG, "turning off debugging\n");
      debug_level = 0;
    }
  else if(sig==SIGWINCH)
    {
      syslog(LOG_DEBUG,"nacctd, revisions:\n%s\n%s\n%s\n%s\n", 
	     rcs_revision_main_c, rcs_revision_process_c,
	     rcs_revision_config_c, rcs_revision_daemon_c);
    }
  else if(sig==SIGTSTP)
    {
      DEBUG(DBG_STATE, sprintf(dbg, "received SIGTSTP\n"));
      may_write = 0;
    }
  else if(sig==SIGCONT)
    {
      DEBUG(DBG_STATE, sprintf(dbg, "received SIGCONT\n"));
      may_write = 1;
    }
  else if(sig==SIGIOT)
    {
      DEBUG(DBG_STATE, sprintf(dbg, "reopening socket\n"));
      reopen_socket();
    }
  else
    {
      DEBUG(DBG_SIGNAL, sprintf(dbg, "signal_debug received signal %d, this can't happen\n", sig));
      syslog(LOG_INFO,"signal_debug received signal %d, this can't happen\n", sig);
    }
}

void signal_ignore(int sig)
{
  DEBUG(DBG_SIGNAL, sprintf(dbg, "got signal %d, ignoring\n", sig));
}



