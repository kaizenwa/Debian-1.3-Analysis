    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "server_def.h"
#include "s_extern.h"
#include "co_extern.h"

extern int errno;

#ifdef VMS  
#define malloc VAXC$MALLOC_OPT
#define free VAXC$FREE_OPT
extern int isvar; /* in server_file.c */
#endif

extern int no_unnamed;

static int myport = 0;
static int myfd;
static int interrupted = 0;
static int thc[THCCOUNT];
static time_t thcbase;

extern int priv_mode;
extern unsigned int maxthcallowed;

static void server_interrupt PROTO1(int, signum)
{
  interrupted = 1;
#ifndef RELIABLE_SIGNALS
  signal(SIGALRM,server_interrupt);
#endif
}

/****************************************************************************
 *  This is the message filter.  It is called by main with a timeout value.
 *  If timeout is -1, it will never time out.  Otherwise, it waits for a
 *  message.  If timed out, it returns.  Otherwise it pass it through checks.
 *  Those message that passed get sent to the dispatch loop.
 ****************************************************************************/

int server_loop PROTO1(unsigned long, timeout)
{
  unsigned long cur_time;
  HTAB *hp;
  char *ir;
  UBUF rbuf;
  struct sockaddr_in from;
  unsigned int u, sum, mask, rlen, rkey;
  int retval, bytes, old;
  unsigned char *s, *d, *t;
  
  while(1) {
    mask = 1 << myfd;
    if(interrupted) {
      dump_htab();
      dump_iptab();
      interrupted = 0;
    }
    retval = _x_select(&mask, timeout);
      
    if(retval == -1) {
      if(errno == EINTR) continue;
      perror("select");
      exit(1);
    }
      
    if(retval == 1) {   /* an incoming message is waiting */
      bytes = sizeof(from);
      if((bytes = recvfrom(myfd,(char*)&rbuf,sizeof(rbuf),0,
			   (struct sockaddr *)&from, &bytes)) < UBUF_HSIZE)
	continue;
	  
      rlen = BB_READ2(rbuf.bb_len);
      if((int) (rlen+UBUF_HSIZE) > bytes) continue;	/* truncated.  */
	  
      if(!(ir = check_ip(from.sin_addr.s_addr)))
	ir = priv_mode ? "DFSP service not available": "N";
	  
      switch (*ir) {
        case 'D':	/* disabled host - return error message */
	  if (rbuf.cmd == CC_BYE)
	    break;
	  send_error(&from,&rbuf,&ir[1]);
	  continue;
	case 'I':	/* ignore the host */
	  continue;
	case 'N':	/* normal host */
	  break;
	default:
	  fputs("check_ip() returned illegal host type\n",stderr);
	  exit(0);
      }
	  
      if(!(hp = find_host(from.sin_addr.s_addr))) {
	fputs("find host failed\n",stderr);
	exit(0);
      }
	  
      if(hp->hostname == 0 && no_unnamed) {
	send_error(&from,&rbuf, REVERSE_ERR_MSG);
	continue;
      }
	  
      old = 0;
      cur_time = time((time_t *) 0);
	  
      rkey = BB_READ2(rbuf.bb_key);
      if(hp->next_key != rkey) {
	if(!hp->active)
	  hp->last_key = hp->next_key = rkey;
	else {
	  if(hp->last_key == rkey) {
	    if(cur_time < hp->last_acc + 3) continue;
	    old = 1;
	  } else {
	    if(cur_time < hp->last_acc + 60) continue;
	  }
	}
      }
	  
      hp->active = 1;
      hp->last_acc = cur_time;
	  
      s = (unsigned char *) &rbuf;
      d = s + bytes;
      u = rbuf.sum; rbuf.sum = 0;
      for(t = s, sum = bytes; t < d; sum += *t++);
      sum = (sum + (sum >> 8)) & 0xff;
      if(sum != u) continue;			/* wrong check sum */
	  
      server_get_packet(bytes,&rbuf,old,hp,&from);
    } else return(0);				/* got a timeout */
  }
}

/****************************************************************************
 * Routine to return a 16-bit key with random number in the first 8-bits and
 * zero in the second 8-bits.
 ****************************************************************************/

unsigned long get_next_key PROTO0((void))
{
  unsigned long k;
  
  k = random();
  k = k ^ (k >> 8) ^ (k >> 16) ^ (k << 8);
  
  return(k & 0xff00);
}

/****************************************************************************
 * Generic routine for sending reply back to clients.
 *        from: client address structure.
 *          ub: pointer to the message buffer.
 *  len1, len2: lengths of the two data regions in the message buffer.
 ****************************************************************************/

int server_reply PROTO4(struct sockaddr_in *, from, UBUF *, ub,
			int, len1,int, len2)
{
  unsigned char *s, *t, *d;
  unsigned sum;
  time_t thcclock;
  int i, thcsum;
  
  if(dbug) fprintf(stderr,"snd (%c,%d,%d,%lu) ---> %d.%d.%d.%d\n",
		   ub->cmd, len1, len2, BB_READ4(ub->bb_pos),
		   ((unsigned char *)(&(from->sin_addr.s_addr)))[0],
		   ((unsigned char *)(&(from->sin_addr.s_addr)))[1],
		   ((unsigned char *)(&(from->sin_addr.s_addr)))[2],
		   ((unsigned char *)(&(from->sin_addr.s_addr)))[3]);
  
  BB_WRITE2(ub->bb_len,len1);
  
  ub->sum = 0;
  s = (unsigned char *) ub;
  d = s + (len1 + len2 + UBUF_HSIZE);
  for(t = s, sum = 0; t < d; sum += *t++);
  ub->sum = sum + (sum >> 8);
  
  /*
   * Check that we do not exceed maximum throughput allowed before sending
   */
  if(maxthcallowed)
    for(;;) {
      time(&thcclock);
      if(thcclock > thcbase) {
	if(thcclock>thcbase+THCCOUNT) {
	  int i;
	  for(i = 0; i < THCCOUNT; thc[i++]=0);
	  thcbase = thcclock;
	} else {
	  while (thcclock > thcbase) {
	    for(i = THCCOUNT-1; i>0; i--) thc[i] = thc[i-1];
	    thc[0] = 0;
	    thcbase++;
	  }
	}
      }
      for(i = 0, thcsum = 0; i< THCCOUNT; thcsum+= thc[i++]);
      thcsum /= THCCOUNT;
      if(dbug)
{
	fprintf(stderr, "Average throughput: %d bytes/s | ", thcsum);
for (i= THCCOUNT-1;i>=0;i--) fprintf(stderr,"%5d ",thc[i]) ;
fprintf(stderr,"\n") ;
}
      if(thcsum <= maxthcallowed) {
	thc[0]+=(len1+len2+UBUF_HSIZE);
	break;
      }
      if(dbug) fprintf(stderr, "Throughput too high, waiting.\n");
      sleep(1);
    }
  
  if(sendto(myfd,(char *)ub,(len1 + len2 + UBUF_HSIZE),0,
	    (struct sockaddr *)from,sizeof(struct sockaddr_in)) == -1) {
    perror("sendto");
    exit(1);
  }
}

/****************************************************************************
 * Send an error string.
 ****************************************************************************/

int send_error PROTO3(struct sockaddr_in *, from, UBUF *, ub, char *, msg)
{
  char *d;
  
  for(d = ub->buf; *d++ = *msg++; );
  ub->cmd = CC_ERR;
  
  server_reply(from,ub,d-ub->buf,0);
}

/****************************************************************************
 * Send a block of data read from the file 'fp'.  Offset information is
 * contained in the input ub message buffer, which also doubles as the output
 * message buffer.
 ****************************************************************************/

void send_file PROTO5(struct sockaddr_in *, from, UBUF *, ub, FILE *, fp,
		      int, has_len, char *, lp)
{
  int bytes, len;
  unsigned long pos;
#ifdef VMS
  char *buf;  /* temp buffer for seek on var.rec.format files */
  register int rtime = 5; /* max number of fread retries (var.rec.) */
#endif
  
  if(has_len == 2) {	/* recover length field if it exists */
    len  = (len=lp[0] << 8) + lp[1];
    if(len > UBUF_SPACE || len < 0) len = UBUF_SPACE;
  } else len  = UBUF_SPACE; /* use default if it doesn't exist */

  pos = BB_READ4(ub->bb_pos);
  
#ifdef VMS
  if(isvar) {
    /* fseek fails on files in variable record format */
    rewind(fp);
    buf=(char *)malloc(pos);
    bytes = fread(buf,1, pos, fp); /* dummy read pos bytes */
    free(buf);
  } else
    if(pos != ftell(fp)) fseek(fp, pos, 0);
#else
  fseek(fp,pos,0);
#endif
  
  bytes = fread(ub->buf, 1, len, fp);
  
#ifdef VMS /* some stupid VMS-bug in fread */
  while (!bytes && errno==65535 && rtime) {
    rtime--;
    bytes=fread(ub->buf,1, len, fp);
  }
#endif
  
  server_reply(from,ub,bytes,0);
}

/****************************************************************************
 * The two UDP socket initialization routines.  One for running alone.
 * The other for running under inetd.
 ****************************************************************************/

void init_network PROTO1(int, port)
{
  int i;
  
  /* init throughput control */
  if(maxthcallowed) {
    for(i = 0; i<THCCOUNT; thc[i++]=0);
    time(&thcbase);
  }
  
  myport = port;
  
  if((myfd = _x_udp(&myport)) == -1) {
    perror("socket open");
    exit(1);
  }
  
  if(dbug) {
    fprintf(stderr,"listening on port %d\n",myport);
    fflush(stderr);
  }
  
  signal(SIGALRM,server_interrupt);
}

void init_inetd PROTO0((void))
{
  myfd = dup(0);
  
  signal(SIGALRM,server_interrupt);
}
