#define I_IOCTL
#define I_ERRNO
#define I_TYPES
#define I_STAT
#define I_STRING
#define I_CTYPE
#define I_INET
#define I_TIME
#define I_LIMITS
#define I_STROPT
#define I_POLL
#include "includes.h"

#include "debug.h"

#define BUFF_INC_SIZE 1024
#define MAX_BUFF 32767

char *term_strherror(int);

int termerrno = 0;

int share = -1;

/*------------------------------------------------------------------------*/
/* Do a partial read into the buffer. */
static int do_read_into_buff(int fd, struct Buffer *b, int size) {
  int r = 0, t;

  

  if (!size || size > b->alloced - 1 - b->size) 
    size = b->alloced - 1 - b->size;

  termerrno = 0;
  t = b->alloced - b->start;
  if (t > size) t = size;
#ifdef SVR3 /* System Vr3 doesn't restart system calls */
  while ((r = read(fd, b->data + b->start, t)) == -1 && errno == EINTR);
#else
  r = read(fd, b->data + b->start, t);
#endif
  if (r <= 0)  {
    if (!r) termerrno = 1;
    else 
#ifndef SVR4
      if (errno != ERR_BLOCK)
#endif /* SVR4 */
        termerrno = errno + 1;
    return r;
  }
  DEBUG_LL(stderr, "%s: d_r_i_b: read1 from %d (%d) did %d\n", 
	   term_server, fd, t, r);
  
  b->start += r;
  b->size += r;
  SANITY(b->start <= b->alloced);
  if (b->start == b->alloced)
    b->start = 0;
  size -= r;
  if (!size || b->start != 0) return r;
  
  t = b->alloced - b->start;
  if (t > size) t = size;
 
#ifdef SVR3                   /* System Vr3 doesn't restart system calls */
  while ((t = read(fd, b->data + b->start, t)) == -1 && errno == EINTR);
#else
  t = read(fd, b->data + b->start, t);
#endif
  if (t <= 0) {
    if (!t) termerrno = 1;
    else if(errno != ERR_BLOCK)
      termerrno = errno+1;
    return r;
  }
  DEBUG_LL(stderr, "%s: d_r_i_b: read2 from %d (%d) did %d\n", 
	   term_server, fd, t, t);
  
  b->start += t;
  b->size += t;
  if (b->start == b->alloced)
    b->start = 0;
  size -= t;
  r += t;
  return r;
}

static int inc_buffer(struct Buffer *b, int do_move) {
  if( (b->data == NULL) || (b->alloced == 0)) {
    if( (b->data = (un_char *)malloc((1+BUFF_INC_SIZE)*sizeof(un_char))) == NULL){
      x__perror("Term: malloc()");
      return -1;
    }
    b->alloced = BUFF_INC_SIZE + 1;
    b->size=0;
  }else if(b->alloced >= MAX_BUFF) {
    DEBUG_UDP(stderr,"%s:%s",term_server,"Term: max buffer\n");
    return -1;
  }else if ((b->data = (un_char *)realloc(b->data, sizeof(un_char)*
     (((b->alloced+BUFF_INC_SIZE) < 0 || (b->alloced+BUFF_INC_SIZE > MAX_BUFF))) ?
      MAX_BUFF : b->alloced+BUFF_INC_SIZE)) == NULL) {
    x__perror("%Term: realloc");
    return -1;
  }else { 
    b->alloced += BUFF_INC_SIZE;
    if (b->alloced > MAX_BUFF || b->alloced < BUFF_INC_SIZE)
      b->alloced = MAX_BUFF;
  }
  if (!b->size) b->start=b->end=0;

	/* It took me ages to realize this is what is being done here! */

	/* OK, we use buffers as a continuous loop.  So when we reach the eob */
	/* (end of buffer) we just continue on at the bob (beginning of buffer). */
	/* So if we want to increase the size of the buffer while it is in use */
	/* we must take data at the eob and move it to the new eob so we have */
	/* more space where we need it in the mob (middle of buffer). */

	/* This means "start" is where we can start adding more data to the */
	/* buffer, and "end" is where we last ended removing the data. */

  if ( do_move && (b->end > b->start)) {    /* curses. need to move data. */
    int i; char *new_eob, *old_eob;
    new_eob = (caddr_t)b->data + b->alloced - 1;
    old_eob = (caddr_t)b->data + b->alloced - BUFF_INC_SIZE - 1;
    for (i = b->alloced - BUFF_INC_SIZE - b->end; i > 0; --i)
      *new_eob-- = *old_eob--;
    b->end += BUFF_INC_SIZE;
  }
  return 0;
}



/*-----------------------------------------------------------------------*/

#ifndef O_NONBLOCK
#define O_NONBLOCK FNDELAY
#endif

void set_nonblock(int fd) {
  int val = 1;		/* Checker V0.5 needs assignment even if !USE_IOCTL */
#ifdef USE_IOCTL
  ioctl(fd, FIONBIO, &val);
#else
  val = x__fcntl(fd, F_GETFL,0);
  if (!(val & O_NONBLOCK))
    x__fcntl (fd, F_SETFL, val | O_NONBLOCK);
#endif
}

int set_block(int fd) { 
  int val = 0;		/* Checker V0.5 needs assignment even if !USE_IOCTL */
#ifdef USE_IOCTL
  ioctl(fd, FIONBIO, &val);
  return 1;
#else
  val = x__fcntl(fd, F_GETFL, 0);
  if (val & O_NONBLOCK)
    x__fcntl (fd, F_SETFL, val & ~O_NONBLOCK);
  return !(val & O_NONBLOCK);
#endif
}

/* Read from a file-descriptor into a ring buffer. Read at most size bytes */
int read_into_buff(int fd, struct Buffer *b, int z) {
  int ret, l = 0, size;

  if (! b->alloced) inc_buffer(b,0);
 
  if ((size = z) > b->alloced - 1 - b->size) inc_buffer(b,1); 

  if (!size)
    size = b->alloced - 1 - b->size;

  do {
    ret = do_read_into_buff(fd, b, size);
    if (ret < 1) break;
    l += ret;
    size -= ret;
    if (!z && !termerrno && !size && b->alloced < PIPE_BUFFER) {
      if (inc_buffer(b,1) < 0) 
        termerrno = 1;
      else
        size = b->alloced - 1 - b->size;
    }
  } while (!termerrno && size);
  if (!b->size) b->start=b->end=0;
  if (!l) return ret;
  return l;
}

#ifdef hcx
/* Under HCX/UX 5.1 (at least), writes of 256 bytes or more can return */
/* a -1 with errno == EAGAIN, but actually write the data successfully. */
#undef _POSIX_PIPE_BUF
#define _POSIX_PIPE_BUF 128
#endif

/*------------------------------------------------------------------------*/
/* Write from ring buffer to file descriptor. Write at most size bytes    */

int internal_write_from_buff(int fd, struct Buffer *b, int size, int async) {
  int r = 0, t, i;

  SANITY(b->size <= b->alloced);
  if (!size) size = b->size;
  if (size > b->size) size = b->size;

  while (size > 0) {	/* We use a loop in case there is a maximum block size */
    t = b->alloced - b->end;
    if (t > size) t = size;
    if (t > _POSIX_PIPE_BUF) t = _POSIX_PIPE_BUF;

    i = write(fd, b->data + b->end, t);

    if (i <= 0) {
      if (!i) termerrno = 1;
#ifdef SVR3
      else if (i == -1 && errno == EINTR) continue;
#endif
      else if (errno != ERR_BLOCK)
        termerrno = errno+1;
      return r;
    }
    DEBUG_LL(stderr, "%s: d_w_f_b: write from %d (%d) did %d\n", 
	   term_server, fd, t, i);

    r += i;

    b->end += i;
    b->size -= i;
    size -= i;
    SANITY(size <= b->size);

    SANITY(b->end <= b->alloced);
    if (b->end == b->alloced) b->end = 0;
    if (i != t && async)
      break;
  };
  if (!b->size) b->start=b->end=0;
  return r;
}

int write_from_buff(int fd, struct Buffer *b, int size) {
  return internal_write_from_buff(fd, b, size, 0);
}

int write_from_buff_async(int fd, struct Buffer *b, int size) {
  return internal_write_from_buff(fd, b, size, 1);
}

int add_to_buffer(struct Buffer *B, un_char c) {

  if (! B->alloced) B->size=0;
  if (B->size >= B->alloced - 2) {
    
    DEBUG_LL (stderr, "%s:add_to_buffer:growing buffer from %d, %d\n",
	     term_server, B->size, B->alloced);

    if( inc_buffer(B,(B->alloced > 0)) < 0) {
      termerrno = 1;
      return -1;
    }
  }
  B->data[B->start++] = (c);
  if (B->start == B->alloced) B->start = 0;
  B->size++;
  return 0;
}

int get_from_buffer(struct Buffer *b) {
  un_char c;
  if (b->size < 1)
    return -1;
  c = b->data[b->end++];
  --b->size;
  if (b->end >= b->alloced)
    b->end = 0;
  if (!b->size) b->start=b->end=0;
  return (int) c&255;
}
/*-----------------------------------------------------------------------*/

	/* KLUDGE. suns always need stderr, so ignore the config.h if */
	/* it says otherwise. This is mostly just a sanity check */
#if !defined(NO_STRERROR) && !defined(BUILD_LIBC)
char *x__strerror(int errno) {
  extern char *sys_errlist[];
  
  return sys_errlist[errno];
}
#endif

	/* This is to allow easy checking of effective access */
int eaccess(char *pathname, int mode){
  struct stat file_status;
  int m;

  if(stat(pathname,&file_status) == -1) return -1;
  if(! mode) return 0;

  if(file_status.st_uid == geteuid())
    m = mode << 6;
  else if ( file_status.st_gid == getegid())
    m = mode << 3;
  else
    m = mode;

  return  - ((file_status.st_mode & m) != m) ; 
}

	/* This tests whether we are in shared mode */
void set_share_mode(int priv, int share_new){
  char *e;
  static int share_old = -2;

  share = share_new;
  if (share_old == -2) {
    e = getenv("TERMMODE");
    if (e) 
      share_old = atoi(e);
    if(share_old < 0 || share_old > 2) share_old = -1;
  }

  if (share_old != share) {
#ifdef _POSIX_SAVED_IDS
    if (savedeid  >= 0) {
      if (share > 1) 
        setgid(savedeid);
      else if (share > 0)
        setgid(savedeid);
      savedeid = -1;
    }
#endif
    if (share < 0) share = share_old;
  }

  if (share < 0) {
    share = (getgid() != getegid());
    if (! share)
      share = (getuid() != geteuid() && getuid()) ? 2 : -1;
  }

  if (! share) {
    if (getgid() != getegid()) 
      setgid(getgid());
    else if (geteuid() != 0 && geteuid() != getuid())
      setuid(getuid());
  }
#ifdef _POSIX_SAVED_IDS
  else if (share > 0 && savedeid < 0){
    if ( share == 2 ) {
      savedeid = geteuid();
      setuid(getuid());
    }else {
      savedeid = getegid();
      setgid(getgid());
    }
  }
#endif

  if (share != share_old) 
    share_old = share;
  
  if (priv) {
#ifdef _POSIX_SAVED_IDS
    savedeid = -1;
#endif
    if (geteuid() && geteuid() != getuid()) setuid(getuid());
    if (getegid() && getegid() != getgid()) setgid(getgid());
  }
}

char *str_version(long unsigned int version){
  static char v[25];
  if(version%100 < 50 )	/* These are bug fix releases */
    sprintf(v, "%lu.%lu.%lu",(version/10000),
      (version/100)%100,version%100);
  else if(version%100 < 77 ) 	/* These are alpha releases */
    sprintf(v, "%lu.%lu%c (alpha)",(version/10000),
      (version/100)%100,'a'+(char)((version-51)%100));
  else if(version%100 < 90 ) 	/* These are alpha releases */
    sprintf(v, "%lu.%lu%c%c (alpha)\r\n",(version/10000),
      (version/100)%100,'z','a'+(char)((version-77)%100));
  else
    sprintf(v, "alpha pre-release of %lu.%02lu pl %02lu",(version/10000),
      1+(version/100)%100,version%10);
  return v;
}

	/* This is an unreliable protocol.  So in the case of errors */
	/* we must just try to continue the best we can. */

int recvfrom_into_buff(struct Client *cl) {
  struct sockaddr_in from_addr;
  int r = 0, i, fd, type;
  struct Buffer *b;
  static un_char *from = NULL;
  static size_t alloced = 0;
  unsigned int avail = 0, len, hdr = 0, t;
  unsigned long lhost, *host;
  unsigned int lport, *port;

  host = &cl->udp_host;
  port = (unsigned int *)&cl->udp_port;

  fd = cl->fd;
  b = &cl->in_buff;
  type = cl->udp_type;

  termerrno = 0;

  DEBUG_UDP(stderr,"%s# recvfrom_i_b called.\n",term_server);

#ifndef FIONREAD
  /*
   * System V release 3 doesn't have an FIONREAD even on sockets, but
   * in looking at the code it looks like a select here just to tell
   * if there is any data available is sufficient.  Size is always set
   * to PIPE_BUFFER or greater so I suspect if I just read PIPE_BUFFER I'll
   * be OK.  It might require bumping PIPE_BUFFER  up to the maximum message
   * size.
   */
  {
    struct pollfd fds[2];
    int status;
    
    fds[0].fd = fd;
    fds[0].events = POLLIN | POLLPRI | POLLERR | POLLHUP;
    fds[0].revents = 0;
   
    errno = 0;
    status = -1;
    while (status == -1) {
      status = poll(fds, 1L, 2);
      if (errno != EAGAIN)
      break;
    }

    if (status == -1) {
      x__perror("ERROR in poll request");
      return 0;
    }

    if (fds[0].revents & POLLIN | POLLPRI)
      avail = PIPE_BUFFER;
    else
      return 0;
  }
#else
  if( ioctl( fd, FIONREAD, &avail) < 0)  return 0;
#endif
  if (avail < PIPE_BUFFER) avail = PIPE_BUFFER;

  if( type & UDP_T_RECADDHDR) hdr = HEADER_SIZE; else hdr = 0;

  if ( ! from) {
    from = (un_char *)malloc(sizeof(un_char)*BUFF_INC_SIZE);
    alloced = BUFF_INC_SIZE;
  }
  SANITY(alloced);

  while ( from && (avail+hdr) > alloced && alloced < MAX_BUFF) {
    from = (un_char *)realloc(from,sizeof(un_char)*
     ((alloced+BUFF_INC_SIZE > MAX_BUFF) ?
      MAX_BUFF : alloced+BUFF_INC_SIZE));
    alloced += BUFF_INC_SIZE;
    break;
  }
  if (alloced > MAX_BUFF) alloced = MAX_BUFF;
  avail = alloced;

  if (! from) {		/* OK, this seems like a reasonable exception... */
    alloced = 0;
    termerrno = 1;
    return -1;
  }

  len = sizeof(from_addr);
  if ((i = x__recvfrom(fd, from+hdr+2, avail, 0,
         (struct sockaddr *)&from_addr, (int *)&len)) < 0) {
    DEBUG_UDP(stderr,"recvfrom %d : %s\n", avail,x__strerror(errno)); 
    				/* We failed, thats tuff, we should still */
				/* Leave the socket open to try again... */
    free(from);
    from = NULL;
    alloced = 0;
    return 0;
  }
  SANITY(avail <= alloced);

  DEBUG_UDP(stderr,"%s# recvfrom()'d: host %lx, port %d.\n", term_server, 
                    (long)ntohl(from_addr.sin_addr.s_addr), 
                    ntohs(from_addr.sin_port));

  DEBUG_UDP(stderr,"# state 1: avail %d, avail+hdr %d, i %d, b->size %d.\n",
                    avail, avail+hdr, i, b->size);

  avail = i + hdr + 2;
  lhost = ntohl(from_addr.sin_addr.s_addr);
  lport = ntohs(from_addr.sin_port);
  if (type & UDP_T_RECADDHDR) {
    from[2] = (lhost>>24)&255;
    from[3] = (lhost>>16)&255;
    from[4] = (lhost>>8)&255;
    from[5] = lhost&255;
    from[6] = (lport>>8)&255;
    from[7] = lport&255;
  }

  if ( ! *host || ! *port ) {
    *host = lhost;
    *port = lport;
  }

  while( avail > (b->alloced - b->size) && b->alloced < MAX_BUFF) { /* +HEADER_SIZE just to make sure... */
    if( inc_buffer(b,1) < 0) {
      termerrno = 1; 	/* Our buffer is gone, so we have no choice but to */
			/* report this error. */
      return -1;
    }
  }

	/* If necessary, truncate the message, and store the size */

  if ( avail > (b->alloced - b->size) ) avail = b->alloced - b->size;
  from[0] = (avail>>8)&255;
  from[1] = avail & 255;

	/* We must be careful here, since there is no guarantee that the */
	/* available memory in the buffer is continuous!! */

	/* The first chunk. */

  t = b->alloced - b->start;
  if( t > avail) t = avail; /* do it ALL at once */

  DEBUG_UDP(stderr,"%s# state 2: t %d, avail+hdr+2 %d.\n", term_server, t, avail);

  memcpy( b->data + b->start, from, t); /* bugger checking the return code... */

  b->start += t;
  if( b->start == b->alloced)
    b->start = 0;
  b->size += t;


  DEBUG_UDP(stderr,"%s# state 3: b->start %d, b->size %d.\n", term_server, 
    b->start, b->size);

  if (t == avail) { /* all done, return */
    DEBUG_UDP(stderr,"%s# return 1 (%d) - all done.\n", term_server, t);
    return (int) t;
  }

	/* The memory wasn't continuous, so now we need to get the rest. */

  r = avail - t;

  DEBUG_UDP(stderr,"%s# round 2: r %d.\n", term_server,r);

  SANITY(b->start == 0);
  memcpy( b->data, from + t, r); /* b->start needed? */

  b->start += r;
  SANITY(b->start < b->alloced);
  b->size += r;

  DEBUG_UDP(stderr,"%s# state 3: r %d, b->start %d, b->size %d.\n", term_server,
    r, b->start, b->size);
  DEBUG_UDP(stderr,"%s# return 2 (%d) - all done.\n", term_server, avail+hdr);

	/* Now we are really done. */

  return (int) avail;
}



/* ------------------------------------------------------------------------------
 * send to_from_buff() 
 * basically, transfer from Buffer *b to the array *to and returns the number of
 * chars send.
 *
 * also, take care of the header... header is the first HEADER_SIZE bytes of the
 * packet telling host/port to sendto()'it (or host/port it was recvfrom()''ed)
 * ans the size of the packet, INCLUDING header.
 *
 */


	/* This is an unreliable protocol.  So in the case of errors */
	/* we must just try to continue the best we can.  If we are able */
	/* to try another message, we shouldn't return an error. */

int sendto_from_buff(struct Client *cl) {
  int type, *port;
  int r=0, t, i;
  struct Buffer *b;
  unsigned long int *host;
  unsigned long int lhost;
  unsigned short int lport;
  struct sockaddr_in toaddr_in;
  static un_char *to = NULL;
  static size_t alloced = 0;

  b = &cl->out_buff;
  type = cl->udp_type;
  host = &cl->udp_host;
  port = &cl->udp_port;

  termerrno = 0; /* this sucker cost be around 2 hours of debugging! :) */

  DEBUG_UDP(stderr,"%s@ sendto_from_buff called.\n",term_server);
  DEBUG_UDP(stderr,"%s@b->size %u b->end %d\n",term_server,
    b->size, b->end);

	/* If we don't have the header yet we can't do anything. */
  if ( ! b->alloced || b->size < 2 ) {
    DEBUG_UDP(stderr,"%s@only have one udp byte.\n",term_server);
    cl->udp_size=b->size;
    return 0;
  }
  cl->udp_size =
    (b->data[b->end]<<8) +
    (b->data[(b->end + 1) % b->alloced]&255);

  if (cl->udp_size < HEADER_SIZE+2 || cl->udp_size > MAX_BUFF) {
    DEBUG_UDP(stderr,"%s@ term invalid udp packet size.\n",term_server);
    cl->udp_size=b->size=b->start=b->end=0;
    termerrno = 1; 	/* OK, once we have an internal error like this */
    return -1;		/* we are doomed. */
  }

  if (b->size < cl->udp_size) { 
    DEBUG_UDP(stderr,"%s@udp message is not complete yet. %d/%d\n",
      term_server, b->size, cl->udp_size);
    return 0;	/* I'll be back. */
  }

  if (b->size - 2 < HEADER_SIZE) {
    DEBUG_UDP(stderr,"%s@term udp packet is too small.\n",term_server);
    cl->udp_size=b->size=b->start=b->end=0;
    termerrno = 1; 	/* OK, once we have an internal error like this */
    return -1;		/* we are doomed. */
  }
  b->size -= 2;
  b->end = (b->end + 2) % b->alloced;
  cl->udp_size -= 2;

  if (! to) {
    to = (un_char *)malloc(sizeof(un_char)*BUFF_INC_SIZE);
    alloced = BUFF_INC_SIZE;
  }
  SANITY(alloced);

  while (to && cl->udp_size > alloced && alloced < MAX_BUFF) {
    to = (un_char *)realloc(to,sizeof(un_char)*
     ((alloced+BUFF_INC_SIZE > MAX_BUFF) ?
      MAX_BUFF : alloced+BUFF_INC_SIZE));
    alloced += BUFF_INC_SIZE;
  }

  if (! to) {		/* Not much we can do if we are out of memory! */
    DEBUG_UDP(stderr,"%s@ allocation failure.\n",term_server);
    alloced = 0;
    termerrno = 1;
    return -1;
  }

  if (alloced > MAX_BUFF) alloced = MAX_BUFF;
  SANITY(alloced >= BUFF_INC_SIZE);

  if ( cl->udp_size > alloced ) cl->udp_size = alloced;	/* Anyone sending too large  of a */
					/* udp message deserves what they get. */
  t = cl->udp_size;
  if (t > b->alloced - b->end) t = b->alloced - b->end;

  DEBUG_UDP(stderr,"@ 1st chunk: t %d, b->end %d\n",t,b->end);
  SANITY(to && b->data);
  memcpy(to, b->data + b->end, t);

  b->size -= t;
  b->end = (b->end + t) % b->alloced;

  if ((r = cl->udp_size - t) > 0) {
    SANITY(r <= b->size);
    SANITY(b->end == 0);
    DEBUG_UDP(stderr,"@ 2nd chunk: r %d , b->end %d\n",r,b->end);
    memcpy (to + t, b->data, r);
    b->size -= r;
    b->end = (b->end + r) % b->alloced;
    t += r;
  } 
  
  if (!b->size) b->start=b->end=0;

  lhost = (to[0]<<24)+(to[1]<<16)+(to[2]<<8)+(to[3]);
  lport = (to[4]<<8)+(to[5]);
  DEBUG_UDP(stderr,"%s@message from %lx, %u\n",term_server,lhost,lport);

  if ( !(type & UDP_T_SENDIGNOREHDR) && (lhost || lport)) {
    /* don't ignore header, therefore put it in *host *port for sendto()'ing */
    DEBUG_UDP(stderr,"%s@ changing: from *host, *port %lx, %u to ",
      term_server, *host, *port);
    *host = lhost;
    *port = lport;
    DEBUG_UDP(stderr,"%lx, %u\n",*host, *port);
  }

  toaddr_in.sin_family = AF_INET;
  toaddr_in.sin_addr.s_addr = htonl(*host);
  toaddr_in.sin_port = htons(*port);
  if (toaddr_in.sin_addr.s_addr == INADDR_ANY ||
       toaddr_in.sin_addr.s_addr == inet_addr("127.0.0.254"))
    toaddr_in.sin_addr.s_addr = inet_addr("127.0.0.1");

  if( type & UDP_T_SENDSTRIPHDR) /* strip header before sending? */
    i = x__sendto(cl->fd, to + HEADER_SIZE, t - HEADER_SIZE, 0,
               (struct sockaddr *)&toaddr_in, sizeof(toaddr_in));
  else
    i = x__sendto(cl->fd, to, t, 0,
               (struct sockaddr *)&toaddr_in, sizeof(toaddr_in));

  cl->udp_size = 0;
  DEBUG_UDP(stderr,"%s@b->size %u b->end %d\n",
    term_server, b->size, b->end);

  if (i < 0) {
    DEBUG_UDP(stderr,"%s@ return 2.5 (%d) %lx, %u - sendto() failed.\n",
      term_server, i, *host, *port);
    *host = 0;	/* This unbinds the socket... */
    *port = 0;
    return t + 2;	/* Another error.  Tuff luck. */
  }

  DEBUG_UDP(stderr,"%s@ return 3 (%d) - did sendto(): host %lx, port %u, i %d\n",
    term_server, t, *host, *port, i);

  return t + 2;
}


/* This is grossly overly complicated because I try to convert invalid */
/* addresses to something recognizable */

char *sockaddr_to_sstr(struct sockaddr *addr, int trans) {
  return NULL;
}


void get_term_localaddr(unsigned long default_addr) {
  char hostname[sizeof(term_localhost)];
  struct hostent *hp;
  struct in_addr addr_in;

  if (term_localaddr != INADDR_ANY) return;

  memset(term_localhost,0,sizeof(term_localhost));
#if defined(SYSV) && !defined(DYNIXPTX)
  { struct utsname unam;
    uname(&unam);
    strcpy(hostname, unam.nodename); }
#else
  x__gethostname (hostname, sizeof(hostname));
#endif
  strcpy(term_localhost, hostname);
  if (hostname[0] >= '0' && hostname[0] <= '9') {
    addr_in.s_addr = inet_addr(hostname);
  }else {
    hp=host_lookup(hostname,0,AF_INET,0,NULL);
    if (!hp) {
      extern int h_errno;
      fprintf(stderr,"%s: gethostbyname: %s: %s\n",term_server, hostname,
        term_strherror(h_errno));
      addr_in.s_addr = default_addr;
      strcpy (term_localhost, "127.0.0.1");
    }else {
      memcpy(&addr_in, hp->h_addr, hp->h_length);
    }
  }
  term_localaddr = ntohl(addr_in.s_addr);
}

char *sockaddr_to_str(struct sockaddr *addr, int trans) {
  struct sockaddr_in addr_in;
  static char portname[24];
  unsigned long int j;
  char *a,*p;

#define UC(y) (int) ((int) 0xff & (int) (y))
  memset(&addr_in,0,sizeof(struct sockaddr_in));
  memcpy(&addr_in,addr,sizeof(struct sockaddr_in));
  p = (char *)&addr_in.sin_port;

  if ( ! addr_in.sin_addr.s_addr || addr_in.sin_family != AF_INET ){
    struct hostent *hp;
    char hostname[259];

#if defined(SYSV) && !defined(DYNIXPTX)
    { struct utsname unam;
      uname(&unam);
      strcpy(hostname, unam.nodename); }
#else
    x__gethostname (hostname, sizeof(hostname));
#endif
    if (hostname[0] >= '0' && hostname[0] <= '9') {
      addr_in.sin_addr.s_addr = inet_addr(hostname);
    }else {
      hp=host_lookup(hostname,0,AF_INET,0,NULL);
      if (!hp) {
        herror ("Term: gethostbyname");
        addr_in.sin_addr.s_addr = INADDR_ANY;
      }else {
        memcpy(&addr_in.sin_addr, hp->h_addr, hp->h_length);
      }
    }
  };
  j = addr_in.sin_addr.s_addr;
  if ( trans && (! j || j == INADDR_ANY || j == inet_addr("127.0.0.1"))) 
    j = ( remote_term_version >= 20000 ) ?
      inet_addr("127.0.0.254") : inet_addr("127.0.0.1");
  a = (char *)&j;
  (void) sprintf(portname, "%u,%u,%u,%u,%u,%u",
           UC(a[0]), UC(a[1]), UC(a[2]), UC(a[3]), UC(p[0]), UC(p[1]));
  return portname;
}

struct sockaddr *sstr_to_sockaddr(char *addr, unsigned long defaultaddr){
  return NULL;
}

struct sockaddr *str_to_sockaddr(char *addr, unsigned long defaultaddr){
  static struct sockaddr_in name_in;
  int j;
  char port[35], *u, *v;

  strncpy(port,addr,35);
  for(j=0;j<3;j++) *strchr(port,',')='.';
  u=strchr(port,',');
  *u='\0';
  v=strchr(++u,',');
  *v='\0';

  name_in.sin_addr.s_addr = inet_addr(port);
  if ( name_in.sin_addr.s_addr == INADDR_ANY
    || name_in.sin_addr.s_addr == inet_addr("127.0.0.1")
    || name_in.sin_addr.s_addr == inet_addr("127.0.0.254") )
    name_in.sin_addr.s_addr = defaultaddr;
    
  name_in.sin_family = AF_INET;
  name_in.sin_port = htons((atoi(u)<<8)+atoi(++v));
  return (struct sockaddr *)&name_in;
}   


struct sockaddr *make_sockaddr(unsigned int port, char *host,
     unsigned long defaulthost) {
  static struct sockaddr_in addr_in;
  struct hostent *hp;
  char *hostname=NULL, *colon;
  int s=259;

  memset(&addr_in,0,sizeof(addr_in)); 
  if (host) 
    s = (strlen(host)+1 < 259) ? 259 : strlen(host)+1;
 
  hostname = (char *)malloc(sizeof(char)*s);
  *hostname = '\0';
  if (host) {
    strcpy(hostname,host);
    if ((colon = strchr(hostname,':'))) *colon = '\0';
  }else if (defaulthost != INADDR_ANY) {
#if defined(SYSV) && !defined(DYNIXPTX)
    { struct utsname unam;
      uname(&unam);
      strcpy(hostname, unam.nodename);
    }
#else
    x__gethostname (hostname, sizeof(hostname));
#endif
  }

  addr_in.sin_family = AF_INET;
  if (! *hostname || ! strcmp(hostname,"localhost")
      || ! strcmp(hostname,"remotehost")
      || ! strcmp(hostname,"127.0.0.1")
      || ! strcmp(hostname,"127.0.0.254") ) {
    addr_in.sin_addr.s_addr = defaulthost;
  }else if (hostname[0] >= '0' && hostname[0] <= '9') {
    addr_in.sin_addr.s_addr = inet_addr(hostname);
  }else {
    hp=host_lookup(hostname,0,AF_INET,0,NULL);
    if (!hp) {
      if (host) {
	fprintf(stderr,"Term: gethostbyname ");
        herror (hostname);
        if (hostname) free(hostname);
        return NULL;
      }
      addr_in.sin_addr.s_addr = defaulthost;
      addr_in.sin_family = AF_INET;
    }else {
      memcpy(&addr_in.sin_addr, hp->h_addr, hp->h_length);
      addr_in.sin_family = hp->h_addrtype;
    }
  }

  if (hostname) free(hostname);
  addr_in.sin_port = htons(port);
  return (struct sockaddr *)&addr_in;
}


char *get_term_path(char **ptr) {
  int len;
  static char path[PATH_MAX];
  static char *home_p = NULL, *share_p = NULL;
  char *p = NULL;

  path[0] = '\0';

#define DEFAULT_PATH SHAREDIR ":/usr/local/lib/term:/usr/lib/term:/usr/etc:/etc"

  if (!home_p && !share) {
    *ptr = NULL;
    p = getenv( "TERMDIR" );
    if (!p) p = getenv( "HOME" );
    if (!p) p = "";

    home_p = (char *)malloc( sizeof(char) * (strlen(p)+strlen(DEFAULT_PATH)+2) ); 
    sprintf(home_p,"%s:%s",p,DEFAULT_PATH);
  }

  if (!share_p && share) {
    *ptr = NULL;
    p = getenv( "TERMSHARE" );
    if (!p) p = "";

    share_p = (char *)malloc( sizeof(char) * (strlen(p)+strlen(DEFAULT_PATH)+2) );
    sprintf(share_p,"%s:%s",p,DEFAULT_PATH);
  }

  if ( ! *ptr ) {
    if (share)
      p = share_p;
    else
      p = home_p;
  }else p = strchr(*ptr, ':');

  if (p) {
    char *q;

    while (*p == ':') p++;
    len = strlen(p) + 1;
    strncpy(path, p,(len > sizeof(path)) ? sizeof(path) : len);
    if ((q=strchr(path,':'))) *q = '\0';
  }

  *ptr = p;
  if (! *path)
    return NULL;
  return path;
}

#define CKSBUF 2048

/* Run a 16-bit CRC over the first nbytes of the given file
   Beware, this eats CPU...   -ot
*/
unsigned short file_crc(char *fname, long nbytes)
{
  char buf[CKSBUF];
  unsigned short sum = 0;
  int f, i, n = 0;

  if ((f = open(fname, O_RDONLY)) <0)
    return 0;
  while (nbytes>0) {
    if ((n = read(f, buf, nbytes<CKSBUF ? nbytes : CKSBUF)) <0) 
      return 0;
    if (!n)
      break;
    for (i=0; i<n; ++i)
      sum = update_crc(sum, (unsigned char) buf[i]);
    nbytes -= n;
  }
  x__close(f);
  if (nbytes) return 0;
  if (! sum) sum = 1;
  return sum;
}

int checkfd(int fd) {
  if (fd >= 0 && x__fcntl(fd,F_GETFD,0) < 0 && errno == EBADF) return -1;
  return fd;
}

int checkstream(int fd, int type) {
  fd_set in, out, except;
  struct timeval timeout;

  if ((fd=checkfd(fd))<0) return 0;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  FD_ZERO(&in);
  FD_ZERO(&out);
  FD_ZERO(&except);
  if (! type)
    FD_SET(fd,&in);
  else
    FD_SET(fd,&out);

  return (select(1+fd,&in,&out,&except,&timeout) >= 0);
}
