#define I_SYS
#define I_ERRNO
#define I_IOCTL
#define I_STRING
#define I_STAT
#define I_INET
#define I_CTYPE
#define I_TIME
#define I_UTIME
#include "includes.h"

#include "debug.h"

#define FATAL 1

void get_term_localaddr(unsigned long);
char *sockaddr_to_str(struct sockaddr *,int trans);
struct sockaddr *str_to_sockaddr(char *,unsigned long);
struct sockaddr *make_sockaddr(unsigned int port, char *host,
  unsigned long defaulthost);
char *term_strherror(int err);
int convert_to_8(int,int,un_char *,un_char *);
int convert_from_8(int,int,un_char *,un_char *);

#ifdef X_DEBUG
static int x_debug = 0;
#endif

extern do_stats(un_char *, int, struct Client *);

/*
 * This modules handles multiplexing the clients onto the serial stream.
 * 
 * do_link_in() is called when there are in packets waiting, and
 * do_link_out() is called when there is something in the link_out buffer and
 * 	the serial out buffer is empty.
 */

/*-----------------------------------------------------------------------*/
/* Local function prototypes */
int get_data(un_char *, int *, int);
void put_data(un_char *, int);
int get_client_data(struct Client *);
void put_client_data(struct Client *, int);
void init_client(struct Client *cl);
/*-----------------------------------------------------------------------*/
/* Data */

int curr_in_stream = -1, 
  curr_out_stream = -1 ;
/* we have this in the open to peek to see if compression is 
 * wanted...  croutons
 */
static struct Client *curr_client = 0;
int new_packet = 0;

/* Convert an address to a string */

static char *hostent_to_str(struct hostent *addr) {
  static char hostname[300];
  struct in_addr *addr_in;

  if(addr->h_addrtype==AF_INET){
    addr_in = (struct in_addr *) addr->h_addr;
    sprintf(hostname,"%lx %s",(long)ntohl(addr_in->s_addr),addr->h_name);
  }else{
    sprintf(hostname,"0 %s",addr->h_name);
  };
  return hostname;
}

/* To complicate matters, I require the first client then transmit version */
/* # and later go back and transmit the first real packet */
/*-----------------------------------------------------------------------*/
void do_link_out(void) {
  int len, type;
  static int sent_version = 0, old_len = 0, old_type = -1;

  /* Add another packet to the out packet list */
  
  if (p_out_num >= window_size ) {
				/* naff off. The packet window is full. */
				/* This is actually normal, so don't */
				/* print a message. */
#if 0
    DEBUG_LINK(stderr, "Tried to do link_out with p_out_num == %d\n",
	       p_out_num);
#endif
    return;
  }

  /* put some data in the packet. Get up to 'max' bytes. */
  /* returns the length. If the length is -ve then it has been compressed */
  
  new_packet = 1;

  if (!old_len){
    int i;
    i = (sent_version) ? p_out_s : ((p_out_s+1)%N_PACKETS);
    if (remote_term_version >= 20000) 
      len = get_data(p_out[i].data,&type, packet_len);
    else
      len = get_data(p_out[i].data,&type,
        (packet_len <= out_mask) ? packet_len : out_mask);
    SANITY(packet_len <= out_mask+1);
  }else { 
    len = old_len;
    type = old_type;
    old_len = 0;
  }

  if (! len) {
    /* All the data waiting was control data for local daemon */
    /* We can handle this. */
    return;
  }

  if (! sent_version) { 
    sprintf((char *)p_out[p_out_s].data, "VERSION %lu %lx %s %u",
      (unsigned long)VERSION, term_localaddr, term_localhost, 
      (unsigned int)geteuid());
    old_len = len;
    old_type = type;
    len = strlen((char *)p_out[p_out_s].data) + 1;
    type = 0;
  }

  p_out[p_out_s].timeout = 0;	/* Transmit right away */
  p_out[p_out_s].queue = &curr_client->queue;
  p_out[p_out_s].trans = 0;
  p_out[p_out_s].len = (len > 0) ? len : -len;
  p_out[p_out_s].type = type;
  DEBUG_LINK(stderr, "%s:Added pack %d to out Q %d\n", term_server, p_out_s,
    curr_client->number);
  p_out_s = (p_out_s + 1) % N_PACKETS;

  sent_version = 1;
  curr_client->queue++;
  p_out_num ++;
}


  	/* Here is where I handle client priorities. */
	/* Basically I run a little lottery with each client having a # of */
        /* tickets based on priorities.  I don't use a random # because */
	/* gambling is illegal in most states. :-) */

int do_lottery(void) {
  int count=0, i, j, high = -10000, low=10000, high_min, h;
  int tickets[MAX_CLIENTS], winner;
  static int next=0;

	/* Find the lowest priority, and initialize */

  high_min = packet_len;
  for(i=0;i<MAX_CLIENTS;i++) {
    if (clients[i].in_buff.size) {
      j = clients[i].priority - clients[i].queue;
      if (j > high) {
        high_min = clients[i].in_buff.size;
        high = j;
      }else if (j == high || high_min > clients[i].in_buff.size) {
        high_min = clients[i].in_buff.size;
      } 
      low = (low < j) ? j : low;
    }
    tickets[i] = 0;
  };
  if (high == -10000) return -1;

#if 0
	/* This is more unix like... I'm not really sure this is well suited */
	/* For term, since it might be preferable to have processes suspend */
	/* or nearly so when running trsh or such... */
  if (high > 20) high = 20;
  if (low < -20) low = -20;
  for(h=high, count = 0; h > -20 ;--h)
#else
	/* The less we have queued the fairer I am.  If there is only one slot */
	/* left we should give it to the most deserving... */
  for(h=high, count = 0; count < window_size_max - p_out_num;--h)
#endif
    for(i=0;i<MAX_CLIENTS;i++) 
      if (clients[i].in_buff.size &&
          clients[i].priority - clients[i].queue >= h &&
	  (high_min > packet_len - 40 || clients[i].in_buff.size <=
             high_min + clients[i].priority + 20) ) {
    tickets[i]++;
    count++;
  } 
  if (! count) return -1;

  winner = (next++ % count);

	/* OK, lets search for the winning ticket */

  while (winner >= 0)
    for (i=0;i<MAX_CLIENTS;i++)
      if (tickets[i] > 0) {
    if (! winner--) break;
    tickets[i]--; 
  }

	/* And the winner is client i */

  return i;
}


void do_link_in(void) {
  /* Takes packet of the in packet list and feeds it to clients. */
  static un_char uncomp_buff[2049];
  int l;
  while (p_in[p_in_e].type >= 0) {
    /* feed data out */
    DEBUG_LINK(stderr, "%s: Handling p %d off in Q\n", term_server, p_in_e);
    if (p_in[p_in_e].type == 1) {	/* compressed data */
      extern int stat_uncomp_in, stat_uncomp_out;
      l =uncompress(p_in[p_in_e].data , p_in[p_in_e].len,
		    uncomp_buff);
      stat_uncomp_in += p_in[p_in_e].len;
      stat_uncomp_out += l;
      put_data(uncomp_buff, l);	
    } else if (p_in[p_in_e].type == 0) {	/* uncompressed 8 bit data */
      if (! remote_term_version) {
        memset(term_remotehost,0,sizeof(term_remotehost));
        if(sscanf((char *)p_in[p_in_e].data, "VERSION %lu %lx %s %u",
            &remote_term_version, &term_remoteaddr, term_remotehost, &term_remoteuser) < 1) {
          remote_term_version = 10800;
          put_data(p_in[p_in_e].data, p_in[p_in_e].len);
        }
        if(! remote_term_version)
          remote_term_version = 10800;
	if (! term_remoteaddr || term_remoteaddr == ntohl(inet_addr("127.0.0.1")))
          term_remoteaddr = htonl(inet_addr("127.0.0.254"));
        if (! term_remotehost[0] || !strcmp(term_remotehost,"localhost"))
          strcpy(term_remotehost,"remotehost");
      } else    
        put_data(p_in[p_in_e].data, p_in[p_in_e].len);
    } else if (p_in[p_in_e].type == 5) {        /* new seven bit data */
      l = convert_to_8(tok_byte_width_in,p_in[p_in_e].len, p_in[p_in_e].data,
        uncomp_buff);
      put_data(uncomp_buff, l);
    } else {	/* seven bit data */
      l = s_2_e_buff(p_in[p_in_e].data, uncomp_buff,
		     p_in[p_in_e].len);
      put_data(uncomp_buff, l);
    }

    p_in[p_in_e].type = -1;
    p_in_e = (p_in_e + 1) % N_PACKETS;
    p_in_num --;
  }
}

/*---------------------------------------------------------------------------*/

/* This is where compression will eventually get done */
/* For now , we just get some bytes */
/* we are compressing now. */
int get_data(un_char *b, int *type, int len) {
  extern int tok_byte_width_out;
  int i, j, k;
  /* this is a horrible kludge, but without major reorganization, 
   * this is the simplest way.  we need to know if the current 
   * packet for the current client should be compressed, so we 
   * need to know the client.  we get the first byte (which forces
   * which client we are getting data from) and then peek and 
   * see what that client wants. we then pass along the byte
   * that we read.. (this is the REAL kludge).
   * croutons.
   */
				/* Get a byte, so we we have a client. */
  *type = -1;
  k = get_client_byte();
  if ( k < 0 )			/* no data */
  	return 0;
				/* Now that we have a client, we can */
				/* check to see whether we want to */
				/* compress the data or not. */
				/* The idea is if compression failed, we */
				/* don't compress again for a while. */
  switch (curr_client->compress) {
    case 48:
      curr_client->compress = 32;
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
    case 32:
      if ((j = compress(b, len-1, k)) != 0)  {
        DEBUG_SER(stderr,"%s:compressing packet\n",term_server);
        if (j < 0 ) {
          ++curr_client->compress;
          j = ( -j + tok_byte_width_out - 1) / tok_byte_width_out;
        }else { 
          if (curr_client->compress > 1)
            curr_client->compress -= curr_client->compress / 2;
          j = ( j + tok_byte_width_out - 1) / tok_byte_width_out;
        }
        *type = 1;
        return -j;
      }
      break;
    default:
      ++curr_client->compress;
      break;
  }
				/* If we only have a seven bit output */
				/* line, then we want to pack 8 bytes */
				/* to 7 seven bit bytes. */
  if (seven_bit_out) {
    DEBUG_SER(stderr,"%s:sevenbit packet\n",term_server);
    len = (len * 7) / 8;
    b[0] = 0;
    if (remote_term_version < 20251) {
      j = e_2_s_put(b, (unsigned) k, 0);
      while ((j>>3) < len) {
        if ((i = get_client_byte()) < 0) break;
        j = e_2_s_put(b, (unsigned) i, j);
      }
      *type = 2;
      return (j>>3) + 1;
    }else {
      for(j=0, i=k;i >= 0;i=get_client_byte()) {
        b[j++]=i;
        if (j >= len) break;
      }
      if ((j=convert_from_8(tok_byte_width_out,j,b,b)) < 0) {
        *type = 0;
        return -j;
      }else {
        *type = 5;
        return j;
      }
    }
  } else {			/* Else, just dump the data, we can */
				/* handle it. */
    b[0] = k; /* put the first byte in the array */
    j = 1;
 
    *type = 0; 
    while (j < len) {
      i = get_client_byte();
      if (i < 0) break;
      b[j ++ ]  = i;
    }
  }
  return j;
}
/*---------------------------------------------------------------------------*/

void ADD_BUFF(struct Client *clt, un_char c)
{
  add_to_buffer( &((clt)->out_buff), c);
}

void ADD_IN_BUFF(struct Client *clt, un_char c)
{
  add_to_buffer( &((clt)->in_buff), c);
}

void clear_buffers(struct Client *cl) {
  cl->in_buff.size = cl->in_buff.start = cl->in_buff.end = 0;
  cl->out_buff.size = cl->out_buff.start = cl->out_buff.end = 0;

  add_to_buffer (& (cl->in_buff), 0);
  get_from_buffer (& (cl->in_buff ) );

  add_to_buffer (& (cl->out_buff), 0);
  get_from_buffer (& (cl->out_buff ) );
}

void add_ret_buff(struct Client *cl, int which, int byte)  {
  if (!which)
    ADD_IN_BUFF(cl, (un_char) byte);
  else
    put_client_data(cl, byte);
}

void add_ret_buff_str(struct Client *cl, int which, char *s) {
  int i;
  for (i = 0; s[i];++i)
    add_ret_buff(cl, which, (int) s[i]);
}

/* The following return an error.  The return should only be fatal
   if normally the command would put the socket in DUMB mode.
 */

void ret_hfail(struct Client *cl, int which, int fatal, char *p) {
  int olderrno;
  extern int h_errno;

  olderrno = errno;
  add_ret_buff(cl, which, SWITCH);
  add_ret_buff(cl, which, SWITCH-3);
  add_ret_buff(cl, which, I_FAIL);
  if (p) {
    add_ret_buff_str(cl, which, p);
    add_ret_buff_str(cl, which, ": ");
  }
  add_ret_buff_str(cl, which, term_strherror(h_errno));
  add_ret_buff(cl, which, 0);
  if(!fatal){
    errno = olderrno;
    return;
  }

  add_ret_buff(cl, which, SWITCH);
  add_ret_buff(cl, which, SWITCH-2);
  add_ret_buff(cl, which, C_CLOSE);
  add_ret_buff(cl, which, 0);
  errno = olderrno;
}


/* The following return an error.  The return should only be fatal
   if normally the command would put the socket in DUMB mode.
 */

void ret_fail(struct Client *cl, int which, int fatal, char *p) {
  int olderrno;

  olderrno = errno;
  add_ret_buff(cl, which, SWITCH);
  add_ret_buff(cl, which, SWITCH-3);
  add_ret_buff(cl, which, I_FAIL);
  if (p) {
    add_ret_buff_str(cl, which, p);
    add_ret_buff_str(cl, which, ": ");
  }
  add_ret_buff_str(cl, which, x__strerror(olderrno));
  add_ret_buff(cl, which, 0);
  if(!fatal){
    errno = olderrno;
    return;
  }

  add_ret_buff(cl, which, SWITCH);
  add_ret_buff(cl, which, SWITCH-2);
  add_ret_buff(cl, which, C_CLOSE);
  add_ret_buff(cl, which, 0);
  errno = olderrno;
}

void ret_ok(struct Client *cl, int which) {
  add_ret_buff(cl, which, SWITCH);
  add_ret_buff(cl, which, SWITCH-3);
  add_ret_buff(cl, which, I_OK);
  add_ret_buff(cl, which, 0);
}

void do_control(int local , struct Client *cl, un_char *c) {
#if defined(SYSV) && !defined(DYNIXPTX)
  struct utsname unam;
#endif
  DEBUG_FP(stderr, "%s:do_control %s on client %d:%s:\n",
	 term_server, (local) ? "local" : "remote", cl->number, c);

  switch(c[0]) {
  case C_NAME:
    DEBUG_FP(stderr, "%s:c_name\n", term_server);
    sprintf(cl->name, "%s", (char *)(c+1));
    break;
  case C_PUTENV:
    DEBUG_FP(stderr, "%s:c_putenv\n", term_server);
    if (strchr((char *)(c+1),'=')) term_putenv((char *)(c+1));
    break;
  case C_CLOSE:		/* Close the file descriptor and the client */
    DEBUG_FP(stderr, "%s:c_close\n", term_server);
    if (!isdigit((char) c[1])) {
      if (cl->state == 1)
        cl->state = 3;		/* Go to flush buffers and close. */
    }else {
      int k;
      k = atoi((char *)c + 1);
      if (k <  0 || k >= MAX_CLIENTS) break;
      if (clients[k].state == 1)
        clients[k].state = 3;
    }
    break;
  case C_CLCLOSE:	/* Just close the file descriptor */
    DEBUG_FP(stderr, "%s:c_clclose %s\n", term_server,(char *)c+1);
    if (!isdigit((char) c[1])) {
      if (cl->state == 1)
        cl->state = 4;  /* This closes the file descriptor, */
			/* but leaves the client active... */
    }else {
      int k;
      k = atoi((char *)c + 1);
      if (k <  0 || k >= MAX_CLIENTS) {
        break;
      }
      if (clients[k].state == 1)
        clients[k].state = 4;
    }
    break;
  case C_DUMB:
    DEBUG_FP(stderr, "%s:c_dumb\n", term_server);
    cl->type &= ~T_SMART;
    cl->dump_count = 0;
    break;
  case C_DUMP:
  case C_DUMP_OLD:
    DEBUG_FP(stderr, "%s: c_dump %d\n", term_server, atoi((char *) (c+1)));
    if (c[0] == C_DUMP_OLD) ret_ok(cl, local);
    cl->type &= ~T_SMART;
    cl->dump_count = atol((char *)c+1)+1;
    break;
  case C_CHMOD: /* There is no return value, so we can speed things up */
    DEBUG_FP(stderr,"%s: c_chmod %s\n",term_server, (char *)c+1);
    {
      int k;
      long atime, mtime;
      char *a;

      atime = 0;
      mtime = 0;
      a = strchr((char *)c+1,'\n');
      if (a != NULL && sscanf((char *)c+1,"%o %ld %ld", &k, &atime,
           &mtime) < 1) {
        DEBUG_FP(stderr,"%s: c_chmod invalid arguments\n",term_server);
        break;
      }
      if (chmod(++a,k) < 0) {
        DEBUG_FP(stderr, "%s: chmod %s failed: %s\n",term_server, a,
          x__strerror(errno));
        break;
      }

      { 
        long now;
        struct utimbuf utb;

        now = (long)time(NULL);
        utb.actime = (time_t) (atime + now);
        utb.modtime = (time_t) (mtime + now);
        if (utime(a,&utb) < 0) { 
          DEBUG_FP(stderr, "%s: utimes failed: %s\n",term_server,
            x__strerror(errno));
          break;
        }
      }
    }
    break;
  case C_SETPEERNAME:
    DEBUG_FP(stderr, "%s: c_setpeername %s\n", term_server, (char *)c+1);
    {
      memcpy((struct sockaddr *)&cl->peername, 
        str_to_sockaddr((char *)c+1,inet_addr("127.0.0.1")),
        sizeof(struct sockaddr));
    }
    break;
  case C_OPEN:
    DEBUG_FP(stderr,"%s:got c_open %s\n", term_server, (char *)c+1);
  case C_UPLOAD:
    if (c[0] == C_UPLOAD)
      DEBUG_FP(stderr,"%s:got c_upload %s\n", term_server, (char *)c+1);
  case C_DOWNLOAD:
    if (c[0] == C_DOWNLOAD)
      DEBUG_FP(stderr,"%s:got c_download %s\n", term_server, (char *)c+1);
    {
      long l = 0;
      int mode = 0666;
      char *a;

      a = strchr((char *)c+1,'\n');
      if (a != NULL) {
        if (sscanf((char *)c+1,"%ld %o", &l, &mode) < 1) {
          ret_fail(cl,local,FATAL, "open() invalid options");
          DEBUG_FP(stderr,"%s: c_open invalid options\n", term_server);
          break;
        }
      }else a = (char *)c;

      if (cl->fd >=0) x__close(cl->fd);

      if (c[0] == C_OPEN) {
        chmod(++a,0600);
        cl->fd = open(a, O_RDWR | O_CREAT, mode);
        if (cl->fd > 0) chmod(a,mode);
      }else if (c[0] == C_UPLOAD) {
        unlink(++a);
        cl->fd = open(a, O_WRONLY | O_CREAT | O_TRUNC, mode);
        if (cl->fd > 0) chmod(a,mode); /* This probably isn't necessary. */
      }else {
        cl->fd = open(++a, O_RDONLY);
      }

      if (cl->fd < 0) {
        ret_fail(cl,local,FATAL, "open() failed");
        DEBUG_FP(stderr, "%s: open failed: %s\n",term_server,x__strerror(errno));
        break;
      }
      set_nonblock(cl->fd);
      if (l != 0 && lseek(cl->fd, (off_t)l, 0) == -1) {
        ret_fail(cl,local, FATAL, "lseek failed");
        DEBUG_FP(stderr, "%s: lseek failed: %s\n",term_server,
	  x__strerror(errno));
        x__close(cl->fd);
        cl->fd = -1;
        break;
      }

    }
    cl->state = 1;
    if (c[0] != C_DOWNLOAD)  {
      cl->type= T_WRFILE;
      cl->cl_type = CL_FILE;
      ret_ok(cl,local);
      break;
    }
	/* We allow C_DOWNLOAD to fall through to C_STAT */

  case C_STAT:
    if (c[0] == C_STAT)
      DEBUG_FP(stderr, "%s: c_stat %s\n", term_server, (char *)(c+1));
    {
      struct stat st;
      int type, permissions;
      unsigned short cksum=0;
      long now, cklen = -1;
      char *a, *b, buff[128];

      a = strchr((char *)c+1,'\n');
      if (a == NULL) 
	a = (char *)c;
      else if ((b = strchr(a+1,'\n'))) {
        sscanf(a+1, "%ld", &cklen);
        a = b;
      }
      now = (long)time(NULL);
      if (stat(++a, &st)< 0) {
	ret_fail(cl, local, ! FATAL, "stat() failed");
	DEBUG_FP(stderr, "%s: stat() failed\n", term_server);
        if(c[0] == C_DOWNLOAD) {
          x__close(cl->fd);
          cl->fd = -1;
        }
	break;
      }
      add_ret_buff(cl, local,SWITCH);
      add_ret_buff(cl, local,SWITCH-3);
      add_ret_buff(cl, local,I_OK);
				/* Get type */
      if (S_ISREG(st.st_mode)) type = 0;
      else if (S_ISDIR(st.st_mode)) type = 1;
      else type = 2;
				/* Now get permissions. */
      permissions = (int) (st.st_mode & 511);
      if (getuid() == st.st_uid)
	permissions >>= 6;
      else if (getgid() == st.st_gid)
	permissions >>= 3;
      permissions &= 07;

                                /* Get checksum if desired. */
      if (cklen >= 0) cksum = file_crc(a, cklen ? cklen : st.st_size);
	
      sprintf(buff, "%ld %d %d %ld %ld %o %u",
	(long)st.st_size, type, permissions,
	(long)st.st_atime - now, (long)st.st_mtime - now,
	(int)st.st_mode, cksum);
      add_ret_buff_str(cl, local, buff);
      add_ret_buff(cl, local,0);

      if (c[0] == C_DOWNLOAD)  {
        cl->type= T_RDFILE;
        cl->cl_type = CL_FILE;
        cl->dump_count = (long)st.st_size + cl->in_buff.size;
      }
      break;
    }
    
  case C_UNLINK:
    DEBUG_FP(stderr, "%s:c_unlink\n", term_server);
    if (unlink(c+1)) {
      ret_fail(cl, local, ! FATAL, "unlink failed");
      DEBUG_FP(stderr, "%s: unlink failed: %s\n", term_server,
	x__strerror(errno));
    } else
      ret_ok(cl, local);
    break;

  case C_PTYEXEC:
  case C_EXEC:
    if (cl->fd>=0) x__close(cl->fd);
    DEBUG_FP(stderr, "%s: %s on client %d (%s) \n", term_server,
	     c[0]==C_PTYEXEC?"C_PTYEXEC":"C_EXEC", cl->number, c+1); 
    if (rshtype > 0) cl->fd = -5;
    else {
#ifndef NO_PTYEXEC
      if (c[0] == C_PTYEXEC)
        cl->fd = open_pty((char *)(c + 1));
      else 
#endif
        cl->fd = open_socket((char *)(c + 1));
    }
    if (cl->fd < 0) {
      char *p;
      DEBUG_FP(stderr, "%s: failed to open client: error: %d\n",
	       term_server, cl->fd); 

      switch (cl->fd) {
      case -1: p = "Couldn't get pty"; break;
      case -2: p = "fchmod() failed"; break;
      case -3: p = "fork() failed"; break;
      case -4: p = "S_Pipe() failed"; break;
      case -5: p = "Permission denied"; break;
      default: p = "Unknown failure"; break;
      }
      errno = 0;
      ret_fail(cl, local, FATAL, p);
      break;
    }
    set_nonblock(cl->fd);
    DEBUG_FP(stderr, "%s: opened client\n", term_server);
    cl->type = T_WRFILE | T_RDFILE;
    cl->cl_type = CL_CHILD;
    cl->state = 1;
    cl->pid = pty_pid;
    DEBUG_FP(stderr, "%s: got pid %d\n", term_server, pty_pid);
    ret_ok(cl, local);
    break;

  case C_BIND:
    DEBUG_FP(stderr, "%s: c_bind %s\n", term_server, (char *)(c+1));
    {
      unsigned int port=0;
      int s,backlog=5;

      sscanf((char *)c+1, "%u %d", &port, &backlog);
      DEBUG_FP(stderr, "%s: c_bind %u %d\n", term_server, port, backlog);

      if ((s = bind_tcp_listen(port,backlog)) < 0) {
	ret_fail(cl, local , FATAL, "bind_tcp_listen() failed");
	DEBUG_FP(stderr, "%s:bind_tcp_listen failed (%d):%s\n", term_server,
           port,x__strerror(errno));
      }
      if (! port) {
        struct sockaddr_in addr_in;
        int k=sizeof(addr_in);

        if (x__getsockname(s, (struct sockaddr *)&addr_in, &k) < 0) {
          DEBUG_FP(stderr, "%s:getsockname failed: %s\n", term_server,
	    x__strerror(errno));
        }else {
  	  DEBUG_FP(stderr, "%s:sockname returned %s\n", term_server,
            sockaddr_to_str((struct sockaddr *)&addr_in, 0));
          port = ntohs(addr_in.sin_port);
        }
      }
      if (cl->fd>=0) x__close(cl->fd);
      cl->fd = s;
      set_nonblock(cl->fd);
      cl->cl_type = CL_BOUND;
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
      add_ret_buff(cl, local,SWITCH);
      add_ret_buff(cl, local,SWITCH-3);
      add_ret_buff(cl, local,I_OK);
      {
        char cport[10];
        sprintf(cport,"%u",port);
        add_ret_buff_str(cl,local,cport);
      }
      add_ret_buff(cl, local,0);
    }
    break;
  case C_ACCEPT:
    {
      struct sockaddr_in addr_in;

      int ain = sizeof(addr_in);
      int s;

      cl->type |= T_RDFILE;

      DEBUG_FP(stderr, "%s: c_accept %s\n", term_server, (char *)(c+1));
				/* Get the socket to try and accept() */
				/* on. */
      s = atoi((char *) (c+1));
				/* Error checking..  */
      if (s <  0 || s >= MAX_CLIENTS || clients[s].fd < 0 ||
	   clients[s].cl_type != CL_BOUND) {
	errno = 0;
	ret_fail(cl, local, FATAL, "Client out of range");
        DEBUG_FP(stderr,"Client out of range\n");
	break;
      }else /* The bound port can accept more connections... */
        clients[s].type |= T_RDFILE | T_WRFILE;  

				/* try the actual accept(). */
      s = x__accept(clients[s].fd , (struct sockaddr *) &addr_in, &ain);
      if (s < 0) {
	ret_fail(cl, local, FATAL, "Accept failed");
        DEBUG_FP(stderr, "%s: accept failed: %s\n", term_server, x__strerror(errno));
	break;
      }

      memcpy(&cl->peername,&addr_in,sizeof(cl->peername));

      add_ret_buff(cl, local,SWITCH);
      add_ret_buff(cl, local,SWITCH-3);
      add_ret_buff(cl, local,I_OK);
      add_ret_buff_str(cl,local,sockaddr_to_str((struct sockaddr *)&addr_in,1));
      add_ret_buff(cl, local,0);

      DEBUG_FP(stderr,"%s:got c_accept\n", term_server);
      if (cl->fd>=0) x__close(cl->fd);
      cl->fd = s;
      set_nonblock(cl->fd);
      cl->cl_type = CL_SOCKET;
      DEBUG_FP(stderr, "%s:name is %s\n", term_server, (char *)(c+1));
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
    }
    break;
#if !defined(X_STREAMS_PIPE) && !defined(SVR3)
  case C_X_SERVER:
#endif
  case C_SOCKET:
    DEBUG_FP(stderr, "%s: c_socket %s\n", term_server, (char *)(c+1));
    {

      int s;

      if (! strcmp((char *)(c+1),REMOTE_X)) {
	int screen=0;
	char *display, path[270];

/* This allows us to connect to the DISPLAY specified by DISPLAY */
/* at the time the term connection was formed. */

        if ((display = getenv("DISPLAY"))) {
	  int i;
          for (i = 0; display[i] && display[i] != ':'; i++);
          if (display[i] == ':')
 	    screen = atoi(&display[i+1]);
        }
	sprintf(path,"%s/%s%d",LOCAL_X_DIR,LOCAL_X_NAME,screen);
	s = open_unix(path);
      }else {
	s = open_unix((char *)(c+1));
      }
      if (s >= 0) {
        DEBUG_FP(stderr,"%s:got c_socket\n", term_server);
        if (cl->fd>=0) x__close(cl->fd);
        cl->fd = s;
        set_nonblock(cl->fd);
        cl->cl_type = CL_SOCKET;
        DEBUG_FP(stderr, "%s:name is %s\n", term_server, (char *)(c+1));
        cl->type = T_RDFILE | T_WRFILE;
        cl->state = 1;
        ret_ok(cl, local);
	break;
      }
#if defined(X_STREAMS_PIPE) || !defined(X_TCP_SOCKET)
      else{
	ret_fail(cl, local, FATAL, "open_unix() failed");
	DEBUG_FP(stderr, "%s:open_unix failed: %s\n", term_server,
	  x__strerror(errno));
        break;
      }
#endif
    }
#ifndef X_STREAMS_PIPE
#ifdef SVR3
  case C_X_SERVER:
#endif
    {
      int s;
      struct hostent *hostaddr = NULL;
      char *display;
      char host[259];
      int  screen = 0;
      struct sockaddr_in sock_in;
      int i = 0;

      /*
       * This code will use the environment variable DISPLAY that was in
       * effect at the time "term" was started to direct all X connections
       * to.   It only works for INET domain sockets
       */
      if ( (display = getenv("DISPLAY"))) {
        for (i = 0; display[i] && display[i] != ':'; i++) 
 	  host[i] = display[i];
        if (display[i] == ':')
          screen = atoi(&display[i+1]);
      }
      host[i] = 0;

      if (*host) {
        struct in_addr *addr_in;

        hostaddr = host_lookup(host,0,AF_INET,0,NULL);
        if (! hostaddr) {
  	  ret_fail(cl, local, FATAL, "Failed to find DISPLAY host");
  	  DEBUG_FP(stderr, "Failed to find DISPLAY host %s", host);
	  break;
        }
	addr_in = (struct in_addr *) hostaddr->h_addr;
  	sock_in.sin_addr.s_addr = addr_in->s_addr;
      }else {
	sock_in.sin_addr.s_addr = htonl(term_localaddr);
      }

      if ((s = x__socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	ret_fail(cl, local, FATAL, "socket allocation failed");
	x__perror("Failed to allocate socket for X display");
	break;
      }
      
      sock_in.sin_family = AF_INET;
      sock_in.sin_port = htons(6000+screen);

      DEBUG_FP(stderr, "Attempting connection to host %lu.%lu.%lu.%lu\n",
	      sock_in.sin_addr.s_addr & 0xff,
	      (sock_in.sin_addr.s_addr >> 8) & 0xff,
	      (sock_in.sin_addr.s_addr >> 16) & 0xff,
	      (sock_in.sin_addr.s_addr >> 24) & 0xff);
      if (x__connect(s, (struct sockaddr *)&sock_in, sizeof(sock_in))) {
	ret_fail(cl, local, FATAL, "Failed to connect to display");
	DEBUG_FP(stderr,"Connection attempt to X server failed");
	break;
      }
      DEBUG_FP(stderr, "Connection was successful\n");

      DEBUG_FP(stderr, "%s:got c_x_socket\n", term_server);
      if (cl->fd>=0) x__close(cl->fd);
      cl->fd = s;
      set_nonblock(cl->fd);
      cl->cl_type = CL_SOCKET;
      DEBUG_FP(stderr, "%s:name is %s\n", term_server, (char *)(c+1));
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
      ret_ok(cl, local);
    }
    break;
#else /* X_STREAMS_PIPE */
  case C_X_SERVER:
    DEBUG_FP(stderr, "%s: c_x_server %s\n", term_server, (char *)(c+1));
    {

      int s;
      int display_num;
      char *display, *screen;

      /* check DISPLAY variable for screen number */

      display = getenv("DISPLAY");
      if (display == 0)
	display = ":0";

      screen = strchr(display, ':');
      if (screen != 0)
	display_num = atoi((char *)(screen + 1));
      else
	display_num = 0;

      s = MakeStreamPipeConnection(display_num);
      if (s < 0) {
	ret_fail(cl, local, FATAL, "X connection failed");
	DEBUG_FP(stderr, "%s: x connection failed: %s\n", term_server,
	  x__strerror(errno));
	break;
      }

      DEBUG_FP(stderr,"%s:got c_x_server\n", term_server);
      if (cl->fd>=0) x__close(cl->fd);
      cl->fd = s;
      set_nonblock(cl->fd);
      cl->cl_type = CL_SPIPE;
      DEBUG_FP(stderr, "%s:name is %s\n", term_server, (char *)(c+1));
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
      ret_ok(cl, local);
    }
    break;
#endif
  case C_PORT:
    DEBUG_FP(stderr, "%s: c_port %s\n", term_server, (char *)(c+1));
    {
      struct sockaddr *addr;
      char *hostname = NULL;
      int s, old=0;
      unsigned int port;
      char *colon;
      
      colon = strchr((char *)c+1,':');
      sscanf((colon) ? (colon+1) : (char *)c+1,"%u %d",&port,&old);
      hostname=(colon) ? ((char *)c+1) : NULL;

      if (old && cl->fd >= 0) {
        s = cl->fd;
      }else if ((s = x__socket(AF_INET, SOCK_STREAM, 0 )) < 0) {
        ret_fail(cl, local, FATAL, "Socket() failed");
        DEBUG_FP(stderr, "%s: Socket() failed: %s\n", term_server, 
          x__strerror(errno));
        break;
      }

      if (!(addr=make_sockaddr(port,hostname,inet_addr("127.0.0.1")))) {
        errno = 0;
        ret_fail(cl, local, FATAL, "Can't get local address");
        DEBUG_FP(stderr,"%s: Can't get local address\n", term_server);
        x__close(s);
        break;
      }
 
#ifndef USE_CONNBLOCK
      /* Set nonblock mode before connecting so connect() returns right away */
      set_nonblock(s);
#endif

      DEBUG_FP(stderr, "%s: connecting to to %s\n", term_server,
	  sockaddr_to_str(addr,INADDR_ANY));
      if (x__connect(s,addr,sizeof(struct sockaddr))<0) {
#ifndef USE_CONNBLOCK
        if (errno != EINPROGRESS && errno != ERR_BLOCK) {
#endif
          ret_fail(cl,local, FATAL, "connect() failed");
          DEBUG_FP(stderr, "%s: connect() failed: %s\n", term_server,
            x__strerror(errno));
          x__close(s);
          break;
#ifndef USE_CONNBLOCK
        }
#endif
      }
      
      DEBUG_FP(stderr,"%s:got c_port\n", term_server);
      if (cl->fd>=0 && ! old) x__close(cl->fd);
      cl->fd = s;
      set_nonblock(cl->fd);
      cl->cl_type = CL_SOCKET;
      cl->type = T_RDFILE | T_WRFILE;
#ifndef USE_CONNBLOCK
      cl->state = 5;
      cl->timeout = current_time + term_inc*30;
		/* 30-second connect timeout */
#else
      cl->state = 1;
      ret_ok(cl, local);
#endif
    }
    break;
  case C_LISTEN:
    DEBUG_FP(stderr, "%s: c_listen %s\n", term_server, (char *)(c+1));
    {
      int k, queue;

      if(sscanf((char *)c+1,"%d %d",&k,&queue) < 2) {
	errno = 0;
#if 0
        ret_fail(cl, local, ! FATAL, "Insufficient argc");
#endif
        DEBUG_UDP(stderr, "%s:insufficient argc\n", term_server);
        break;
      }
      if (k <  0 || k >= MAX_CLIENTS || clients[k].fd < 0 ||
	   clients[k].cl_type != CL_BOUND) {
	errno = 0;
#if 0
	ret_fail(cl, local, FATAL, "Client out of range");
#endif
        DEBUG_FP(stderr,"%s: Client out of range\n",term_server);
	break;
      }
      if (x__listen(clients[k].fd, queue) < 0) { /* If we can't listen... */
#if 0
        ret_fail(cl, local, ! FATAL, "listen() failed");
#endif
        DEBUG_FP(stderr,"%s: listen() failed: %s\n", term_server, 
          x__strerror(errno));
        x__close(clients[k].fd);
        clients[k].fd = -1;
        break;
      }
#if 0
      ret_ok(cl, local);
#endif
    }
    break;
  case C_PRIORITY_OLD:
  case C_PRIORITY:
    DEBUG_FP(stderr, "%s: c_priority %d\n", term_server,
      atoi((char *) (c+1)));
    cl->priority = atoi((char *) (c+1));
    if (c[0] == C_PRIORITY_OLD) ret_ok(cl, local);
    break;
  case C_COMPRESS:
    DEBUG_FP(stderr, "%s: c_compress %c\n", term_server, c[1]);

    switch(c[1]) {
    case 'y':	/* yes */
    case 'c':	/* compress */
    case 'Y':	/* caps too */
    case 'C':   
    case 1: 	/* true */
    case '1':	/* in ascii */
      cl->compress = 1;
      ret_ok(cl, local);
      break;
    case 'n':	/* no */
    case 'u':	/* uncompress */
    case 'r':	/* raw */
    case 'N':	/* caps too */
    case 'U':	
    case 'R':
    case 0:  	/* false */
    case '0':  	/* in ascii */
      cl->compress = 0;
      ret_ok(cl, local);
      break;
    default:
      errno = 0;
      ret_fail(cl, local, ! FATAL, "Invalid argument");
      DEBUG_FP(stderr,"%s: invalid argument\n", term_server);
      break;
    }
    break;

  case C_STATS: 
    {
      int opt;
      un_char ret[2000];
/*      extern do_stats(un_char *, int, struct Client *); */

      DEBUG_FP(stderr, "%s:c_stats\n", term_server);
      opt = atoi((char *) (c+1));
      add_ret_buff(cl, local, SWITCH);
      add_ret_buff(cl, local, SWITCH-3);
      add_ret_buff(cl, local, I_OK);

      do_stats(ret, opt, cl);

      add_ret_buff_str(cl, local, (char *)ret);
      add_ret_buff(cl, local, 0);
      break;
    }
  case C_SEEK:
  {
    long l;
    DEBUG_FP(stderr, "%s:c_seek %s\n", term_server, c+1);
    if(sscanf((char *)(c+1),"%ld",&l)<1) l=0;
    if (lseek(cl->fd, (off_t)l, 0) < (off_t)0) {
      ret_fail(cl, local, ! FATAL, "lseek() failed");
      DEBUG_FP(stderr, "%s:c_seek failed\n",term_server);
      break;
    }
    ret_ok(cl, local);
    break;
  }
  case C_RESIZE_OLD:
  case C_RESIZE:
    DEBUG_FP(stderr, "%s:c_resize %s\n", term_server, c+1);
    {
#ifdef USE_SIGWINCH
      void do_resize(int number, int rows, int cols, int ypixels, int xpixels);
      int number;
      int rows, cols, ypixels, xpixels;
      sscanf((char *) (c+1), "%d %d %d %d %d",
				&number, &rows, &cols, &ypixels, &xpixels);
      do_resize(number, rows, cols, ypixels, xpixels);
#endif	/* USE_SIGWINCH */
    }
    if (c[0] == C_RESIZE_OLD) ret_ok(cl, local);
    break;

  case C_BINDN: /* ftp special -ot */
    DEBUG_FP(stderr, "%s: c_bindn %s\n", term_server,c+1);
    {
      int s, k;
      struct sockaddr_in addr_in;
      
      s = bind_tcp_listen((unsigned int) atoi((char *)(c+1)),5);
      if (s < 0) {
	errno = 0;
        ret_fail(cl, local , FATAL, "bind_tcp_listen() failed");
        DEBUG_FP(stderr, "%s:bind_tcp_listen failed\n", term_server);
        break;
      }
      k=sizeof(addr_in);
      if (x__getsockname(s, (struct sockaddr *)&addr_in, &k) < 0) {
        ret_fail(cl, local, FATAL, "getsockname() failed");
        DEBUG_FP(stderr, "%s:getsockname failed: %s\n", term_server,
	  x__strerror(errno));
        break;
      }
      
      DEBUG_FP(stderr, "%s:sockname returned %lx %x\n", term_server,
             (long)ntohl(addr_in.sin_addr.s_addr), ntohs(addr_in.sin_port));
      ret_ok(cl, local);

      add_ret_buff_str(cl,local,sockaddr_to_str((struct sockaddr *)&addr_in,1));
      add_ret_buff(cl, local,0);

      if (cl->fd>=0) x__close(cl->fd);
      cl->fd = s;
      set_nonblock(cl->fd);
      cl->cl_type = CL_BOUND;
      cl->type = T_RDFILE | T_WRFILE;
      cl->state = 1;
    }
    break;

  case C_GETSOCKNAME: /* Return the local sockname -warlord */
    DEBUG_FP(stderr, "%s: c_getsockname %s\n", term_server,(c+1));
    {
      int s, k, kt;
      struct sockaddr addr;
     
      if(! *(c+1)){
        s = cl->fd;
        kt= cl->cl_type;
      } else {
        k = atoi((char *)c+1);
        if (k <  0 || k >= MAX_CLIENTS) {
	  errno = 0;
     	  ret_fail(cl, local, ! FATAL, "Client out of range");
          DEBUG_FP(stderr, "%s: client out of range\n", term_server);
	  break;
        };
        s=clients[k].fd;
        kt=clients[k].cl_type;
      };

      if ( s < 0 ) {
	errno = 0;
        ret_fail(cl, local, ! FATAL, "Client closed");
        DEBUG_FP(stderr, "%s:client closed\n", term_server);
        break;
      } 
				/* Error checking..  */
      if (kt != CL_BOUND && kt != CL_SOCKET) {
	errno = 0;
        ret_fail(cl, local, ! FATAL, "Invalid client type");
        DEBUG_FP(stderr, "%s:invalid client type\n", term_server);
        break;
      }
      
      k=sizeof(addr);
      if (x__getsockname(s, &addr, &k) < 0) {
        ret_fail(cl, local, ! FATAL, "getsockname() failed");
        DEBUG_FP(stderr, "%s:getsockname failed: %s\n", term_server,
	  x__strerror(errno));
        break;
      }
      
      /* Convert this to a string */
      
      /* Set up the return buffer */
      add_ret_buff(cl, local, SWITCH);
      add_ret_buff(cl, local, SWITCH-3);
      add_ret_buff(cl, local, I_OK);
      add_ret_buff_str(cl, local, sockaddr_to_str(&addr,1));
      add_ret_buff(cl, local,0);
      
      DEBUG_FP(stderr, "%s:sockname returned\n", term_server);
    }
    break;

  case C_GETPEERNAME: /* Return the peername -warlord */
    DEBUG_FP(stderr, "%s: c_getpeername %s\n", term_server,(c+1));
    {
      int s, k, kt;
      struct sockaddr addr;
      struct sockaddr_in *addr_in; 

      if(! *(c+1)){
        s = cl->fd;
        kt = cl->cl_type;
        addr_in = &cl->peername;
      } else {
        k = atoi((char *)c+1);
        if (k <  0 || k >= MAX_CLIENTS) {
	  errno = 0;
     	  ret_fail(cl, local, ! FATAL, "Client out of range");
          DEBUG_FP(stderr, "%s: client out of range\n", term_server);
	  break;
        };
        s=clients[k].fd;
        kt=clients[k].cl_type;
        addr_in = &clients[k].peername;
      }

      if ( s < 0 ) {
        errno = 0;
        ret_fail(cl, local, ! FATAL, "Client closed");
        DEBUG_FP(stderr, "%s:client closed\n", term_server);
        break;
      } 
      
      if (kt != CL_SOCKET) {
	errno = 0;
        ret_fail(cl, local, ! FATAL, "Invalid client type");
        DEBUG_FP(stderr, "%s:invalid client type\n", term_server);
        break;
      }
      
      k=sizeof(addr);
      if (~addr_in->sin_family) {
        if (k > sizeof(struct sockaddr_in)) k = sizeof(struct sockaddr_in);
        memcpy((struct sockaddr_in *)&addr, addr_in, k);
      }else if (x__getpeername(s, &addr, &k) < 0) {
        ret_fail(cl, local, ! FATAL, "getpeername() failed");
        DEBUG_FP(stderr, "%s: getpeername failed: %s\n", term_server,
	  x__strerror(errno));
        break;
      }
      
      /* Set up the return buffer */
      add_ret_buff(cl, local, SWITCH);
      add_ret_buff(cl, local, SWITCH-3);
      add_ret_buff(cl, local, I_OK);
      add_ret_buff_str(cl, local, sockaddr_to_str(&addr,1));
      add_ret_buff(cl, local,0);
      
      DEBUG_FP(stderr, "%s:peername returned\n", term_server);
    }
    break;

  case C_GETHOSTNAME:   /* Canonical hostname */
    DEBUG_FP(stderr, "%s: c_gethostname %s\n", term_server,
             ((*(c+1)) ? (char *) (c+1) : "<none>"));
    {
      struct hostent *hp=NULL;
      char hostname[259];
     
      if (*(c+1)) {
        strcpy(hostname, (char *) (c+1));
      } else {
#if defined(SYSV) && !defined(DYNIXPTX)
        uname(&unam);
        strcpy(hostname, unam.nodename);
#else
        x__gethostname(hostname, sizeof(hostname));
#endif /* SYSV */
      }
     
      if (isdigit(*hostname)) {
        unsigned long k;
        k=inet_addr(hostname); 
        hp=host_lookup((char *)&k, sizeof(k), AF_INET, 0, NULL);
      }
      if (!hp) { 
        hp=host_lookup(hostname,0,AF_INET,0,NULL);
      }
      if (!hp) {
	extern int h_errno;
        ret_hfail(cl, local, ! FATAL, "gethostbyname() failed");
        DEBUG_FP(stderr, "Term: gethostbyname() failed: %s\n",
          term_strherror(h_errno));
        break;
      }
      
      add_ret_buff(cl, local, SWITCH);
      add_ret_buff(cl, local, SWITCH-3);
      add_ret_buff(cl, local, I_OK);
      add_ret_buff_str(cl, local, hostent_to_str(hp));
      add_ret_buff(cl, local,0);
    }
    break;
    
  case C_BINDS: /* One time tcp term socket */
    DEBUG_FP(stderr, "%s: c_binds\n", term_server);
    {
      int j,k;
      struct sockaddr_in addr_in;
      char port[10];

      for (j=0;j<MAX_CLIENTS;j++) 
        if(onetime_term[j].socket < 0) break;

      if (j == MAX_CLIENTS) {
        errno = 0;
        ret_fail(cl, local, FATAL, "Too many clients.");
	DEBUG_FP(stderr, "%s:c_binds failed, too many clients",term_server);
        break;
      } 
 
      onetime_term[j].socket = bind_tcp_listen(0,5);
      onetime_term[j].errors = 0;
      onetime_term[j].timeout = current_time + term_inc * 30;
	 /* allow 30 seconds for connection */

      if (onetime_term[j].socket < 0) {
	errno = 0;
        ret_fail(cl, local , FATAL, "bind_tcp_listen() failed");
        DEBUG_FP(stderr, "%s:bind_tcp_listen failed\n", term_server);
        break;
      }

      k=sizeof(addr_in);
      if (x__getsockname(onetime_term[j].socket, (struct sockaddr *)&addr_in, &k) < 0) {
        ret_fail(cl, local, FATAL, "getsockname() failed");
        DEBUG_FP(stderr, "%s: getsockname failed: %s\n", term_server,
	  x__strerror(errno));
        x__close(onetime_term[j].socket);
        onetime_term[j].socket = -1;
        break;
      }
      
      DEBUG_FP(stderr, "%s:sockname returned %lx %u\n", term_server,
             (long)ntohl(addr_in.sin_addr.s_addr), ntohs(addr_in.sin_port));
      ret_ok(cl, local);
      (void) sprintf(port, "%d", ntohs(addr_in.sin_port));

      add_ret_buff(cl, local,SWITCH);
      add_ret_buff(cl, local,SWITCH-3);
      add_ret_buff(cl, local,I_OK);
      
      add_ret_buff_str(cl, local, port);
      add_ret_buff(cl, local,0);
    }
    break;


  case C_USOCK:
    DEBUG_FP(stderr, "%s: c_usock %s\n", term_server, (char *)c+1);
    {
      int s,k,udp_type;

      if(sscanf((char *)c+1,"%d %d",&k,&udp_type) < 2) {
	errno = 0;
        ret_fail(cl, local, ! FATAL, "Insufficient argc");
        DEBUG_UDP(stderr, "%s:insufficient argc\n", term_server);
        break;
      }else if (k <  0 || k >= MAX_CLIENTS){
	errno = 0;
        ret_fail(cl, local, ! FATAL, "Client out of range");
        DEBUG_FP(stderr,"client out of range\n");
        break;
      }else if( (s = x__socket(AF_INET,SOCK_DGRAM,0)) < 0) {
        ret_fail(cl,local,! FATAL,"dgram socket() failed");      /* 0 ? is it fatal? */
        DEBUG_UDP(stderr, "%s: dgram socket failed: %s\n", term_server,
	  x__strerror(errno));
        break;
      }

      if( clients[k].fd >= 0) x__close(clients[k].fd);

      clients[k].fd = s;
      clients[k].type = T_WRFILE | T_RDFILE | T_UDP;
      clients[k].cl_type = CL_SOCKET;
      clients[k].state = 1;
      clients[k].udp_type = udp_type;
      clients[k].udp_size = 0;

      set_nonblock(clients[k].fd);

      clients[k].udp_host = 0;
      clients[k].udp_port = 0;
      clients[k].parent = cl->number;
      ret_ok(cl,local);
    }
    break;

  case C_UBIND:
    DEBUG_FP(stderr, "%s: c_ubind %s\n", term_server, (char *)c+1);
    {
      int k;
      unsigned int port=0;
      struct sockaddr *addr;

      sscanf((char *)c+1,"%d %u",&k,&port);

      DEBUG_FP(stderr,"%s:- client %d, port %u.\n", term_server, k, port);

/* hmmm.. maybe do some more sanity checks? is T_UDP? */
/* Naa, leave it general so people can use if for other things. */

      if (k <  0 || k >= MAX_CLIENTS || clients[k].fd < 0) {
	errno = 0;
	ret_fail(cl, local, ! FATAL, "Client out of range");
        DEBUG_FP(stderr,"client out of range\n");
	break;
      }

      if (!(addr=make_sockaddr(port,NULL,INADDR_ANY))) {
	errno = 0;
        ret_fail(cl, local, ! FATAL, "Can't get local address");
        DEBUG_FP(stderr,"can't get local address\n");
        break;
      }

      if( x__bind(clients[k].fd,addr,sizeof(struct sockaddr)) < 0) {
        ret_fail(cl,local, ! FATAL,"bind() failed");
        DEBUG_FP(stderr,"term: bind() failed: %s: %s\n", (char *)c+1,
          x__strerror(errno));
        break;
      }
      ret_ok(cl,local);
    }
    break;

  case C_UDPSET:
    DEBUG_FP(stderr, "%s: c_udpset %s\n", term_server, (char *) c+1);
    {
      int k;
      char *ptr;
      struct sockaddr_in *addr_in;

      k = atoi((char *)c+1);
      if (k < 0 || k >= MAX_CLIENTS) {
	errno = 0;
        ret_fail(cl,local,! FATAL,"client out of range");
        DEBUG_FP(stderr,"client out of range\n");
        break;
      }

      ptr = strchr((char *)c+1,':');
      if (ptr == NULL) { 
	errno = 0;
        ret_fail(cl,local,! FATAL,"No address specified");
        DEBUG_FP(stderr,"no address specified\n");
        break;
      } 
      
      addr_in = (struct sockaddr_in *) str_to_sockaddr(++ptr,
        inet_addr("127.0.0.1"));

      if (addr_in == NULL) {
        ret_fail(cl,local,! FATAL,"Address not recognized");
        DEBUG_FP(stderr,"address not recognized\n");
        break;
      } 

      clients[k].udp_host = ntohl(addr_in->sin_addr.s_addr);
      clients[k].udp_port = ntohs(addr_in->sin_port);

      DEBUG_FP(stderr, "c_udpset %lx %x\n",clients[k].udp_host,clients[k].udp_port);

      ret_ok(cl,local);
    }
    break;

  case C_QUIT:
    DEBUG_FP(stderr, "%s: c_quit %s\n", term_server, (char *)c+1);
    if (c[1] == '\0')
      do_shutdown = -1;
    else {
      do_shutdown = atoi((char *)c+1);	/* 1 == no hangup, 2 == force hangup */
      if (do_shutdown < 1)
        do_shutdown = -1;		/* -1 == take the default action */
    }
    break;

  default:
    break;
  } /* switch */
} /* function */

void init_client(struct Client *cl) {
  extern int compressing; /* the default from main */
  cl->type = T_RDFILE | T_WRFILE;
  cl->udp_type = 0;
  cl->udp_size = 0;
  cl->dump_count = 0;
  cl->cl_type = CL_SOCKET;
  cl->state = 1;
  cl->compress = compressing;
  cl->c_state = 0;
  cl->number = (cl - &clients[0]);
  cl->priority = 0;		/* default priority. Higher is better. */
  cl->queue = 0;	
  cl->name[0] = 0;
  cl->parent = -1;
  cl->peername.sin_port = ~0;
  cl->peername.sin_addr.s_addr = htonl(term_localaddr);
  cl->peername.sin_family = ~0;
  cl->owner = -1;
  if (cl->fd > 0) {
    x__close(cl->fd);
    cl->fd = -1;
  } 
  clear_buffers(cl);
  DEBUG_LINK(stderr, "Init client %d\n", cl->number);
}

/*---------------------------------------------------------------------------*/
/* Returns next client to read. Will be beefed up later to support priorities*/
/* A client with a priority of n will get n/(sum) of the packets available   */
/* maybe :) */

struct Client * get_next_client(void) { 
  int i, j=0;
				/* Check to see if any of the clients */
				/* are closing. This gets priority. */
  for (i = 0; i < MAX_CLIENTS;++i)
    if (clients[i].state == 2) {
      DEBUG_STATE(stderr, "get_n_c ret cl %d\n", i);
      return &clients[i];
    }

  if ((j = do_lottery()) < 0) return 0;	/* Nobody was ready */

  return &clients[j];
}

int get_client_data(struct Client *cl) {
  int i;			/* If nothing ready, signal that. */
  SANITY(cl);
  if (!cl->in_buff.size) 
    return -1;
  SANITY(cl->in_buff.end <= cl->in_buff.alloced);
  SANITY(cl->in_buff.end >= 0);
  SANITY(cl->in_buff.size >= 0);
  SANITY(cl->in_buff.size <= cl->in_buff.alloced);

				/* get the next byte from the buffer.*/
  
  
  i = cl->in_buff.data[cl->in_buff.end++];
  if (cl->in_buff.end == cl->in_buff.alloced)	/* Wrap the buffer */
				/* round if we have */
				/* hit the end. */
    cl->in_buff.end = 0;
  cl->in_buff.size --;		/* Update count of bytes left in buffer. */

  SANITY(cl->dump_count >= 0);
  if (cl->dump_count) {		/* If we are currently dumping, update */
				/* the dump count, and go smart if we */
				/* have finished. */
    if (!--cl->dump_count && cl->cl_type != CL_FILE)
      cl->type |= T_SMART;
  }

  DEBUG_STATE(stderr, "\tgcd:%d\n", i);
  return i;
}

void put_client_data(struct Client *cl, int i) {
  while (1) {
    DEBUG_STATE(stderr, "p_c_d: %d: state %d i %d c_len %d\n", 
		cl->number, cl->c_state, i, cl->c_len);
    switch (cl->c_state) {
    case 0:
      if (i == SWITCH) {
	cl->c_state = 1;
	return;
      }
      
      ADD_BUFF(cl, (un_char) i);
      return;
    case 1:
      if (i == SWITCH) {
	ADD_BUFF(cl, (un_char) i);
	cl->c_state = 0;
	return ;
      }
      cl->c_len = 0; /* yes. We do want to throw this byte away */
      /* It is just a remote control message flag */
      if (i == SWITCH - 3) {	/* It is a result message. */
	if (cl->type & T_SMART) {
	  ADD_BUFF(cl, (un_char) SWITCH);
	  ADD_BUFF(cl, SWITCH-3);
	}
	cl->c_state = 3;
      }else {
	cl->c_state = 2;
      }
      return;
    case 2:
      /* XXX Skip commands that are too long.  This is not very elegant. */
      if (cl->c_len < sizeof(cl->control)-1)
        cl->control[cl->c_len++] = i;
      if (i) return;
      if (cl->c_len >= sizeof(cl->control)-1) {
        WARNING(stderr, "%s:command skipped, too long: \"%.30s...\"\n",
            term_server, cl->control);
    	errno = E2BIG;
        ret_fail(cl, 0, 1, "command skipped");
      }else {
        do_control(0, cl , cl->control);
      }
      cl->c_state = 0;
      return;
    case 3:
      if (cl->type & T_SMART) ADD_BUFF(cl, (un_char) i);
      if (i) return;
      cl->c_state = 0;
      return;
    default:
      cl->c_state = 0;
      break;
    }
  }
}



/* Return the next byte that should go down the serial link */
/* Another bloody finite state machine. ;) */
/* This was a bitch to write. Sigh. More things than I thought needed */
/* to be handled. And it still isn't perfect. :( */

int get_client_byte() {
  static int state = 0,
  next, max;
  
  static char control[1024];
  struct Client *cl;
  int i;

  while (1) {
    DEBUG_STATE(stderr, "get_c_b: state %d next %d max %d cl %d\n", state,
		next, max, !curr_client ? -1 : curr_client->number);

    switch(state) {
    case 0: /* looking for new client */
      new_packet = 0;
      cl = get_next_client();
      if (cl == 0) 
	return -1;
      
      if (!cl->in_buff.size && cl->state == 2) { /* closing down */
				/* We have emptied the buffers, so */
				/* just tell the remote end that , and */
				/* finish off. */
	DEBUG_FP(stderr, "%s:sending c_close\n", term_server);
	/* ++kay: should be state 3, not -1 */
	cl->state = 3;
	state = 4;
	sprintf(control, "%c%c%c%c%c%c", SWITCH, cl->number + SWITCH + 1,
		SWITCH, SWITCH - 2, C_CLOSE, 0);
	curr_client = cl;
	next = 0;
	max = 6;
	break;
      }
      
      if (cl != curr_client) {
	curr_client = cl;
	state = 1; 
	return SWITCH;
      }
      state = 2;
      break;    
    case 1:
      state = 2;
      return curr_client->number + SWITCH + 1;
      break;
    case 2:
				/* If we are a new packet, then check */
				/* to see if there is a new client */
				/* with a greater probability. */
      if (new_packet) {
	new_packet = 0;
	state = 0;
	break;
      }

      i = get_client_data(curr_client);
      if (i == SWITCH) {
	if (!(curr_client->type & T_SMART)) {
	  state = 4;
	  max = 1; next = 0;
	  control[0] = SWITCH;	
	  return SWITCH;
	}
	state = 3;
	break;
      } else if (i < 0) {
	state = 0;
	break;
      }
      {
	extern int stat_cooked_out;
	++stat_cooked_out;
      }
      return i;
      break;
    case 3:
      i = get_client_data(curr_client);
      if (i < 0)
	return -1;
      
      if (i == SWITCH) {
	/* It is an escaped escape code */
	state = 4;	
 
	max = 1; next = 0;
	return SWITCH;
      }
      /* ok. We have some sort of control message */
      if ( i  > SWITCH ) {
	/* Hmm. It is trying to switch streams on its own. welllll. ok. */
	/* we'll let it. Note that is can only reliably insert 1 byte at */
	/* a time */
	control[0] = (i-SWITCH-1<MAX_CLIENTS) ? SWITCH-1 : i;
	next  = 0;
	max = 1;
	state = 4;
	return SWITCH;
	break;
      }
      if ( i != SWITCH - 1) {
	/* stuff for remote. Just pass it thru */
	/* might not be SWITCH - 2, but if it isn't, we don't want */
	/* to know. ;) */
	control[0] = i;
	next = 0; max = 1;
	state = 4;
	return SWITCH;
	break;
      }
      /* ok. a real control message for us. */
      state = 5;
      next = 0;
      break;
    case 4:
      if (next + 1 == max) 
	state = 2;
      return control[next++];
      break;
    case 5: /* A local control message */
      /* note that there is a nasty bit here. All other streams block while */
      /* we are waiting for this control message. I thought about */
      /* programming around this but decided that it was too messy. */
   
      /* note that most local control messages don't make much sense. */
      i = get_client_data(curr_client);
      if (i < 0)
	return -1;
      
      if (next > 1023) {
        if (curr_client->fd >= 0) x__close(curr_client->fd);
      }else {
        control[next++] = i;
      }
      
				/* If this isn't the end of the */
				/* message, keep going.. */
      if (i)
	break;

      if (next > 1023) {
        WARNING(stderr, "%s:local command skipped, too long: \"%.30s...\"\n",
            term_server, control);
      }else {
        do_control(1, curr_client, (un_char *) control);
      }

      state = 2;
      break;
    } /* switch */
  } /* while */
}

/*----------------------------------------------------------------------*/
/* Transfers the next 'len' bytes from the link, to clients.  Note that */
/* we handle control information here. */
void put_data(un_char *b, int len) {
  static struct Client *curr_client = 0;
  static int state = 0;
  int i, d;
  
  i = 0;
  while (i < len) {
    DEBUG_STATE(stderr, "%s:put_d: s %d, cl %d d %d\n", term_server, state, 
		!curr_client ? 0 : curr_client->number, b[i]);
    switch(state) {
    case 0:
      d = b[i++];
      if (d == SWITCH) {
	state = 1;
	break;
      }
      /* ok. Just put data to current client */
      if (!curr_client)
	break;
      put_client_data(curr_client, d);
      break;
    case 1:
      d = b[i++];
      /* checked for escaped escape */
      if (d == SWITCH) {
	if (curr_client) {
	  put_client_data(curr_client, d);
	  put_client_data(curr_client, d);
	}
       	state = 0;
	break;
      }
      /* check for stream switch */
      if (d > SWITCH && d - SWITCH - 1 < MAX_CLIENTS ) {
	curr_client = &clients[d - SWITCH - 1];
	if (curr_client->state < 0) 
	  init_client(curr_client);
	DEBUG_LINK(stderr, "%s:stream switch to %d\n", term_server,
          d - SWITCH - 1);
	state = 0;
	break;
      }
      if (curr_client) {
	put_client_data(curr_client, SWITCH);
	put_client_data(curr_client, d);
      }
      state = 0;
      break;
    default:
      state = 0;
      break;
    } /* switch */
  } /* while */
} /* function */

