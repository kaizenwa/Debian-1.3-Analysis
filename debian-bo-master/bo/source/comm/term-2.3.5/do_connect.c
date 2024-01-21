#define I_ERRNO
#define I_SYS
#define I_IOCTL
#define I_STRING
#define I_SIGNAL
#include "includes.h"

typedef struct {
  int client; 
  int server;
  struct Buffer from;
  struct Buffer to;
} Con;

static void sig_ignore(int dummy) {
  signal(SIGPIPE, sig_ignore);
}

void do_connect(int num, int *svs, int (*get_server)(int,struct sockaddr *)) {
  int max, num_cons = 0;
  int serv, j;
#if defined(STREAMS_PIPE) || defined(X_STREAMS_PIPE)
  int dumb;
#endif
  Con *cons = 0;

  serv = connect_server(term_server);
  if (serv < 0) {
    fprintf(stderr, "Couldn't contact term server.\n");
    exit(1);
  }

  if ((j=x__fork()) < 0) 
    exit(1);
  else if (j)
    exit(0);

  signal(SIGPIPE, sig_ignore);

  x__close(0);
  x__close(1);
  x__close(2);
  lose_ctty();
  chdir("/");
  
  for (max = 0; max < num;++max)
    set_nonblock(svs[max]);

  while (1) {
    fd_set in, out, except;
    Con *c;
    int i, ret, loop;
    
    FD_ZERO(&in);
    FD_ZERO(&out);
    FD_ZERO(&except);
    max = -1;
    for (i = 0; i < num;++i) {
      FD_SET(svs[i], &in);
      if (max < svs[i]) max = svs[i];
    }

    FD_SET(serv, &except);
    FD_SET(serv, &in);
    if (serv > max) max = serv;
    
				/* Ok. Build a list of all the sockets */
				/* we want to look at.. */
    for (i = 0, c = cons; i < num_cons;++i, ++c) {
      if (c->client >= 0) {	/* If this socket can we read and */
				/* written..  */
	if (c->to.size) {	/* If there is something to go to it */
				/* then select for writing. */
	  FD_SET(c->client, &out);
	  if (c->client > max) max = c->client;
	} else if (c->server >=0) { /* Else select for more data if we can. */
	  FD_SET(c->server, &in);
	  if (c->server > max) max = c->server;
	} else {		/* ok there was nothing in the buffer, */
				/* and we couldn't get anymore , so */
				/* just close. */
	  x__close(c->client);
	  c->client = -1;
	}
      }
      if (c->server >= 0) {
	if (c->from.size) {
	  FD_SET(c->server, &out);
	  if (c->server > max) max = c->server;
	} else if (c->client >=0) {
	  FD_SET(c->client, &in);
	  if (c->client > max) max = c->client;
	} else {
	  x__close(c->server);
	  c->server = -1;
	}
      }				/* if if server >=0 */
    }				/* Of for() */

    select(max+1, &in, &out, &except, 0);

				/* Check for term going away... */
    if (FD_ISSET(serv, &except) || FD_ISSET(serv, &in)) {
				/* Ok. Critical . Close every thing.*/
      exit(0);
    }
    for (loop = 0; loop < num;++loop)
      if (FD_ISSET(svs[loop], &in)) { /* new connection */
	struct sockaddr addr;
	int saddr = sizeof(addr),i ;
	for (i = 0; i < num_cons;i ++)
	  if (cons[i].server < 0 && cons[i].client < 0) break;
        if(i>MAX_CLIENTS) continue;

	if (i >= num_cons) {
	  if (!cons)
	    cons = (Con *) malloc(sizeof(Con));
	  else
	    cons = (Con *) realloc((char *) cons, (num_cons + 1) *
				   sizeof(Con)); 
	  memset(&cons[(i = num_cons++)],0,sizeof(Con));
	}
	c = &cons[i];
#if defined(STREAMS_PIPE) || defined(X_STREAMS_PIPE)
	c->client = CheckClientConnection(svs[loop]);
	if (c->client != -2) {		/* we have a streams pipe */
		if (c->client == -1) {
			fprintf(stderr, "can't add client\n");
			continue;
		} /* fall through to after "accept" */
	} else	/* not a streams pipe */
#endif
	c->client = x__accept(svs[loop], (struct sockaddr *) &addr, &saddr);
	
	if ((c->server = get_server(loop,&addr)) <0) {
	  x__perror("Couldn't open term");
	  x__close(c->client);
	  continue;
	}
	
	set_nonblock(c->server);
	set_nonblock(c->client);

	
	c->to.size = c->to.start = c->to.end = 0;
	c->from.size = c->from.start = c->from.end = 0;

	add_to_buffer(&c->to, 0);
	get_from_buffer(&c->to);
	add_to_buffer(&c->from, 0);
	get_from_buffer(&c->from);

      }
    
    for (i = 0, c = cons; i < num_cons;++i, ++c) {
      if (c->client < 0) continue;
      if (FD_ISSET(c->client,&in))
	ret = read_into_buff(c->client, &c->from, 0);
      else if (FD_ISSET(c->client, &out))
	ret = write_from_buff(c->client, &c->to, 0);
      else continue;
				/* Handle possible error condition */
      if (ret <=0 && termerrno) {
				/* an error has occurred or a stream */
				/* has closed. Close connection. NYF*/
	x__close(c->client);
	c->client = -1;
	continue;
      }
    }

    for (i = 0, c = cons; i < num_cons;++i, ++c) {
      if (c->server < 0) continue;
      if (FD_ISSET(c->server, &out)) 
	ret = write_from_buff(c->server, &c->from, 0);
      else if (FD_ISSET(c->server, &in))
	ret = read_into_buff(c->server, &c->to, 0);
      else continue;
				/* Handle possible error condition */
      if (ret<=0 && termerrno) {
	x__close(c->server);
	c->server = -1;
      }
    }
  }
}

