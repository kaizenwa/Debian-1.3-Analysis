/*
 *   A server for a multi-player version of Tetris
 *
 *   Copyright (C) 1996 Roger Espel Llima <roger.espel.llima@pobox.com>
 *
 *   Started: 10 Oct 1996
 *   Version: 1.0
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation. See the file COPYING for details.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/socket.h>
#include <errno.h>
#include <string.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>

#define DEFAULTPORT 19503

#define PROT_VERS_1 1
#define PROT_VERS_2 0
#define PROT_VERS_3 0

#define RESTARTDELAY 4

#define OP_NICK	1
#define OP_PLAY 2
#define OP_FALL 3
#define OP_DRAW 4
#define OP_LOST 5
#define OP_GONE 6
#define OP_CLEAR 7
#define OP_NEW 8
#define OP_LINES 9
#define OP_GROW 10
#define OP_MODE 11
#define OP_LEVEL 12
#define OP_BOT 13
#define OP_KILL 14
#define OP_PAUSE 15
#define OP_CONT 16
#define OP_VERSION 17
#define OP_BADVERS 18
#define OP_MSG 19
#define OP_YOUARE 20
#define OP_LINESTO 21


int clients = 0;
int bots = 0;
int norestart = 0;
int onceonly = 0;
int timetoplay = 0;
int verbose = 0;
int level = -1;
int mode = -1;
int paused = 0;

struct user {
  int fd;
  unsigned char nick[16];
  unsigned char number;
  struct user *next, *nextvictim;
  char active;
  char introduced;
  unsigned char readbuf[4096];
  int nread;
  char playing;
  char isbot;
};

struct user *user0 = NULL;

#define NEXT(u) ((u)->next ? (u)->next : user0)

int port = DEFAULTPORT;
struct sockaddr_in saddr;
int lfd;
unsigned char realbuf[512], *buf = realbuf + 4;

int interrupt;

fd_set fds;

/* like memcpy, but guaranteed to handle overlap when s <= t */
void copydown(char *s, char *t, int n) {
  for (; n; n--)
    *(s++) = *(t++);
}

void fatal(char *s) {
  fprintf(stderr, "%s.\n", s);
  exit(1);
}

void syserr(char *s) {
  fprintf(stderr, "fatal: %s failed.\n", s);
  exit(1);
}

void broadcast(struct user *except, int len, int activeonly) {
  struct user *u;

  realbuf[0] = realbuf[1] = realbuf[2] = 0;
  realbuf[3] = (unsigned char)len;
  for (u=user0; u; u=u->next)
    if (u != except && (u->active || !activeonly) && u->introduced)
      write(u->fd, realbuf, 4 + len);
}

void sendtoone(struct user *to, int len) {
  realbuf[0] = realbuf[1] = realbuf[2] = 0;
  realbuf[3] = (unsigned char)len;
  write(to->fd, realbuf, 4 + len);
}

void dropuser(struct user *u) {
  struct user *v, *w;
  
  if (verbose)
    printf("dropping client %d (%s)\n", u->number, u->nick);
  if (u == user0)
    user0 = u->next;
  else {
    for (v=user0; v; v=v->next)
      if (v->next && v->next == u) {
	v->next = u->next;
	break;
      }
  }
  close(u->fd);

  if (u->introduced) {
    buf[0] = u->number;
    buf[1] = OP_GONE;
    broadcast(u, 2, 0);
  }

  for (v=user0; v; v=v->next) {
    if (v->nextvictim == u) {
      for (w=NEXT(v); w!=v; w=NEXT(w)) {
	if (w->active && w->playing) {
	  v->nextvictim = w;
	  break;
	}
      }
      if (v->nextvictim == u)
	v->nextvictim = NULL;
    }
  }

  if (u->isbot)
    bots--;

  free(u);
  clients--;

  if (onceonly && clients == bots) {
    if (verbose)
      printf("no human clients left: exiting\n");
    exit(0);
  }

  if (clients == 0) {
    level = mode = -1;
    timetoplay = 0;
  }
}

void do_play() {
  struct user *v, *w;

  for (w=user0; w; w=w->next) {
    if (w->introduced) {
      w->active = 1;
      w->playing = 1;
      w->nextvictim = NULL;
      for (v=NEXT(w); v!=w; v=NEXT(v)) {
	if (v->introduced) {
	  w->nextvictim = v;
	  break;
	}
      }
    }
  }
  if (paused) {
    paused = 0;
    buf[1] = OP_CONT;
    broadcast(NULL, 2, 0);
  }
  buf[1] = OP_PLAY;
  broadcast(NULL, 2, 0);
}

int main(int argc, char *argv[]) {
  int on = 1, slen, i, sl;
  struct user *u, *v, *w;
  int mfd;
  unsigned char nxn;
  int r; 
  unsigned int len;
  int tcp = -1;
  struct protoent *tcpproto;
  struct timeval tv;

#ifndef NeXT
  struct sigaction sact;
#endif

  if ((tcpproto = getprotobyname("tcp")) != NULL)
    tcp = tcpproto->p_proto;

#ifdef NeXT
  signal(SIGPIPE, SIG_IGN);
#else
  sact.sa_handler = SIG_IGN;
  sigemptyset(&sact.sa_mask);
  sact.sa_flags = 0;
  sigaction(SIGPIPE, &sact, NULL);
#endif

  while (argc >= 2) {
    if (strcmp(argv[1], "-once") == 0) {
      onceonly = 1;
      argv++;
      argc--;
    } else if (strcmp(argv[1], "-h") == 0 ||
	       strcmp(argv[1], "-help") == 0) {
      printf(
      
"Use: xtserv [options]\n"
"Options:   -once       --  exit when all clients disconnect\n"
"           -v          --  be verbose\n"
"           -p port     --  set server on given port\n"
"           -norestart  --  don't auto-start games\n"

);
      exit(0);
    } else if (strcmp(argv[1], "-norestart") == 0) {
      argv++;
      argc--;
      norestart = 1;
    } else if (strcmp(argv[1], "-p") == 0) {
      if (argc < 3)
	fatal("Missing argument for -p");
      port = atoi(argv[2]);
      argv += 2;
      argc -= 2;
    } else if (strcmp(argv[1], "-v") == 0) {
      verbose = 1;
      argv++;
      argc--;
    } else fatal("Unrecognized option, try -h for help");
  }

  lfd = socket(PF_INET, SOCK_STREAM, 0);
  saddr.sin_family = AF_INET;
  saddr.sin_addr.s_addr = htonl(INADDR_ANY);
  saddr.sin_port = htons(port);

  if (lfd < 0)
    syserr("socket");
  setsockopt(lfd, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(int));
  if (bind(lfd, (struct sockaddr *)&saddr, sizeof(saddr)) < 0)
    syserr("bind");

  listen(lfd, 5);

  if (verbose)
    printf("xtris server started up, listening on port %d\nusing xtris protocol version %d.%d.%d\n\n", port, PROT_VERS_1, PROT_VERS_2, PROT_VERS_3);

  while(1) {
    interrupt = 0;

    FD_ZERO(&fds);
    mfd = lfd;
    u = user0;
    while (u) {
      FD_SET(u->fd, &fds);
      if (u->fd > mfd)
	mfd = u->fd;
      u = u->next;
    }
    FD_SET(lfd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 500000;
    if ((sl = select(mfd + 1, &fds, NULL, NULL, &tv)) < 0)
      if (errno != EINTR)
	syserr("select");
      else continue;
    
    if (sl < 0)
      continue;

    if (clients > 0 && clients == bots) {
      if (verbose)
	printf("only bots left... dropping all bots\n");
      while (user0)
	dropuser(user0);
      continue;
    }
    
    if (timetoplay && time(NULL) >= timetoplay) {
      buf[0] = 0;
      do_play();
      if (verbose)
	printf("everyone lost... restarting game\n");
      timetoplay = 0;
      continue;
    }

    if (sl == 0)
      continue;

    if (FD_ISSET(lfd, &fds)) {
      /* new connection */
      u = malloc(sizeof (struct user));
      slen = sizeof(saddr);
      u->fd = accept(lfd, (struct sockaddr *)&saddr, &slen);
      if (u->fd < 0) {
	if (errno == EINTR) {
	  free(u);
	  continue;
	} else syserr("accept");
      }
      if (tcp != -1)
	setsockopt(u->fd, tcp, TCP_NODELAY, (char *)&on, sizeof(int));
      u->nick[0] = 0;
      u->next = user0;
      u->nextvictim = NULL;
      u->active = 0;
      u->nread = 0;
      u->playing = 0;
      u->isbot = 0;
      u->introduced = 0;
      user0 = u;

      nxn = 1;
    again:
      v = u->next;
      while(v) {
	if (v->number == nxn) {
	  nxn++;
	  goto again;
	}
	v = v->next;
      }
      u->number = nxn;
      if (verbose)
	printf("client %d connecting from %s\n", nxn, inet_ntoa(saddr.sin_addr));
      clients++;
      buf[1] = OP_YOUARE;
      buf[0] = u->number;
      sendtoone(u, 2);
      continue;
    }

    u = user0;
    do {
      if (FD_ISSET(u->fd, &fds)) {
	r = read(u->fd, u->readbuf + u->nread, 4096 - u->nread);
	if (r <= 0) {
	  if (verbose)
	    printf("EOF from client %d (%s)\n", u->number, u->nick);
	  dropuser(u);
	  interrupt = 1;
	  break;
	}
	u->nread += r;
	while (u->nread >= 4 && u->nread >= 4 + u->readbuf[3]) {
	  len = u->readbuf[3];
	  if (u->readbuf[0] || u->readbuf[1] || u->readbuf[2]) {
	    if (verbose)
	      printf("crap from client %d (%s)\n", u->number, u->nick);
	    write(u->fd, "\033]50;kanji24\007\033(0", 16);
	    dropuser(u);
	    interrupt = 1;
	    break;
	  }
	  memcpy(buf, &u->readbuf[4], len);
	  u->nread -= 4 + len;
	  copydown(u->readbuf, u->readbuf + 4 + len, u->nread);

	  buf[0] = u->number;
	  if (!u->introduced && buf[1] != OP_NICK) {
	    dropuser(u);
	    interrupt = 1;
	    break;
	  }

	  switch(buf[1]) {
	    case OP_NICK:
	      if (len>16) len=16;
	      memcpy(u->nick, &buf[2], len-2);
	      u->nick[len-2] = 0;
	      for (i=0; i<len-2; i++) {
		if (u->nick[i] < ' ' || 
		    (u->nick[i] > 0x7e && u->nick[i] <= 0xa0)) {
		  u->nick[i] = 0;
		  break;
		}
	      }

	      if (!u->introduced) {
		buf[0] = u->number;
		buf[1] = OP_NEW;
		broadcast(u, 2, 0);
	      }
	      
	      if (verbose)
		printf("client %d calls itself \"%s\"\n", u->number, u->nick);
	      buf[1] = OP_NICK;
	      broadcast(u, len, 0);

	      if (!u->introduced) {
		for (v=user0; v; v=v->next) {
		  if (v != u && v->introduced) {
		    buf[0] = v->number;
		    buf[1] = OP_NEW;
		    sendtoone(u, 2);
		    buf[1] = OP_NICK;
		    memcpy(&buf[2], v->nick, 14);
		    sendtoone(u, 2+strlen(v->nick));
		  }
		}
		if (level >= 0) {
		  buf[0] = 0;
		  buf[1] = OP_LEVEL;
		  buf[2] = level;
		  sendtoone(u, 3);
		}
		if (mode >= 0) {
		  buf[1] = OP_MODE;
		  buf[2] = mode;
		  sendtoone(u, 3);
		}
	      }

	      u->introduced = 1;
	      break;

	    case OP_KILL:
	      v = u;
	      for (u=user0; u; u=u->next) {
		if (u->number == buf[2])
		  break;
	      }
	      if (u) {
		if (u->isbot) {
		  if (verbose)
		    printf("client %d (%s) kills bot %d (%s)\n", v->number, v->nick, u->number, u->nick);
		  dropuser(u);
		  interrupt = 1;
		  break;
		} else {
		  if (verbose)
		    printf("client %d (%s) attempting to kill non-bot %d (%s)\n", v->number, v->nick, u->number, u->nick);
		}
	      }
	      break;

	    case OP_PLAY:
	      if (verbose)
		printf("client %d (%s) starts game\n", u->number, u->nick);
	      timetoplay = 0;
	      do_play();
	      break;
	    
	    case OP_MODE:
	      mode = buf[2];
	      if (verbose)
		printf("client %d (%s) sets mode %d (%s)\n", u->number, u->nick, buf[2], buf[2] == 0 ? "normal" : (buf[2] == 1 ? "fun" : "unknown"));
	      broadcast(NULL, 3, 0);
	      break;

	    case OP_PAUSE:
	      if (verbose)
		printf("client %d (%s) pauses game\n", u->number, u->nick);
	      broadcast(NULL, 2, 0);
	      paused = 1;
	      break;

	    case OP_CONT:
	      if (verbose)
		printf("client %d (%s) continues game\n", u->number, u->nick);
	      broadcast(NULL, 2, 0);
	      paused = 0;
	      break;

	    case OP_BOT:
	      if (!u->isbot)
		bots++;
	      u->isbot = 1;
	      if (verbose)
		printf("client %d (%s) declares itself to be a bot\n", u->number, u->nick);
	      break;
	    
	    case OP_LEVEL:
	      level = buf[2];
	      if (verbose)
		printf("client %d (%s) sets level %d\n", u->number, u->nick, buf[2]);
	      broadcast(NULL, 3, 0);
	      break;

	    case OP_LOST:
	      if (verbose)
		printf("client %d (%s) has lost\n", u->number, u->nick);
	      u->playing = 0;
	      broadcast(u, 2, 1);
	      i = 0;
	      for (v=user0; v; v=v->next) {
		if (v->nextvictim == u) {
		  for (w=NEXT(v); w!=v; w=NEXT(w)) {
		    if (w->active && w->playing) {
		      v->nextvictim = w;
		      break;
		    }
		  }
		  if (v->nextvictim == u)
		    v->nextvictim = NULL;
		}
	      }
	      for (v=user0; v; v=v->next)
		if (v->playing)
		  i++;
	      if (i < 2 && clients > 1 && !norestart)
		timetoplay = time(NULL) + RESTARTDELAY;
	      break;

	    case OP_CLEAR:
	    case OP_GROW:
	      broadcast(u, 2, 1);
	      break;
	    
	    case OP_MSG:
	      buf[len] = 0;
	      if (verbose)
		printf("client %d (%s) sends message: %s\n", u->number, u->nick, &buf[2]);
	      broadcast(u, len, 0);
	      break;

	    case OP_DRAW:
	    case OP_FALL:
	      broadcast(u, len, 1);
	      break;
	    
	    case OP_VERSION:
	      if (len != 5 || buf[2] != PROT_VERS_1 || buf[3] != PROT_VERS_2) {
		if (verbose)
		  printf("client %d (%s) has wrong protocol version %d.%d.%d\n", u->number, u->nick, buf[2], buf[3], buf[4]);
		buf[1] = OP_BADVERS;
		buf[2] = PROT_VERS_1;
		buf[3] = PROT_VERS_2;
		buf[4] = PROT_VERS_3;
		sendtoone(u, 5);
		dropuser(u);
		interrupt = 1;
	      } else {
		if (verbose)
		  printf("client %d (%s) uses protocol version %d.%d.%d\n", u->number, u->nick, buf[2], buf[3], buf[4]);
	      }
	      break;

	    case OP_LINES:
	      if (len != 3) {
		if (verbose)
		  printf("client %d (%s) sends crap for an OP_LINES\n", u->number, u->nick);
		dropuser(u);
		interrupt = 1;
		break;
	      }
	      if (u->nextvictim) {
		if (verbose)
		  printf("client %d (%s) sends %d %s to client %d (%s)\n", u->number, u->nick, (int)buf[2], buf[2] == 1 ? "line" : "lines", u->nextvictim->number, u->nextvictim->nick);
		sendtoone(u->nextvictim, 3);
		buf[3] = u->nextvictim->number;
		buf[1] = OP_LINESTO;
		broadcast(u->nextvictim, 4, 1);
		for (v=NEXT(u->nextvictim); v!=u->nextvictim; v=NEXT(v)) {
		  if (v->active && v != u && v->playing) {
		    u->nextvictim = v;
		    break;
		  }
		}
	      } else if (verbose)
		printf("client %d (%s) makes %d %s but has no victim\n", u->number, u->nick, (int)buf[2], buf[2] == 1 ? "line" : "lines");
	      break;
	    
	    default:
	      if (verbose)
		printf("opcode %d from client %d (%s) not understood\n", buf[0], u->number, u->nick);
	  }
	}
      }
      if (u && !interrupt)
	u = u->next;
    } while (u && !interrupt);
  }
}

