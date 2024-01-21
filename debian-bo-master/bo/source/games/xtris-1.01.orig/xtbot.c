/*
 *   An automatic player for xtris, a multi-player version of Tetris.
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
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>

#define DEFAULTPORT 19503

#define OP_NICK 1
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


static int shape[7][4][8] = {

    /*  The I   ++++++++  */
 {
    {  1, 3, 2, 3, 3, 3, 4, 3 },
    {  3, 1, 3, 2, 3, 3, 3, 4 },
    {  1, 3, 2, 3, 3, 3, 4, 3 },
    {  3, 1, 3, 2, 3, 3, 3, 4 }
 },
    /*  The O      ++++
                   ++++   */
 {
    {  2, 3, 3, 3, 2, 2, 3, 2 },
    {  2, 3, 3, 3, 2, 2, 3, 2 },
    {  2, 3, 3, 3, 2, 2, 3, 2 },
    {  2, 3, 3, 3, 2, 2, 3, 2 }
 },

    /*  The T   ++++++
                  ++      */
 {
    {  3, 2, 2, 3, 3, 3, 4, 3 },
    {  3, 2, 3, 3, 3, 4, 4, 3 },
    {  2, 2, 3, 2, 4, 2, 3, 3 },
    {  2, 3, 3, 2, 3, 3, 3, 4 }
 },

    /*  The L   ++++++
                ++        */
 {
    {  2, 2, 2, 3, 3, 3, 4, 3 },
    {  3, 2, 3, 3, 3, 4, 4, 2 },
    {  2, 2, 3, 2, 4, 2, 4, 3 },
    {  2, 4, 3, 2, 3, 3, 3, 4 }
 },

    /*  The J   ++++++
                    ++    */
 {
    {  2, 3, 3, 3, 4, 2, 4, 3 },
    {  3, 2, 3, 3, 3, 4, 4, 4 },
    {  2, 2, 3, 2, 4, 2, 2, 3 },
    {  2, 2, 3, 2, 3, 3, 3, 4 }
 },

    /*  The S     ++++
                ++++      */
 {
    {  2, 2, 3, 2, 3, 3, 4, 3 },
    {  3, 3, 3, 4, 4, 2, 4, 3 },
    {  2, 2, 3, 2, 3, 3, 4, 3 },
    {  3, 3, 3, 4, 4, 2, 4, 3 }
 },

    /*  The Z   ++++
                  ++++    */
 {
    {  2, 3, 3, 2, 3, 3, 4, 2 },
    {  3, 2, 3, 3, 4, 3, 4, 4 },
    {  2, 3, 3, 2, 3, 3, 4, 2 },
    {  3, 2, 3, 3, 4, 3, 4, 4 }
 }
    
};

static int rotations[7] = { 2, 1, 4, 4, 4, 2, 2 };

int funmode = 0;
int paused = 0;
int quiet = 0;

/* server stuff */

int sfd;
int port = DEFAULTPORT;
unsigned char realbuf[512], readbuf[1024], *buf = realbuf + 4;
int nread = 0;

char mynick[16];

int pit[20][10], oldpit[20][10], lines[20], values[10][4];

char simkeys[200], *sk = NULL;

int delays[10] = { 1500000, 1200000, 900000, 700000, 500000, 350000, 200000,
		   120000, 80000 };
int piece, rotation, x, y, next, level = 5;
int playing = 0, got_play = 0, interrupt = 0, addlines = 0, addsquares = 0;

#define EMPTY 7
#define GRAY 8

char keys[] = "JjklL ";

/* like memcpy, but guaranteed to handle overlap when s <= t */
void copydown(char *s, char *t, int n) {
  for (; n; n--)
    *(s++) = *(t++);
}

/* same, in case s >= t; s and t are starting positions */
void copyup(char *s, char *t, int n) {
  t += n-1;
  s += n-1;
  for (; n; n--)
    *(s--) = *(t--);
}

void fatal(char *s) {
  if (!quiet)
    fprintf(stderr, "%s.\n", s);
  exit(1);
}

void u_sleep(int i) {
  struct timeval tm;
  tm.tv_sec = i / 1000000;
  tm.tv_usec = i % 1000000;
  select(0, NULL, NULL, NULL, &tm);
}

void sendbuf(int len) {
  realbuf[0] = realbuf[1] = realbuf[2] = 0;
  realbuf[3] = (unsigned char)len;
  buf[0] = 0;
  write(sfd, realbuf, 4 + len);
}

/* a random number generator loosely based on RC5;
   assumes ints are at least 32 bit */

unsigned int my_rand() {
  static unsigned int s = 0, t = 0, k = 12345678;
  int i;

  if (s == 0 && t == 0) {
    s = (unsigned int)getpid();
    t = (unsigned int)time(NULL);
  }
  for (i=0; i<12; i++) {
    s = (((s^t) << (t&31)) | ((s^t) >> (31 - (t&31)))) + k;
    k += s + t;
    t = (((t^s) << (s&31)) | ((t^s) >> (31 - (s&31)))) + k;
    k += s + t;
  }
  return s;
}

int fits(int piece, int rotation, int x, int y) {
  int xx, yy, i;

  for (i=0; i<4; i++) {
    xx = x + shape[piece][rotation][2*i];
    yy = y - shape[piece][rotation][2*i+1];
    if (xx < 0 || xx > 9 || yy > 19)
      return 0;
    if (yy >= 0 && pit[yy][xx] != EMPTY)
      return 0;
  }
  return 1;
}

void put(int piece, int rotation, int x, int y, int color) {
  int xx, yy, i;

  for (i=0; i<4; i++) {
    xx = x + shape[piece][rotation][2*i];
    yy = y - shape[piece][rotation][2*i+1];
    if (yy >= 0)
      pit[yy][xx] = color;
  }
}

#define remove(p, r, x, y) put(p, r, x, y, EMPTY)

void refresh() {
  int x, y;
  int i = 2;

  buf[1] = OP_DRAW;

  for (x=0; x<10; x++) {
    for (y=0; y<20; y++) {
      if (pit[y][x] != oldpit[y][x]) {
	buf[i] = x;
	buf[i+1] = y;
	buf[i+2] = pit[y][x];
	i+=3;
	if (i > 200) {
	  sendbuf(i);
	  i = 2;
	  buf[1] = OP_DRAW;
	}
	oldpit[y][x] = pit[y][x];
      }
    }
  }
  if (i > 2)
    sendbuf(i);
}

void refreshme(n) {
  int x, y;

  for (x=0; x<10; x++)
    for (y=0; y<n; y++)
      if (pit[y][x] != oldpit[y][x]) {
	oldpit[y][x] = pit[y][x];
      }
}

void copyline(int i, int j) {
  int n;

  for (n=0; n<10; n++)
    pit[j][n] = pit[i][n];
}

void emptyline(int i) {
  int n;

  for (n=0; n<10; n++)
    pit[i][n] = EMPTY;
}

void falline(int i) {
  int x;
  for (x = i-1; x>=0; x--)
    copyline(x, x+1);
  emptyline(0);
}

void growline() {
  int n;
  for (n=0; n<19; n++)
    copyline(n+1, n);
  emptyline(19);
}

void fallines() {
  int n, c, x, fallen = 0;

  buf[1] = OP_FALL;
  for (n=0; n<20; n++) {
    c = 0;
    for (x=0; x<10; x++)
      if (pit[n][x] != EMPTY)
	c++;
    if (c == 10) {
      falline(n);
      buf[2 + fallen++] = (unsigned char)n;
    }
  }
  if (fallen) {
    sendbuf(2 + fallen);
    refreshme(20);
  }
  if (fallen > 1 || (fallen > 0 && funmode)) {
    buf[1] = OP_LINES;
    buf[2] = (unsigned char)fallen;
    sendbuf(3);
  }
}

void newlines() {
  int i = addlines, j, k;

  if (playing) {
    while (i > 0) {
      growline();
      buf[1] = OP_GROW;
      sendbuf(2);
      k = 0;
      for (j=0; j<10; j++)
      if (my_rand() % 7 < 3) {
        pit[19][j] = my_rand() % 7;
	k++;
      }
      if (k == 10)
	pit[19][my_rand() % 10] = EMPTY;
      i--;
    }
    if (addlines > 0) {
      refreshme(20 - addlines);
      refresh();
    }
    addlines = 0;
  }
}

void newsquares() {
  int n = addsquares, i, j, k;

  if (playing) {
    while (n > 0) {
      for (i=0; i<20; i++) {
	k = 0;
	for (j=0; j<10; j++) {
	  if (pit[i][j] != EMPTY)
	    k++;
	}
	if (k)
	  break;
      }
      if (k == 0 || i == 0) {
	i = 19;
	j = my_rand() % 10;
      } else {
	j = my_rand() % k;
	for (k=0; k<10; k++) {
	  if (pit[i][k] != EMPTY)
	    if (j == 0) {
	      pit[i-1][k] = GRAY;
	      break;
	    } else j--;
	}
      }
      n--;
    }
    if (addsquares > 0)
      refresh();
    addsquares = 0;
  }
}

void setlevel(int n) {
  if (n < 0)
    n = 0;
  if (n > 9)
    n = 9;
  level = n;
}

void setmode(int n) {
  funmode = (n != 0);
}

void dokey(char keysym) {
  if (!playing)
    return;
    
  if (keysym == keys[5]) {
    remove(piece, rotation, x, y);
    while(fits(piece, rotation, x, ++y));
    y--;
    put(piece, rotation, x, y, piece);
    refresh();
  } else if (keysym == keys[1]) {
    remove(piece, rotation, x, y);
    if (fits(piece, rotation, --x, y)) {
      put(piece, rotation, x, y, piece);
      refresh();
    } else put(piece, rotation, ++x, y, piece);
  } else if (keysym == keys[3]) {
    remove(piece, rotation, x, y);
    if (fits(piece, rotation, ++x, y)) {
      put(piece, rotation, x, y, piece);
      refresh();
    } else put(piece, rotation, --x, y, piece);
  } else if (keysym == keys[2]) {
    remove(piece, rotation, x, y);
    rotation = (rotation + 1) & 3;
    if (fits(piece, rotation, x, y)) {
      put(piece, rotation, x, y, piece);
      refresh();
    } else {
      rotation = (rotation + 3) & 3;
      put(piece, rotation, x, y, piece);
    }
  } else if (keysym == keys[0]) {
    remove(piece, rotation, x, y);
    while (fits(piece, rotation, --x, y));
    x++;
    put(piece, rotation, x, y, piece);
    refresh();
  } else if (keysym == keys[4]) {
    remove(piece, rotation, x, y);
    while (fits(piece, rotation, ++x, y));
    x--;
    put(piece, rotation, x, y, piece);
    refresh();
  }
}

void doserverstuff();

#define tvgeq(a, b) ((a).tv_sec > (b).tv_sec ||  \
		      ((a).tv_sec == (b).tv_sec && (a).tv_usec >= (b).tv_usec))

#define tvnormal(a) do { \
		      while ((a).tv_usec >= 1000000) { \
		        (a).tv_usec -= 1000000;  \
			(a).tv_sec++;  \
		      }  \
		    } while(0)


void tvdiff(struct timeval *a, struct timeval *b, struct timeval *r) {
  if (a->tv_usec >= b->tv_usec) {
    r->tv_usec = a->tv_usec - b->tv_usec;
    r->tv_sec = a->tv_sec - b->tv_sec;
  } else {
    r->tv_usec = a->tv_usec + 1000000 - b->tv_usec;
    r->tv_sec = a->tv_sec - b->tv_sec - 1;
  }
}

void waitfor(int delay) {
/* wait while reading and responding to server stuff */
 
  struct timeval tv, till, now;
  int r = 0;
  fd_set rfds;

  interrupt = 0;
  gettimeofday(&now, NULL);
  till = now;
  till.tv_usec += delay;
  tvnormal(till);

  do {
    FD_ZERO(&rfds);
    FD_SET(sfd, &rfds);

    gettimeofday(&now, NULL);

    if (tvgeq(now, till))
      break;
    else
      tvdiff(&till, &now, &tv);

    r = select(sfd + 1, &rfds, NULL, NULL, &tv);
    if (r < 0 && errno != EINTR) {
      perror("select");
      fatal("fatal: select() failed");
    }
    if (r < 0)
      FD_ZERO(&rfds);

    if (FD_ISSET(sfd, &rfds))
      doserverstuff();

  } while (!interrupt);
}

void doserverstuff() {
  int r;
  unsigned int len;

  r = read(sfd, readbuf + nread, 1024 - nread);
  if (r < 0)
    fatal("Error reading from server");
  if (r == 0)
    fatal("Connection to server lost");
  nread += r;

  while (nread >= 4 && nread >= 4 + readbuf[3]) {
    len = readbuf[3];
    if (readbuf[0] || readbuf[1] || readbuf[2])
      fatal("Wrong server line length");
    memcpy(buf, &readbuf[4], len);
    nread -= 4 + len;
    copydown(readbuf, readbuf + 4 + len, nread);

    switch(buf[1]) {
      case OP_PLAY:
	got_play = interrupt = 1;
	break;

      case OP_PAUSE:
	paused = interrupt = 1;
	break;
      
      case OP_CONT:
	paused = 0;
	interrupt = 1;
	break;

      case OP_LINES:
	if (funmode) {
	  switch(buf[2]) {
	    case 1:
	      addsquares++;
	      break;
	    case 2:
	      addlines++;
	      break;
	    case 3:
	      addlines += 3;
	      break;
	    case 4:
	      addlines += 5;
	      break;
	  }
	} else {
	  if (buf[2] >= 2 && buf[2] <= 4)
	    addlines += buf[2];
	}
	break;

      case OP_LEVEL:
	setlevel(buf[2]);
	break;

      case OP_MODE:
	setmode(buf[2]);
	break;
    }
  }
}

void connect2server(char *h) {
  struct hostent *hp;
  struct sockaddr_in s;
  struct protoent *tcpproto;
  int on = 1;

  if (h) {
    if ((s.sin_addr.s_addr = inet_addr(h)) == -1) {
      hp = gethostbyname(h);
      if (!hp)
	fatal("Host not found");
      s.sin_addr = *(struct in_addr *)(hp->h_addr_list[0]);
    }
  } else s.sin_addr.s_addr = inet_addr("127.0.0.1");
  s.sin_port = htons(port);
  s.sin_family = AF_INET;
  sfd = socket(PF_INET, SOCK_STREAM, 0);
  if (sfd < 0)
    fatal("Out of file descriptors");
  if ((tcpproto = getprotobyname("tcp")) != NULL)
    setsockopt(sfd, tcpproto->p_proto, TCP_NODELAY, (char *)&on, sizeof(int));

  if (connect(sfd, (struct sockaddr *)&s, sizeof(s)) < 0)
    fatal("Can't connect to server");

  buf[1] = OP_NICK;
  memcpy(&buf[2], mynick, strlen(mynick));
  sendbuf(2 + strlen(mynick));
  buf[1] = OP_BOT;
  sendbuf(2);
}

int eval(int xx, int yy, int firsthole) {
  int x, y, p, v, i;

  v = 0;
  for (x=0; x<10; x++) {
    y = 0;
    while (y<20 && (pit[y][x] == EMPTY || lines[y] == 10))
      y++;
    y++;
    for (; y<20; y++)
      if (pit[y][x] == EMPTY)
	v -= 17;
    p = 0;
    y = 0;
    if (next != 1 || piece == 1) {
      while (y<20 && (pit[y][x] == EMPTY || lines[y] == 10) && 
	     ((x > 0 && (pit[y][x-1] == EMPTY || lines[y] == 10)) || 
	      (x < 19 && (pit[y][x+1] == EMPTY || lines[y] == 10))))
        y++;
      for (; y<20 && pit[y][x] == EMPTY; y++, p++);
      if (p >= 2)
	v -= 5*(p-1);
    }
  }
  for (i=0; i<4; i++) {
    if (xx + shape[piece][rotation][2*i] == firsthole &&
        lines[yy - shape[piece][rotation][2*i+1]] < 10)
      v -= 4;
      break;
  }
  i = 0;
  for (y=0; y<20; y++)
    if (lines[y] == 10)
      i++;
  switch(i) {
    case 0:
      break;
    case 1:
      v += 9;
      break;
    case 2:
      v += 50;
      break;
    case 3:
      v += 100;
      break;
    case 4:
      v += 200;
      break;
  }
  if (yy < 7)
    v -= 10;
  v += 3*yy + (xx > 2 ? xx - 2 : 2 - xx) - 2*(rotation&1);

  return v;
}

void decide() {
  int x0, y0;
  int i, j, k, v;
  int maxval, ties, addstars, len;
  int firsthole = -100, maxheight = 19;

  simkeys[0] = 0;
  sk = simkeys;

  x0 = x;
  y0 = y;

  for (i=0; i<19; i++) {
    for (j=0; j<10 ; j++) {
      if (pit[i][j] != EMPTY && maxheight > i)
	maxheight = i;
      if (pit[i][j] != EMPTY && pit[i+1][j] == EMPTY) {
	firsthole = j;
	goto holefound;
      }
    }
  }
holefound:

  for (i=0; i<10; i++)
    for (j=0; j<4; j++)
      values[i][j] = -100000;
  maxval = -100000;
  ties = 0;

  for (x=-3; x<=6; x++) {
    for (rotation=0; rotation<rotations[piece]; rotation++) {
      y = y0;
      if (!fits(piece, rotation, x, y))
	continue;
      while (fits(piece, rotation, x, ++y));
      put (piece, rotation, x, --y, piece);

      for (i=0; i<20; i++) {
	k = 0;
	for (j=0; j<10; j++)
	  if (pit[i][j] != EMPTY)
	    k++;
	lines[i] = k;
      }

      values[x+3][rotation] = v = eval(x, y, firsthole);

      remove(piece, rotation, x, y);

      if (v == maxval)
	ties++;
      else if (v > maxval) {
	maxval = v;
	ties = 1;
      }
    }
  }

  if (ties == 0)
    return;

  k = my_rand() % ties;

  for (x=-3; x<=6; x++) {
    for (rotation=0; rotation<rotations[piece]; rotation++) {
      if (values[x+3][rotation] == maxval) {
	if (k == 0)
	  goto chosen;
	else
	  k--;
      }
    }
  }
chosen:

  for (i=0; i<rotation; i++)
    *sk++ = 'k';

  if (x > x0)
    for (i=x0; i<x; i++)
      *sk++ = 'l';
  else
    for (i=x0; i>x; i--)
      *sk++ = 'j';
  
  *sk = 0;

  len = strlen(simkeys);

  if (len) {

    addstars = maxheight - 4;
    if (addstars >= len)
      addstars = len - 1;
    if (addstars < 0)
      addstars = 0;
    
    sk = simkeys;
    for (; addstars; addstars--) {
      i = 1 + (my_rand() % (3 * len));
      for (; i; i--) {
	sk++;
	if (*sk == 0)
	  sk = simkeys;
	while (*sk == '*' || sk[1] == '*' || sk[1] == 0) {
	  sk++;
	  if (*sk == 0)
	    sk = simkeys;
	}
      }
      copyup(sk+2, sk+1, 2*len);
      sk[1] = '*';
    }
  }

  strcat(simkeys, "** ");

  sk = simkeys;
  rotation = 0;
  x = x0;
  y = y0;
}

int main(int argc, char *argv[]) {
  int i, j;

  strcpy(mynick, "xtris bot");

  while (argc >= 2 && argv[1][0] == '-') {
    if (strcmp(argv[1], "-h") == 0 ||
	       strcmp(argv[1], "-help") == 0) {
      printf("Use: xtbot [ -n nick ] [ -quiet ] [ server.name [ port ] ]\n");
      exit(0);
    } else if (strcmp(argv[1], "-quiet") == 0) {
      quiet = 1;
      argv++;
      argc--;
    } else if (strcmp(argv[1], "-n") == 0) {
      if (argc < 3)
	fatal("Missing argument for -n");
      strncpy(mynick, argv[2], 14);
      argv += 2;
      argc -= 2;
    } else fatal("Unrecognized option, try \"xtbot -help\"");
  }

  if (argc > 2) {
    port = atoi(argv[2]);
    if (port < 1024)
      fatal("bad port number");
  }

  connect2server(argv[1]);
  
  for (i=0; i<20; i++)
    for (j=0; j<10; j++)
      pit[i][j] = oldpit[i][j] = EMPTY;

  while(1) {
    playing = 0;
    do {
      waitfor(999999);
    } while(!got_play);

  start_game:
    buf[1] = OP_CLEAR;
    sendbuf(2);
    got_play = 0;
    addlines = 0;
    addsquares = 0;
    playing = 1;

    for (i=0; i<20; i++)
      for (j=0; j<10; j++)
	pit[i][j] = oldpit[i][j] = EMPTY;

    if (funmode) {
      for (i=10; i<20; i++) {
	pit[i][0] = my_rand() % 7;
	pit[i][9] = my_rand() % 7;
      }
      for (i=0; i<24; i++)
	pit[15 + my_rand() % 5][my_rand() % 10] = my_rand() % 7;
      refresh();
    }

    while (fits(piece = next, rotation = 0, x = 2, y = 3)) {
      next = my_rand() % 7;
      decide();
      while(fits(piece, rotation, x, y)) {
	put(piece, rotation, x, y, piece);
	refresh();
	if (got_play)
	  goto start_game;
	if (sk && *sk) {
	  while (*sk && *sk != '*')
	    dokey(*sk++);
	  if (*sk == '*')
	    sk++;
	}
	while (paused)
	  waitfor(1000000);
	waitfor(delays[level-1]);
	while (paused)
	  waitfor(1000000);
	remove(piece, rotation, x, y);
	y++;
      }
      y--;
      put(piece, rotation, x, y, piece);
      fallines();
      newlines();
      newsquares();
    }
    buf[1] = OP_LOST;
    sendbuf(2);
  }
}

