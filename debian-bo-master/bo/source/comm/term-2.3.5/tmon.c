/*
>From hendrick@edmund.cs.andrews.edu Tue Dec 29 10:02:07 1992
Return-Path: <hendrick@edmund.cs.andrews.edu>
Received: from edmund.cs.andrews.edu by tartarus.uwa.edu.au (5.65c/SMI-4.1)
	id AA07713; Tue, 29 Dec 1992 10:02:02 +0800
Received: by edmund.cs.andrews.edu (5.57/Ultrix3.0-C)
	id AA11714; Mon, 28 Dec 92 21:01:39 -0500
From: hendrick@edmund.cs.andrews.edu (John Hendrickson)
Message-Id: <9212290201.AA11714@edmund.cs.andrews.edu>
Subject: tmon
To: oreillym
Date: Mon, 28 Dec 92 21:01:37 EST
X-Mailer: ELM [version 2.3 PL11]
Status: O
*/

#define I_CTYPE
#define I_MEMORY
#define I_TIMES
#define I_TTY
#define I_TYPE
#define I_SIGNAL
#define I_SYS
#define I_STAT
#define I_PARAM
#include "includes.h"
#include "client.h"

#ifndef HZ
#define HZ_ENV
static int HZ=60;
#endif

static int termsrv; /* server */

#define NULLS ((char *)0)
int	LI,	/* One less than screen length in termcap entry */
	CO;	/* Screen width */

#ifdef ONE_CLIENT
# define main tmon
#else
int term_debug = 0;
#endif


static char tc[2048];	/* termcap buffer */
static char tbuf[128], PC, *CF, *CL, *CM, *CN, *SO, *SE;

char *tgetstr ();
char *tgoto ();
int tgetent ();
int tgetnum ();
char *getenv ();
void tputs();

#define Tgetstr(code) (char *)((s = (char *)tgetstr(code,&p)) ? s : "")

/*	initialize termcap stuff */
static void get_ttype()
{
	char *ttytype;
	char *p = tbuf;
	char *s;

	if ((ttytype = getenv("TERM")) == NULLS) {
		printf ("TERM not set\n");
		exit(6);
	}
	if (tgetent(tc, ttytype) != 1) {
		printf("Can't load %s", ttytype);
		exit(7);
	}
	LI = tgetnum("li") - 1;
	CO = tgetnum("co");
	if ((s=Tgetstr("pc")) == NULLS)
		PC = '\0';
	else
		PC = (char)*s;
	

	CL = Tgetstr("cl");  /* clear screen */
	CM = Tgetstr("cm");  /* move cursor sequence */
	SE = Tgetstr("se");  /* standout mode off */
	SO = Tgetstr("so");  /* standout mode on */
	CF = Tgetstr("vi");  /* cursor off string */
	CN = Tgetstr("ve");  /* cursor on string */
}

static void putchr ( c )
char c;
{
	fputc ( c, stdout );
}

static void cls()		{ tputs(CL,LI,putchr); }

static void cur_on()	{ tputs(CN,1,putchr); }

static void cur_off()	{ tputs(CF,1,putchr); }

static void ttgoto(row, col)
int row, col;
{
	tputs(tgoto(CM, col, row),1,putchr);
}

static void drawline(row, col, len)
int row, col, len;
{
	ttgoto(row, col);
        fputc('+', stdout);
	while (len--)
		fputc('-', stdout);
        fputc('+', stdout);
}

static void theend(int sig) {
  ttgoto ( 24, 0 ); 
  fflush(stdout);
  terminal_restore(0,0);
  cur_on();
  exit(sig<0 ? 1 : 0);
}

static void STAT(int local, int st) {
  if (send_command(termsrv, C_STATS, local, "%d",st)< 0) {
    ttgoto(23,0);
    printf("C_STATS command failed. Abort.");
    theend(-1);
  }
}

static void get_compression_stats(int ser, int pos) {
  long lcomp;
  long d1=0, d2=0;
  STAT ( ser, -2 );
  sscanf ( command_result, "%ld %ld", &d1, &d2);
  if (d1 != 0)
    lcomp = ((d1 - d2) * 100) / d1;
  else
    lcomp  = 0;
  ttgoto ( 2, pos);
  printf ( "%4ld %%", lcomp );
}

static int get_buffer_stats( int ser, int pos ) {
  unsigned d1=0, d2=0;
  int i=4, count=0;
  STAT (ser, -1);
  while (command_result && *command_result) {
    int k=0;
    for (k=0;isspace(command_result[k]) && command_result[k];k++);
    for (;!isspace(command_result[k]) && command_result[k];k++);
    for (;isspace(command_result[k]) && command_result[k];k++);
    for (;!isspace(command_result[k]) && command_result[k];k++);
    for (;isspace(command_result[k]) && command_result[k];k++);
    if (sscanf(command_result, "%u %u", &d1, &d2) < 2) break;
    count++; 
    if (i == 20) {
      printf ( "..." );
    }else if (i < 20) {
      if (d2) {
        ttgoto (i++, pos);
        printf ( "%3u: %3u %3u", count, d1, d2 );
      }
      if (! command_result[k]) break;
      command_result = &command_result[k];
    }
  }
  return count;
}

int main(int argc, char *argv[]) 
{
  struct timeval timeout;
  int i, x, y, rhist[33],ahist[33],ihist[33],ohist[33];
  unsigned long last_recv=0, last_ack=0, last_in=0, last_out=0, d1, d2, d3, d4;
  unsigned int scale=0, timer=5, window_size=0;
  unsigned long packet_timeout;
  unsigned int max_cps;
  int stim, ttim;
  fd_set reads;
  char line[35];
  float dtim;
  struct tms Tbuf;

  timeout.tv_sec = 0;
  timeout.tv_usec = 0; 

#ifdef SOCKS
SOCKSinit(argv[0]);
#endif

#ifdef HZ_ENV
  {
    char *hz = getenv ("HZ");
    if (hz)
      HZ = atoi(hz);
  }
#endif
  /* set_nonblock(0); */
  priority = 3;
  client_options(argc, argv,"",NULL);

  get_ttype ();
  termsrv = connect_server(term_server);
  terminal_save(0);
  signal(SIGHUP, theend);
  signal(SIGINT, theend);
  signal(SIGQUIT, theend);
  signal(SIGTERM, theend);
  signal(SIGPIPE, theend);
  terminal_raw(0);
  cur_off();
  STAT ( 1, -5);

  sscanf(command_result,"%u %d %lu",&max_cps,&window_size,&packet_timeout);
  scale = max_cps + 10;

  STAT ( 1, -3);
  sscanf ( command_result, "%lu %lu %lu %lu", &last_recv, &last_ack,
    &last_in, &last_out );
  stim=times(&Tbuf);
  /*  STAT ( 1, -8); */
  /* cooked_out = atoi(command_result); */

  memset ( line, 0, sizeof (line) );
  memset ( rhist, 0, sizeof (rhist) );
  memset ( ahist, 0, sizeof (ahist) );
  memset ( ihist, 0, sizeof (ihist) );
  memset ( ohist, 0, sizeof (ohist) );

  cls ();
  printf ( "TERM link monitor" );

  ttgoto ( 2, 1 );
  printf ( "local %s", str_version(VERSION));
  ttgoto ( 2, 41);
  printf ( "remote %s", str_version(remote_term_version));
  
  for (i=3;i<22;i++){ ttgoto(i,1); putchr('|'); }
  for (i=3;i<22;i++){ ttgoto(i,41); putchr('|'); }
  for (i=3;i<22;i++){ ttgoto(i,35); putchr('|'); }
  for (i=3;i<22;i++){ ttgoto(i,75); putchr('|'); }
  drawline ( 3,1,33 );
  drawline ( 3,41,33 );
  drawline ( 21,1,33 );
  drawline ( 21,41,33 );

  while ( 1 )
  { 
  get_compression_stats(1, 30);
  get_compression_stats(0, 70);

  STAT ( 1, -5);
  sscanf(command_result,"%u %d %lu",&max_cps,&window_size,&packet_timeout);
  ttgoto ( 0, 28 );
  printf ( "SCALE :%6u (cps)", scale-10);
  ttgoto ( 0, 56 );
  printf ( "window : %4u", window_size);
  ttgoto ( 1, 56 );
  printf ( "timeout: %4lu", packet_timeout);

  STAT ( 1, -3);
  sscanf ( command_result, "%lu %lu %lu %lu", &d1, &d2, &d3, &d4);

  for (i=1;i<33;i++) {
    rhist[i-1]=rhist[i];
    ahist[i-1]=ahist[i];
    ihist[i-1]=ihist[i];
    ohist[i-1]=ohist[i];
  }
  ttim=times(&Tbuf);
  dtim=((float)(ttim-stim) * HZ ) / 10000.0;
  stim=ttim;

  ttgoto(2, 60);
  
  if (! last_recv) last_recv=d1;
  if (! last_ack) last_ack=d2; 
  if (! last_in) last_in=d3;
  if (! last_out) last_out=d4; 
  rhist[32]=(d1-last_recv)/dtim;
  ahist[32]=(d2-last_ack)/dtim;
  ihist[32]=(d3-last_in)/dtim;
  ohist[32]=(d4-last_out)/dtim;
  if(ihist[32]>scale) scale = ihist[32]+112; 
  if(ohist[32]>scale) scale = ohist[32]+112; 
  i=scale/18;
  for (y=20;y>3;y--) {
    for (x=0;x<33;x++) {
       if ( rhist[x] > i )
           line[x]='#';
       else if ( ihist[x] > i )
           line[x]='.';
       else
           line[x]=' ';
    }
    ttgoto(y,2);
    printf ( "%s", line );
    i += (scale/18);
  }

  i=scale/18;
  for (y=20;y>3;y--) {
    for (x=0;x<33;x++)
       if ( ahist[x] > i )
           line[x]='#';
       else if ( ohist[x] > i )
           line[x]='.';
       else 
           line[x]=' ';
    ttgoto(y,42);
    printf ( "%s", line );
    i += (scale/18);
  }

  ttgoto ( 22, 1 );
  printf ( "Incoming: %6lu (cps)  %9lu k",(unsigned long)((d1-last_recv)/dtim),d1/1024);
  ttgoto ( 22, 41 );
  printf ( "Outgoing: %6lu (cps)  %9lu k\n",(unsigned long)((d2-last_ack)/dtim),d2/1024);
  last_recv=d1;last_ack=d2;last_in=d3;last_out=d4;

  i = get_buffer_stats(1, 3);
  ttgoto (1,1);
  printf("Local: %u",i);
  i = get_buffer_stats(0, 43);
  ttgoto (1,41);
  printf("Remote: %u",i);

  ttgoto(24, 0);
  fflush(stdout);
  while (1) {
  timeout.tv_sec = timer;
  timeout.tv_usec = 0; 
  FD_ZERO ( &reads );
  FD_SET ( 0, &reads );
  select ( 1, &reads, 0, 0, &timeout);
  if ( FD_ISSET ( 0, &reads ) ) {
    read(0, line, 1); 
      if ((tolower(*line) == 'q')||(*line == '\003'))
	theend(0);
    }
    else break;
  }
 }
}

