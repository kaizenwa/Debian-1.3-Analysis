/* $Copyright: $
 * Copyright (c) 1995, 1996, 1997 by Steve Baker (ice@mama.indstate.edu)
 * All Rights reserved
 *
 * This software is provided as is without any express or implied
 * warranties, including, without limitation, the implied warranties
 * of merchant-ability and fitness for a particular purpose.
 */
#include <utmp.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <paths.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

static char *version = "$Version: $ sac v1.5 (c) 1995, 1996, 1997 by Steve Baker $";

static char *month[] = {
  "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",NULL
};

/* FSSTND compliance courtesy of Edward S. Marshall */
#ifndef _PATH_WTMP
#define _PATH_WTMP "/var/log/wtmp" /* FSSTND compliant */
#endif

#ifndef RADIUS_DIR
#define RADIUS_DIR	"/usr/adm/radacct"
#endif

#define scopy(s)	(strcpy(malloc(strlen(s)+1),s))
#define min(a,b)	((a) < (b) ? (a) : (b))
#define max(a,b)	((a) > (b) ? (a) : (b))

/* This will break if someone has an 89 year old wtmp  =) */
#define MINUSZERO	-32767		/* HACK ALERT!! */
#define PLUSZERO	+32767		/* HACK ALERT!! */

enum {
  TOTAL	  = 0x0001,	/* Total login time */
  DAY     = 0x0002,	/* Daily average */
  USER    = 0x0003,	/* Per user login time */
  TTY     = 0x0004,	/* Show usage / tty */
  RAW	  = 0x0005,	/* Show everything */
  AVERAGE = 0x0010,	/* Average login time / login */
  HOUR    = 0x0020,	/* Hourly profile */
  CLIP	  = 0x0040,	/* Multiple logins during same time count only once */
  MINMAX  = 0x0080	/* Show min max # logins at one time */
};
enum { FALSE=0, TRUE=1 };
enum { USERLIST, EXCLUDELIST, TTYLIST, HOSTLIST, PORTMASTERLIST, FILELIST };

struct day *mkday();
struct usr *adduser(), *finduser();
struct tty *findtty();
void *malloc(), *realloc(), *malloc();
time_t getdate(), churn_time();
u_long hourrangespec();

struct day {
  time_t start, stop;
  short day, month, year, minlogins, maxlogins;
  time_t time;
  time_t h[24];
  u_long logins;
  struct usr *us;
  struct tty *tty;
  struct day *nxt;
} *days = NULL, *end=NULL;
/*
 * Keep a pointer at the end of the days list, since we'll usually be
 * adding crap to it, not to days before.  What 'o what do we do about
 * time changes aka time warps?
 */

struct usr {
  char user[UT_NAMESIZE+1];
  time_t time;
  time_t h[24];
  u_long logins, xlogins;
  struct usr *nxt;
} *us = NULL;

struct tty {
  char line[UT_LINESIZE+1];
  time_t time, h[24];
  u_long logins, xlogins;
  struct tty *nxt;
} *ttys = NULL;

/* excluded users list */
struct exc {
  char user[UT_NAMESIZE+1];
  struct exc *nxt;
} *ex = NULL;

/* ttys list */
struct ttys {
  char *line;
  char ispat;		/* Is tty a pattern?	*/
  struct ttys *nxt;
} *tty = NULL;

/* hosts list */
struct hosts {
  char *host;
  char ispat;		/* Is host a pattern?	*/
  char len,ss;		/* Is host a substring?	*/
  struct hosts *nxt;
} *hosts = NULL;

struct user {
  char user[UT_NAMESIZE+1];
  char line[UT_LINESIZE+1];
  char host[UT_HOSTSIZE+1];
  time_t in;
  struct user *nxt;
} *usr = NULL;

struct tacacs3_utmp {
  char ut_line[8];
  char ut_user[8];
  char ut_host[16];
  time_t ut_time;
};

struct tacacs4_utmp {
  char ut_line[8];
  char ut_user[8];
  char ut_host[16];
  time_t ut_time;
  char ut_comment[16];
};

struct file {
  char *file;
  void (*gronk)();
  struct file *nxt;
};

char fix = FALSE, exclude = FALSE, fixtty = FALSE, fixhost = FALSE;
char *back = NULL;
u_short type = TOTAL;
time_t total = 0, sd = 0, ed = 0, backtime = 0, curtime = 0;
time_t lastent = 0;
int ndays = 0, logins = 0, loggedin = 0, minlogins = -1, maxlogins = 0;
signed int sm = 0, em = 0;
u_long hourmask = 0;

char buf[1025];

void main(), usage(), addexclude(), addtty(), addhost(), addportmaster();
void doitold(), doitboth(), doitftp(), doittacacs3(), doittacacs4(), doit();
void doitradius(), checkday(), do_reboot(), login(), logout(), changetime();
void apply_hours(), user_apply_hours(), cleanup(), release(), report();
void print_hours(), tty_apply_hours();
int isexcluded(), istty(), ishost(), ispattern(), patmatch();
int read_radius(), read_block();
char **split();
u_long stoi();

/*
 * sac [-w wtmplist|-] [-acdfFhmoprtX[3|4]] [-b hour[:min[:sec]]] [-s start]
 *     [-e end] [[-u] userlist] [-x [userlist]] [-T [ttylist]] [-H [hostlist]]
 *     [-M hourrangespec[,hourrangspec[,...]]] [-R [portmasterlist]] [--help]
 *     [--version]
 */
void main(argc,argv)
int argc;
char **argv;
{
  struct file *f, *files = NULL;
  char *start = NULL, *end = NULL;
  char listtype = USERLIST;
  int i, j, n, fd=0;
  time_t t;
  void (*gronk)() = doit;

  for(n=i=1;i<argc;i=n) {
    n++;
    if (argv[i][0] == '-') {
      for(j=1;argv[i][j];j++) {
	switch (argv[i][j]) {
	  case 'w':
	    listtype = FILELIST;
	    break;
	  case 'p':
	    type = USER | (type & 0xFFF0);
	    break;
	  case 'd':
	    type = DAY | (type & 0xFFF0);
	    break;
	  case 't':
	    type = TTY | (type & 0xFFF0);
	    break;
	  case 'r':
	    type = RAW | (type & 0xFFF0) | HOUR;
	    break;
	  case 'a':
	    type |= AVERAGE;
	    break;
	  case 'c':
	    type |= CLIP;
	    break;
	  case 'h':
	    type |= HOUR;
	    break;
	  case 'm':
	    type |= MINMAX;
	    break;
	  case 'F':
	    gronk = doitftp;
	    break;
	  case 'f':
	    gronk = doitboth;
	    break;
	  case 'R':
	    gronk = doitradius;
	    listtype = PORTMASTERLIST;
	    break;
	  case 'o':
	    gronk = doitold;
	    break;
	  case 'X':
	    gronk = doittacacs3;
	    if (argv[i][j+1] == '4') {
	      gronk = doittacacs4;
	      j++;
	    } else if (argv[i][j+1] == '3') {
	      j++;
	    }
	    break;
	  case 'b':
	    if (argv[n] == NULL) usage(7);
	    back = argv[n++];
	    break;
	  case 's':
	    if (argv[n] == NULL) usage(7);
	    start = argv[n++];
	    break;
	  case 'e':
	    if (argv[n] == NULL) usage(7);
	    end = argv[n++];
	    break;
	  case 'x':
	    listtype = EXCLUDELIST;
	    break;
	  case 'T':
	    listtype = TTYLIST;
	    break;
	  case 'u':
	    listtype = USERLIST;
	    break;
	  case 'H':
	    listtype = HOSTLIST;
	    break;
	  case 'M':
	    if (argv[n] == NULL) usage(7);
	    hourmask |= hourrangespec(argv[n++]);
	    break;
	  case '-':
	    if (j == 1) {
	      if (!strcmp("--help",argv[i])) usage(4);
	      if (!strcmp("--version",argv[i])) usage(9);
	    }
	  default:
	    usage(1);
	}
      }
    } else {
      switch(listtype) {
	case USERLIST:
	  adduser(argv[i]);
	  fix = TRUE;
	  break;
	case EXCLUDELIST:
	  addexclude(argv[i]);
	  exclude = TRUE;
	  break;
	case TTYLIST:
	  addtty(argv[i]);
	  fixtty = TRUE;
	  break;
	case HOSTLIST:
	  addhost(argv[i]);
          fixhost = TRUE;
	  break;
	case PORTMASTERLIST:
	case FILELIST:
	  f = malloc(sizeof(struct file));
	  if (listtype == FILELIST) f->file = scopy(argv[i]);
	  else {
	    sprintf(buf,"%s/%s/detail",RADIUS_DIR,argv[i]);
	    f->file = scopy(buf);
	  }
	  f->gronk = gronk;
	  f->nxt = NULL;
	  if (files == NULL) files = f;
	  else {
	    f->nxt = files;
	    files = f;
	  }
	  break;
      }
    }
  }

  if (start) {
    if (start[0] == '-' || start[0] == '+') {
      for(i=1;start[i];i++) if (!isdigit(start[i])) usage(2);
      sm = atoi(start);
      if (sm == 0) {
	if (start[0] == '-') sm = MINUSZERO;
	else sm = PLUSZERO;
      }
    } else sd = getdate(start);
  }
  if (end) {
    if (end[0] == '-' || end[0] == '+') {
      t = time(0);
      for(i=1;end[i];i++) if (!isdigit(end[i])) usage(2);
      em = atoi(end);
      if (em == 0) {
	if (end[0] == '-') em = MINUSZERO;
	else em = PLUSZERO;
      }
    } else ed = getdate(end);
  }
  if (back) backtime = (curtime=time(0)) - churn_time(back);

  if (files == NULL) {
    files = malloc(sizeof(struct file));
    files->file = scopy(_PATH_WTMP);
    files->gronk = gronk;
    files->nxt = NULL;
  }
  for(f=files;f;f=f->nxt) {
    if (!strcmp(f->file,"-")) fd = 0;
    else {
      if ((fd = open(f->file,O_RDONLY)) < 0) {
	fprintf(stderr,"sac: Error opening '%s' for reading.\n",f->file);
	continue;
      }
    }
    f->gronk(fd);
    if (fd != 0) close(fd);
  }
  cleanup();
  report();
}

void usage(n)
int n;
{
  switch (n) {
    case 1:
      fprintf(stderr,"usage: sac [-w [wtmp-list|-]] [-acdfFhmoprtX[3|4]] [-s start] [-e end]\n       [-b H[:M[:S]]] [[-u] userlist] [-x [userlist]] [-T [ttylist]]\n       [-H [hostlist]] [-M hour-range-spec[,hour-range-spec[,...]]]\n       [-R [portmaster-list]] [--version] [--help]\n");
      break;
    case 2:
      fprintf(stderr,"sac: Invalid date.  Format: +days | -days | mm/dd/yy\n");
      break;
    case 3:
      fprintf(stderr,"sac: Invalid time.  Format: hours[:minutes[:seconds]]\n");
      break;
    case 4:
      fprintf(stderr,"usage: sac [-w [wtmp-list|-]] [-acdfFhmoprtX[3|4]] [-s start] [-e end]\n       [-b H[:M[:S]]] [[-u] userlist] [-x [userlist]] [-T [ttylist]]\n       [-H [hostlist]] [-M hour-range-spec[,hour-range-spec[,...]]]\n       [-R [portmaster-list]] [--version] [--help]\n");
      fprintf(stderr,"    -w [wtmp|-] Read alternate wtmp file(s).\n");
      fprintf(stderr,"    -d          Perform daily accounting.\n");
      fprintf(stderr,"    -a          Average usage / login.\n");
      fprintf(stderr,"    -t          Report usage by tty line.\n");
      fprintf(stderr,"    -r          Show raw statistics about accounting file(s).\n");
      fprintf(stderr,"    -h          Show hourly profile.\n");
      fprintf(stderr,"    -p          Per user accounting.\n");
      fprintf(stderr,"    -f          Perform accounting for ftp logins too.\n");
      fprintf(stderr,"    -F          Perform accounting for ftp logins only.\n");
      fprintf(stderr,"    -m          Show min/max # of concurrent logins.\n");
      fprintf(stderr,"    -c          Perform login clipping.\n");
      fprintf(stderr,"    -o          Read old decrepit wtmp format.\n");
      fprintf(stderr,"    -X[3]       Read tacacs 3.4 - 3.5 wtmp format.\n");
      fprintf(stderr,"    -X4         Read tacacs 4.x wtmp format.\n");
      fprintf(stderr,"    -R [pmstrs] Read Radius detail files for each portmaster listed.\n");
      fprintf(stderr,"    -s start    Display accounting info from `start'.\n");
      fprintf(stderr,"    -e end      Display accounting info up to `end'.\n");
      fprintf(stderr,"    -M hourspec Select only specific hours to perform accounting on.\n");
      fprintf(stderr,"    -b H:M:S    Show accounting info from the last few hours:minutes:seconds.\n");
      fprintf(stderr,"    -x [users]  Does not perform accounting for [users].\n");
      fprintf(stderr,"    -T [ttys]   Perform accounting on only those ttys listed.\n");
      fprintf(stderr,"    -H [hosts]  Perform accounting only for the hosts listed.\n");
      fprintf(stderr,"    -u [users]  Perform accounting only for the users listed.\n");
      fprintf(stderr,"    userlist    Perform accounting only for the users listed.\n");
      break;
    case 5:
      fprintf(stderr,"sac: Invalid hour range. Format: (0-23)[-(0-23)[,...]]\n");
      break;
    case 6:
      fprintf(stderr,"sac: Malformed wildcard.\n");
      break;
    case 7:
      fprintf(stderr,"sac: Missing argument.\n");
      break;
    case 8:
      fprintf(stderr,"sac: Must specify at least one portmaster with -R option \n     or specify stdin or detail file with -w.\n");
      break;
    case 9:
      {
	char *v = version+12;
	printf("%.*s\n",(int)strlen(v)-1,v);
	exit(0);
      }
  }
  exit(1);
}

/*
 * Turn "mm/dd/yy" into time in seconds.
 */
time_t getdate(s)
char *s;
{
  struct tm tm;
  int y;
  time_t t;

/*
 * <puke>
 * Need a real date parser that can handle different formats and separators
 */
  if (!isdigit(s[0]) || !isdigit(s[1]) || isdigit(s[2])) usage(2);
  if (!isdigit(s[3]) || !isdigit(s[4]) || isdigit(s[5])) usage(2);
  if (!isdigit(s[6]) || !isdigit(s[7]) || s[8]) usage(2);

  tm.tm_mon = (((s[0]-'0')*10) + (s[1]-'0')) -1;
  tm.tm_mday = (((s[3]-'0')*10) + (s[4]-'0'));
  y = (((s[6]-'0')*10) + (s[7]-'0'));
  tm.tm_year = (y < 70? 100 + y : y);
  tm.tm_isdst = -1;
  tm.tm_hour = 2;
  tm.tm_min = tm.tm_sec = 0;

  t = mktime(&tm);
  if (t == (time_t)(-1)) {
    fprintf(stderr,"oops\n");
    usage(2);
  }
  return t;
}

time_t churn_time(s)
char *s;
{
  time_t t = 0, mult=3600;
  u_char nbuf[20], p;

  for(p=0;*s;s++) {
    if (*s == ':') {
      nbuf[p] = (u_char)0;
      t += atoi(nbuf) * mult;
      p = 0;
      if (mult > 1) mult /= 60;
      else usage(3);
    } else if (isdigit(*s)) {
      if (p < 15) nbuf[p++] = (u_char)*s;
      else usage(3);
    } else usage(3);
  }
  nbuf[p] = (u_char)0;
  t += atoi(nbuf) * mult;

  return t;
}

/*
 * hourrangespec = [0-23][-[0-23]][,...]
 */
u_long hourrangespec(s)
char *s;
{
  u_long mask = 0;
  int i= 0, j = 0, p = 0;

  while(1) {
    if (!*s) usage(5);
    for(i=0;isdigit(*s) && i < 24;s++) i = (i*10) + (*s-'0');
    if (i > 23) usage(5);

    if (*s == '-') {
      s++;
      if (!*s) usage(5);
      for(j=0;isdigit(*s) && j < 24;s++) j = (j*10) + (*s-'0');
      if (j > 23) usage(5);

      for(p=i;p<=j;p++) mask |= (1<<p);
    } else mask |= (1<<i);
    if (!*s) return mask;
    if (*s++ == ',') continue;
    usage(5);
  };
  return mask;
}

void doit(fd)
int fd;
{
  struct utmp u;
  void *ut = &u;
  int n, m;

  while (1) {
    for(n=m=read(fd,ut,sizeof(struct utmp));n < sizeof(struct utmp);n+=m=read(fd,ut+n,sizeof(struct utmp)-n)) {
      if (m < 0) {
	perror("sac");
	return;
      }
      if (!m) return;
    }

    lastent = u.ut_time;
    if (u.ut_time == 0) continue;	/* corrupted wtmp entry! */
    checkday(u);  /* Do we have this day allocated? */
    /* Q: Why does the following bother me? */
    /* A: It may not handle all possible cases. Wtmp documentation sucks.
     *    Programs are also pretty free to put whatever the hell they want
     *    in wtmp.
     */
    if (u.ut_line[0]) {
      if (u.ut_user[0]) {
	if (!strcmp("~",u.ut_line) && (!strcmp("reboot",u.ut_user) || !strncmp("shutdown",u.ut_user,8))) do_reboot(u);
	else if (u.ut_type == USER_PROCESS && strncmp("ftp",u.ut_line,3)) login(u);
	else if (u.ut_type == DEAD_PROCESS && strncmp("ftp",u.ut_line,3)) logout(u,0);
	else if (u.ut_type == LOGIN_PROCESS) logout(u,0);
      } else {
	if (!strcmp("|",u.ut_line) || !strcmp("{",u.ut_line)) changetime(u);
	else logout(u,0);
      }
    }
  }
}

/*
 * Do it the old way, where u.ut_type is invalid or doesn't get used.
 * This is for non-standard wtmp's kept by programs such as tacacs.
 * Note: Xterm writes a wtmp entry on exit that looks like a login entry
 *	 if we ignore the ut_type field (which is "dead process" in this
 *	 case).  This will obviously screw things up.
 *
 * This may not be 100%.
 */
void doitold(fd)
int fd;
{
  struct utmp u;
  void *ut = &u;
  int n, m;

  while (1) {
    for(n=m=read(fd,ut,sizeof(struct utmp));n < sizeof(struct utmp);n+=m=read(fd,ut+n,sizeof(struct utmp)-n)) {
      if (m < 0) {
	perror("sac");
	return;
      }
      if (!m) return;
    }

    lastent = u.ut_time;
    if (u.ut_time == 0) continue;	/* corrupted wtmp entry! */
    checkday(u);
    if (u.ut_line[0]) {
      if (u.ut_user[0]) {
	if (!strcmp("~",u.ut_line) && (!strcmp("reboot",u.ut_user) || !strncmp("shutdown",u.ut_user,8))) do_reboot(u);
	else if (strcmp("LOGIN",u.ut_user) && strcmp("~",u.ut_line) && strncmp("ftp",u.ut_line,3)) login(u);
	else logout(u,0);
      } else {
	if (!strcmp("|",u.ut_line) || !strcmp("{",u.ut_line)) changetime(u);
	else logout(u,0);
      }
    }
  }
}

/*
 * Do it the tacacs 3.4-3.5 way, with tacacs ancient utmp format, which may work
 * for other ancient (such as old BSD) programs.
 * This too may not be 100%.
 */
void doittacacs3(fd)
int fd;
{
  struct tacacs3_utmp t;
  struct utmp u;
  void *ut = &t;
  int n, m;

  while (1) {
    for(n=m=read(fd,ut,sizeof(struct tacacs3_utmp));n < sizeof(struct tacacs3_utmp);n+=m=read(fd,ut+n,sizeof(struct tacacs3_utmp)-n)) {
      if (m < 0) {
	perror("sac");
	return;
      }
      if (!m) return;
    }

    lastent = u.ut_time;
    if (t.ut_time == 0) continue;	/* corrupted wtmp entry! */
    /* put tacacs_utmp into a normal utmp for the rest of this */
    u.ut_time = t.ut_time;
    strncpy(u.ut_line,t.ut_line,8);
    strncpy(u.ut_user,t.ut_user,8);
    strncpy(u.ut_host,t.ut_host,16);
/*    u.ut_line[8] = u.ut_user[8] = 0; */

    checkday(u);
    if (u.ut_line[0]) {
      if (u.ut_user[0]) {
	if (!strcmp("~",u.ut_line) && (!strcmp("reboot",u.ut_user) || !strncmp("shutdown",u.ut_user,8))) do_reboot(u);
	else if (strcmp("LOGIN",u.ut_user) && strcmp("~",u.ut_line) && strncmp("ftp",u.ut_line,3)) login(u);
	else logout(u,0);
      } else {
	if (!strcmp("|",u.ut_line) || !strcmp("{",u.ut_line)) changetime(u);
	else logout(u,0);
      }
    }
  }
}

/*
 * Do it the new 4.x xtacacs way...  Added the comment field -- 16 bytes to store
 * the PID?
 * This too may not be 100%.
 */
void doittacacs4(fd)
int fd;
{
  struct tacacs4_utmp t;
  struct utmp u;
  void *ut = &t;
  int n, m;

  while (1) {
    for(n=m=read(fd,ut,sizeof(struct tacacs4_utmp));n < sizeof(struct tacacs4_utmp);n+=m=read(fd,ut+n,sizeof(struct tacacs4_utmp)-n)) {
      if (m < 0) {
	perror("sac");
	return;
      }
      if (!m) return;
    }

    lastent = u.ut_time;
    if (t.ut_time == 0) continue;	/* corrupted wtmp entry! */
    /* put tacacs_utmp into a normal utmp for the rest of this */
    u.ut_time = t.ut_time;
    strncpy(u.ut_line,t.ut_line,8);
    strncpy(u.ut_user,t.ut_user,8);
    strncpy(u.ut_host,t.ut_host,16);
/*    u.ut_line[8] = u.ut_user[8] = 0; */

    checkday(u);
    if (u.ut_line[0]) {
      if (u.ut_user[0]) {
	if (u.ut_user[0] == '?') logout(u,0);
	else if (!strcmp("~",u.ut_line) && (!strcmp("reboot",u.ut_user) || !strncmp("shutdown",u.ut_user,8))) do_reboot(u);
	else if (strcmp("LOGIN",u.ut_user) && strcmp("~",u.ut_line) && strncmp("ftp",u.ut_line,3)) login(u);
	else logout(u,0);
      } else {
	if (!strcmp("|",u.ut_line) || !strcmp("{",u.ut_line)) changetime(u);
	logout(u,0);
      }
    }
  }
}

/*
 * Perform accounting on ftp logins only.  This handles ftp entries generated
 * from wu-ftpd at least.
 */
void doitftp(fd)
int fd;
{
  struct utmp u;
  void *ut = &u;
  int n, m;

  while (1) {
    for(n=m=read(fd,ut,sizeof(struct utmp));n < sizeof(struct utmp);n+=m=read(fd,ut+n,sizeof(struct utmp)-n)) {
      if (m < 0) {
	perror("sac");
	return;
      }
      if (!m) return;
    }

    lastent = u.ut_time;
    if (u.ut_time == 0) continue;	/* corrupted wtmp entry! */
    checkday(u);  /* Do we have this day allocated? */
    if (u.ut_line[0]) {
      if (u.ut_user[0]) {
	if (!strcmp("~",u.ut_line) && (!strcmp("reboot",u.ut_user) || !strcmp("shutdown",u.ut_user))) do_reboot(u);
	else if (!strncmp("ftp",u.ut_line,3)) login(u);
      } else {
	if (!strcmp("|",u.ut_line) || !strcmp("{",u.ut_line)) changetime(u);
	else if (!strncmp("ftp",u.ut_line,3)) logout(u,0);
      }
    }
  }
}

/*
 * Perform accounting on both normal logins and ftp simultaneously.
 */
void doitboth(fd)
int fd;
{
  struct utmp u;
  void *ut = &u;
  int n, m;

  while (1) {
    for(n=m=read(fd,ut,sizeof(struct utmp));n < sizeof(struct utmp);n+=m=read(fd,ut+n,sizeof(struct utmp)-n)) {
      if (m < 0) {
	perror("sac");
	return;
      }
      if (!m) return;
    }

    lastent = u.ut_time;
    if (u.ut_time == 0) continue;	/* corrupted wtmp entry! */
    checkday(u);  /* Do we have this day allocated? */
    if (u.ut_line[0]) {
      if (u.ut_user[0]) {
	if (!strcmp("~",u.ut_line) && (!strcmp("reboot",u.ut_user) || !strncmp("shutdown",u.ut_user,8))) do_reboot(u);
	else if (u.ut_type == USER_PROCESS || !strncmp("ftp",u.ut_line,3)) login(u);
	else if (u.ut_type == DEAD_PROCESS) logout(u,0);
	else if (u.ut_type == LOGIN_PROCESS) logout(u,0);
      } else {
	if (!strcmp("|",u.ut_line) || !strcmp("{",u.ut_line)) changetime(u);
	else logout(u,0);
      }
    }
  }
}

/*
 * Perform accounting for radius log files for livingston portmasters.
 */
void doitradius(fd)
int fd;
{
  struct utmp u;
  FILE *rfd;

  /* I don't want to re-invent the wheel, until I know it works, cause no one
     may want to use this anyway. */

  rfd = fdopen(fd,"r");

  while (read_radius(rfd,&u) >= 0) {
    checkday(u);  /* Do we have this day allocated? */
    if (u.ut_line[0]) {
      if (u.ut_user[0]) login(u);
      else logout(u,0);
    }
  }
}

/*
 * Reads a radius entry.  Radius logs suck suck suck... It would be nice if you
 * had the option of storing the logs in a nice machine readable format since it
 * would probably make cutting through the log 100 times faster at least.
 *
 * In this function we read forward until we get to a date entry, then parse
 * the date, and read the info block.
 */
int read_radius(rfd,u)
FILE *rfd;
struct utmp *u;
{
  char **s;
  int n, i, r;
  struct tm t;

  while(fgets(buf,1024,rfd) != NULL) {
    if (buf[0] == '\n' || buf[0] == ' ') continue;
    s = split(buf,&n);
    if (n < 7) continue;
    for(i=0;i<12;i++)
      if (!strcmp(s[1],month[i])) t.tm_mon = i;
    t.tm_mday = stoi(s[2]);
    t.tm_hour = stoi(s[3]);
    t.tm_min = stoi(s[4]);
    t.tm_sec = stoi(s[5]);
    t.tm_year = stoi(s[6]) - 1900;
    t.tm_isdst = -1;
    u->ut_time = mktime(&t);
    r = read_block(rfd,u);
    if (r < 0) return -1;
    if (r == 0) continue;
/*    printf("%d: [%-8.8s] %-8.8s %.16s\n",u->ut_time,u->ut_user,u->ut_line,u->ut_host); */
    return 0;
  }
  return -1;
}

enum {
  USER_NAME, CLIENT_ID, CLIENT_PORT_ID, ACCT_STATUS_TYPE, ACCT_SESSION_TIME,
  USELESS
};
struct radius_str {
  char *id;
  char use;
} radius_strs[] = {
  {"User-Name",		USER_NAME},
  {"Client-Id",		CLIENT_ID},
  {"Client-Port-Id",	CLIENT_PORT_ID},
  {"Acct-Status-Type",	ACCT_STATUS_TYPE},
/*  {"Acct-Session-Time",	ACCT_SESSION_TIME}, */
  {NULL, USELESS}
};

/*
 * Check the ID strings in the block against those above, if it's one of them,
 * then process it.  Exit when we hit a blank line.
 */
int read_block(rfd,u)
FILE *rfd;
struct utmp *u;
{
  char **s, stopflg;
  int i, n;

  for(stopflg = FALSE;fgets(buf,1024,rfd) != NULL;) {
    if (buf[0] == '\n') {
      if (stopflg) u->ut_user[0] = 0;
      return 1;
    }
    s = split(buf,&n);
    for(i=0;radius_strs[i].id;i++)
      if (!strcmp(radius_strs[i].id,s[0]))
	switch(radius_strs[i].use) {
	  case USER_NAME:
	    strncpy(u->ut_user,s[2],UT_NAMESIZE);
	    break;
	  case CLIENT_ID:
	    strncpy(u->ut_host,s[2],UT_HOSTSIZE);
	    break;
	  case CLIENT_PORT_ID:
	    strncpy(u->ut_line,s[2],UT_LINESIZE);
	    break;
	  case ACCT_STATUS_TYPE:
	    if (!strcmp("Stop",s[2])) stopflg = TRUE;
	    break;
/* Might be usefull for kludging in logout entries at the begining of a chopped log.
   We would ignore all login entries and use only the logout entries, but then what
   to do about the login entries at the _end_ of a log.  We just can't win.
	  case ACCT_SESSION_TIME:
	    break;
*/
	}
  }
  return -1;
}

/*
 * split the line into words broken by whitespace, colons, and quotes
 */
char **split(s,n)
char *s;
int *n;
{
  static char *w[256];
  int i;

  for(i=0;*s;) {
    while (*s && (isspace(*s) || *s == ':' || *s == '"')) s++;
    if (!*s) break;
    w[i++] = s;
    while (*s && !isspace(*s) && *s != ':' && *s != '"') s++;
    if (!*s) break;
    *s = 0;
    s++;
  }
  w[*n = i] = NULL;
  return w;
}

/*
 * A simple atoi, for speed.
 * Atoi() might actually be faster. Ought to test that.
 */
u_long stoi(s)
char *s;
{
  u_long n,i;

  if (s[1] == 0) {
    return s[0] - '0';
  } else if (s[2] == 0) {
    return ((s[0]-'0')*10) + (s[1]-'0');
  } else {
    for(i=n=0;s[i];i++)
      n = (n*10) + (s[i]-'0');
    return n;
  }
}


/*
 * Make sure the day that the wtmp entry corresponds to, exists in our
 * days list.  If day is less than the starting day by more than a day, or
 * greater than the last day by more than a day, allocate all the days in
 * between as well.
 */
void checkday(u)
struct utmp u;
{
  struct day *d, *p;

  if (days == NULL) {
    end = days = mkday(u.ut_time);
  } else {
    if (u.ut_time < days->start) {
      p = d = mkday(u.ut_time);
      while (p->stop+1 < days->start) {
        p->nxt = mkday(p->stop+1);
	p = p->nxt;
      }
      p->nxt = days;
      days = d;
    } else if (u.ut_time > end->stop) {
      p = d = mkday(end->stop+1);
      while (p->stop < u.ut_time) {
        p->nxt = mkday(p->stop+1);
	p = p->nxt;
      }
      end->nxt = d;
      end = p;
    }
  }
}

/*
 * Makes a day entry.  We'll assume a day is exactly 24 hours or 86400
 * seconds long.  I don't know if this is the case or not.  I don't think
 * the time algorithm takes into account leap seconds.
 *
 * Oh boy!  Daylight savings time screws this all to hell.  We can't
 * assume anything.  Check the time at 12:00:00am and 11:59:59pm to get
 * the time bound for the day.
 */
struct day *mkday(t)
time_t t;
{
  struct day *d;
  struct tm tm, *xtm;
  int i;
  time_t x;

  d = malloc(sizeof(struct day));
  xtm = localtime(&t);
  xtm->tm_hour = xtm->tm_min = xtm->tm_sec = 0;
  bcopy(xtm,&tm,sizeof(struct tm));

  d->start = mktime(xtm);

  x = d->start+86399;
  xtm = localtime(&x);

  tm.tm_isdst = -1;
  xtm = &tm;
  xtm->tm_hour = 23;
  xtm->tm_min = 59;
  xtm->tm_sec = 59;
  d->stop = mktime(xtm);
/*
  if (d->stop > (d->start + (24*60*60))) fprintf(stderr,"Created long day: %s %d.\n",month[tm.tm_mon],tm.tm_mday);
  if (d->start > (d->stop - ((24*60*60)-1))) fprintf(stderr,"Created short day: %s %d.\n",month[tm.tm_mon],tm.tm_mday);
*/

  d->nxt = NULL;
  d->us = NULL;
  d->tty = NULL;
  d->day = xtm->tm_mday;
  d->month = xtm->tm_mon;
  d->year = xtm->tm_year;
  d->logins = d->time = d->maxlogins = 0;
  d->minlogins = -1;
  for(i=0;i<24;i++) d->h[i] = 0;
  ndays++;

  return d;
}

void login(u)
struct utmp u;
{
  struct user *q;
  struct day *d;
  int l;

  /*
   * If we still have a login on this line, it's logged out for sure now!
   * Wtmp could be corrupted.
   */
  logout(u,1);

/*   printf ("Logging in  %s [%s] <%ld>...\n",u.ut_user,u.ut_line,u.ut_time); */
  q = malloc(sizeof(struct user));
  strncpy(q->line,u.ut_line,UT_LINESIZE);
  strncpy(q->user,u.ut_user,UT_NAMESIZE);
  strncpy(q->host,u.ut_host,UT_HOSTSIZE);
  q->user[UT_NAMESIZE] = q->line[UT_LINESIZE] = q->host[UT_HOSTSIZE] = 0;

  q->in = u.ut_time;
  q->nxt = usr;
  usr = q;

  if ((type & MINMAX) == MINMAX && !(fix && (finduser(us,q->user)) == NULL) && !(exclude && isexcluded(q->user)) && !(fixtty && !istty(q->line)) && !(fixhost && !ishost(q->host))) {
    l = loggedin++;
    maxlogins = max(maxlogins,loggedin);
    if (minlogins > -1) minlogins = min(minlogins,l);
    else minlogins = l;
    
    if (u.ut_time < end->start) {
      for(d=days;d;d=d->nxt) {
        if (u.ut_time >= d->start && u.ut_time <= d->stop) {
	  d->maxlogins = max(d->maxlogins,loggedin);
	  if (d->minlogins > -1) d->minlogins = min(d->minlogins,l);
	  else d->minlogins = l;
	  break;
	}
      }
    } else {
      end->maxlogins = max(end->maxlogins,loggedin);
      if (end->minlogins > -1) end->minlogins = min(end->minlogins,l);
      else end->minlogins = l;
    }
  }
}

void logout(u,f)
struct utmp u;
char f;
{
  struct user *p, *q;
  struct day *d;

  for(p=q=usr;p;) {
    if (!strcmp(u.ut_line,p->line)) {
/*      printf ("Logging out %s [%s] <%ld>...\n",u.ut_user,u.ut_line,u.ut_time); */
      release(p,u.ut_time);

      if ((type & MINMAX) == MINMAX && !(fix && (finduser(us,p->user)) == NULL) && !(exclude && isexcluded(p->user)) && !(fixtty && !istty(p->line)) && !(fixhost && !ishost(p->host))) {
	loggedin = max(0,loggedin-1);
	if (minlogins > -1) minlogins = min(minlogins,loggedin);
	else minlogins = loggedin;
    
	if (u.ut_time < end->start) {
	  for(d=days;d;d=d->nxt) {
	    if (u.ut_time >= d->start && u.ut_time <= d->stop) {
	      if (d->minlogins > -1) d->minlogins = min(d->minlogins,loggedin);
	      else d->minlogins = loggedin;
	      break;
	    }
	  }
	} else {
	  if (end->minlogins > -1) end->minlogins = min(end->minlogins,loggedin);
	  else end->minlogins = loggedin;
	}
      }
      if (p == usr) {
	usr = p->nxt;
	free(p);
	p = q = usr;
      } else {
        q->nxt = p->nxt;
	free(p);
	p = q->nxt;
      }
      continue;
    }
    q = p;
    p = p->nxt;
  }
}

/*
 * logout everyone on reboots or crashes.
 */
void do_reboot(u)
struct utmp u;
{
  struct user *p, *q;
  struct day *d;

  for(p=usr;p;) {
    release(p,u.ut_time);
    q = p;
    p=p->nxt;
    free(q);
  }
  usr = NULL;

  if ((type & MINMAX) == MINMAX) {
    loggedin = 0;
    minlogins = 0;
    
    if (u.ut_time < end->start) {
      for(d=days;d;d=d->nxt)
        if (u.ut_time >= d->start && u.ut_time <= d->stop) {
	  d->minlogins = 0;
	  break;
	}
    } else end->minlogins = 0;
  }
}

/*
 * A utmp entry denoted with a line of "|" denotes the "old" time before
 * netdate updated the time. An entry with line of "{" denotes the new time.
 * The difference in time is applied to the login time of every user still
 * logged in.
 */
void changetime(u)
struct utmp u;
{
  static time_t old;
  struct user *p;
  signed long dif;

  if (!strcmp("|",u.ut_line)) {
    old = u.ut_time;
    return;
  }
  dif = (signed long)(u.ut_time - old);
  for(p=usr;p;p=p->nxt) p->in -= dif;
}

/*
 * Apply login time for users who haven't logged out yet (or that the wtmp file
 * in particular indicates haven't logged out by the EOF) to the days that
 * are in days list.  Login time is not applied to days not listed in the
 * wtmp file (therefor all the days between the first and last wtmp entries).
 * The reason for this is that if you're inspecting an old wtmp file, the
 * wtmp may not indicate all logouts for the last day.  It is not possible
 * to know when the wtmp file really truly ends however, so we apply the
 * minimum of the current time or the ending time for the last day.
 */
void cleanup()
{
  time_t t = time(0);
  struct day *lastday;

/* Ooops, the -t option can't just ignore time that isn't in our days list
 * so we'll just force t to not go beyond the end of the last day in the
 * wtmp file.  An option should be made so you can opt to just use the
 * last wtmp entry for the end time.
 */
  if (lastent) {
    lastday = mkday(lastent);
    if (t > lastday->stop) t = lastday->stop;
  }
/*
 * Oops, if we're clipping, we have to remove the released time from the
 * list.  This is pretty easily done, by moving usr, instead of using a
 * temporary pointer.
 */
  for(;usr;usr=usr->nxt)
    release(usr,t);
}

/*
 * Release a login entry, applying the login time to the particular day
 * entries.
 * A user is logged in on a particular day when:
 *   in  >= start  &&  in  <= stop ||
 *   out >= start  &&  out <= stop ||
 *   in  <  start  &&  out >  stop
 */
void release(u,t)
struct user *u;
time_t t;
{
  struct day *p;
  struct usr *up;
  struct user *q;
  struct tty *tp;
  int i;

  if ((signed long)((t - u->in) + 1) < 0) return;

  /*
   * Clipping assumes that time moves forward. Only in computers is this
   * not necessarily a safe assumption.
   * Clipping rules:
   *   If (login time < all other login times)
   *      reduce logout time (t) to least login time (-1).
   *   else clip entirely.
   */
  if ((type & CLIP) == CLIP) {
    for(q=usr;q;q=q->nxt) {
      if (q != u && !strcmp(u->user,q->user)) {
	/* throw it out if he's already logged in earlier */
	if (q->in <= u->in) return;
	/* reduce the logout time to the lowest login time. */
	if (q->in < t) t = q->in-1;
      }
    }
  }

  if (backtime) {
    if (u->in > curtime || t < backtime) return;
    if (u->in < backtime && t > backtime) u->in = backtime;
    if (u->in < curtime && t > curtime) t = curtime;
  }

/*
 * If we've fixed the user list, then we only want to apply the login time
 * if that user is in our user list.
 */
  if (fix && (up = finduser(us,u->user)) == NULL) return;

/*
 * Check the exclude user list.
 */
  if (exclude && isexcluded(u->user)) return;

/*
 * Check the tty list.
 */
  if (fixtty && !istty(u->line)) return;

/*
 * Check the host list.
 */
  if (fixhost && !ishost(u->host)) return;

/* Only count logins that we actually apply */
  logins++;

  /* if we're logging usage / tty then apply login time to tty entry */
  if ((type & 0x000F) == TTY || (type & 0x000F) == RAW) {
    tp=findtty(ttys,u->line);
    if (tp == NULL) {
      tp = malloc(sizeof(struct tty));
      bzero(tp,sizeof(struct tty));
      strncpy(tp->line,u->line,UT_LINESIZE);
      if (ttys == NULL) ttys = tp;
      else {
	tp->nxt = ttys;
	ttys = tp;
      }
    }

/*
    tp->time += (t - u->in) + 1;
    tp->logins++;
*/

    if (u->in < end->start) {
      for(p=days;p;p=p->nxt) {
	if (u->in >= p->start && u->in <= p->stop) tty_apply_hours(u->in,t,p,tp);
	else if (t >= p->start && t <= p->stop) tty_apply_hours(u->in,t,p,tp);
	else if (u->in < p->start && t > p->stop) tty_apply_hours(u->in,t,p,tp);
      }
    } else tty_apply_hours(u->in,t,end,tp);
    if ((type & 0x000F) != RAW) return;
  }

  /* if we're logging usage / user then apply login time to user entry. */
  if (((type & 0x000F) == USER || (type&0x000F) == RAW) && (((up = finduser(us,u->user)) != NULL) || !fix)) {
    if (up == NULL) up = adduser(u->user);

    if (u->in < end->start) {
      for(p=days;p;p=p->nxt) {
	if (u->in >= p->start && u->in <= p->stop) user_apply_hours(u,t,p);
	else if (t >= p->start && t <= p->stop) user_apply_hours(u,t,p);
	else if (u->in < p->start && t > p->stop) user_apply_hours(u,t,p);
      }
    } else user_apply_hours(u,t,end);
    if ((type & 0x000F) != RAW) return;
  }

/*
 * We go through this for both daily and total. The total will be accumulated
 * at the end since we don't know the starting and ending dates until we're
 * done.
 */
  if (u->in < end->start) {
    /* Ugh, it's probably yesterday, but we've got to start all the way at the
     * beginning. I wonder if double-linking the list would speed things up.
     */
    for(p=days;p;p=p->nxt) {
      if (u->in >= p->start && u->in <= p->stop) {
	p->time += (min(p->stop,t) - u->in) + 1;
	p->logins++;
	if (hourmask || (type & HOUR) == HOUR) apply_hours(u->in,t,p->start,p->h);
      } else if (t >= p->start && t <= p->stop) {
	p->time += (t - max(p->start,u->in)) + 1;
	p->logins++;
	if (hourmask || (type & HOUR) == HOUR) apply_hours(u->in,t,p->start,p->h);
      } else if (u->in < p->start && t > p->stop) {
	p->time += 86400;
	p->logins++;
	if (hourmask || (type & HOUR) == HOUR)
	  for(i=0;i<24;i++) p->h[i] += 3600;
      }
    }
  } else {
    end->time += (min(end->stop,t) - max(end->start,u->in)) + 1;
    end->logins++;
    if (hourmask || (type & HOUR) == HOUR) apply_hours(u->in,t,end->start,end->h);
  }
}

void apply_hours(in,out,start,h)
time_t in, out, start, h[24];
{
  int i;
  time_t b, e;

  b = start;
  e = start + 3599;
  for(i=0;i<24;i++) {
    if (in >= b && in <= e) h[i] += (min(e,out) - in) + 1;
    else if (out >= b && out <= e) h[i] += (out - max(b,in)) + 1;
    else if (in < b && out > e) h[i] += 3600;

    b += 3600;
    e += 3600;
  }
}

void user_apply_hours(u,out,d)
struct user *u;
time_t out;
struct day *d;
{
  int i;
  time_t b, e;
  struct usr *up;

  if ((up = finduser(d->us,u->user)) == NULL) {
    up = malloc(sizeof(struct usr));
    bzero(up,sizeof(struct usr));
    strncpy(up->user,u->user,UT_NAMESIZE);
    up->nxt = d->us;
    d->us = up;
  }

  up->time += (min(out,d->stop) - max(u->in,d->start)) + 1;
  if (max(u->in,d->start) == u->in) up->logins++;
  else up->xlogins++;

  if (hourmask || ((type & HOUR) == HOUR)) {
    b = d->start;
    e = d->start + 3599;
    for(i=0;i<24;i++) {
      if (u->in >= b && u->in <= e) up->h[i] += (min(e,out) - u->in) + 1;
      else if (out >= b && out <= e) up->h[i] += (out - max(b,u->in)) + 1;
      else if (u->in < b && out > e) up->h[i] += 3600;

      b += 3600;
      e += 3600;
    }
  }
}

void tty_apply_hours(in,out,d,t)
time_t in,out;
struct day *d;
struct tty *t;
{
  int i;
  time_t b, e;
  struct tty *tp;

  if ((tp = findtty(d->tty,t->line)) == NULL) {
    tp = malloc(sizeof(struct tty));
    bzero(tp,sizeof(struct tty));
    strncpy(tp->line,t->line,UT_LINESIZE);
    if (d->tty == NULL) d->tty = tp;
    else {
      tp->nxt = d->tty;
      d->tty = tp;
    }
  }
  
  tp->time += (min(out,d->stop) - max(in,d->start)) + 1;
  if (max(in,d->start) == in) tp->logins++;
  else tp->xlogins++;

  if (hourmask || ((type & HOUR) == HOUR)) {
    b = d->start;
    e = d->start + 3599;
    for(i=0;i<24;i++) {
      if (in >= b && in <= e) tp->h[i] += (min(e,out) - in) + 1;
      else if (out >= b && out <= e) tp->h[i] += (out - max(b,in)) + 1;
      else if (in < b && out > e) tp->h[i] += 3600;

      b += 3600;
      e += 3600;
    }
  }
}

struct usr *adduser(s)
char *s;
{
  struct usr *u;

  u = malloc(sizeof(struct usr));
  bzero(u,sizeof(struct usr));
  strncpy(u->user,s,UT_NAMESIZE);
  u->nxt = us;
  us = u;
  return us;
}

struct usr *finduser(up,s)
struct usr *up;
char *s;
{
  struct usr *u;

  for(u=up;u;u=u->nxt)
    if (!strcmp(s,u->user)) return u;
  return NULL;
}

void addexclude(s)
char *s;
{
  struct exc *u;

  u = malloc(sizeof(struct exc));
  strncpy(u->user,s,UT_NAMESIZE);
  u->user[UT_NAMESIZE] = 0;
  u->nxt = ex;
  ex = u;
  return;
}

int isexcluded(s)
char *s;
{
  struct exc *u;

  for(u=ex;u;u=u->nxt)
    if (!strncmp(s,u->user,UT_NAMESIZE)) return TRUE;

  return FALSE;
}

struct tty *findtty(tt,l)
struct tty *tt;
char *l;
{
  struct tty *t;

  for(t=tt;t;t=t->nxt)
    if (!strncmp(t->line,l,UT_LINESIZE)) return t;
  return NULL;
}

void addtty(s)
char *s;
{
  struct ttys *t;
  char ispat;

  ispat = ispattern(s);
  if (ispat == -1 || (ispat && patmatch("ttyp0",s) == -1)) usage(6);

  t = malloc(sizeof(struct ttys));
  t->line = scopy(s);
  t->ispat = ispat;
  t->nxt = tty;
  tty = t;
  return;
}

int istty(s)
char *s;
{
  struct ttys *t;

  for(t=tty;t;t=t->nxt) {
    if (t->ispat) {
      if (patmatch(s,t->line) == 1) return TRUE;
      continue;
    }
    if (!strncmp(s,t->line,UT_LINESIZE)) return TRUE;
  }

  return FALSE;
}

void addhost(s)
char *s;
{
  struct hosts *h;
  char ispat;

  ispat = ispattern(s);
  if (ispat == -1 || (ispat && patmatch("tty",s) == -1)) usage(6);

  h = malloc(sizeof(struct hosts));
  h->host = scopy(s);
  h->ispat = ispat;
  h->len = strlen(h->host);
  if (!ispat && ((index(h->host,'.') == NULL) || (h->host[h->len-1] == '.'))) h->ss = TRUE;
  else h->ss = FALSE;
  h->nxt = hosts;
  hosts = h;
  return;
}

int ishost(s)
char *s;
{
  struct hosts *h;

  for(h=hosts;h;h=h->nxt) {
    if (h->ispat) {
      if (patmatch(s,h->host) == 1) return TRUE;
      continue;
    }
    if (h->ss) if (!strncmp(s,h->host,h->len)) return TRUE;
    else if (!strncmp(s,h->host,UT_HOSTSIZE)) return TRUE;
  }

  return FALSE;
}

/* Tests for *, ?, and [] */
int ispattern(s)
char *s;
{
  for(;*s;s++) {
    if (*s == '*') return 1;
    if (*s == '?') return 1;
    if (*s == '[') {
      for(;*s;s++)
	if (*s == ']') return 1;
      return -1;
    }
  }
  return 0;
}

/*
 * Patmatch() code courtesy of Thomas Moore (dark@mama.indstate.edu)
 * returns:
 *    1 on a match
 *    0 on a mismatch
 *   -1 on a syntax error in the pattern
 */
int patmatch(buf,pat)
char *buf,*pat;
{
  int match = 1,m,n;

  while(*pat && match) {
    switch(*pat) {
      case '[':
	pat++;
	if(*pat != '^') {
	  n = 1;
	  match = 0;
	} else {
	  pat++;
	  n = 0;
	}
	while(*pat != ']'){
	  if(*pat == '\\') pat++;
	  if(!*pat /* || *pat == '/' */ ) return -1;
	  if(pat[1] == '-'){
	    m = *pat;
	    pat += 2;
	    if(*pat == '\\' && *pat)
		pat++;
	    if(*buf >= m && *buf <= *pat)
		match = n;
	    if(!*pat)
		pat--;
	  } else if(*buf == *pat) match = n;
	  pat++;
	}
	buf++;
	break;
      case '*':
	pat++;
	if(!*pat) return 1;
	while(*buf && !(match = patmatch(buf++,pat)));
	return match;
      case '?':
	if(!*buf) return 0;
	buf++;
	break;
      case '\\':
	if(*pat)
	    pat++;
      default:
	match = (*buf++ == *pat);
	break;
    }
    pat++;
    if(match<1) return match;
  }
  if(!*buf) return match;
  return 0;
}



void report()
{
  struct day *d;
  struct usr *u, *up;
  struct tty *t, *tp;
  time_t h[24];
  int i;

  /*
   * Narrow down the days list to the range of days specified by the -s and -e
   * options.  This seems kludged in to me, but I really can't see how else to
   * do it.
   */
  if (sd || ed || sm || em) {
    if (!sd) {
      if (sm == MINUSZERO) sd = end->start;
      else if (!sm || sm == PLUSZERO) sd = days->start;
      else if (sm < 0) sd = (end->start + (86400*sm)) + 7200;
      else if (sm > 0) sd = (days->start + (86400*sm)) + 7200;
    }

    if (!ed) {
      if (em == PLUSZERO) ed = days->start;
      else if (!em || em == MINUSZERO) ed = end->start;
      else if (em < 0) ed = (end->start + (86400*em)) + 7200;
      else if (em > 0) ed = (days->start + (86400*em)) + 7200;
    }

    if (sd > ed) exit(0);
    for(d=days;d;d=d->nxt) {
      if (d->start <= sd && d->stop >= sd) days = d;
      if (d->start <= ed && d->stop >= ed) d->nxt = NULL;
    }
  }

  /* Produce the reports... */
  switch(type & 0x000f) {
    case TOTAL:
      for(ndays=total=0,d=days;d;d=d->nxt) {
	if (hourmask) {
	  for(i=0;i<24;i++) if (hourmask&(1<<i)) total += d->h[i];
	} else total += d->time;
	ndays++;
      }
      if (backtime) printf("Total: %12.2f over %s hours.\n", (float)total/3600, back);
      else printf("Total: %12.2f over %d days.\n", (float)total/3600, ndays);
      if ((type & AVERAGE) == AVERAGE) printf("Average: %10.2f / day, %10.2f / login\n",((float)total/3600) / ndays,((float)total/3600) / max(1,logins));
      if ((type & MINMAX) == MINMAX) printf("Logins: %11d   Min: %3d   Max: %3d\n",logins, max(0,minlogins), maxlogins);
      if ((type & HOUR) == HOUR) {
	for(i=0;i<24;i++) h[i] = 0;
	for(d=days;d;d=d->nxt)
	  for(i=0;i<24;i++) h[i] += d->h[i];
	print_hours(h,total);
      }
      break;
    case DAY:
      for(d=days;d;d=d->nxt) {
	if (hourmask) {
	  for(total=i=0;i<24;i++) if (hourmask&(1<<i)) total += d->h[i];
	} else total = d->time;

	printf("%s %2d  total %10.2f",month[d->month],d->day,(float)total/3600);
	if ((type & AVERAGE) == AVERAGE) {
	  if (d->logins)
	    printf("  %5ld logins, %8.2f hrs/login",d->logins,((float)total/3600)/d->logins);
	  else {
	    printf("     no logins");
	    if ((type & MINMAX) == MINMAX) printf("                    ");
	  }
	}
        if ((type & MINMAX) == MINMAX) {
	  if (d->minlogins == -1) d->maxlogins = d->minlogins = d->logins;
	  printf("   Min: %-3d Max: %d",d->minlogins, d->maxlogins);
	}
	putchar('\n');
	if ((type & HOUR) == HOUR) print_hours(d->h,d->time);
      }
      break;
    case USER:
      for(u=us;u;u=u->nxt) {
	for(total=0,d=days;d;d=d->nxt) {
	  if ((up = finduser(d->us,u->user)) != NULL) {
	    if (hourmask) {
	      for(i=0;i<24;i++) if (hourmask&(1<<i)) u->time += up->h[i];
	    } else u->time += up->time;

	    u->logins += up->logins;
	    if (d == days) u->logins += up->xlogins;
	    if ((type & HOUR) == HOUR) for(i=0;i<24;i++) u->h[i] += up->h[i];
	  }
	}
	printf("\t%-8s %10.2f",u->user,(float)u->time/3600);
	if ((type & AVERAGE) == AVERAGE) {
	  if (u->logins)
	    printf("\t%5ld logins, %10.2f hours / login.",u->logins,((float)u->time/3600) / u->logins);
	  else
	    printf("\t   no logins");
	}
	putchar('\n');
	if ((type & HOUR) == HOUR) print_hours(u->h,u->time);
      }
      break;
    case TTY:
      for(t=ttys;t;t=t->nxt) {
	for (d=days;d;d=d->nxt) {
	  if ((tp = findtty(d->tty,t->line)) != NULL) {
	    if (hourmask) {
	      for(total=i=0;i<24;i++) if (hourmask&(1<<i)) t->time += tp->h[i];
	    } else t->time += tp->time;
	    t->logins += tp->logins;
	    if (d == days) t->logins += tp->xlogins;
	    if ((type & HOUR) == HOUR) for(i=0;i<24;i++) t->h[i] += tp->h[i];
	  }
	}

	if (t->time == 0) continue;
	printf("\t%-8s %10.2f",t->line,((float)t->time)/3600);
	if ((type & AVERAGE) == AVERAGE)
	  printf("\t%5ld logins, %10.2f hours / login.",t->logins,((float)t->time/3600) / t->logins);
	putchar('\n');
	if ((type & HOUR) == HOUR) print_hours(t->h,t->time);
      }
      break;
    case RAW:
      for(i=0;i<24;i++) h[i] = 0;
      for(ndays=total=0,d=days;d;d=d->nxt) {
	if (hourmask) {
	  for(i=0;i<24;i++) {
	    if (hourmask&(1<<i)) {
	      total += d->h[i];
	      h[i] += d->h[i];
	    }
	  }
	} else {
	  total += d->time;
	  for(i=0;i<24;i++) h[i] += d->h[i];
	}
	ndays++;
	for(u=us;u;u=u->nxt) {
	  if ((up = finduser(d->us,u->user)) != NULL) {
	    if (hourmask) {
	      for(i=0;i<24;i++) if (hourmask&(1<<i)) u->time += up->h[i];
	    } else u->time += up->time;

	    u->logins += up->logins;
	    if (d == days) u->logins += up->xlogins;
	    for(i=0;i<24;i++) u->h[i] += up->h[i];
	  }
	}
	for(t=ttys;t;t=t->nxt) {
	  if ((tp = findtty(d->tty,t->line)) != NULL) {
	    if (hourmask) {
	      for(i=0;i<24;i++) if (hourmask&(1<<i)) t->time += tp->h[i];
	    } else t->time += tp->time;
	    t->logins += tp->logins;
	    if (d == days) t->logins += tp->xlogins;
	    for(i=0;i<24;i++) t->h[i] += tp->h[i];
	  }
	}
      }
      printf("Days in report: %d  Start: %02d/%02d/%02d  End: %02d/%02d/%02d  Hourmask: %06lX\n",ndays,days->month,days->day,days->year,end->month,end->day,end->year,hourmask&0x0FFF);
      printf("Total: %lu (%u logins) hours:",total,logins);
      for(i=0;i<24;i++) printf(" %lu",h[i]);
      printf("\n\n");
      for(i=0,u=us;u;u=u->nxt) i++;
      printf("Users in report: %d\n",i);
      for(u=us;u;u=u->nxt) {
	printf("  %8s total: %-10lu (%5ld logins) hours:",u->user,u->time,u->logins);
	for(i=0;i<24;i++) printf(" %lu",u->h[i]);
	printf("\n");
      }
      printf("\n");
      for(i=0,t=ttys;t;t=t->nxt) i++;
      printf("TTY's in report: %d\n",i);
      for(t=ttys;t;t=t->nxt) {
	printf("  %8s total: %-10lu (%5ld logins) hours:",t->line,t->time,t->logins);
	for(i=0;i<24;i++) printf(" %lu",t->h[i]);
	printf("\n");
      }
      printf("\n");
      for(d=days;d;d=d->nxt) {
	printf("%02d/%02d/%02d total: %-10lu (%5ld logins) hours:",d->month,d->day,d->year,d->time,d->logins);
	for(i=0;i<24;i++) printf(" %lu",d->h[i]);
	printf("\n");
	for(u=d->us;u;u=u->nxt) {
	  printf("  %8s total: %-10lu (%5ld logins) hours:",u->user,u->time,u->logins+(d==days?u->xlogins:0));
	  for(i=0;i<24;i++) printf(" %lu",u->h[i]);
	  printf("\n");
	}
	printf("\n");
	for(t=d->tty;t;t=t->nxt) {
	  printf("  %8s total: %-10lu (%5ld logins) hours:",t->line,t->time,t->logins+(d==days?t->xlogins:0));
	  for(i=0;i<24;i++) printf(" %lu",t->h[i]);
	  printf("\n");
	}
	printf("\n");
      }
  }
}

void print_hours(h,total)
time_t h[24], total;
{
  static char *bar = "########################################################################";
  int i, bl = strlen(bar)-8;
  float p[24], scale, maxp = 0;

  if (!total) {
    for(i=0;i<24;i++)
      printf("%02d-: %8.02f\n",i,(float)0);
  } else {
    for(maxp=0,i=0;i<24;i++) {
      p[i] = (float)h[i] / (float)total;
      if (p[i] > maxp) maxp = p[i];
    }
    scale = (float)bl / maxp;

    for(i=0;i<24;i++) {
      if (hourmask && !(hourmask & (1<<i))) continue;
      printf("%02d-: %8.02f %.*s\n",i,(float)h[i]/3600,(int)(scale*p[i]),bar);
    }
  }
}
