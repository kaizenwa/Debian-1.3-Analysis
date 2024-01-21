/* $Copyright: $
 * Copyright (c) 1995, 1996, 1997 by Steve Baker (ice@mama.indstate.edu)
 * All Rights reserved
 *
 * This software is provided as is without any express or implied
 * warranties, including, without limitation, the implied warranties
 * of merchant-ability and fitness for a particular purpose.
 */
#include <stdlib.h>
#include <utmp.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>


static char *version = "$Version: $ rawtmp v1.4 (c) 1995, 1996, 1997 by Steve Baker $";

static char *month[] = {
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

#ifndef _PATH_WTMP
#define _PATH_WTMP  "/var/log/wtmp"
#endif

enum { FALSE=0, TRUE=1 };

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

char *num(), *host();
void usage(), doit(), doit_tacacs3(), doit_tacacs4();
time_t getdate(), churn_time();

char dflg = FALSE, useaddr = FALSE;
char *start = NULL, *end = NULL, *back = NULL;
time_t sd = 0, ed = 0, backtime = 0, curtime= 0;
int fd;

/*
 * rawtmp [-w wtmp] [-X[3|4]d] [-s start] [-e end] [-b hours:minutes:seconds]
 */
void main(argc,argv)
int argc;
char **argv;
{
  char *file = _PATH_WTMP, tacacs3 = FALSE, tacacs4 = FALSE;
  int i, j, n;

  for(n=i=1;i<argc;i=n) {
    n++;
    if (argv[i][0] == '-') {
      for(j=1;argv[i][j];j++) {
	switch (argv[i][j]) {
	  case 'w':
	    file = argv[n++];
	    break;
	  case 'X':
	    if (argv[i][j+1] == '4') {
	      j++;
	      tacacs4 = TRUE;
	    } else if (argv[i][j+1] == '3') {
	      j++;
	      tacacs3 = TRUE;
	    } else tacacs3 = TRUE;
	    break;
	  case 'b':
	    back = argv[n++];
	    break;
	  case 's':
	    start = argv[n++];
	    break;
	  case 'e':
	    end = argv[n++];
	    break;
	  case 'd':
	    dflg = TRUE;
	    break;
	  case 'a':
	    useaddr = TRUE;
	    break;
	  case '-':
	    if (j == 1) {
	      if (!strcmp("--help",argv[i])) usage(4);
	      if (!strcmp("--version",argv[i])) usage(5);
	    }
	  default:
	    usage(1);
	}
      }
    } else {
    }
  }

  if (start) {
    if (isdigit(start[0])) {
      sd = getdate(start);
    } else usage(2);
  }
  if (end) {
    if (isdigit(end[0])) {
      ed = getdate(end) + 86399;
    } else usage(2);
  }
  if (back) backtime = (curtime=time(0)) - churn_time(back);

  if (!strcmp(file,"-")) fd = 0;
  else fd = open(file,O_RDONLY);

  if (tacacs4) doit_tacacs4();
  else if (tacacs3) doit_tacacs3();
  else doit();
  close(fd);
}

void doit()
{
  static char *ltype[] = {
    "UNKNOWN (0)", "Run Level", "Boot Time", "New Time", "Old Time",
    "INIT Process", "LOGIN Process", "User Process", "Dead Process", NULL
  };
  struct tm *tm;
  struct utmp u;
  void *ut = &u;
  int n, m;
  unsigned char c;

  if (dflg) printf("     ");
  printf("  Time        User    ID  Line      Host              PID   Type\n");
  
  while (1) {
    for(n=m=read(fd,ut,sizeof(struct utmp));n < sizeof(struct utmp);n+=m=read(fd,ut+n,sizeof(struct utmp)-n)) {
      if (m < 0) {
	perror("rawtmp");
	return;
      }
      if (!m) return;
    }
    if (back && u.ut_time < backtime) continue;
    if (start && u.ut_time < sd) continue;
    if (end && u.ut_time > ed) continue;
    c = u.ut_id[0] & 0x7F;
    u.ut_id[0] = (c && (c > '~' || c < ' '))? '?' : c;
    c = u.ut_id[1] & 0x7F;
    u.ut_id[1] = (c && (c > '~' || c < ' '))? '?' : c;

    if (dflg) {
      tm = localtime(&u.ut_time);
      printf("%3s %02d %02d:%02d:%02d: ",
	     month[tm->tm_mon], tm->tm_mday, tm->tm_hour,
	     tm->tm_min, tm->tm_sec);
    } else printf ("%-10ld: ",u.ut_time);

    printf("%-8.8s [%-2.2s] %-8.8s %-16.16s [%5d] %s\n",
	   u.ut_user,u.ut_id,u.ut_line,host(u.ut_host,u.ut_addr),u.ut_pid & 0xFFFF,
	   (u.ut_type>0 && u.ut_type<9?ltype[u.ut_type]:num(u.ut_type)));

/*
    printf("%-8.8s [%-2.2s] %-12.12s %3d.%03d.%03d.%03d [%5d] %s\n",
	   u.ut_user,u.ut_id,u.ut_line,
	   u.ut_addr&0xFF,(u.ut_addr>>8)&0xFF,(u.ut_addr>>16)&0xFF,(u.ut_addr>>24)&0xFF,
	   u.ut_pid & 0xFFFF,(u.ut_type>0 && u.ut_type<9?ltype[u.ut_type]:num(u.ut_type)));
*/
  }
}

void doit_tacacs3()
{
  struct tm *tm;
  struct tacacs3_utmp u;
  void *ut = &u;
  int n, m;

  if (dflg) printf("     ");
  printf("  Time        User         Line            Host\n");
  
  while (1) {
    for(n=m=read(fd,ut,sizeof(struct tacacs3_utmp));n < sizeof(struct tacacs3_utmp);n+=m=read(fd,ut+n,sizeof(struct tacacs3_utmp)-n)) {
      if (m < 0) {
	perror("rawtmp");
	return;
      }
      if (!m) return;
    }
    if (back && u.ut_time < backtime) continue;
    if (start && u.ut_time < sd) continue;
    if (end && u.ut_time > ed) continue;

    if (dflg) {
      tm = localtime(&u.ut_time);
      printf("%3s %02d %02d:%02d:%02d: ",
	     month[tm->tm_mon], tm->tm_mday, tm->tm_hour,
	     tm->tm_min, tm->tm_sec);
    } else printf ("%-10ld: ",u.ut_time);

    printf("%-8.8s  [%-12.12s]  %-16.16s\n",
	   u.ut_user,u.ut_line,u.ut_host);
  }
}

void doit_tacacs4()
{
  struct tm *tm;
  struct tacacs4_utmp u;
  void *ut = &u;
  int n, m;

  if (dflg) printf("     ");
  printf("  Time        User         Line            Host           Comment\n");
  
  while (1) {
    for(n=m=read(fd,ut,sizeof(struct tacacs4_utmp));n < sizeof(struct tacacs4_utmp);n+=m=read(fd,ut+n,sizeof(struct tacacs4_utmp)-n)) {
      if (m < 0) {
	perror("rawtmp");
	return;
      }
      if (!m) return;
    }
    if (back && u.ut_time < backtime) continue;
    if (start && u.ut_time < sd) continue;
    if (end && u.ut_time > ed) continue;

    if (dflg) {
      tm = localtime(&u.ut_time);
      printf("%3s %02d %02d:%02d:%02d: ",
	     month[tm->tm_mon], tm->tm_mday, tm->tm_hour,
	     tm->tm_min, tm->tm_sec);
    } else printf ("%-10ld: ",u.ut_time);

    printf("%-8.8s  [%-12.12s]  %-16.16s %-16.16s\n",
	   u.ut_user,u.ut_line,u.ut_host,u.ut_comment);
  }
}


char *num(n)
unsigned short n;
{
  static char nbuf[30];

  sprintf(nbuf,"UNKNOWN (%d)",n);
  return nbuf;
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

  tm.tm_mon = atoi(s) -1;
  tm.tm_mday = atoi(s+3);
  y = atoi(s+6);
  tm.tm_year = (y < 70? 100 + y : y);
  tm.tm_isdst = tm.tm_hour = tm.tm_min = tm.tm_sec = 0;

  t = mktime(&tm);
  if (t == (time_t)(-1)) usage(2);
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

char *host(h,a)
char *h;
u_long a;
{
  static char buf[20];

  if (!useaddr) return h;
  sprintf(buf,"%d.%d.%d.%d",(int)(a&255),(int)((a>>8)&255),(int)((a>>16)&255),(int)(a>>24));
  return buf;
}

void usage(n)
int n;
{
  switch (n) {
    case 1:
      fprintf(stderr,"usage: rawtmp [-w wtmp|-] [-X[3|4]d] [-s start] [-e end] [-b H[:M[:S]]]\n");
      break;
    case 2:
      fprintf(stderr,"rawtmp: Invalid date.  Format: mm/dd/yy\n");
      break;
    case 3:
      fprintf(stderr,"rawtmp: Invalid time.  Format: hours[:minutes[:seconds]]\n");
      break;
    case 4:
      fprintf(stderr,"usage: rawtmp [-w wtmp|-] [-X[3|4]d] [-s start] [-e end] [-b H[:M[:S]]]\n");
      fprintf(stderr,"    -w wtmp|-   Read alternate wtmp file.\n");
      fprintf(stderr,"    -X[3]       Read tacacs 3.x wtmp format.\n");
      fprintf(stderr,"    -X4         Read tacacs 4.0 wtmp format.\n");
      fprintf(stderr,"    -d          Output time in MMM DD HH:MM:SS date format.\n");
      fprintf(stderr,"    -s start    Display accounting info from `start'.\n");
      fprintf(stderr,"    -e end      Display accounting info up to `end'.\n");
      fprintf(stderr,"    -b H:M:S    Show accounting info from the last few hours/minutes/seconds.\n");
      break;
    case 5:
      {
        char *v = version+12;
	printf("%.*s\n",(int)strlen(v)-1,v);
	exit(0);
      }
  }
  exit(1);
}
