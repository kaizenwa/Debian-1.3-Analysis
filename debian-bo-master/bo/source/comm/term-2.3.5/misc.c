#define I_IOCTL
#define I_SYS
#define	I_TTY
#define I_SIGNAL
#define I_LIMITS
#define I_CTYPE
#include "includes.h"
#include "debug.h"


extern char breakout_string[256];

void update_time(void) {
  struct timeval t;
  extern int bytes_left;
  unsigned long old_time = current_time;
  
  gettime(&t);
  current_time = ((unsigned long)t.tv_sec) * (unsigned long)term_inc + 
		(unsigned long)(t.tv_usec / (1000000 / (unsigned long)term_inc));
  if (max_cps) {
    bytes_left += ((current_time - old_time) * max_cps) / term_inc;
    if (bytes_left > block_size) bytes_left = block_size;
  }else {
    bytes_left = block_size;
  }
  if (bytes_left < 0) bytes_left = 0;/* sanity check */
}

void do_debug(int level, char *c) {
/*  fprintf(stderr, "%s\n", c); */
  return;
}

void do_noise(int a) {
  static int count=0;
  extern int modem_noise;

  if (modem_noise) modem_noise++;

	/* This routine tries to print things out in a readable format */
	/* without ever outputting the breakout string or upper case on */
	/* the remote machine. */
  if (!write_noise && !do_shutdown) return;
  a ^= byte_shift;

  if (do_shutdown) {
    fprintf(stderr, "%c",a);
  }else if ((a != '\b' && a != '\t' && a != '\n' &&
      a != '\r' && a < ' ') || a > 127) {
    fprintf(stderr, "<%u>", a);
    count = 0;
  }else if ( !remote ) {
    fprintf(stderr, "%c",a);
  }else {
    a = tolower(a);
    if (a != breakout_string[count]) {
      count = 0;
      fprintf(stderr, "%c", a);
    }else if ( count++ > 4 || escapes[a]) {
      if ( a != toupper(a) && a >= 'A' && a < 'A'+MAX_TYPE)
        fprintf(stderr, "%c", toupper(a));
      else 
        fprintf(stderr, "<%d>", a);
      count = 0;
    }else { 
       fprintf(stderr, "%c", a);
    }
  }
}

void do_alert(char *s) {
  if (remote) return;
  fprintf(stderr, "%s", s);
}

/* rebuild the arg list
 * compliment to build_arg()
 *
 * by: croutons
 * 
 * mallocs the array that is returned.
 * see build_arg() for other assumptions.
 */
char ** rebuild_arg(char * f)
{
	int i,s;
	static char ** a;

        if (a) free(a);
	DEBUG_FP(stderr,"%s:rebuild :%s:\n", term_server, f);
	if ( ! f || !f[0]) return NULL;
	for ( s = 0, i = 2; '\0' != f[s]; s++ ) { 
/*		if ( '\xff' == f[s] ) { Ultrix MIPS compiler chokes on this */
		if ( '\377' == f[s] ) {
			i++;
		}
	}
	if ( NULL == ( a = (char**) malloc(i*sizeof(char*)) ) ) {
		return NULL;
	}
	a[0]=f;
	for ( s = i = 0; '\0' != f[s]; s++ ) {
/*		if ( '\xff' == f[s] ) { */
		if ( '\377' == f[s] ) {
			f[s] = '\0';
			a[++i] = &f[s+1];
		}
	}
	a[i] = NULL;
	return a;
}

#ifdef USE_SIGWINCH
void do_resize(int number, int rows, int cols, int ypixels, int xpixels)
{
  int i;
#ifdef USE_WINCHKILL
  int pg;
#endif

  for (i=0; i < MAX_CLIENTS; i++)
    {
      if (clients[i].fd >= 0 && clients[i].number == number)
	{
#ifdef TIOCSWINSZ
	  struct winsize ws;

	  ws.ws_row = rows;
	  ws.ws_col = cols;
	  ws.ws_ypixel = ypixels;
	  ws.ws_xpixel = xpixels;
	  ioctl(clients[i].fd, TIOCSWINSZ, &ws);
#else
#ifdef TIOCSSIZE
	  struct ttysize ts;

	  ts.ts_lines = rows;
	  ts.ts_cols = cols;
	  ts.ts_yyy = ypixels;
	  ts.ts_xxx = xpixels;
	  ioctl(clients[i].fd, TIOCSSIZE, &ts);
#endif
#endif

#ifdef USE_WINCHKILL
#ifdef SYSV
	  pg = getpgid(clients[i].pid);
	  if (pg > 0)
	      kill(pg, SIGWINCH);
#else
	  pg = getpgrp(clients[i].pid);
	  if (pg > 0)
	      killpg(pg, SIGWINCH);
#endif
#endif	/* USE_WINCHKILL */
	}
    }

    return;
}
#endif /* USE_SIGWINCH */

