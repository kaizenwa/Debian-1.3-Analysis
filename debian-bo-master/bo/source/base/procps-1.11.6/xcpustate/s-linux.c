/*
 * System dependent file for Linux
 * By Andy Burgess aab@cichlid.com 6-10-94
 * Adapted from s-bsd.c by Chris Siebenmann cks@white.toronto.edu 
 */

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <linux/version.h>

#define dprintf if(0)printf

#define MAX_STATES 5
#define MAX_BARS 10
#define MAX_PACKETS_PER_SECOND 200

/* Called at the beginning to inquire how many bars are needed. */
int
num_bars()
{
    return 4;
}

/* Called after num_bars to ask for the bar names */

char **
label_bars(nbars)
{	static char *name[MAX_BARS];
	int i = 0;

	name[i++] = "CPU   ";
#ifdef TEST
	name[i++] = "Test  ";
#endif
	name[i++] = "Memory";
	name[i++] = "Swap  ";
	name[i++] = "Ether ";
	return name;
}

/* 
 *  Called after the bars are created to perform any machine dependent
 *  initializations.
 */

int mem, pstat, net;

void
init_bars(nbars)
int nbars;
{
	if(nbars >= 1) {
	  mem = open("/proc/meminfo", O_RDONLY);
	  if(mem == -1) perror("open /proc/meminfo"), exit(1);
	}
	if(nbars >= 2) {
	  pstat = open("/proc/stat", O_RDONLY);
	  if(pstat == -1) perror("open /proc/stat"), exit(1);
	}
	if(nbars >= 4) {
	  net = open("/proc/net/dev", O_RDONLY);
	  if(net == -1) perror("open /proc/net/dev"), exit(1);
	}
}

/* 
 *  This procedure gets called every interval to compute and display the
 *  bars. It should call draw_bar() with the bar number, the array of
 *  integer values to display in the bar, and the number of values in
 *  the array.
 */

void
display_bars(nbars)
  int nbars;
{
	int spare, len, bar = 0, states[MAX_STATES], nstates, status;
	long user, nice, system, idle;
	static long prev_user, prev_nice, prev_system, prev_idle;
	long total, used, free, shared, buffers;
	long stotal, sused, sfree;
	long rpkt, tpkt, rerr, terr, coll;
	static long prev_rpkt, prev_tpkt, prev_coll;
	char *p, buf[5000];

	/*
	close(mem);
	close(pstat);
	init_bars();
	*/

	if(!nbars--)
	  return;
	lseek(pstat, 0, SEEK_SET);
	len = read(pstat, buf, sizeof(buf));
	buf[len] = 0;
	status = sscanf(buf, "%*s %ld %ld %ld %ld",
			&user, &nice, &system, &idle);
	dprintf("CPU: %ld %ld %ld %ld\n", idle, user, nice, system);
	if(status == EOF)
	  perror("sscanf /proc/stat"), exit(1);
	if(status != 4)
	  fprintf(stderr, "sscanf /proc/stat returned %d\n", status), exit(1);

	nstates = 0;
	states[nstates++] = idle - prev_idle; /* white */
	states[nstates++] = user - prev_user; /* grey */
	states[nstates++] = nice - prev_nice; /* dark grey */
	states[nstates++] = system - prev_system; /* black */
	draw_bar(bar++, states, nstates);
	prev_idle = idle;
	prev_user = user;
	prev_nice = nice;
	prev_system = system;

#ifdef TEST
	{ static int x = 0;
	  if(!nbars--)
	    return;
	  nstates = 0;
	  states[nstates++] = 10;
	  states[nstates++] = x++;
	  states[nstates++] = x++;
	  states[nstates++] = x++;
	  draw_bar(bar++, states, nstates);
	}
#endif
	if(!nbars--)
	  return;
	lseek(mem, 0, SEEK_SET);
	len = read(mem, buf, sizeof(buf));
	buf[len] = 0;
	status = sscanf(buf,
		 "%*[^\n] %*[^:]: %ld %ld %ld %ld %ld %*[^:]: %ld %ld %ld",
			&total, &used, &free, &shared, &buffers,
			&stotal, &sused, &sfree );
	if(status != 8) {
	  fprintf(stderr, "sscanf /proc/meminfo returned %d\nparsing:\n%s\n",
		  status, buf); 
	  exit(1);
	}
	if(status == EOF)
	  perror("fscanf /proc/meminfo"), exit(1);

	used -= buffers;
	/* big numbers seem to sometimes confuse draw_bar() */
	used /= 1000;
	free /= 1000;
	shared /= 1000;
	buffers /= 1000;
	dprintf("Memory: %ld %ld %ld %ld\n", free, used, shared, buffers);
	nstates = 0;
	states[nstates++] = free; /* white */
	states[nstates++] = buffers; /* grey */
	states[nstates++] = 0; /* shared; /* dark grey */
	states[nstates++] = used; /* black */
	draw_bar(bar++, states, nstates);

	if(!nbars--)
	  return;
	sfree /= 1000;
	sused /= 1000;
	dprintf("Swap: %ld %ld\n", sfree, sused);
	nstates = 0;
	states[nstates++] = sfree; /* white */
	states[nstates++] = sused; /* grey */
	states[nstates++] = 0; /* darkgrey */
	states[nstates++] = 0; /* black */
	draw_bar(bar++, states, nstates);
/*   
Inter-|   Receive                  |  Transmit
 face |packets errs drop fifo frame|packets errs drop fifo colls carrier
 eth0: 436694    0    0    0    0   499043    0    0    0    13    0
*/

	if(!nbars--)
	  return;
	lseek(net, 0, SEEK_SET);
	len = read(net, buf, sizeof(buf));
	buf[len] = 0;
	p = buf;
	/* find first ethernet/ppp/slip/lo device */
	if ((p = strstr(buf, "eth")) ||
	    (p = strstr(buf, "ppp")) ||
	    (p = strstr(buf, "sl")) ||
	    (p = strstr(buf, "lo")))
	  status = sscanf(p, "%*[^:]: %ld %ld %*d %*d %*d %ld %ld %*d %*d %*d %ld %*d",
			  &rpkt, &rerr, &tpkt, &terr, &coll);
	else /* no network device found */
	  rpkt = rerr = tpkt = terr = coll = 0;
	dprintf("Net: %ld %ld %ld %ld %ld\n", rpkt, rerr, tpkt, terr, coll);
	nstates = 0;
	total = rpkt + tpkt + coll - prev_rpkt - prev_tpkt - prev_coll;
	spare = MAX_PACKETS_PER_SECOND - total;
	if(spare < 0) spare = 0; /* white */
	states[nstates++] = spare; /* white */
	states[nstates++] = rpkt - prev_rpkt; /* grey */
	states[nstates++] = tpkt - prev_tpkt; /* darkgrey */
	states[nstates++] = coll - prev_coll; /* black */
	draw_bar(bar++, states, nstates);
	prev_rpkt = rpkt;
	prev_tpkt = tpkt;
	prev_coll = coll;
}
