/*

   procinfo.c

   Displays general info from /proc.

   Date:        1995-07-08 05:30:34
   Last Change: 1996-04-28 20:12:52

   Copyright (c) 1994, 1995, 1996 svm@kozmix.ow.nl

   This software is released under the GNU Public Licence. See the
   file `COPYING' for details. Since you're probably running Linux I'm
   sure your hard disk is already infested with copies of this file,
   but if not, mail me and I'll send you one.

   $Id: procinfo.c,v 1.33 1996/04/28 18:14:44 svm Exp svm $
 */

#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <unistd.h>
#include <signal.h>
#include <termios.h>
#include <termcap.h>
#include <time.h>
#include <stdlib.h>
#include <sys/time.h>

struct info new, old;

void init_terminal_data (void);
char *my_tgets (char *te);

char *cd;			/* clear to eos */
char *ce;			/* clear to eol */
char *cl;			/* clear screen */
char *cm;			/* move cursor */
char *ho;			/* home */
char *se;			/* standout off */
char *so;			/* standout on */
char *ve;			/* cursor on */
char *vi;			/* cursor off */
int co;				/* columns */
int li;				/* lines */
int sg;				/* cookie width */

int fs = 0, redrawn = 0;
int show_stat = 0;
int show_moddev = 0;
int show_sys = 0;
int show_all = 0;
int show_diff = 0;
int show_new_mem = 0;
int irq_array = 0;
int io_or_blk = 0;
int have_m_c = 0;

FILE *loadavgfp, *meminfofp, *modulesfp, *statfp, *uptimefp, *devicesfp,
   *filesystemsfp, *interruptsfp, *dmafp, *cmdlinefp;

char line[1024], cmdline[1024];

FILE *versionfp;
char *version = NULL;

float rate = 1.0;		/* per interval or per sec */

char booted[40];

#include "procinfo.h"

struct info new, old;

static void
first_page (long sl)
{
    long elapsed;
    char loadavg[32];
    int i;

/*=== /proc/uptime ===*/

    fseek (uptimefp, 0L, SEEK_SET);
    fgets (line, sizeof (line), uptimefp);
    new.uptime = (long) (atof (strtok (line, " ")) * 100.0);

/*=== /proc/meminfo ===*/

#if 0 /* Used to work... */
    fseek (meminfofp, 0L, SEEK_SET);
#else
    fclose (meminfofp);
    meminfofp = myfopen (PROC_DIR "meminfo");
#endif
    fgets (line, sizeof (line), meminfofp);
    if (strstr (line, "cached:"))
	have_m_c = 1;

    printf ("Memory:      Total        Used        Free      "
	    "Shared     Buffers      %s\n", have_m_c ? "Cached" : "");
    fgets (line, sizeof (line), meminfofp);
    strtok (line, " ");
    new.m_to = VAL / 1024;
    new.m_us = VAL / 1024;
    new.m_fr = VAL / 1024;
    new.m_sh = VAL / 1024;
    new.m_bu = VAL / 1024;
    if (have_m_c)
	new.m_ca = VAL / 1024;
    if (show_new_mem) {
	printf ("Mem:  %12ld%12ld%12ld%12ld%12ld",
		new.m_to, new.m_us, new.m_fr, new.m_sh,
		new.m_bu);
	if (have_m_c)
	    printf ("%12ld\n", new.m_ca);
	else
	    putchar ('\n');
    } else {

/* We're going to cheat a bit by replacing rate with sl/1e6 in the
   DIFF macro, so that we'll end up with nice multiples of 4 when
   using the -d option. */

#define rate (sl/1000000)
	printf ("Mem:  %12ld%12ld%12ld%12ld%12ld",
		DIFF (m_to), DIFF (m_us), DIFF (m_fr), DIFF(m_sh), DIFF (m_bu));
	if (have_m_c)
	    printf ("%12ld\n", DIFF (m_ca));
	else
	    putchar ('\n');
#undef rate
    }
    fgets (line, sizeof (line), meminfofp);
    strtok (line, " ");
    new.s_to = VAL / 1024;
    new.s_us = VAL / 1024;
    new.s_fr = VAL / 1024;
    if (show_new_mem) {
	printf ("Swap: %12ld%12ld%12ld\n\n",
		new.s_to, new.s_us, new.s_fr);
    } else {
	printf ("Swap: %12ld%12ld%12ld\n\n",
		DIFF (s_to), DIFF (s_us), DIFF (s_fr));
    }
/*    fgets (line, sizeof (line), meminfofp); */

    fseek (loadavgfp, 0L, SEEK_SET);
    fgets (line, sizeof (line), loadavgfp);
    strcpy (loadavg, line);
    loadavg[strlen (loadavg) - 1] = '\0';
    fgets (line, sizeof (line), loadavgfp);

    printf ("Bootup: %s    Load average: %s\n\n", booted, loadavg);

/*=== /proc/stat ===*/

    fseek (statfp, 0L, SEEK_SET);
    while (fgets (line, sizeof (line), statfp)) {
	char *type = strtok (line, " ");

	if (ISSTR ("cpu")) {
	    new.cpu_user = VAL;
	    new.cpu_nice = VAL;
	    new.cpu_sys = VAL;
	    new.cpu_idle = VAL;
	} else if (ISSTR ("disk")) {
	    new.disk[0] = VAL;
	    new.disk[1] = VAL;
	    new.disk[2] = VAL;
	    new.disk[3] = VAL;
	} else if (ISSTR ("disk_rio") && io_or_blk == 0) {
	    new.disk_r[0] = VAL;
	    new.disk_r[1] = VAL;
	    new.disk_r[2] = VAL;
	    new.disk_r[3] = VAL;
	} else if (ISSTR ("disk_wio") && io_or_blk == 0) {
	    new.disk_w[0] = VAL;
	    new.disk_w[1] = VAL;
	    new.disk_w[2] = VAL;
	    new.disk_w[3] = VAL;
	} else if (ISSTR ("disk_rblk") && io_or_blk == 1) {
	    new.disk_r[0] = VAL;
	    new.disk_r[1] = VAL;
	    new.disk_r[2] = VAL;
	    new.disk_r[3] = VAL;
	} else if (ISSTR ("disk_wblk") && io_or_blk == 1) {
	    new.disk_w[0] = VAL;
	    new.disk_w[1] = VAL;
	    new.disk_w[2] = VAL;
	    new.disk_w[3] = VAL;
	} else if (ISSTR ("page")) {
	    new.pgin = VAL;
	    new.pgout = VAL;
	} else if (ISSTR ("swap")) {
	    new.swin = VAL;
	    new.swout = VAL;
	} else if (ISSTR ("intr")) {
	    if (irq_array) {
		VAL;		/* First value is total of all interrupts,
				   for compatibility with rpc.rstatd. We
				   ignore it. */
		for (i = 0; i < 16; i++)
		    new.intr[i] = VAL;
	    } else
		new.old_intr = VAL;
	} else if (ISSTR ("ctxt"))
	    new.ctxt = VAL;
    }

    elapsed = new.uptime;

    if (irq_array) {
	if (fs && old.uptime)
	    elapsed = DIFF (intr[0]);
    } else {
	if (fs && old.uptime)
	   /* This won't be exact... */
	    elapsed = 100 * sl;
    }

    printf ("user  : %s %s",
	    hms (DIFF (cpu_user)), perc (DIFF (cpu_user), elapsed));
    printf ("  page in : %8ld", DIFF (pgin));
    if (new.disk_r[0])
	printf ("  disk 1: %8ldr%8ldw\n", DIFF (disk_r[0]),
		DIFF (disk_w[0]));
    else if (new.disk[0])
	printf ("  disk 1: %8ld\n", DIFF (disk[0]));
    else
	putchar ('\n');

    printf ("nice  : %s %s",
	    hms (DIFF (cpu_nice)), perc (DIFF (cpu_nice), elapsed));
    printf ("  page out: %8ld", DIFF (pgout));
    if (new.disk_r[1])
	printf ("  disk 2: %8ldr%8ldw\n", DIFF (disk_r[1]),
		DIFF (disk_w[1]));
    else if (new.disk[1])
	printf ("  disk 2: %8ld\n", DIFF (disk[1]));
    else
	putchar ('\n');

    printf ("system: %s %s",
	    hms (DIFF (cpu_sys)), perc (DIFF (cpu_sys), elapsed));
    printf ("  swap in : %8ld", DIFF (swin));
    if (new.disk_r[2])
	printf ("  disk 3: %8ldr%8ldw\n", DIFF (disk_r[2]),
		DIFF (disk_w[2]));
    else if (new.disk[2])
	printf ("  disk 3: %8ld\n", DIFF (disk[2]));
    else
	putchar ('\n');

    printf ("idle  : %s %s",
	    hms (DIFF (cpu_idle)), perc (DIFF (cpu_idle), elapsed));
    printf ("  swap out: %8ld", DIFF (swout));
    if (new.disk_r[3])
	printf ("  disk 4: %8ldr%8ldw\n", DIFF (disk_r[3]),
		DIFF (disk_w[3]));
    else if (new.disk[3])
	printf ("  disk 4: %8ld\n", DIFF (disk[3]));
    else
	putchar ('\n');

    printf ("uptime: %s         context : %8ld", hms (new.uptime),
	    DIFF (ctxt));

    if (irq_array) {
	char irq_label[22][16];

	if (interruptsfp) {
	    int i;

	    for (i = 0; i < 16; i++)
		irq_label[i][0] = '\0';

	    fseek (interruptsfp, 0L, SEEK_SET);
	    while (fgets (line, sizeof (line), interruptsfp)) {
		char *p;
		ptrdiff_t off;

		i = atol (strtok (line, ":"));
		p = strtok (NULL, "\n");
		off = p - line;
		strncpy (irq_label[i], &line[12 + off], 20);
		if (line[10 + off] == '+')
		    strcat (irq_label[i], "+");
	    }
	}
/*=== /proc/dma ===*/

	if (dmafp) {
	    int i;
	    char tmp[22];

	    fseek (dmafp, 0L, SEEK_SET);
	    while (fgets (line, sizeof (line), dmafp)) {
		int foo = strcspn (&line[4], " \n");

		for (i = 0; i < 16; i++) {
		    if (strncmp (&line[4], irq_label[i],
				 foo) == 0) {
			sprintf (tmp, " [%ld]",
				 atol (strtok (line, ":")));
			strcat (irq_label[i], tmp);
		    }
		}
	    }
	}
	fputs ("\n\n", stdout);
	for (i = 0; i < 8; i++)
	    printf ("irq %2d: %9ld %-21s "
		    "irq %2d: %9ld %-21s\n",
		    i, DIFF (intr[i]), irq_label[i],
		    i + 8, DIFF (intr[i + 8]), irq_label[i + 8]);
    } else
	printf ("\tinterrupts: %8ld\n", DIFF (old_intr));

}

static void
second_page (void)
{
    int barf = 0;

    if (show_all)
	putchar ('\n');

   /* We're not fclosing cmdlinefp because we're using it later as a check. */
    fgets (cmdline, 1024, cmdlinefp);

    if (cmdlinefp)
	printf ("Kernel Command Line:\n  %s\n", cmdline);

    fseek (modulesfp, 0L, SEEK_SET);
    printf ("Modules:\n");
    while (fgets (line, sizeof (line), modulesfp)) {
	char *mod;
	long pg;
	char *status, *tmp;
	int used = 0;

	if (line[0] == ' ')	/* This just showed up in 1.3.33 */
	    continue;
	tmp = strdup (line);
	barf += 20;
	mod = strtok (line, " ");
	pg = atol (strtok (NULL, " ")) * 4;
	status = strtok (NULL, ")");
	if (strchr (tmp, ']'))
	    used++;
	printf ("%c%4ld %1s%-10s   ",
		status ? status[2] : ' ', pg,
		used ? "*" : " ", mod);
	free (tmp);
    }
   /* if (co && (barf % co)) */
    printf ("%s\n", fs ? ce : "");

    if (devicesfp) {
	int maj[2][MAX_DEV];
	char *n[2][MAX_DEV];
	int count[2] =
	{0, 0};
	int which = 0, i, j;

	memset (n, 0, sizeof (n));
	fseek (devicesfp, 0L, SEEK_SET);
	printf ("%s\nCharacter Devices:                      "
		"Block Devices:\n",
		fs ? ce : "");
	while (fgets (line, sizeof (line), devicesfp)) {
	    switch (line[0]) {
	    case 'C':
		which = CDRV;
		break;
	    case 'B':
		which = BDRV;
		break;
	    case '\n':
		break;
	    default:
		maj[which][count[which]] =
		    atoi (strtok (line, " "));
		n[which][count[which]] =
		    strdup (strtok (NULL, "\n"));
		count[which]++;
		break;
	    }
	}

	j = (1 + MAX (count[0], count[1])) / 2;
	for (i = 0; i < j; i++) {
	    if (n[CDRV][i]) {
		printf ("%2d %-16s ", maj[CDRV][i], n[CDRV][i]);
		free (n[CDRV][i]);
	    } else
		fputs ("                    ", stdout);
	    if (n[CDRV][i + j]) {
		printf ("%2d %-16s ",
			maj[CDRV][i + j], n[CDRV][i + j]);
		free (n[CDRV][i + j]);
	    } else
		fputs ("                    ", stdout);
	    if (n[BDRV][i]) {
		printf ("%2d %-16s ", maj[BDRV][i], n[BDRV][i]);
		free (n[BDRV][i]);
	    } else
		fputs ("                     ", stdout);
	    if (n[BDRV][i + j]) {
		printf ("%2d %-16s ",
			maj[BDRV][i + j], n[BDRV][i + j]);
		free (n[BDRV][i + j]);
	    }
	    printf ("%s\n", fs ? ce : "");
	    if (i >= count[CDRV] && i >= count[BDRV])
		break;
	}
    }				/* devicesfp */
    if (filesystemsfp) {
	barf = 0;
	fseek (filesystemsfp, 0L, SEEK_SET);
	printf ("%s\n", fs ? ce : "");
	printf ("File Systems:%s\n", fs ? ce : "");
	while (fgets (line, sizeof (line), filesystemsfp)) {
	    char *fs;
	    char tmp[21];

	    barf += 20;
	    fs = strchr (line, '\t');
	    fs = strtok (fs + 1, "\n");
	    if (line[0] == 'n') {
		sprintf (tmp, "[%s]", fs);
		printf ("%-19s", tmp);
	    } else
		printf ("%-19s", fs);
	    if (barf == 80)
		putchar ('\n');
	    else
		putchar (' ');
	}
	if (co && (barf % co))
	    printf ("%s\n", fs ? ce : "");
    }				/* filesystemsfp */
}

int
main (int ac, char **av)
{
    long sl = 5000000L;
    int getoptopt;

#ifndef DEBUG
    char outbuf[4096];

#endif
    int i;			/* random int */
    struct timeval then, now;

    while ((getoptopt = getopt (ac, av, "fn:msadDF:bhv")) != EOF) {
	switch (getoptopt) {
	case 'n':
	    sl = (long) (atof (optarg) * 1000000.0);
	   /* PLUMMET */
	case 'f':
	    fs = 1;
	    break;
	case 'm':
	    show_moddev = 1;
	    break;
	case 's':
	    show_stat = 1;
	    break;
	case 'a':
	    show_all = 1;
	    break;
	case 'F':
	    if ((freopen (optarg, "wb", stdout)) == NULL) {
		fprintf (stderr, "%s: ", av[0]);
		perror ("unable to open new output file");
		exit (errno);
	    }
	    break;
	case 'D':
	    show_new_mem = fs = 1;	/* show new memory & page stats, diff
					   on rest */
	case 'd':
	    show_diff = fs = 1;
	    break;
	case 'b':
	    io_or_blk = 1;
	    break;
	case 'v':
	    printf ("This is procinfo version " VERSION "\n");
	    exit (0);
	case 'h':
	default:
	    printf ("procinfo version %s\n"
		    "usage: %s [-fmadDsvh] [-nN] [-F<file>]\n"
		    "\n"
		    "\t-f\trun full screen\n"
		    "\t-nN\tpause N second between updates (implies -f)\n"
		    "\t-m\tdisplay module and device info\n"
		    "\t-s\tdisplay /proc/sys/ info\n"
		    "\t-a\tdisplay all info\n"
		  "\t-d\tshow differences rather than totals (implies -f)\n"
	       "\t-D\tshow current memory/swap usage, differences on rest\n"
		    "\t-F<file>  print output to file -- normally a tty\n"
		    "\t-v\tprint version info\n"
		    "\t-h\tprint this help\n",
		    VERSION, av[0]);
	    exit (getoptopt == 'h' ? 0 : 1);
	}
    }

    if (show_moddev + show_sys == 0)
	show_stat = 1;
    if (show_all)
	show_stat = show_moddev = show_sys = 1;

    if (sl == 0)
	nice (-20);

    for (i = 1; i < NSIG; i++)
	signal (i, bye);
    signal (SIGTSTP, tstp);
    signal (SIGCONT, cont);

    versionfp = myfopen (PROC_DIR "version");
    uptimefp = myfopen (PROC_DIR "uptime");
    loadavgfp = myfopen (PROC_DIR "loadavg");
    meminfofp = myfopen (PROC_DIR "meminfo");
    statfp = myfopen (PROC_DIR "stat");
    modulesfp = myfopen (PROC_DIR "modules");
   /* These may be missing, so check for NULL later. */
    devicesfp = fopen (PROC_DIR "devices", "r");
    filesystemsfp = fopen (PROC_DIR "filesystems", "r");
    interruptsfp = fopen (PROC_DIR "interrupts", "r");
    dmafp = fopen (PROC_DIR "dma", "r");
    cmdlinefp = fopen (PROC_DIR "cmdline", "r");

#ifndef DEBUG
    setvbuf (stdout, outbuf, _IOFBF, sizeof (outbuf));
#endif
    init_terminal_data ();
    window_init (0);
    cd = my_tgets ("cd");
    ce = my_tgets ("ce");
    cl = my_tgets ("cl");
    cm = my_tgets ("cm");
    ho = my_tgets ("ho");
    se = my_tgets ("se");
    so = my_tgets ("so");
    ve = my_tgets ("ve");
    vi = my_tgets ("vi");
    sg = tgetnum ("sg");	/* Oh god. */

/*
   If you're suffering from terminals with magic cookies, 1) you have
   my sympathy, and 2) you might wish just to forget about standout
   altogether. In that case, define COOKIE_NOSOSE.
 */

#ifdef COOKIE_NOSOSE
    if (sg > 0) {
	sg = 0;			/* Just forget about se/so */
	se = so = "";
    }
#endif
    if (sg < 0)
	sg = 0;

   /* Gets called from window_init, but in case stdout is redirected: */
    version = make_version (versionfp);

   /* See what the intr line in /proc/stat says. */
    while (fgets (line, sizeof (line), statfp)) {
	if (!strncmp ("intr", line, 4)) {
	   /* If this line has a space somewhere after line[5], it's a
	      new-style intr. */
	    if (strchr (&line[5], ' '))
		irq_array = 1;
	    continue;
	}
       /* While we're at it, fill in booted. */
	else if (!strncmp ("btime", line, 5)) {
	    time_t btime;

	    btime = (time_t) atol (&line[6]);
	    strftime (booted, sizeof (booted), "%c", localtime (&btime));
	    continue;
	}
    }

    if (fs)
	printf ("%s%s", cl, vi);

    gettimeofday (&now, 0);
    while (42) {
	if (redrawn) {
	    redrawn = 0;
	    fputs (cl, stdout);
	}
	if (fs)
	    printf ("%s%s%*s%s\n\n", ho, so, -(co - 2 * sg), version, se);
	else
	    printf ("%s\n\n", version);

	if (show_stat)
	    first_page (sl);

	if (show_moddev)
	    second_page ();

	if (fs) {
	    fputs (cd, stdout);
	    fflush (stdout);
	    if (sl)
		usleep (sl);

	    then = now;
	    gettimeofday (&now, 0);
	    rate = (float) now.tv_sec + (float) now.tv_usec / 1.0e6 -
		(float) then.tv_sec - (float) then.tv_usec / 1.0e6;

	} else {
	    putchar ('\n');
	    exit (0);
	}
	memcpy (&old, &new, sizeof (struct info));
    }				/* 42 */
}

/*
   Local variables:
   rm-trailing-spaces: t
   End:
 */
