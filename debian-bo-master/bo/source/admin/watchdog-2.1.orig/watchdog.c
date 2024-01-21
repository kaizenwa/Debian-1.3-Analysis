/*************************************************************/
/* Original version was an example in the kernel source tree */
/*                                                           */
/* Rest was written by me, Michael Meskes                    */
/* meskes@informatik.rwth-aachen.de                          */
/*                                                           */
/*************************************************************/
#include "version.h"
#include <sys/types.h>
#include <sys/wait.h>
#include <linux/quota.h>
#include <syscall.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#if defined(USE_SYSLOG)
#include <syslog.h>
#endif				/* USE_SYSLOG */
#include <paths.h>
#include <utmp.h>
#include <mntent.h>

#if !defined(PIDFILE)
#define PIDFILE "/var/run/watchdog.pid"
#endif				/* !PIDFILE */

#if !defined(DEVNAME)
#define DEVNAME "/dev/watchdog"
#endif				/* !DEVNAME */

#if !defined(TIMER_MARGIN)
#define TIMER_MARGIN 60
#endif				/* !defined TIMER_MARGIN */

#define TRUE 1
#define FALSE 0

extern int umountmain(int argc, char *argv[]);

int softdog, softboot = FALSE;
char *devname = DEVNAME;
char *progname;
int maxload1 = MAXLOAD, maxload5 = MAXLOAD * 3 / 4, maxload15 = MAXLOAD / 2;

/* write a log entry on exit */
void log_end()
{
#if defined(USE_SYSLOG)
    /* Log the closinging message */
    syslog(LOG_INFO, "stopping daemon (%d.%d)", MAJOR_VERSION, MINOR_VERSION);
    closelog();
#endif				/* USE_SYSLOG */
    return;
}

/* close the device and check for error */
void close_dev()
{
    int err;

    err = close(softdog);
    if (err == -1) {
#if defined(USE_SYSLOG)
	syslog(LOG_ALERT, "cannot close %s", devname);
#else				/* USE_SYSLOG */
	perror(progname);
#endif				/* USE_SYSLOG */
    }
    return;
}

/* on exit we close the device and log that we stop */
void terminate(int arg)
{
    close_dev();
    log_end();
    exit(0);
}

void usage(void)
{
    fprintf(stderr, "%s version %d.%d, usage:\n", progname, MAJOR_VERSION, MINOR_VERSION);
#if defined(USE_SYSLOG)
    fprintf(stderr, "%s [-i <interval> [-f]] [-n <file-name>] [-d <dev-name>] [-l <max loadavg>] [-v] [-s]\n", progname);
#else				/* USE_SYSLOG */
    fprintf(stderr, "%s [-i <interval> [-f]] [-n <file-name>] [-d <dev-name>] [-l <max loadavg>] [-s]\n", progname);
#endif				/* USE_SYSLOG */
    exit(1);
}

/* get the basename of file <name> */
char *basename(char *name)
{
    char *tmp = strrchr(name, '/');

    return (tmp ? tmp + 1 : name);
}

/* panic: we're still alive but shouldn't */
void panic(void)
{
    /* if we are still alive, we just exit */
    close_dev();
    fprintf(stderr, "WATCHDOG PANIC: Still alive after sleeping %d seconds!\n", 4 * TIMER_MARGIN);
#if defined(USE_SYSLOG)
    openlog(progname, LOG_PID, LOG_DAEMON);
    syslog(LOG_ALERT, "still alive after sleeping %d seconds", 4 * TIMER_MARGIN);
    closelog();
#endif
    exit(1);
}

void mnt_off()
{
    FILE *fp;
    struct mntent *mnt;

    fp = setmntent(MNTTAB, "r");
    while ((mnt = getmntent(fp)) != (struct mntent *) 0) {
	/* First check if swap */
	if (!strcmp(mnt->mnt_type, MNTTYPE_SWAP))
	    if (swapoff(mnt->mnt_fsname) < 0)
		perror(mnt->mnt_fsname);

	/* quota only if mounted at boot time && filesytem=ext2 */
	if (hasmntopt(mnt, MNTOPT_NOAUTO) || strcmp(mnt->mnt_type, MNTTYPE_EXT2))
	    continue;

	/* group quota? */
	if (hasmntopt(mnt, MNTOPT_GRPQUOTA))
	    if (quotactl(QCMD(Q_QUOTAOFF, GRPQUOTA), mnt->mnt_fsname, 0, (caddr_t) 0) < 0)
		perror(mnt->mnt_fsname);

	/* user quota */
	if (hasmntopt(mnt, MNTOPT_USRQUOTA))
	    if (quotactl(QCMD(Q_QUOTAOFF, USRQUOTA), mnt->mnt_fsname, 0, (caddr_t) 0) < 0)
		perror(mnt->mnt_fsname);

    }
    endmntent(fp);
}

void myreboot(void)
{
    reboot(0xfee1dead, 672274793, 0x01234567);
}

/* shut down the system */
void do_shutdown(void)
{
    int i = 0, fd;
    char *uav[3] =
    {"watchdog", "-a", NULL};

    /* soft-boot the system */
    /* first close the watchdog device */
    close_dev();
#if defined(USE_SYSLOG)
    /* now tell syslog what's happening */
    syslog(LOG_INFO, "shutting down the system");
    closelog();
#endif				/* USE_SYSLOG */

    /* We cannot start shutdown, since init might not be able to fork. */
    /* That would stop the reboot process. So we try rebooting the system */
    /* ourselves. Note, that it is very likely we cannot start any rc */
    /* script either, so we do it all here. */

    /* Start with closing the files. */
    for (i = 0; i < 3; i++)
	if (!isatty(i))
	    close(i);
    for (i = 3; i < 20; i++)
	close(i);
    close(255);

    /* Ignore all signals. */
    for (i = 1; i < _NSIG; i++)
	signal(i, SIG_IGN);

    /* First idle init. */
    if (kill(1, SIGTSTP) < 0) {
	/* Okay, this shouldn't happen, but if it does we still have to */
	/* cause the reboot */
	perror(progname);
    }
    /* Kill all processes. */

    (void) kill(-1, SIGTERM);
    sleep(3);
    (void) kill(-1, SIGKILL);
    sleep(1);			/* Give init the chance to collect zombies. */

    /* Record the fact that we're going down */
    if ((fd = open(_PATH_WTMP, O_WRONLY | O_APPEND)) >= 0) {
	time_t t;
	struct utmp wtmp;

	time(&t);
	strcpy(wtmp.ut_user, "shutdown");
	strcpy(wtmp.ut_line, "~");
	strcpy(wtmp.ut_id, "~~");
	wtmp.ut_pid = 0;
	wtmp.ut_type = RUN_LVL;
	wtmp.ut_time = t;
	write(fd, (char *) &wtmp, sizeof(wtmp));
	close(fd);
    }
    /* Turn off accounting */
    if (acct(NULL) < 0)
	perror(progname);

    /* Turn off quota and swap */
    mnt_off();

    /* umount will exit at the end, so we have to make sure reboot is called */
    /* from the exit code. Changing the umount code to use returns would     */
    /* make much more work, in particular when upgrading the umount version. */
    if (!atexit(myreboot))
	umountmain(2, uav);
    else
	myreboot();

    exit(0);
}

/* Try to sync */
void sync_system(void)
{
    sync();
    sync();
}

/* check if process table is full */
int check_fork(void)
{
    pid_t forkresult;
    
    forkresult = fork();
    if (!forkresult)
	exit(0);		/* child, exit immediately */
    else if (forkresult < 0) {	/* fork failed */
#if defined(USE_SYSLOG)
	syslog(LOG_ERR, "process table is full!");
#endif				/* USE_SYSLOG */
	return (TRUE);
    } else {
	/* fork was okay          */
	/* wait for child to stop */
	if (waitpid(forkresult, NULL, 0) < 0) {
#if defined(USE_SYSLOG)
	    syslog(LOG_ERR, "child %d does not exist", forkresult);
#else				/* USE_SYSLOG */
	    perror(progname);
#endif				/* USE_SYSLOG */
	    if (softboot)
		return (TRUE);
	}
    }
    return (FALSE);
}

int check_file(char *filename)
{
    /* in filemode stat file */
    if (filename != NULL) {
	struct stat buf;

	if (stat(filename, &buf) == -1) {
#if defined(USE_SYSLOG)
	    syslog(LOG_ERR, "cannot stat %s", filename);
#else				/* USE_SYSLOG */
	    perror(progname);
#endif				/* USE_SYSLOG */
	    if (softboot)
		return (TRUE);
	}
    }
    return (FALSE);
}

int check_load()
{
    int load, avg1, avg5, avg15;
    char buf[40];

    /* open the load average file */
    load = open("/proc/loadavg", O_RDONLY);
    if (load == -1) {
#if defined(USE_SYSLOG)
	syslog(LOG_ERR, "cannot open /proc/loadavg (errno =%d)", errno);
#else				/* USE_SYSLOG */
	perror(progname);
#endif				/* USE_SYSLOG */
	if (softboot)
	    return (TRUE);
    }
    /* read the line (there is only one) */
    if (read(load, buf, sizeof(buf)) < 0) {
#if defined(USE_SYSLOG)
	syslog(LOG_ERR, "read /proc/loadavg gave errno = %d", errno);
#else				/* USE_SYSLOG */
	perror(progname);
#endif				/* USE_SYSLOG */
	if (softboot)
	    return (TRUE);
    }
    /* we only care about integer values */
    avg1 = atoi(buf);
    avg5 = atoi(strchr(buf, ' '));
    avg15 = atoi(strchr(strchr(buf, ' ') + 1, ' '));

    if (close(load) < 0) {
#if defined(USE_SYSLOG)
	syslog(LOG_ERR, "close /proc/loadavg gave errno = %d", errno);
#else				/* USE_SYSLOG */
	perror(progname);
#endif				/* USE_SYSLOG */
	if (softboot)
	    return (TRUE);
    }
    if (avg1 > maxload1 || avg5 > maxload5 || avg15 > maxload15) {
#if defined(USE_SYSLOG)
	syslog(LOG_ERR, "loadavg %d %d %d > %d %d %d!", avg1, avg5, avg15,
	       maxload1, maxload5, maxload15);
#endif				/* USE_SYSLOG */
	return (TRUE);
    }
    return (FALSE);
}

int main(int argc, char *const argv[])
{
    FILE *fp;
    int c, tint = 10, force = FALSE, sync_it = FALSE;
    char *filename = NULL;
    char log[128];
#if defined(USE_SYSLOG)
    char *opts = "d:i:n:fsvbl:";
    int verbose = FALSE;
    long count = 0L;
#else				/* USE_SYSLOG */
    char *opts = "d:i:n:fsbl:";
#endif				/* USE_SYSLOG */

    progname = basename(argv[0]);
    /* check the options */
    while ((c = getopt(argc, argv, opts)) != EOF) {
	switch (c) {
	case 'n':
	    filename = optarg;
	    break;
	case 'd':
	    devname = optarg;
	    break;
	case 'i':
	    tint = atoi(optarg);
	    break;
	case 'f':
	    force = TRUE;
	    break;
	case 's':
	    sync_it = TRUE;
	    break;
	case 'b':
	    softboot = TRUE;
	    break;
	case 'l':
	    maxload1 = atoi(optarg);
	    maxload5 = maxload1 * 3 / 4;
	    maxload15 = maxload1 / 2;
	    break;
#if defined(USE_SYSLOG)
	case 'v':
	    verbose = TRUE;
	    break;
#endif				/* USE_SYSLOG */
	default:
	    usage();
	}
    }
    if (tint < 0)
	usage();

    if (tint >= TIMER_MARGIN && !force) {
	fprintf(stderr, "%s warning:\n", progname);
	fprintf(stderr, "This interval length might reboot the system while the process sleeps!\n");
	fprintf(stderr, "To force this interval length use the -f option.\n");
	exit(1);
    }
    if (maxload1 < MINLOAD && !force) {
	fprintf(stderr, "%s warning:\n", progname);
	fprintf(stderr, "Using this maximal load average might reboot the system to often!\n");
	fprintf(stderr, "To force this load average use the -f option.\n");
	exit(1);
    }
    if (fork())
	exit(0);

    /* Okay, we're a daemon     */
    /* but we're still attached to the tty */
    setsid();

    /* now we're free */
#if defined(USE_SYSLOG)
    /* with USE_SYSLOG we don't do any console IO */
    close(0);
    close(1);
    close(2);
    /* Log the starting message */
    openlog(progname, LOG_PID, LOG_DAEMON);
    sprintf(log, "starting daemon (%d.%d): ", MAJOR_VERSION, MINOR_VERSION);
    sprintf(log + strlen(log), "%s%ds%s%s mla=%d", (filename == NULL) ? "" : filename,
	    tint, sync_it ? " sync" : "", softboot ? " soft" : "", maxload1);
    syslog(LOG_INFO, log);
#endif				/* USE_SYSLOG */

    /* open the device */
    softdog = open(devname, O_WRONLY);
    if (softdog == -1) {
#if defined(USE_SYSLOG)
	syslog(LOG_ERR, "cannot open %s", devname);
	log_end();
#else				/* USE_SYSLOG */
	perror(progname);
#endif				/* USE_SYSLOG */
	exit(1);
    }
    /* tuck my process id away */
    fp = fopen(PIDFILE, "w");
    if (fp != NULL) {
	fprintf(fp, "%d\n", getpid());
	(void) fclose(fp);
    }
    /* set signal term to call terminate() */
    /* to make sure softdog device is closed */
    signal(SIGTERM, terminate);

    /* main loop: update after <tint> seconds */
    while (1) {
	/* write to the softdog device */
	if (write(softdog, "\0", 1) < 0) {
#if defined(USE_SYSLOG)
	    syslog(LOG_ERR, "write watchdog device gave error %d!", errno);
#endif
	    if (softboot)
		do_shutdown();
	}
	/* do verbose logging */
#if defined(USE_SYSLOG)
	if (verbose) {
	    count++;
	    syslog(LOG_INFO, "write (#%ld) to %s", count, devname);
	}
#endif				/* USE _SYSLOG */

	/* sync system if we have to */
	if (sync_it)
	    sync_system();

	/* write to the softdog device */
	if (write(softdog, "\0", 1) < 0) {
#if defined(USE_SYSLOG)
	    syslog(LOG_ERR, "write watchdog device gave error %d!", errno);
#endif
	    if (softboot)
		do_shutdown();
	}
	/* check process table */
	if (check_fork())
	    do_shutdown();

	/* write to the softdog device */
	if (write(softdog, "\0", 1) < 0) {
#if defined(USE_SYSLOG)
	    syslog(LOG_ERR, "write watchdog device gave error %d!", errno);
#endif
	    if (softboot)
		do_shutdown();
	}
	/* check load average */
	if (check_load())
	    do_shutdown();

	/* write to the softdog device */
	if (write(softdog, "\0", 1) < 0) {
#if defined(USE_SYSLOG)
	    syslog(LOG_ERR, "write watchdog device gave error %d!", errno);
#endif
	    if (softboot)
		do_shutdown();
	}
	/* in filemode stat file */
	if (check_file(filename))
	    do_shutdown();

	/* write to the softdog device */
	if (write(softdog, "\0", 1) < 0) {
#if defined(USE_SYSLOG)
	    syslog(LOG_ERR, "write watchdog device gave error %d!", errno);
#endif
	    if (softboot)
		do_shutdown();
	}
	/* finally sleep some seconds */
	sleep(tint);
    }
}
